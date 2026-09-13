;;; http.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025, 2026  Anthony Green <green@moxielogic.com>

;;;; ---------------------------------------------------------------------------
;;;;  Very small shim so we can continue writing (http-get …) just as we wrote
;;;;  (dex:get …) before.  Only the subset of keyword arguments actually used
;;;;  by ocicl is supported.  Anything else can be added later if required.
;;;; ---------------------------------------------------------------------------

(uiop:define-package #:ocicl.http
  (:use #:cl #:drakma)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:export #:http-get #:configure-drakma-proxy-from-env
           #:*verify-tls*
           #:call-with-transient-retries #:with-transient-retries))

(in-package #:ocicl.http)

(defvar *proxy-basic-auth* nil)
(defvar *verify-tls* t)

(defun %first-existing-file (paths)
  (loop for path in paths
        when (and path (probe-file path))
          return path))

(defun %first-existing-dir (paths)
  (loop for path in paths
        when (and path (uiop:directory-exists-p path))
          return path))

(defun %resolve-ca-locations ()
  "Return two values: CA-FILE and CA-DIRECTORY for TLS verification."
  (let* ((env-ca-file (uiop:getenv "OCICL_CA_FILE"))
         (env-ca-dir (uiop:getenv "OCICL_CA_DIR"))
         (ca-file (and env-ca-file (string/= env-ca-file "") env-ca-file))
         (ca-dir (and env-ca-dir (string/= env-ca-dir "") env-ca-dir)))
    (unless ca-file
      (setf ca-file
            (%first-existing-file
             '("/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"
               "/etc/pki/tls/certs/ca-bundle.crt"
               "/etc/ssl/certs/ca-certificates.crt"
               "/etc/ssl/cert.pem"))))
    (unless ca-dir
      (setf ca-dir
            (%first-existing-dir
             '("/etc/ssl/certs"
               "/etc/pki/ca-trust/extracted/pem"
               "/etc/pki/ca-trust/extracted/openssl"))))
    (values ca-file ca-dir)))

(defun %split-userinfo (authority)
  "Return two values USER and PASS (either may be NIL)."
  (when authority
    (let* ((at (position #\@ authority))
           (creds (when at (subseq authority 0 at))))
      (when creds
        (if-let ((colon (position #\: creds)))
          (values (subseq creds 0 colon)
                  (subseq creds (1+ colon)))
          (values creds nil))))))

(defun configure-drakma-proxy-from-env
    (&key (proxy-vars '("HTTPS_PROXY" "https_proxy"
                        "HTTP_PROXY"  "http_proxy"))
          (no-proxy-vars '("NO_PROXY" "no_proxy")))
  "Populate Drakma’s proxy settings from traditional env vars."
  ;; ---- pick first non-empty proxy var -----------------------------------
  (when-let ((raw (loop for v in proxy-vars
                        for val = (uiop:getenv v)
                        when (and val (string/= val "")) return val)))
      ;; allow host[:port] | user:pass@host[:port] | full URL
      (let* ((uri (puri:parse-uri
                   (if (search "://" raw) raw (format nil "http://~A" raw))))
             (host (puri:uri-host uri))
             (port (or (puri:uri-port uri) 80))
             (authority (puri:uri-authority uri)))
        (setf drakma:*default-http-proxy*
              (if port (list host port) host))
        (multiple-value-bind (user pass) (%split-userinfo authority)
          (setf *proxy-basic-auth* (when user (list user (or pass "")))))))
  ;; ---- NO_PROXY ---------------------------------------------------------
  (when-let ((raw (loop for v in no-proxy-vars
                        for val = (uiop:getenv v)
                        when (and val (string/= val "")) return val)))
    (setf drakma:*no-proxy-domains*
          (uiop:split-string raw :separator ","))))

(defun header-alist->hash-table (alist)
  "Convert HTTP header ALIST to a lowercase string-keyed hash table."
  (let ((ht (make-hash-table :test #'equalp)))
    (dolist (h alist ht)
      (destructuring-bind (name . value) h
        (setf (gethash
               (string-downcase
                (etypecase name
                  (string name)
                  (symbol (symbol-name name))))
               ht)
              value)))))

(define-condition http-fetch-error (error)
  ((message :initarg :message :reader http-fetch-error-message))
  (:report (lambda (condition stream)
             (write-string (http-fetch-error-message condition) stream))))

(define-condition tls-verification-failure (http-fetch-error)
  ())

(define-condition http-status-error (http-fetch-error)
  ((status :initarg :status :reader http-status-error-status)))

(defun %http-get-once (url &key headers force-string force-binary want-stream verbose)
  "Perform one HTTP GET of URL.  Returns BODY STATUS HEADERS.
  - BODY is a string unless WANT-STREAM is T (and only when STATUS < 400).
  - STATUS is the numeric HTTP status code.
  - HEADERS is a hash-table whose keys are *string* header names."
  (let ((drakma:*header-stream* (or verbose drakma:*header-stream*)))
    (multiple-value-bind (body status-code response-headers _uri _stream)
               (labels ((friendly-tls-message (cond)
                          (let* ((txt (princ-to-string cond))
                                 (host (ignore-errors (puri:uri-host (puri:parse-uri url)))))
                            (cond
                              ;; pure-tls specific errors
                              ((and *verify-tls*
                                    (or (search "certificate chain not anchored" txt :test #'char-equal)
                                        (search "trust-anchor" txt :test #'char-equal)))
                               (format nil "TLS verification failed: certificate not trusted for ~A. CA certificates may not be loaded. Try: 1) Install ca-certificates package, 2) Set OCICL_CA_FILE to your CA bundle path, or 3) Rebuild with USE_LEGACY_OPENSSL=1." (or host url))) ; lint:suppress max-line-length
                              ;; Generic TLS verification errors
                              ((and *verify-tls*
                                    (or (search "certificate verify failed" txt :test #'char-equal)
                                        (search "unable to get local issuer certificate" txt :test #'char-equal)
                                        (search "unknown ca" txt :test #'char-equal)))
                               (format nil "TLS verification failed: CA not trusted for ~A. Try setting OCICL_CA_FILE/OCICL_CA_DIR or use -k/--insecure for testing." (or host url))) ; lint:suppress max-line-length
                              ((and *verify-tls*
                                    (or (search "host" txt :test #'char-equal)
                                        (search "hostname" txt :test #'char-equal))
                                    (search "mismatch" txt :test #'char-equal))
                               (format nil "TLS verification failed: certificate name mismatch for ~A." (or host url)))
                              ((and *verify-tls* (search "self" txt :test #'char-equal)
                                    (search "signed" txt :test #'char-equal))
                               (format nil "TLS verification failed: self-signed certificate for ~A. Provide CA via OCICL_CA_FILE/OCICL_CA_DIR or use -k/--insecure for testing." (or host url))) ; lint:suppress max-line-length
                              ((or (search "expired" txt :test #'char-equal)
                                   (search "not yet valid" txt :test #'char-equal))
                               (format nil "TLS verification failed: server certificate time validity issue for ~A. Check system clock or server certificate." (or host url)))
                              ((or (search "timeout" txt :test #'char-equal)
                                   (search "timed out" txt :test #'char-equal))
                               (format nil "TLS connection timed out to ~A. Check network/proxy; consider setting OCICL_HTTP_TIMEOUT." (or host url)))
                              (t nil)))))
                 ;; Drakma uses :additional-headers, not :headers.
                 (let* ((verify (if *verify-tls* :required nil))
                        (ca-file nil)
                        (ca-dir nil)
                        (timeout-env (uiop:getenv "OCICL_HTTP_TIMEOUT"))
                        (conn-timeout (ignore-errors (and timeout-env (parse-integer timeout-env))))
                        (tls-debug (uiop:getenv "OCICL_TLS_DEBUG")))
                   (multiple-value-setq (ca-file ca-dir)
                     (%resolve-ca-locations))
                   (when verbose
                     (format verbose "; TLS verify=~A ca-file=~A ca-dir=~A~%"
                             verify ca-file ca-dir))
                   (when tls-debug
                     (format t "; TLS implementation: ~A~%"
                             (cond ((find-package :pure-tls) "pure-tls")
                                   ((find-package :cl+ssl) "cl+ssl")
                                   (t "unknown")))
                     (format t "; TLS verify=~A ca-file=~A ca-dir=~A~%"
                             verify ca-file ca-dir))
                   (handler-case
                       (drakma:http-request url
                                            :method :get
                                            :additional-headers headers
                                            :want-stream want-stream
                                            :force-binary force-binary
                                            :verify verify
                                            :ca-file ca-file
                                            :ca-directory ca-dir
                                            #+(or abcl clisp lispworks mcl openmcl sbcl)
                                            :connection-timeout
                                            #+(or abcl clisp lispworks mcl openmcl sbcl)
                                            conn-timeout
                                            :proxy-basic-authorization *proxy-basic-auth*)
                     #+pure-tls
                     (pure-tls:tls-verification-error (e)
                       (let* ((host (ignore-errors (puri:uri-host (puri:parse-uri url))))
                              (msg (format nil "TLS verification failed for ~A: ~A. Ensure CA certificates are installed (e.g., 'sudo dnf install ca-certificates'). You can also set OCICL_CA_FILE or rebuild with USE_LEGACY_OPENSSL=1." ; lint:suppress max-line-length
                                           (or host url) e)))
                         (when verbose
                           (format verbose "; underlying error: ~A~%" e))
                         (error 'tls-verification-failure :message msg)))
                     #+pure-tls
                     (pure-tls:tls-error (e)
                       (let* ((host (ignore-errors (puri:uri-host (puri:parse-uri url))))
                              (msg (friendly-tls-message e)))
                         (when verbose
                           (format verbose "; underlying error: ~A~%" e))
                         (error 'http-fetch-error
                                :message (or msg (format nil "TLS error for ~A: ~A" (or host url) e)))))
                     (error (e)
                       (let ((msg (friendly-tls-message e)))
                         (when verbose
                           (format verbose "; underlying error: ~A~%" e))
                         (if (and msg (uiop:string-prefix-p "TLS verification failed" msg))
                             (error 'tls-verification-failure :message msg)
                             (error 'http-fetch-error :message (or msg (princ-to-string e)))))))))
             ;; Convert Drakma’s header alist to the hash-table expected elsewhere.
             (let ((body (if (and force-string (not want-stream) (< status-code 400))
                             ;; ensure body is a Lisp string; leave it untouched otherwise
                             ;; (error bodies are discarded, so don't risk decoding them)
                             (babel:octets-to-string body :encoding :utf-8)
                             body)))
               (values body status-code (header-alist->hash-table response-headers))))))

(defvar *http-max-retries* 3
  "How many times to retry a transient HTTP failure, beyond the first attempt.")

(defvar *retry-output* *error-output*
  "Stream for retry diagnostics, or NIL to suppress them during live UI use.")

(defvar *retry-output-lock* (bt:make-lock "ocicl HTTP retry output"))

(defun %write-retry-diagnostic (what url delay)
  "Write one indivisible retry diagnostic when retry output is enabled."
  (when *retry-output*
    (bt:with-lock-held (*retry-output-lock*)
      (format *retry-output* "; ~A for ~A; retrying in ~As~%"
              what url delay)
      (finish-output *retry-output*))))

(defun %transient-http-status-p (status)
  (member status '(408 429 500 502 503 504)))

(defun %sleep-before-retry (what url attempt)
  (let ((delay (expt 2 attempt)))
    (%write-retry-diagnostic what url delay)
    (sleep delay)))

(defun http-get (url &key headers force-string force-binary want-stream verbose)
  "Roughly emulates the subset of DEXADOR:GET used by ocicl.

  Returns BODY  STATUS  HEADERS just like DEXADOR:GET did.
  - BODY is a string unless WANT-STREAM is T.
  - STATUS is the numeric HTTP status code.
  - HEADERS is a hash-table whose keys are *string* header names.

  A response status of 400 or greater signals HTTP-STATUS-ERROR.
  Transient failures (connection errors, HTTP 408/429/5xx) are retried
  up to *HTTP-MAX-RETRIES* times with exponential backoff; TLS
  verification failures are never retried."
  (loop for attempt from 0 upto *http-max-retries*
        do (block try
             (multiple-value-bind (body status response-headers)
                 (handler-case
                     (%http-get-once url :headers headers
                                         :force-string force-string
                                         :force-binary force-binary
                                         :want-stream want-stream
                                         :verbose verbose)
                   (tls-verification-failure (e)
                     (error e))
                   (error (e)
                     (when (>= attempt *http-max-retries*)
                       (error e))
                     (%sleep-before-retry e url attempt)
                     (return-from try)))
               (cond
                 ((< status 400)
                  (return-from http-get (values body status response-headers)))
                 (t
                  (when (and want-stream (streamp body))
                    (ignore-errors (close body)))
                  (when (and (%transient-http-status-p status)
                             (< attempt *http-max-retries*))
                    (%sleep-before-retry (format nil "HTTP ~A" status) url attempt)
                    (return-from try))
                  (error 'http-status-error
                         :status status
                         :message (format nil "HTTP ~A for ~A" status url))))))))

(defun call-with-transient-retries (thunk &key url)
  "Call THUNK, retrying transient failures with the same policy as HTTP-GET.

HTTP-GET's internal retry loop only covers the connect/request/header phase
of a :WANT-STREAM request — the caller reads the body from the returned
socket stream, so a mid-transfer failure (e.g. a CDN sending RST during a
blob download: \"Connection reset by peer\") escapes every retry path.
Wrapping the whole download-and-verify unit in this function closes that
gap; the unit must be idempotent (blob downloads are: each attempt writes a
fresh temp file and is digest-verified afterward).

Retries up to *HTTP-MAX-RETRIES* times with the shared exponential backoff.
Never retried: TLS verification failures, and HTTP status errors that are
not transient (4xx other than 408/429) — those are deterministic. Any other
error (stream/socket errors, transient statuses that exhausted HTTP-GET's
inner retries, digest mismatches from a corrupted transfer) is retried, and
rethrown once attempts are exhausted."
  (loop for attempt from 0 upto *http-max-retries*
        do (handler-case (return (funcall thunk))
             (tls-verification-failure (e)
               (error e))
             (http-status-error (e)
               (if (and (< attempt *http-max-retries*)
                        (%transient-http-status-p (http-status-error-status e)))
                   (%sleep-before-retry e (or url "download") attempt)
                   (error e)))
             (error (e)
               (when (>= attempt *http-max-retries*)
                 (error e))
               (%sleep-before-retry e (or url "download") attempt)))))

(defmacro with-transient-retries ((&key url) &body body)
  "Evaluate BODY via CALL-WITH-TRANSIENT-RETRIES."
  `(call-with-transient-retries (lambda () ,@body) :url ,url))
