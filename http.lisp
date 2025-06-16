;;;; ---------------------------------------------------------------------------
;;;;  Very small shim so we can continue writing (http-get …) just as we wrote
;;;;  (dex:get …) before.  Only the subset of keyword arguments actually used
;;;;  by ocicl is supported.  Anything else can be added later if required.
;;;; ---------------------------------------------------------------------------

(uiop:define-package #:ocicl.http
  (:use #:cl #:drakma)
  (:export #:http-get #:configure-drakma-proxy-from-env))

(in-package #:ocicl.http)

(defun %split-userinfo (authority)
  "Return two values USER and PASS (either may be NIL)."
  (when authority
    (let* ((at (position #\@ authority))
           (creds (when at (subseq authority 0 at))))
      (when creds
        (let ((colon (position #\: creds)))
          (if colon
              (values (subseq creds 0 colon)
                      (subseq creds (1+ colon)))
              (values creds nil)))))))

(defun configure-drakma-proxy-from-env
    (&key (proxy-vars '("HTTPS_PROXY" "https_proxy"
                        "HTTP_PROXY"  "http_proxy"))
          (no-proxy-vars '("NO_PROXY" "no_proxy")))
  "Populate Drakma’s proxy settings from traditional env vars."
  ;; ---- pick first non-empty proxy var -----------------------------------
  (let* ((raw (loop for v in proxy-vars
                    for val = (uiop:getenv v)
                    when (and val (string/= val "")) return val)))
    (when raw
      ;; allow host[:port] | user:pass@host[:port] | full URL
      (let* ((uri (puri:parse-uri
                   (if (search "://" raw) raw (format nil "http://~A" raw))))
             (host (puri:uri-host uri))
             (port (or (puri:uri-port uri) 80))
             (authority (puri:uri-authority uri)))
        (setf drakma:*default-http-proxy*
              (if port (list host port) host))
        (multiple-value-bind (user pass) (%split-userinfo authority)
          (setf *proxy-user* user
                *proxy-pass* pass)))))
  ;; ---- NO_PROXY ---------------------------------------------------------
  (let* ((raw (loop for v in no-proxy-vars
                    for val = (uiop:getenv v)
                    when (and val (string/= val "")) return val)))
    (when raw
      (setf drakma:*no-proxy-domains*
            (uiop:split-string raw :separator ",")))))

(defun header-alist->hash-table (alist)
  (let ((ht (make-hash-table :test #'equalp)))
    (dolist (h alist ht)
      (destructuring-bind (name . value) h
        (setf (gethash name ht) value)))))

(defun http-get (url &key headers force-string force-binary want-stream verbose)
  "Roughly emulates the subset of DEXADOR:GET used by ocicl.

  Returns BODY  STATUS  HEADERS just like DEXADOR:GET did.
  - BODY is a string unless WANT-STREAM is T.
  - STATUS is the numeric HTTP status code.
  - HEADERS is a hash-table whose keys are *string* header names."
  (setf verbose t)
  (when verbose
    (format verbose drakma:*no-proxy-domains*))
  (let ((old-header-stream drakma:*header-stream*))
    (unwind-protect
         (progn
           (when verbose
             (setf drakma:*header-stream* verbose))
           (handler-bind
               ((error (lambda (c)
                         (format *error-output* "~&~A: ~A~%" (type-of c) c)
                         (sb-debug:backtrace depth)
                         (invoke-debugger c))))
             (multiple-value-bind (body status-code response-headers _uri _stream)
                 ;; Drakma uses :additional-headers, not :headers.
                 (drakma:http-request url
                                      :method :get
                                      :additional-headers headers
                                      :want-stream want-stream
                                      :force-binary force-binary)
               ;; Convert Drakma’s header alist to the hash-table expected elsewhere.
               (let ((body (if (and force-string (not want-stream))
                               ;; ensure body is a Lisp string; leave it untouched otherwise
                               (babel:octets-to-string body :encoding :utf-8)
                               body)))
                 (print "===============================")
                 (print body)
                 (print "===============================")
                 (values body status-code (header-alist->hash-table response-headers))))))
           (setf drakma:*header-stream* old-header-stream))))
