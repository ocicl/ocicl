;;; acme-client.lisp --- Encapsulated ACME client
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Thread-safe ACME client with encapsulated state.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; ACME Client Structure
;;; ----------------------------------------------------------------------------

(defstruct (acme-client (:constructor %make-acme-client))
  "Encapsulated ACME client with all state.
   Thread-safe for concurrent certificate operations."
  ;; Configuration
  (directory-url *staging-url* :type string)
  (skip-tls-verify nil :type boolean)
  ;; State
  (directory nil)          ; Cached directory endpoints
  (account-key nil)        ; EC P-256 private key
  (account-url nil :type (or null string))
  (nonce nil :type (or null string))
  ;; Storage
  (store nil :type (or null cert-store))
  ;; Logging
  (logger nil)  ; Function: (lambda (level format-string &rest args))
  ;; Lock for thread safety
  (lock (bt:make-lock "acme-client")))

(defun make-acme-client (&key (directory-url *staging-url*)
                              (skip-tls-verify nil)
                              store
                              logger)
  "Create a new ACME client.

   DIRECTORY-URL: ACME directory URL (default: Let's Encrypt staging)
   SKIP-TLS-VERIFY: Skip TLS verification (for testing with Pebble)
   STORE: Certificate store (creates default if not provided)
   LOGGER: Logging function (lambda (level format-string &rest args))
           Levels: :debug :info :warn :error"
  (let ((client (%make-acme-client
                 :directory-url directory-url
                 :skip-tls-verify skip-tls-verify
                 :store (or store (make-cert-store))
                 :logger logger)))
    ;; Load account key from store
    (setf (acme-client-account-key client)
          (store-load-account-key (acme-client-store client)))
    client))

;;; ----------------------------------------------------------------------------
;;; Logging
;;; ----------------------------------------------------------------------------

(defun client-log (client level format-string &rest args)
  "Log a message if logger is configured."
  (when (acme-client-logger client)
    (apply (acme-client-logger client) level format-string args)))

(defun default-logger (level format-string &rest args)
  "Default logger that prints to *standard-output*."
  (format t "~&[ACME ~A] ~?~%" level format-string args)
  (force-output))

;;; ----------------------------------------------------------------------------
;;; HTTP Operations (client-scoped)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; HTTP request seam and retry infrastructure
;;;
;;; All ACME HTTP traffic flows through CLIENT-HTTP-REQUEST, which calls the
;;; pluggable *HTTP-REQUEST-FUNCTION* (default DRAKMA:HTTP-REQUEST). Recoverable
;;; responses are mapped onto typed conditions and offered a RETRY restart, so a
;;; policy handler (WITH-ACME-RETRIES) can re-drive the request in place. Waiting
;;; goes through *SLEEP-FUNCTION* so tests can drive Retry-After paths with no
;;; real delay.
;;; ----------------------------------------------------------------------------

(defparameter *http-request-function* 'drakma:http-request
  "Function used to perform HTTP requests. Called as
   (funcall *http-request-function* url &key method content content-type accept)
   and must return (values body status headers) like DRAKMA:HTTP-REQUEST.
   Rebindable so tests can stub the network layer.")

(defparameter *sleep-function* #'sleep
  "Function called to wait between retries, as (funcall *sleep-function* seconds).
   Rebindable so tests can exercise Retry-After paths without real sleeping.")

(defparameter *default-retry-after* 1
  "Fallback delay in seconds when a recoverable response carries no usable
   Retry-After header.")

(defparameter +acme-error-bad-nonce+ "urn:ietf:params:acme:error:badNonce"
  "RFC 8555 problem document type for a rejected nonce.")

(defparameter +acme-error-rate-limited+ "urn:ietf:params:acme:error:rateLimited"
  "RFC 8555 problem document type for a rate-limited request.")

(defun ensure-body-string (body)
  "Return BODY as a string, decoding octet vectors as UTF-8."
  (if (stringp body)
      body
      (flexi-streams:octets-to-string body :external-format :utf-8)))

(defun decode-acme-body (body-str)
  "Decode a non-empty ACME JSON BODY-STR into an alist, or NIL when empty."
  (when (and body-str (> (length body-str) 0))
    (cl-json:decode-json-from-string body-str)))

(defun acme-problem-doc (body-str)
  "Parse BODY-STR as an RFC 7807 problem document, returning the decoded alist,
   or NIL when it is empty or not a JSON object. Never signals on malformed
   input."
  (let ((trimmed (and body-str
                      (string-left-trim '(#\Space #\Newline #\Return #\Tab)
                                        body-str))))
    (when (and trimmed (> (length trimmed) 0) (char= (char trimmed 0) #\{))
      (ignore-errors (cl-json:decode-json-from-string trimmed)))))

(defun parse-retry-after (headers)
  "Return the Retry-After header from HEADERS in seconds as a non-negative
   integer, or NIL when absent or in the HTTP-date form (which callers treat as
   'use the default delay')."
  (let ((raw (rest (assoc :retry-after headers))))
    (when raw
      (let* ((text (string-trim '(#\Space #\Tab #\Return #\Newline)
                                (princ-to-string raw)))
             (seconds (ignore-errors (parse-integer text :junk-allowed nil))))
        (when (and (integerp seconds) (>= seconds 0))
          seconds)))))

(defun recoverable-acme-condition (status problem)
  "Return the condition class name for a recoverable response, or NIL for a
   success or terminal (non-recoverable) response.
   STATUS is the HTTP status; PROBLEM is the decoded problem document or NIL."
  (let ((type (and (consp problem) (rest (assoc :type problem)))))
    (cond
      ((and (integerp status) (<= 400 status 499)
            type (string= type +acme-error-bad-nonce+))
       'acme-bad-nonce)
      ((or (eql status 429)
           (and type (string= type +acme-error-rate-limited+)))
       'acme-rate-limited)
      ((eql status 202)
       'acme-not-ready)
      (t nil))))

(defun client-http-request (client url method builder)
  "Perform a single ACME HTTP request and expose recovery via a RETRY restart.

   BUILDER is a thunk returning the plist of keyword arguments passed to
   *HTTP-REQUEST-FUNCTION* (for example (:method :post :content ...)). For a
   signed POST, BUILDER re-signs the JWS with CLIENT's current nonce each time
   it runs, so re-invoking it after a nonce refresh yields a correctly-signed
   retry. The Replay-Nonce returned by the server is captured into CLIENT before
   any recoverable condition is signalled.

   When the response is recoverable (badNonce, 429/rateLimited, 202/not-ready)
   the matching typed condition is SIGNALLED inside a RESTART-CASE whose RETRY
   restart re-drives this same request. If no handler invokes RETRY (none
   established, or the retry budget is exhausted) SIGNAL returns normally and the
   response is returned as-is, preserving the pre-retry behaviour.

   Returns (values body-string status headers)."
  (restart-case
      (multiple-value-bind (body status headers)
          (apply *http-request-function* url (funcall builder))
        (let ((body-str (ensure-body-string body))
              (nonce (rest (assoc :replay-nonce headers))))
          (when nonce
            (setf (acme-client-nonce client) nonce))
          (let* ((problem (when (or (eql status 202)
                                    (and (integerp status) (>= status 400)))
                            (acme-problem-doc body-str)))
                 (class (recoverable-acme-condition status problem)))
            (when class
              (signal class
                      :status status
                      :problem problem
                      :headers headers
                      :url url
                      :method method
                      :retry-after (parse-retry-after headers))))
          (values body-str status headers)))
    (retry ()
      :report "Refresh ACME state and retry this request."
      (client-http-request client url method builder))))

(defun call-with-acme-retries (thunk &key (max-nonce-retries 3)
                                          (max-retry-after-attempts 10)
                                          (max-total-wait 120)
                                          (handle-not-ready t))
  "Call THUNK with ACME recovery handlers established (see WITH-ACME-RETRIES).

   A badNonce condition is retried up to MAX-NONCE-RETRIES times, re-signing with
   the nonce the server returned in the rejecting response. A rate-limited
   condition (and, when HANDLE-NOT-READY, a not-ready condition) waits per
   Retry-After through *SLEEP-FUNCTION* and retries, bounded by both
   MAX-RETRY-AFTER-ATTEMPTS and MAX-TOTAL-WAIT seconds. When a budget is
   exhausted the handler declines and the condition propagates or the response
   is returned as-is, so recovery is always bounded and never loops forever."
  (let ((nonce-retries 0)
        (wait-attempts 0)
        (total-wait 0))
    (labels ((do-retry (condition)
               (let ((restart (find-restart 'retry condition)))
                 (when restart
                   (invoke-restart restart))))
             (on-bad-nonce (condition)
               (when (< nonce-retries max-nonce-retries)
                 (incf nonce-retries)
                 (do-retry condition)))
             (on-wait (condition)
               (let ((wait (or (acme-http-error-retry-after condition)
                               *default-retry-after*)))
                 (when (and (< wait-attempts max-retry-after-attempts)
                            (<= (+ total-wait wait) max-total-wait))
                   (incf wait-attempts)
                   (incf total-wait wait)
                   (funcall *sleep-function* wait)
                   (do-retry condition)))))
      (if handle-not-ready
          (handler-bind ((acme-bad-nonce #'on-bad-nonce)
                         (acme-rate-limited #'on-wait)
                         (acme-not-ready #'on-wait))
            (funcall thunk))
          (handler-bind ((acme-bad-nonce #'on-bad-nonce)
                         (acme-rate-limited #'on-wait))
            (funcall thunk))))))

(defmacro with-acme-retries ((&key (max-nonce-retries 3)
                                   (max-retry-after-attempts 10)
                                   (max-total-wait 120)
                                   (handle-not-ready t))
                             &body body)
  "Evaluate BODY with ACME request-level recovery established around every
   request it issues. Recoverable ACME responses are handled without unwinding
   the stack: each is mapped onto a RETRY restart that re-drives the offending
   request. See CALL-WITH-ACME-RETRIES for the budget parameters. Exposed so an
   issuance driver can wrap a broader region under one policy; the public client
   functions already establish sensible defaults for standalone calls."
  `(call-with-acme-retries
    (lambda () ,@body)
    :max-nonce-retries ,max-nonce-retries
    :max-retry-after-attempts ,max-retry-after-attempts
    :max-total-wait ,max-total-wait
    :handle-not-ready ,handle-not-ready))

(defun client-get (client url)
  "GET request to ACME endpoint.
   Returns (VALUES decoded-body status). A rate-limited response is retried per
   Retry-After; the return contract is unchanged for success and terminal
   responses."
  (with-acme-retries (:handle-not-ready nil)
    (let ((cl+ssl:*make-ssl-client-stream-verify-default*
            (if (acme-client-skip-tls-verify client)
                nil
                cl+ssl:*make-ssl-client-stream-verify-default*)))
      (multiple-value-bind (body-str status headers)
          (client-http-request client url :get
                               (lambda () (list :method :get)))
        (declare (ignore headers))
        (values (decode-acme-body body-str) status)))))

(defun client-ensure-nonce (client)
  "Ensure CLIENT holds a usable nonce, fetching a fresh one from the directory's
   new-nonce endpoint when needed. Returns the current nonce."
  (unless (acme-client-nonce client)
    (client-get client (rest (assoc :new-nonce (acme-client-directory client)))))
  (acme-client-nonce client))

(defun make-jws-post-builder (client url payload &key use-kid accept)
  "Return a thunk producing the request keyword arguments for a signed ACME POST
   to URL with PAYLOAD. Each call ensures a nonce, builds and signs the JWS with
   CLIENT's current nonce, then clears the nonce (single-use). Because it
   re-signs on every call, invoking it again after a nonce refresh produces a
   correctly-signed retry.
   USE-KID selects the account URL (kid) header instead of the JWK; ACCEPT, when
   supplied, sets the request's Accept header."
  (lambda ()
    (client-ensure-nonce client)
    (let* ((account-key (acme-client-account-key client))
           (protected-header
             (if use-kid
                 `(("alg" . "ES256")
                   ("kid" . ,(acme-client-account-url client))
                   ("nonce" . ,(acme-client-nonce client))
                   ("url" . ,url))
                 `(("alg" . "ES256")
                   ("jwk" . ,(get-public-key-jwk account-key))
                   ("nonce" . ,(acme-client-nonce client))
                   ("url" . ,url))))
           (protected64 (base64url-encode
                         (cl-json:encode-json-to-string protected-header)))
           (payload64 (if payload
                          (base64url-encode
                           (cl-json:encode-json-to-string payload))
                          ""))
           (signature (sign-payload account-key
                                    (format nil "~A.~A" protected64 payload64)))
           (jws `(("protected" . ,protected64)
                  ("payload" . ,payload64)
                  ("signature" . ,signature))))
      (setf (acme-client-nonce client) nil)  ; Nonce is single-use
      (append (list :method :post
                    :content-type "application/jose+json"
                    :content (cl-json:encode-json-to-string jws))
              (when accept (list :accept accept))))))

(defun perform-client-post (client url payload &key use-kid accept
                                                    (handle-not-ready t))
  "Issue a signed ACME POST to URL with request-level retry recovery, returning
   (values body-string status headers). badNonce and rate-limited responses are
   always retried; a 202/not-ready response is retried only when
   HANDLE-NOT-READY is true. Callers that own their own readiness polling
   (poll-status, finalize) pass HANDLE-NOT-READY NIL so a 202 is returned as-is
   rather than waited on a second time."
  (with-acme-retries (:handle-not-ready handle-not-ready)
    (let ((cl+ssl:*make-ssl-client-stream-verify-default*
            (if (acme-client-skip-tls-verify client)
                nil
                cl+ssl:*make-ssl-client-stream-verify-default*)))
      (client-http-request client url :post
                           (make-jws-post-builder client url payload
                                                  :use-kid use-kid
                                                  :accept accept)))))

(defun client-post (client url payload &key use-kid)
  "POST request with JWS body to ACME endpoint.
   USE-KID: Use account URL (kid) instead of JWK in header.
   Returns (VALUES decoded-body status location). A badNonce or rate-limited
   response self-heals (refresh nonce / wait per Retry-After and retry); the
   return contract is unchanged for success and terminal responses."
  (multiple-value-bind (body-str status headers)
      (perform-client-post client url payload :use-kid use-kid
                                              :handle-not-ready nil)
    (values (decode-acme-body body-str)
            status
            (rest (assoc :location headers)))))

;;; ----------------------------------------------------------------------------
;;; ACME Protocol Operations
;;; ----------------------------------------------------------------------------

(defun client-init (client)
  "Initialize ACME client - fetch directory."
  (client-log client :info "Fetching ACME directory from ~A" (acme-client-directory-url client))
  (setf (acme-client-directory client)
        (client-get client (acme-client-directory-url client))))

(defun client-register-account (client email)
  "Register new account or fetch existing one.
   Returns the account URL on success."
  (client-log client :info "Registering ACME account")
  (multiple-value-bind (response status location)
      (client-post client
                   (rest (assoc :new-account (acme-client-directory client)))
                   `(("termsOfServiceAgreed" . t)
                     ("contact" . #(,(format nil "mailto:~A" email)))))
    (cond
      ((member status '(200 201))
       (setf (acme-client-account-url client) location)
       (client-log client :info "Account registered: ~A" location)
       location)
      (t
       (let ((error-type (rest (assoc :type response)))
             (error-detail (rest (assoc :detail response))))
         (error 'acme-error
                :message (format nil "Account registration failed: HTTP ~A - ~A: ~A"
                                 status error-type error-detail)))))))

(defun client-new-order (client domains &key (profile *default-profile*))
  "Create new certificate order for domains.
   PROFILE: ACME profile (\"classic\", \"tlsserver\", \"shortlived\").
            Defaults to *default-profile* (tlsserver).
   Returns (VALUES order-response order-url)."
  (let* ((domain-list (if (listp domains) domains (list domains)))
         (identifiers (coerce (mapcar (lambda (d)
                                         `(("type" . "dns")
                                           ("value" . ,d)))
                                       domain-list)
                              'vector)))
    (client-log client :info "Creating order for ~{~A~^, ~} (profile: ~A)"
                domain-list profile)
    (multiple-value-bind (response status location)
        (client-post client
                     (rest (assoc :new-order (acme-client-directory client)))
                     `(("identifiers" . ,identifiers)
                       ("profile" . ,profile))
                     :use-kid t)
      (if (member status '(200 201))
          (values response location)
          (let ((error-type (rest (assoc :type response)))
                (error-detail (rest (assoc :detail response))))
            (error 'acme-order-error
                   :message (format nil "Order creation failed: HTTP ~A - ~A: ~A"
                                    status error-type error-detail)))))))

(defun client-get-authorization (client auth-url)
  "Get authorization details including challenges."
  (client-post client auth-url nil :use-kid t))

(defun client-respond-challenge (client challenge-url)
  "Tell ACME server to validate the challenge."
  (client-log client :debug "Responding to challenge: ~A" challenge-url)
  (client-post client challenge-url (make-hash-table) :use-kid t))

(defun client-poll-status (client url &key (max-attempts 30) (delay 2) wait-for-valid)
  "Poll order/authorization status until ready or failed.
   Returns (VALUES response status-keyword).

   This is the poll primitive an issuance driver calls to wait for readiness; it
   is a single loop and never establishes a nested readiness wait. A non-string
   :status (for example a problem document's numeric status) is treated as
   non-terminal: wait and poll again rather than comparing it as a string. When
   the response carries a Retry-After header its value sets this loop's sleep
   interval; otherwise DELAY is used. Waiting goes through *SLEEP-FUNCTION*."
  (client-log client :debug "Polling status: ~A" url)
  (loop for attempt from 1 to max-attempts
        do (multiple-value-bind (body-str status headers)
               (perform-client-post client url nil :use-kid t
                                                   :handle-not-ready nil)
             (declare (ignore status))
             (let* ((response (decode-acme-body body-str))
                    (state (rest (assoc :status response)))
                    (wait (or (parse-retry-after headers) delay)))
               (client-log client :debug "Poll ~A/~A: ~A" attempt max-attempts state)
               (cond
                 ((not (stringp state))
                  (funcall *sleep-function* wait))
                 ((string= state "valid")
                  (return (values response :valid)))
                 ((string= state "ready")
                  (if wait-for-valid
                      (funcall *sleep-function* wait)
                      (return (values response :ready))))
                 ((string= state "processing")
                  (funcall *sleep-function* wait))
                 ((string= state "invalid")
                  (return (values response :invalid)))
                 ((string= state "pending")
                  (funcall *sleep-function* wait))
                 (t (funcall *sleep-function* wait)))))
        finally (return (values nil :timeout))))

(defun client-finalize-order (client finalize-url csr-der)
  "Submit CSR to finalize the order.
   Returns (VALUES response status location). badNonce and rate-limited
   responses self-heal; a 202/processing finalize response is returned as-is,
   since order readiness is the caller's responsibility (poll the order to
   :valid), not a second client-side wait."
  (client-log client :info "Finalizing order")
  (multiple-value-bind (body-str status headers)
      (perform-client-post client finalize-url
                           `(("csr" . ,(base64url-encode csr-der)))
                           :use-kid t :handle-not-ready nil)
    (values (decode-acme-body body-str)
            status
            (rest (assoc :location headers)))))

(defun client-download-certificate (client cert-url)
  "Download the issued certificate chain (returns PEM string).

   A 202/not-ready response is retried per Retry-After until the chain is
   available, then the PEM is returned. This is the request-level readiness wait
   the client owns for its own download path. Returns the PEM string on HTTP 200,
   or NIL for a terminal non-200 (unchanged from the pre-retry contract), which
   only occurs once retries are exhausted or the failure is non-recoverable."
  (multiple-value-bind (body-str status headers)
      (perform-client-post client cert-url nil
                           :use-kid t
                           :accept "application/pem-certificate-chain"
                           :handle-not-ready t)
    (declare (ignore headers))
    (if (= status 200)
        body-str
        nil)))

(defun client-compute-key-authorization (client token)
  "Compute key authorization: token.thumbprint"
  (let ((thumbprint (get-jwk-thumbprint
                     (get-public-key-jwk (acme-client-account-key client)))))
    (format nil "~A.~A" token thumbprint)))
