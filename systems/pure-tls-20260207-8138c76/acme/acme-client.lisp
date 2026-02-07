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

(defun client-get (client url)
  "GET request to ACME endpoint."
  (let ((cl+ssl:*make-ssl-client-stream-verify-default*
          (if (acme-client-skip-tls-verify client)
              nil
              cl+ssl:*make-ssl-client-stream-verify-default*)))
    (multiple-value-bind (body status headers)
        (drakma:http-request url :method :get)
      (let ((nonce (rest (assoc :replay-nonce headers)))
            (body-str (if (stringp body)
                          body
                          (flexi-streams:octets-to-string body :external-format :utf-8))))
        (when nonce
          (setf (acme-client-nonce client) nonce))
        (values (when (> (length body-str) 0)
                  (cl-json:decode-json-from-string body-str))
                status)))))

(defun client-post (client url payload &key use-kid)
  "POST request with JWS body to ACME endpoint.
   USE-KID: Use account URL (kid) instead of JWK in header."
  ;; Get fresh nonce if needed
  (unless (acme-client-nonce client)
    (client-get client (rest (assoc :new-nonce (acme-client-directory client)))))

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

    (let ((cl+ssl:*make-ssl-client-stream-verify-default*
            (if (acme-client-skip-tls-verify client)
                nil
                cl+ssl:*make-ssl-client-stream-verify-default*)))
      (multiple-value-bind (body status headers)
          (drakma:http-request url
                               :method :post
                               :content-type "application/jose+json"
                               :content (cl-json:encode-json-to-string jws))
        (let* ((nonce (rest (assoc :replay-nonce headers)))
               (location (rest (assoc :location headers)))
               (body-str (if (stringp body)
                             body
                             (flexi-streams:octets-to-string body :external-format :utf-8))))
          (when nonce (setf (acme-client-nonce client) nonce))
          (values (when (> (length body-str) 0)
                    (cl-json:decode-json-from-string body-str))
                  status
                  location))))))

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
  (client-log client :info "Registering account for ~A" email)
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
   Returns (VALUES response status-keyword)."
  (client-log client :debug "Polling status: ~A" url)
  (loop for attempt from 1 to max-attempts
        do (multiple-value-bind (response status)
               (client-post client url nil :use-kid t)
             (declare (ignore status))
             (let ((state (rest (assoc :status response))))
               (client-log client :debug "Poll ~A/~A: ~A" attempt max-attempts state)
               (cond
                 ((string= state "valid")
                  (return (values response :valid)))
                 ((string= state "ready")
                  (if wait-for-valid
                      (sleep delay)
                      (return (values response :ready))))
                 ((string= state "processing")
                  (sleep delay))
                 ((string= state "invalid")
                  (return (values response :invalid)))
                 ((string= state "pending")
                  (sleep delay))
                 (t (sleep delay)))))
        finally (return (values nil :timeout))))

(defun client-finalize-order (client finalize-url csr-der)
  "Submit CSR to finalize the order."
  (client-log client :info "Finalizing order")
  (client-post client finalize-url
               `(("csr" . ,(base64url-encode csr-der)))
               :use-kid t))

(defun client-download-certificate (client cert-url)
  "Download the issued certificate chain (returns PEM string)."
  ;; Get fresh nonce first
  (unless (acme-client-nonce client)
    (client-get client (rest (assoc :new-nonce (acme-client-directory client)))))

  (let* ((account-key (acme-client-account-key client))
         (protected-header
           `(("alg" . "ES256")
             ("kid" . ,(acme-client-account-url client))
             ("nonce" . ,(acme-client-nonce client))
             ("url" . ,cert-url)))
         (protected64 (base64url-encode
                       (cl-json:encode-json-to-string protected-header)))
         (payload64 "")
         (signature (sign-payload account-key
                                  (format nil "~A.~A" protected64 payload64)))
         (jws `(("protected" . ,protected64)
                ("payload" . ,payload64)
                ("signature" . ,signature))))

    (setf (acme-client-nonce client) nil)

    (let ((cl+ssl:*make-ssl-client-stream-verify-default*
            (if (acme-client-skip-tls-verify client)
                nil
                cl+ssl:*make-ssl-client-stream-verify-default*)))
      (multiple-value-bind (body status headers)
          (drakma:http-request cert-url
                               :method :post
                               :content-type "application/jose+json"
                               :accept "application/pem-certificate-chain"
                               :content (cl-json:encode-json-to-string jws))
        (let ((nonce (rest (assoc :replay-nonce headers)))
              (body-str (if (stringp body)
                            body
                            (flexi-streams:octets-to-string body :external-format :utf-8))))
          (when nonce (setf (acme-client-nonce client) nonce))
          (if (= status 200)
              body-str
              nil))))))

(defun client-compute-key-authorization (client token)
  "Compute key authorization: token.thumbprint"
  (let ((thumbprint (get-jwk-thumbprint
                     (get-public-key-jwk (acme-client-account-key client)))))
    (format nil "~A.~A" token thumbprint)))
