;;; hunchentoot.lisp --- Hunchentoot integration with automatic ACME certificates
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Provides an integrated Hunchentoot acceptor that automatically obtains
;;; and renews Let's Encrypt certificates using TLS-ALPN-01 challenge.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; ACME Acceptor for Hunchentoot
;;; ----------------------------------------------------------------------------

(defclass acme-acceptor (hunchentoot:easy-acceptor)
  ((domains
    :initarg :domains
    :accessor acceptor-domains
    :initform nil
    :documentation "List of domains for the certificate.")
   (email
    :initarg :email
    :accessor acceptor-email
    :documentation "Contact email for Let's Encrypt account.")
   (acme-client
    :accessor acceptor-acme-client
    :initform nil
    :documentation "The ACME client instance.")
   (cert-store
    :accessor acceptor-cert-store
    :initform nil
    :documentation "Certificate storage.")
   ;; Validation state (protected by validation-lock)
   (validation-lock
    :accessor acceptor-validation-lock
    :initform (bt:make-lock "acme-validation")
    :documentation "Lock protecting validation cert/key during challenges.")
   (validation-cert
    :accessor %acceptor-validation-cert
    :initform nil
    :documentation "Current TLS-ALPN-01 validation certificate (DER).")
   (validation-key
    :accessor %acceptor-validation-key
    :initform nil
    :documentation "Current TLS-ALPN-01 validation private key.")
   ;; Renewal configuration
   (renewal-thread
    :accessor acceptor-renewal-thread
    :initform nil
    :documentation "Background certificate renewal thread.")
   (renewal-lock
    :accessor acceptor-renewal-lock
    :initform (bt:make-lock "acme-renewal")
    :documentation "Lock to prevent concurrent renewal attempts.")
   (renewal-interval
    :initarg :renewal-interval
    :accessor acceptor-renewal-interval
    :initform (* 24 60 60)  ; Check daily
    :documentation "Seconds between renewal checks.")
   (renewal-days
    :initarg :renewal-days
    :accessor acceptor-renewal-days
    :initform 30
    :documentation "Renew when certificate expires within this many days.")
   (production
    :initarg :production
    :accessor acceptor-production-p
    :initform t
    :documentation "Use Let's Encrypt production (T) or staging (NIL).")
   (logger
    :initarg :logger
    :accessor acceptor-logger
    :initform nil
    :documentation "Logging function."))
  (:default-initargs
   :port 443)
  (:documentation
   "Hunchentoot acceptor with automatic Let's Encrypt certificate management.

    Creates TLS connections using pure-tls with automatic:
    - Certificate acquisition on startup
    - TLS-ALPN-01 challenge handling from the same port
    - Background certificate renewal

    Example:
      (make-instance 'acme-acceptor
                     :domains '(\"example.com\" \"www.example.com\")
                     :email \"admin@example.com\"
                     :production t)"))

;;; ----------------------------------------------------------------------------
;;; Thread-safe validation cert accessors
;;; ----------------------------------------------------------------------------

(defun acceptor-validation-cert (acceptor)
  "Get validation certificate (thread-safe)."
  (bt:with-lock-held ((acceptor-validation-lock acceptor))
    (%acceptor-validation-cert acceptor)))

(defun acceptor-validation-key (acceptor)
  "Get validation key (thread-safe)."
  (bt:with-lock-held ((acceptor-validation-lock acceptor))
    (%acceptor-validation-key acceptor)))

(defun acceptor-set-validation (acceptor cert key)
  "Set validation certificate and key atomically (thread-safe)."
  (bt:with-lock-held ((acceptor-validation-lock acceptor))
    (setf (%acceptor-validation-cert acceptor) cert)
    (setf (%acceptor-validation-key acceptor) key)))

(defun acceptor-clear-validation (acceptor)
  "Clear validation certificate and key atomically (thread-safe)."
  (bt:with-lock-held ((acceptor-validation-lock acceptor))
    (setf (%acceptor-validation-cert acceptor) nil)
    (setf (%acceptor-validation-key acceptor) nil)))

(defun acceptor-get-validation (acceptor)
  "Get both validation cert and key atomically (thread-safe).
   Returns (VALUES cert key) or (VALUES NIL NIL)."
  (bt:with-lock-held ((acceptor-validation-lock acceptor))
    (values (%acceptor-validation-cert acceptor)
            (%acceptor-validation-key acceptor))))

;;; ----------------------------------------------------------------------------
;;; Acceptor Methods
;;; ----------------------------------------------------------------------------

(defmethod hunchentoot:acceptor-ssl-p ((acceptor acme-acceptor))
  "This acceptor uses SSL/TLS."
  t)

(defmethod initialize-instance :after ((acceptor acme-acceptor) &key store)
  "Initialize the ACME acceptor with certificate store and client."
  (let* ((cert-store (or store (make-cert-store)))
         (directory-url (if (acceptor-production-p acceptor)
                            *production-url*
                            *staging-url*))
         (client (make-acme-client :directory-url directory-url
                                   :store cert-store
                                   :logger (acceptor-logger acceptor))))
    (setf (acceptor-cert-store acceptor) cert-store)
    (setf (acceptor-acme-client acceptor) client)))

(defmethod hunchentoot:start ((acceptor acme-acceptor))
  "Start the acceptor, obtaining certificates via TLS-ALPN-01 if needed.
   The server must be listening to handle ALPN challenge connections."
  (let ((primary-domain (first (acceptor-domains acceptor))))
    ;; Check if we need to obtain a certificate
    (let* ((store (acceptor-cert-store acceptor))
           (have-cert (store-has-certificate-p store primary-domain))
           (need-real-cert (not (and have-cert
                                     (not (store-certificate-expires-soon-p store primary-domain 0))))))
      ;; If no certificate at all, generate a placeholder BEFORE starting the server
      ;; This allows the TLS server to start and handle ALPN challenges
      (unless have-cert
        (acceptor-log acceptor :info "Generating placeholder certificate for ~A" primary-domain)
        (multiple-value-bind (cert-pem key-pem)
            (generate-placeholder-certificate primary-domain)
          (store-save-certificate store primary-domain cert-pem key-pem))
        (acceptor-log acceptor :info "Placeholder certificate ready"))
      ;; Start the server - now we always have SOME certificate
      (call-next-method)
      ;; Now obtain real certificate if needed (server is listening for ALPN challenges)
      (when need-real-cert
        (acceptor-obtain-certificate acceptor))
      ;; Start renewal thread
      (acceptor-start-renewal-thread acceptor))))

(defmethod hunchentoot:stop ((acceptor acme-acceptor) &key soft)
  "Stop the acceptor and renewal thread."
  (declare (ignore soft))
  ;; Stop renewal thread
  (when (acceptor-renewal-thread acceptor)
    (bt:destroy-thread (acceptor-renewal-thread acceptor))
    (setf (acceptor-renewal-thread acceptor) nil))
  (call-next-method))

(defmethod hunchentoot:initialize-connection-stream ((acceptor acme-acceptor) stream)
  "Wrap the connection with pure-tls, handling ACME challenges inline."
  (let* ((primary-domain (first (acceptor-domains acceptor)))
         (cert-path (store-domain-cert-path (acceptor-cert-store acceptor) primary-domain))
         (key-path (store-domain-key-path (acceptor-cert-store acceptor) primary-domain)))
    ;; Only log per-connection start when debugging
    (when pure-tls::*handshake-debug*
      (acceptor-log acceptor :info "TLS connection starting, cert from ~A" cert-path))
    ;; Certificate should exist (placeholder was created at startup if needed)
    (handler-case
        (let ((tls-stream (pure-tls:make-tls-server-stream
                           stream
                           :certificate (namestring cert-path)
                           :key (namestring key-path)
                           :certificate-provider (make-certificate-provider acceptor))))
          ;; Only log per-connection success when debugging
          (when pure-tls::*handshake-debug*
            (acceptor-log acceptor :info "TLS handshake completed successfully"))
          tls-stream)
      (error (e)
        (acceptor-log acceptor :error "TLS handshake failed: ~A" e)
        (error e)))))

(defun make-certificate-provider (acceptor)
  "Create a certificate provider function for TLS-ALPN-01 challenge handling.
   This is called during TLS handshake to potentially override the certificate."
  (lambda (hostname alpn-list)
    ;; Only log when handshake debug is enabled (reduces per-connection noise)
    (when pure-tls::*handshake-debug*
      (acceptor-log acceptor :info "Certificate provider called: hostname=~A alpn=~A" hostname alpn-list))
    ;; Check if this is an ACME TLS-ALPN-01 challenge
    (if (member "acme-tls/1" alpn-list :test #'string=)
        (progn
          (acceptor-log acceptor :info "ALPN acme-tls/1 detected - returning validation cert")
          ;; Atomically get both cert and key
          (multiple-value-bind (cert key)
              (acceptor-get-validation acceptor)
            (if (and cert key)
                (progn
                  (acceptor-log acceptor :info "Validation cert available: cert-len=~A key-type=~A"
                                (length cert) (type-of key))
                  (values (list cert) key "acme-tls/1"))
                (progn
                  (acceptor-log acceptor :error "NO VALIDATION CERT AVAILABLE!")
                  nil))))
        ;; Regular TLS connection - only log in debug mode
        (progn
          (when pure-tls::*handshake-debug*
            (acceptor-log acceptor :info "Regular TLS connection (no acme-tls/1)"))
          nil))))

;;; ----------------------------------------------------------------------------
;;; Certificate Acquisition
;;; ----------------------------------------------------------------------------

(defun acceptor-obtain-certificate (acceptor)
  "Obtain a certificate for the acceptor's domains using TLS-ALPN-01.
   Uses renewal-lock to prevent concurrent renewal attempts."
  (bt:with-lock-held ((acceptor-renewal-lock acceptor))
    (let* ((client (acceptor-acme-client acceptor))
           (store (acceptor-cert-store acceptor))
           (domains (acceptor-domains acceptor))
           (email (acceptor-email acceptor))
           (primary-domain (first domains)))

      (acceptor-log acceptor :info "Obtaining certificate for ~{~A~^, ~}" domains)

      ;; Initialize ACME client
      (client-init client)

      ;; Register account
      (client-register-account client email)

      ;; Create order
      (multiple-value-bind (order order-url)
          (client-new-order client domains)

        ;; Process each authorization
        (let ((auth-urls (cdr (assoc :authorizations order))))
          (dolist (auth-url auth-urls)
            (acceptor-process-authorization acceptor client auth-url)))

        ;; Generate domain key and CSR
        (multiple-value-bind (private-key public-key)
            (generate-domain-key)
          (let* ((csr (generate-csr private-key public-key domains))
                 (finalize-url (cdr (assoc :finalize order))))

            ;; Save private key
            (save-private-key-pem private-key public-key
                                  (store-domain-key-path store primary-domain))

            ;; Finalize order
            (client-finalize-order client finalize-url csr)

            ;; Poll until valid
            (multiple-value-bind (final-order status)
                (client-poll-status client order-url :wait-for-valid t)
              (unless (eq status :valid)
                (error 'acme-order-error
                       :message (format nil "Order finalization failed: ~A" status)))

              ;; Download certificate
              (let* ((cert-url (cdr (assoc :certificate final-order)))
                     (cert-pem (client-download-certificate client cert-url)))
                (unless cert-pem
                  (error 'acme-certificate-error
                         :message "Failed to download certificate"))

                ;; Save certificate
                (with-open-file (out (store-domain-cert-path store primary-domain)
                                     :direction :output :if-exists :supersede)
                  (write-string cert-pem out))

                (acceptor-log acceptor :info "Certificate obtained successfully")))))))))

(defun acceptor-process-authorization (acceptor client auth-url)
  "Process a single authorization using TLS-ALPN-01.
   Sets validation cert atomically and waits for Let's Encrypt to validate."
  (let* ((auth (client-get-authorization client auth-url))
         (challenges (cdr (assoc :challenges auth)))
         (tls-alpn (find "tls-alpn-01"
                         challenges
                         :key (lambda (c) (cdr (assoc :type c)))
                         :test #'string=)))
    (unless tls-alpn
      (error 'acme-challenge-error
             :message "TLS-ALPN-01 challenge not available"))

    (let* ((token (cdr (assoc :token tls-alpn)))
           (challenge-url (cdr (assoc :url tls-alpn)))
           (key-auth (client-compute-key-authorization client token))
           (domain (cdr (assoc :value (cdr (assoc :identifier auth))))))

      ;; Generate validation certificate
      (acceptor-log acceptor :info "Generating validation certificate for ~A" domain)
      (multiple-value-bind (cert-pem key-pem priv-key)
          (generate-validation-certificate domain key-auth)
        (declare (ignore key-pem))
        (acceptor-log acceptor :info "Validation certificate generated, cert-pem length=~A" (length cert-pem))

        ;; Parse cert to DER for in-memory use
        (let ((cert-der (extract-pem-der cert-pem "CERTIFICATE")))
          (acceptor-log acceptor :info "Cert DER parsed, length=~A bytes" (length cert-der))

          ;; Set validation cert/key atomically
          (acceptor-log acceptor :info "Setting validation cert in acceptor...")
          (acceptor-set-validation acceptor cert-der priv-key)
          (acceptor-log acceptor :info "Validation cert SET - ready for ALPN challenge")

          (unwind-protect
               (progn
                 ;; Small delay to ensure memory visibility across threads
                 (sleep 0.1)

                 (acceptor-log acceptor :info "Telling Let's Encrypt to validate (they will connect to port 443)...")

                 ;; Tell ACME server to validate
                 ;; Let's Encrypt will connect to our port 443 with ALPN "acme-tls/1"
                 (client-respond-challenge client challenge-url)
                 (acceptor-log acceptor :info "Challenge response sent, now polling for result...")

                 ;; Poll for completion - Let's Encrypt may retry multiple times
                 ;; Keep validation cert available throughout
                 (multiple-value-bind (result status)
                     (client-poll-status client auth-url :max-attempts 60 :delay 2)
                   (unless (eq status :valid)
                     ;; Extract error details from challenges in the response
                     (let* ((challenges (cdr (assoc :challenges result)))
                            (error-details
                              (loop for ch in challenges
                                    for err = (cdr (assoc :error ch))
                                    when err
                                      collect (format nil "~A: ~A"
                                                      (cdr (assoc :type err))
                                                      (cdr (assoc :detail err))))))
                       (error 'acme-challenge-error
                              :message (format nil "Challenge validation failed: ~A~@[~%  ~{~A~^~%  ~}~]"
                                               status error-details))))
                   (acceptor-log acceptor :debug "Challenge validated for ~A" domain)))

            ;; Always clear validation cert when done (success or failure)
            (acceptor-clear-validation acceptor)))))))

;;; ----------------------------------------------------------------------------
;;; Certificate Renewal
;;; ----------------------------------------------------------------------------

(defun acceptor-start-renewal-thread (acceptor)
  "Start background thread for certificate renewal."
  (setf (acceptor-renewal-thread acceptor)
        (bt:make-thread
         (lambda ()
           (acceptor-renewal-loop acceptor))
         :name "acme-renewal")))

(defun acceptor-renewal-loop (acceptor)
  "Background loop checking for certificate renewal needs."
  (loop
    (sleep (acceptor-renewal-interval acceptor))
    (handler-case
        (let* ((store (acceptor-cert-store acceptor))
               (primary-domain (first (acceptor-domains acceptor))))
          (when (store-certificate-expires-soon-p store primary-domain
                                                   (acceptor-renewal-days acceptor))
            (acceptor-log acceptor :info "Certificate expiring soon, renewing...")
            (acceptor-obtain-certificate acceptor)))
      (error (e)
        (acceptor-log acceptor :error "Renewal failed: ~A" e)))))

;;; ----------------------------------------------------------------------------
;;; Logging
;;; ----------------------------------------------------------------------------

(defun acceptor-log (acceptor level format-string &rest args)
  "Log a message if logger is configured."
  (when (acceptor-logger acceptor)
    (apply (acceptor-logger acceptor) level format-string args)))

;;; ----------------------------------------------------------------------------
;;; Convenience Constructor
;;; ----------------------------------------------------------------------------

(defun make-acme-acceptor (domains email &key
                                         (port 443)
                                         (production t)
                                         (renewal-days 30)
                                         (logger #'default-logger)
                                         store)
  "Create an ACME acceptor with automatic certificate management.

   DOMAINS - Single domain string or list of domains (first is primary).
   EMAIL - Contact email for Let's Encrypt account.
   PORT - HTTPS port (default 443).
   PRODUCTION - Use Let's Encrypt production (default NIL = staging).
   RENEWAL-DAYS - Renew when certificate expires within this many days.
   LOGGER - Logging function (default prints to stdout).
   STORE - Certificate store (creates default if not provided).

   Example:
     (make-acme-acceptor \"example.com\" \"admin@example.com\" :production t)

     (make-acme-acceptor '(\"example.com\" \"www.example.com\")
                         \"admin@example.com\"
                         :production t
                         :logger (lambda (level fmt &rest args)
                                   (apply #'format *error-output* fmt args)))"
  (make-instance 'acme-acceptor
                 :domains (if (listp domains) domains (list domains))
                 :email email
                 :port port
                 :production production
                 :renewal-days renewal-days
                 :logger logger
                 :store store))
