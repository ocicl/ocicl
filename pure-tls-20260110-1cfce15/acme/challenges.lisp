;;; challenges.lisp --- TLS-ALPN-01 challenge handler
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; TLS-ALPN-01 challenge implementation for ACME certificate validation.

(in-package #:pure-tls/acme)

;;; ----------------------------------------------------------------------------
;;; TLS-ALPN-01 Challenge Handler
;;; ----------------------------------------------------------------------------

(defvar *tls-alpn-server* nil
  "Currently running TLS-ALPN-01 validation server state.")

(defun generate-placeholder-certificate (domain)
  "Generate a simple self-signed placeholder certificate for TLS startup.
   This is used when no real certificate exists yet.
   Returns (VALUES cert-pem key-pem)."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :secp256r1)
    (let* ((not-before (get-universal-time))
           (not-after (+ not-before (* 1 24 60 60)))  ; 1 day validity
           (serial (random (expt 2 64)))
           (subject (encode-subject domain))
           (san-ext (encode-critical-extension
                     *oid-subject-alt-name*
                     (encode-san-extension domain)))
           (tbs-cert (encode-sequence
                      (encode-context-tag 0 (encode-integer 2))
                      (encode-integer serial)
                      (encode-sequence (encode-oid *oid-ecdsa-with-sha256*))
                      subject
                      (encode-validity not-before not-after)
                      subject
                      (encode-ec-public-key public-key)
                      (encode-x509-extensions (list san-ext))))
           (raw-signature (let ((digest (ironclad:digest-sequence :sha256 tbs-cert)))
                            (ironclad:sign-message private-key digest)))
           (der-signature (encode-ecdsa-signature raw-signature))
           (certificate (encode-sequence
                         tbs-cert
                         (encode-sequence (encode-oid *oid-ecdsa-with-sha256*))
                         (encode-bit-string der-signature)))
           (cert-pem (wrap-pem "CERTIFICATE" certificate))
           (key-pem (encode-ec-private-key-pem private-key)))
      (values cert-pem key-pem))))

(defun generate-validation-certificate (domain key-authorization)
  "Generate a self-signed validation certificate for TLS-ALPN-01.
   Uses ECDSA P-256 for TLS 1.3 compatibility.
   Returns (VALUES cert-pem key-pem private-key)."
  ;; Generate ECDSA P-256 key (avoids RSA-PSS issues in TLS 1.3)
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :secp256r1)
    (let* (
         ;; Hash the key authorization for the acmeIdentifier extension
         (key-auth-hash (ironclad:digest-sequence
                         :sha256
                         (flexi-streams:string-to-octets key-authorization
                                                         :external-format :utf-8)))
         ;; Validity: now to 7 days from now
         (not-before (get-universal-time))
         (not-after (+ not-before (* 7 24 60 60)))
         ;; Random serial number
         (serial (random (expt 2 64)))
         ;; Subject/Issuer: CN=domain (self-signed)
         (subject (encode-subject domain))
         ;; Subject Alternative Name extension with dNSName
         (san-ext (encode-critical-extension
                   *oid-subject-alt-name*
                   (encode-san-extension domain)))
         ;; acmeIdentifier extension (CRITICAL) containing hash
         ;; The value is an OCTET STRING containing the hash
         (acme-id-ext (encode-critical-extension
                       *oid-acme-identifier*
                       (encode-octet-string key-auth-hash)))
         ;; Build TBSCertificate
         (tbs-cert (encode-sequence
                    ;; version [0] EXPLICIT INTEGER (v3 = 2)
                    (encode-context-tag 0 (encode-integer 2))
                    ;; serialNumber
                    (encode-integer serial)
                    ;; signature algorithm (ECDSA with SHA256)
                    (encode-sequence
                     (encode-oid *oid-ecdsa-with-sha256*))
                    ;; issuer (self-signed, same as subject)
                    subject
                    ;; validity
                    (encode-validity not-before not-after)
                    ;; subject
                    subject
                    ;; subjectPublicKeyInfo
                    (encode-ec-public-key public-key)
                    ;; extensions [3]
                    (encode-x509-extensions (list san-ext acme-id-ext))))
         ;; Sign TBSCertificate with ECDSA (must DER-encode the signature)
         (raw-signature (let ((digest (ironclad:digest-sequence :sha256 tbs-cert)))
                          (ironclad:sign-message private-key digest)))
         (der-signature (encode-ecdsa-signature raw-signature))
         ;; Build Certificate
         (certificate (encode-sequence
                       tbs-cert
                       (encode-sequence
                        (encode-oid *oid-ecdsa-with-sha256*))
                       (encode-bit-string der-signature)))
         ;; Convert to PEM
         (cert-pem (wrap-pem "CERTIFICATE" certificate))
         (key-pem (encode-ec-private-key-pem private-key)))

      (values cert-pem key-pem private-key))))

(defun save-temp-validation-files (cert-pem key-pem)
  "Save validation certificate and key to temporary files.
   Returns (VALUES cert-path key-path)."
  (let ((cert-path (merge-pathnames "acme-validation-cert.pem" (uiop:temporary-directory)))
        (key-path (merge-pathnames "acme-validation-key.pem" (uiop:temporary-directory))))
    (ensure-directories-exist cert-path)
    (with-open-file (out cert-path :direction :output
                                   :if-exists :supersede)
      (write-string cert-pem out))
    (with-open-file (out key-path :direction :output
                                  :if-exists :supersede)
      (write-string key-pem out))
    #+sbcl (sb-posix:chmod (namestring key-path) #o600)
    ;; Save debug copies only when debugging enabled
    (when *acme-debug*
      (let ((debug-cert-path (merge-pathnames "acme-validation-cert-DEBUG.pem" (uiop:temporary-directory)))
            (debug-key-path (merge-pathnames "acme-validation-key-DEBUG.pem" (uiop:temporary-directory))))
        (with-open-file (out debug-cert-path :direction :output
                                             :if-exists :supersede)
          (write-string cert-pem out))
        (with-open-file (out debug-key-path :direction :output
                                            :if-exists :supersede)
          (write-string key-pem out))
        #+sbcl (sb-posix:chmod (namestring debug-key-path) #o600)
        (acme-log "~&[ACME] Debug cert saved to ~A~%" debug-cert-path)))
    (values cert-path key-path)))

(defun start-tls-alpn-server (domain key-authorization &optional (port 443))
  "Start TLS-ALPN-01 validation server on the specified port.
   The server responds to connections with ALPN 'acme-tls/1' using
   a self-signed certificate containing the acmeIdentifier extension."
  (acme-log "~&[ACME] Generating validation certificate for ~A~%" domain)
  (force-output)
  ;; Generate validation certificate
  (multiple-value-bind (cert-pem key-pem)
      (generate-validation-certificate domain key-authorization)

    ;; Save to temp files (pure-tls needs file paths)
    (multiple-value-bind (cert-path key-path)
        (save-temp-validation-files cert-pem key-pem)
      (acme-log "~&[ACME] Validation cert saved to ~A~%" cert-path)
      (force-output)

      ;; Create TCP listener
      (acme-log "~&[ACME] Starting TLS-ALPN-01 server on port ~A~%" port)
      (force-output)
      (let ((listen-socket (usocket:socket-listen "0.0.0.0" port
                                                  :reuse-address t
                                                  :element-type '(unsigned-byte 8)
                                                  :backlog 5)))
        (acme-log "~&[ACME] TLS-ALPN-01 server listening on port ~A~%" port)
        (force-output)
        (setf *tls-alpn-server*
              (list :socket listen-socket
                    :cert-path cert-path
                    :key-path key-path
                    :running t))

        ;; Start accept thread
        (bt:make-thread
         (lambda ()
           (unwind-protect
               (tls-alpn-accept-loop listen-socket cert-path key-path)
             nil))
         :name "tls-alpn-01-server")))))

(defun tls-alpn-accept-loop (listen-socket cert-path key-path)
  "Accept loop for TLS-ALPN-01 validation connections."
  (loop while (and *tls-alpn-server*
                   (getf *tls-alpn-server* :running))
        do (handler-case
               (let ((client-socket (usocket:socket-accept listen-socket
                                                           :element-type '(unsigned-byte 8))))
                 (bt:make-thread
                  (lambda ()
                    (handle-tls-alpn-connection client-socket cert-path key-path))
                  :name "tls-alpn-handler"))
             (usocket:socket-error (e)
               (declare (ignore e))
               (unless (and *tls-alpn-server*
                            (getf *tls-alpn-server* :running))
                 (return)))  ; Expected when stopping
             (error (e)
               (declare (ignore e))))))

(defun handle-tls-alpn-connection (client-socket cert-path key-path)
  "Handle a TLS-ALPN-01 validation connection."
  (acme-log "~&[ACME] Received TLS-ALPN connection~%")
  (force-output)
  (handler-case
      (let ((client-stream (usocket:socket-stream client-socket)))
        (acme-log "~&[ACME] Starting TLS handshake with ALPN acme-tls/1~%")
        (force-output)
        ;; Create TLS server stream with ONLY acme-tls/1 ALPN
        (let ((tls-stream (pure-tls:make-tls-server-stream
                          client-stream
                          :certificate (namestring cert-path)
                          :key (namestring key-path)
                          :alpn-protocols '("acme-tls/1"))))
          (acme-log "~&[ACME] TLS handshake completed successfully~%")
          (force-output)
          ;; The connection will be closed by the ACME server after validation
          ;; Just wait briefly and close
          (sleep 1)
          (close tls-stream)))
    (error (e)
      (acme-log "~&[ACME] TLS handshake error: ~A~%" e)
      (force-output)))
  (ignore-errors (usocket:socket-close client-socket)))

(defun stop-tls-alpn-server ()
  "Stop the TLS-ALPN-01 validation server."
  (when *tls-alpn-server*
    (setf (getf *tls-alpn-server* :running) nil)
    (let ((socket (getf *tls-alpn-server* :socket)))
      (when socket
        (ignore-errors (usocket:socket-close socket))))
    ;; Clean up temp files
    (let ((cert-path (getf *tls-alpn-server* :cert-path))
          (key-path (getf *tls-alpn-server* :key-path)))
      (when cert-path (ignore-errors (delete-file cert-path)))
      (when key-path (ignore-errors (delete-file key-path))))
    (setf *tls-alpn-server* nil)))
