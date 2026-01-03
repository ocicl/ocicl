;;; server.lisp --- TLS 1.3 Server Handshake
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements the TLS 1.3 server handshake state machine.

(in-package #:pure-tls)

;;;; Server Handshake State

(defstruct server-handshake
  "Server handshake state."
  ;; Configuration
  (certificate-chain nil :type list)    ; List of DER-encoded certificates
  (private-key nil)                      ; Server's private key
  (alpn-protocols nil :type list)        ; Protocols we support
  (verify-mode +verify-none+ :type fixnum) ; Client cert requirement
  (trust-store nil)                      ; For verifying client certs
  ;; SNI callback for virtual hosting
  ;; Called with (hostname) after ClientHello, should return
  ;; (values certificate-chain private-key) or NIL to use defaults
  (sni-callback nil)
  ;; Cipher suites we support (in preference order)
  ;; ChaCha20-Poly1305 is preferred for better side-channel resistance
  (cipher-suites (list +tls-chacha20-poly1305-sha256+
                       +tls-aes-256-gcm-sha384+
                       +tls-aes-128-gcm-sha256+)
                 :type list)
  ;; Key exchange state
  (key-exchange nil)
  ;; Selected parameters
  (selected-cipher-suite nil)
  (client-session-id nil :type (or null octet-vector))
  ;; Key schedule
  (key-schedule nil)
  ;; Record layer
  (record-layer nil)
  ;; Handshake transcript (raw bytes for hashing)
  (transcript nil :type (or null octet-vector))
  ;; Client's certificate (if provided, for mTLS)
  (peer-certificate nil)
  (peer-certificate-chain nil :type list)
  ;; Selected ALPN protocol
  (selected-alpn nil :type (or null string))
  ;; Client's offered SNI hostname
  (client-hostname nil :type (or null string))
  ;; Client random (for SSLKEYLOGFILE)
  (client-random nil :type (or null octet-vector))
  ;; Whether we requested a client certificate
  (certificate-requested nil :type boolean)
  ;; Session resumption (PSK)
  (psk-accepted nil :type boolean)  ; T if we accepted client's PSK
  (accepted-psk nil :type (or null octet-vector))  ; The PSK we accepted
  (selected-psk-index nil)  ; Index of selected PSK identity
  (resumption-master-secret nil :type (or null octet-vector))  ; For ticket generation
  (ticket-nonce-counter 0 :type fixnum)  ; Counter for ticket nonces
  ;; State machine state
  (state :start))

(defun server-handshake-update-transcript (hs message-bytes)
  "Add message bytes to the handshake transcript."
  (setf (server-handshake-transcript hs)
        (if (server-handshake-transcript hs)
            (concat-octet-vectors (server-handshake-transcript hs) message-bytes)
            (copy-seq message-bytes))))

;;;; PSK Verification

(defun try-accept-psk (hs psk-ext raw-client-hello cipher-suite)
  "Try to accept a PSK from the client's pre_shared_key extension.
   Returns (VALUES psk selected-index) if a valid PSK is found, NIL otherwise.
   RAW-CLIENT-HELLO must be the complete ClientHello message bytes."
  (declare (ignore hs))  ; HS reserved for future use (e.g., custom ticket decryption)
  (let* ((identities (pre-shared-key-ext-identities psk-ext))
         (binders (pre-shared-key-ext-binders psk-ext)))
    ;; Iterate through offered identities
    (loop for identity in identities
          for binder in binders
          for index from 0
          do
             (let ((ticket-data (psk-identity-identity identity))
                   (obfuscated-age (psk-identity-obfuscated-ticket-age identity)))
               ;; Try to decrypt the ticket
               ;; Ticket format: age-add (4) || lifetime (4) || encrypted-data
               (when (>= (length ticket-data) 8)
                 (let* ((stored-age-add (decode-uint32-be ticket-data 0))
                        (stored-lifetime (decode-uint32-be ticket-data 4))
                        (encrypted-data (subseq ticket-data 8)))
                   (multiple-value-bind (resumption-master-secret ticket-cipher-suite
                                         nonce creation-time)
                       (decrypt-session-ticket encrypted-data)
                     (when (and resumption-master-secret
                                ;; Must match the cipher suite we selected
                                (= ticket-cipher-suite cipher-suite)
                                ;; Validate ticket age
                                (validate-ticket-age creation-time stored-lifetime
                                                     obfuscated-age stored-age-add))
                       ;; Derive the PSK
                       (let ((psk (derive-resumption-psk resumption-master-secret
                                                          nonce cipher-suite)))
                         ;; Compute transcript hash up to but not including binders
                         ;; The binders list length is encoded as 2 bytes before the binders
                         ;; We need to find where the binders start in the ClientHello
                         (let* (;; Total binders length (1 byte per binder length + binder data)
                                (total-binders-len
                                  (loop for b in binders
                                        sum (+ 1 (length b))))
                                ;; Truncated hello is everything except the binders
                                ;; The extension ends with: 2 bytes binders length + binders data
                                (truncated-len (- (length raw-client-hello)
                                                  total-binders-len))
                                (truncated-hello (subseq raw-client-hello 0 truncated-len))
                                (digest (cipher-suite-digest cipher-suite))
                                (transcript-hash (ironclad:digest-sequence digest truncated-hello)))
                           ;; Verify the binder
                           (when (verify-binder psk transcript-hash binder cipher-suite)
                             (return-from try-accept-psk
                               (values psk index)))))))))))
    nil))

;;;; ClientHello Processing

(defun process-client-hello (hs message raw-bytes)
  "Process a ClientHello message from the client."
  (let* ((client-hello (handshake-message-body message))
         (extensions (client-hello-extensions client-hello)))
    ;; Update transcript with ClientHello
    (server-handshake-update-transcript hs raw-bytes)
    ;; Store session ID for echo
    (setf (server-handshake-client-session-id hs)
          (client-hello-legacy-session-id client-hello))
    ;; Store client random for SSLKEYLOGFILE
    (setf (server-handshake-client-random hs)
          (client-hello-random client-hello))
    ;; Extract SNI hostname
    (let ((sni-ext (find-extension extensions +extension-server-name+)))
      (when sni-ext
        (setf (server-handshake-client-hostname hs)
              (server-name-ext-host-name (tls-extension-data sni-ext)))))
    ;; Call SNI callback if provided (for virtual hosting)
    (when (and (server-handshake-sni-callback hs)
               (server-handshake-client-hostname hs))
      (multiple-value-bind (cert-chain priv-key)
          (funcall (server-handshake-sni-callback hs)
                   (server-handshake-client-hostname hs))
        (when cert-chain
          (setf (server-handshake-certificate-chain hs) cert-chain))
        (when priv-key
          (setf (server-handshake-private-key hs) priv-key))))
    ;; Verify we have a certificate after SNI callback
    (unless (server-handshake-certificate-chain hs)
      (error 'tls-handshake-error
             :message "No certificate available for this hostname"
             :state :wait-client-hello))
    ;; Select cipher suite (first mutually supported)
    (let ((client-suites (client-hello-cipher-suites client-hello))
          (server-suites (server-handshake-cipher-suites hs)))
      (let ((selected (find-if (lambda (s) (member s client-suites))
                               server-suites)))
        (unless selected
          (error 'tls-handshake-error
                 :message "No common cipher suite"
                 :state :wait-client-hello))
        (setf (server-handshake-selected-cipher-suite hs) selected)))
    ;; Check for supported_versions extension (required for TLS 1.3)
    (let ((sv-ext (find-extension extensions +extension-supported-versions+)))
      (unless sv-ext
        (error 'tls-handshake-error
               :message "Missing supported_versions extension"
               :state :wait-client-hello))
      (unless (member +tls-1.3+ (supported-versions-ext-versions (tls-extension-data sv-ext)))
        (error 'tls-handshake-error
               :message "Client does not support TLS 1.3"
               :state :wait-client-hello)))
    ;; Check for PSK (session resumption) before key exchange
    ;; We need the psk_key_exchange_modes and pre_shared_key extensions
    (let ((psk-modes-ext (find-extension extensions +extension-psk-key-exchange-modes+))
          (psk-ext (find-extension extensions +extension-pre-shared-key+))
          (accepted-psk nil)
          (selected-psk-index nil))
      ;; Try to accept PSK if client offers one with psk_dhe_ke mode
      (when (and psk-modes-ext psk-ext)
        (let* ((modes-data (tls-extension-data psk-modes-ext))
               (modes (psk-key-exchange-modes-ext-modes modes-data)))
          ;; We only support psk_dhe_ke (PSK with (EC)DHE key exchange)
          (when (member +psk-ke-mode-dhe+ modes)
            (multiple-value-setq (accepted-psk selected-psk-index)
              (try-accept-psk hs (tls-extension-data psk-ext) raw-bytes
                              (server-handshake-selected-cipher-suite hs)))
            (when accepted-psk
              (setf (server-handshake-psk-accepted hs) t)
              (setf (server-handshake-accepted-psk hs) accepted-psk)
              (setf (server-handshake-selected-psk-index hs) selected-psk-index)))))
      ;; Process key_share extension
      (let ((ks-ext (find-extension extensions +extension-key-share+)))
        (unless ks-ext
          (error 'tls-handshake-error
                 :message "Missing key_share extension"
                 :state :wait-client-hello))
        (let* ((ks-data (tls-extension-data ks-ext))
               (client-shares (key-share-ext-client-shares ks-data))
               ;; Find first share for a group we support
               (our-groups '(#.+group-x25519+ #.+group-secp256r1+))
               (selected-share (find-if (lambda (share)
                                          (member (key-share-entry-group share) our-groups))
                                        client-shares)))
          (unless selected-share
            (error 'tls-handshake-error
                   :message "No common key exchange group"
                   :state :wait-client-hello))
          ;; Generate our key pair and compute shared secret
          (let* ((group (key-share-entry-group selected-share))
                 (client-public (key-share-entry-key-exchange selected-share))
                 (key-exchange (generate-key-exchange group)))
            (setf (server-handshake-key-exchange hs) key-exchange)
            ;; Compute shared secret
            (let ((shared-secret (compute-shared-secret key-exchange client-public)))
              ;; Initialize key schedule
              (let ((ks (make-key-schedule-state (server-handshake-selected-cipher-suite hs))))
                ;; Use PSK if accepted, otherwise nil (for regular handshake)
                (key-schedule-init ks accepted-psk)
                (key-schedule-derive-handshake-secret ks shared-secret)
                (setf (server-handshake-key-schedule hs) ks)))))))
    ;; Handle ALPN
    (let ((alpn-ext (find-extension extensions +extension-application-layer-protocol-negotiation+)))
      (when (and alpn-ext (server-handshake-alpn-protocols hs))
        (let* ((client-protos (alpn-ext-protocol-list (tls-extension-data alpn-ext)))
               (server-protos (server-handshake-alpn-protocols hs))
               (selected (find-if (lambda (p) (member p client-protos :test #'string=))
                                  server-protos)))
          (setf (server-handshake-selected-alpn hs) selected))))
    (setf (server-handshake-state hs) :send-server-hello)))

;;;; Server Message Generation

(defun generate-server-hello (hs)
  "Generate the ServerHello message."
  (let* ((key-exchange (server-handshake-key-exchange hs))
         (random (random-bytes 32))
         (extensions
           (list
            ;; supported_versions extension (required)
            (make-tls-extension
             :type +extension-supported-versions+
             :data (make-supported-versions-ext :selected-version +tls-1.3+))
            ;; key_share extension
            (make-tls-extension
             :type +extension-key-share+
             :data (make-key-share-ext
                    :server-share (make-key-share-entry
                                   :group (key-exchange-group key-exchange)
                                   :key-exchange (key-exchange-public-key key-exchange)))))))
    ;; Add pre_shared_key extension if PSK was accepted
    (when (server-handshake-psk-accepted hs)
      (push (make-tls-extension
             :type +extension-pre-shared-key+
             :data (make-pre-shared-key-ext
                    :selected-identity (server-handshake-selected-psk-index hs)))
            extensions))
    (make-server-hello
     :legacy-version +tls-1.2+
     :random random
     :legacy-session-id-echo (or (server-handshake-client-session-id hs)
                                  (make-octet-vector 0))
     :cipher-suite (server-handshake-selected-cipher-suite hs)
     :legacy-compression-method 0
     :extensions extensions)))

(defun send-server-hello (hs)
  "Send the ServerHello message."
  (let* ((hello (generate-server-hello hs))
         (hello-bytes (serialize-server-hello hello))
         (message (wrap-handshake-message +handshake-server-hello+ hello-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    ;; Send (unencrypted)
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Derive handshake traffic secrets from transcript (ClientHello + ServerHello)
    (let ((ks (server-handshake-key-schedule hs)))
      (key-schedule-derive-handshake-traffic-secrets
       ks (server-handshake-transcript hs))
      ;; Store client random in key schedule and log secrets for Wireshark
      (setf (key-schedule-client-random ks) (server-handshake-client-random hs))
      (keylog-write-handshake-secrets ks)
      ;; Feed transcript into key schedule for Finished verification
      (key-schedule-update-transcript ks (server-handshake-transcript hs))
      ;; Install server handshake write keys
      (multiple-value-bind (key iv)
          (key-schedule-derive-write-keys ks :handshake)
        (record-layer-install-keys
         (server-handshake-record-layer hs)
         :write key iv
         (server-handshake-selected-cipher-suite hs)))
      ;; Install client handshake read keys
      (multiple-value-bind (key iv)
          (key-schedule-derive-read-keys ks :handshake)
        (record-layer-install-keys
         (server-handshake-record-layer hs)
         :read key iv
         (server-handshake-selected-cipher-suite hs))))
    (setf (server-handshake-state hs) :send-encrypted-extensions)))

(defun generate-encrypted-extensions (hs)
  "Generate the EncryptedExtensions message."
  (let ((extensions nil))
    ;; ALPN if we selected a protocol
    (when (server-handshake-selected-alpn hs)
      (push (make-tls-extension
             :type +extension-application-layer-protocol-negotiation+
             :data (make-alpn-ext
                    :protocol-list (list (server-handshake-selected-alpn hs))))
            extensions))
    (make-encrypted-extensions :extensions (nreverse extensions))))

(defun send-encrypted-extensions (hs)
  "Send the EncryptedExtensions message."
  (let* ((ee (generate-encrypted-extensions hs))
         (ee-bytes (serialize-encrypted-extensions ee))
         (message (wrap-handshake-message +handshake-encrypted-extensions+ ee-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send (encrypted with server handshake keys)
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Decide next state based on verify mode
    (if (plusp (server-handshake-verify-mode hs))
        (setf (server-handshake-state hs) :send-certificate-request)
        (setf (server-handshake-state hs) :send-certificate))))

(defun generate-certificate-request (hs)
  "Generate a CertificateRequest message."
  (declare (ignore hs))  ; Reserved for future customization
  (let ((extensions
          (list
           ;; signature_algorithms is required
           (make-tls-extension
            :type +extension-signature-algorithms+
            :data (make-signature-algorithms-ext
                   :algorithms (list +sig-ecdsa-secp256r1-sha256+
                                     +sig-rsa-pss-rsae-sha256+
                                     +sig-rsa-pkcs1-sha256+))))))
    (make-certificate-request
     :certificate-request-context (make-octet-vector 0)
     :extensions extensions)))

(defun send-certificate-request (hs)
  "Send a CertificateRequest message (for mTLS)."
  (let* ((req (generate-certificate-request hs))
         (req-bytes (serialize-certificate-request req))
         (message (wrap-handshake-message +handshake-certificate-request+ req-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Mark that we requested a certificate
    (setf (server-handshake-certificate-requested hs) t)
    (setf (server-handshake-state hs) :send-certificate)))

(defun generate-certificate-message (hs)
  "Generate the server's Certificate message."
  (let ((cert-entries
          (mapcar (lambda (cert-der)
                    (make-certificate-entry
                     :cert-data cert-der
                     :extensions nil))
                  (server-handshake-certificate-chain hs))))
    (make-certificate-message
     :certificate-request-context (make-octet-vector 0)
     :certificate-list cert-entries)))

(defun send-certificate (hs)
  "Send the server's Certificate message."
  (let* ((cert-msg (generate-certificate-message hs))
         (cert-bytes (serialize-certificate-message cert-msg))
         (message (wrap-handshake-message +handshake-certificate+ cert-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    (setf (server-handshake-state hs) :send-certificate-verify)))

(defun send-certificate-verify (hs)
  "Send the CertificateVerify message."
  (let* ((ks (server-handshake-key-schedule hs))
         (transcript-hash (key-schedule-transcript-hash-value ks))
         ;; Build the content to sign (per RFC 8446 Section 4.4.3)
         (content (make-certificate-verify-content transcript-hash nil)) ; nil = server
         ;; Sign with server's private key
         (signature (sign-data content (server-handshake-private-key hs)))
         (algorithm (signature-algorithm-for-key (server-handshake-private-key hs)))
         (cv (make-certificate-verify :algorithm algorithm :signature signature))
         (cv-bytes (serialize-certificate-verify cv))
         (message (wrap-handshake-message +handshake-certificate-verify+ cv-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    (setf (server-handshake-state hs) :send-finished)))

(defun send-server-finished (hs)
  "Send the server's Finished message."
  (let* ((ks (server-handshake-key-schedule hs))
         (cipher-suite (server-handshake-selected-cipher-suite hs))
         ;; Compute verify_data
         (transcript-hash (key-schedule-transcript-hash-value ks))
         (verify-data (compute-finished-verify-data
                       (key-schedule-server-handshake-traffic-secret ks)
                       transcript-hash
                       cipher-suite))
         (finished (make-finished-message :verify-data verify-data))
         (finished-bytes (serialize-finished finished))
         (message (wrap-handshake-message +handshake-finished+ finished-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Derive master secret and application secrets
    (key-schedule-derive-master-secret ks)
    (key-schedule-derive-application-traffic-secrets ks (server-handshake-transcript hs))
    ;; Log application secrets for Wireshark
    (keylog-write-application-secrets ks)
    ;; Install server application write keys
    (multiple-value-bind (key iv)
        (key-schedule-derive-write-keys ks :application)
      (record-layer-install-keys
       (server-handshake-record-layer hs)
       :write key iv
       cipher-suite))
    ;; Next state depends on whether we requested a client certificate
    (if (server-handshake-certificate-requested hs)
        (setf (server-handshake-state hs) :wait-client-certificate)
        (setf (server-handshake-state hs) :wait-client-finished))))

;;;; Client Certificate Processing (mTLS)

(defun process-client-certificate (hs message raw-bytes)
  "Process the client's Certificate message."
  (let* ((cert-msg (handshake-message-body message))
         (cert-list (certificate-message-certificate-list cert-msg)))
    ;; Update transcript
    (server-handshake-update-transcript hs raw-bytes)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) raw-bytes)
    ;; Check if client provided a certificate
    (if (null cert-list)
        ;; No certificate provided
        (if (= (server-handshake-verify-mode hs) +verify-required+)
            (error 'tls-certificate-error
                   :message "Client certificate required but not provided")
            ;; verify-peer mode: certificate optional, proceed
            (setf (server-handshake-state hs) :wait-client-finished))
        ;; Parse and store the certificate chain
        (let ((parsed-chain
                (mapcar (lambda (entry)
                          (parse-certificate (certificate-entry-cert-data entry)))
                        cert-list)))
          (setf (server-handshake-peer-certificate-chain hs) parsed-chain)
          (setf (server-handshake-peer-certificate hs) (first parsed-chain))
          (setf (server-handshake-state hs) :wait-client-certificate-verify)))))

(defun process-client-certificate-verify (hs message raw-bytes)
  "Process the client's CertificateVerify message."
  (let* ((cv (handshake-message-body message))
         (ks (server-handshake-key-schedule hs))
         ;; Get transcript hash BEFORE adding this message
         (transcript-hash (key-schedule-transcript-hash-value ks))
         ;; Build expected content (per RFC 8446 Section 4.4.3)
         (content (make-certificate-verify-content transcript-hash t)) ; t = client
         (cert (server-handshake-peer-certificate hs))
         (algorithm (certificate-verify-algorithm cv))
         (signature (certificate-verify-signature cv)))
    ;; Verify signature
    (verify-certificate-verify-signature cert algorithm signature content)
    ;; Update transcript AFTER verification
    (server-handshake-update-transcript hs raw-bytes)
    (key-schedule-update-transcript ks raw-bytes)
    ;; Verify certificate chain if trust store is configured
    (when (server-handshake-trust-store hs)
      (verify-certificate-chain
       (server-handshake-peer-certificate-chain hs)
       (server-handshake-trust-store hs)))
    (setf (server-handshake-state hs) :wait-client-finished)))

(defun process-client-finished (hs message raw-bytes)
  "Process the client's Finished message."
  (let* ((finished (handshake-message-body message))
         (received-verify-data (finished-message-verify-data finished))
         (ks (server-handshake-key-schedule hs))
         (cipher-suite (server-handshake-selected-cipher-suite hs))
         ;; Compute expected verify_data (using transcript BEFORE this message)
         (transcript-hash (key-schedule-transcript-hash-value ks))
         (expected-verify-data (compute-finished-verify-data
                                (key-schedule-client-handshake-traffic-secret ks)
                                transcript-hash
                                cipher-suite)))
    ;; Verify
    (unless (constant-time-equal received-verify-data expected-verify-data)
      (error 'tls-handshake-error
             :message "Client Finished verification failed"
             :state :wait-client-finished))
    ;; Update transcript AFTER verification
    (server-handshake-update-transcript hs raw-bytes)
    (key-schedule-update-transcript ks raw-bytes)
    ;; Derive resumption master secret (for session tickets)
    (key-schedule-derive-resumption-master-secret ks (server-handshake-transcript hs))
    (setf (server-handshake-resumption-master-secret hs)
          (key-schedule-resumption-master-secret ks))
    ;; Install client application read keys
    (multiple-value-bind (key iv)
        (key-schedule-derive-read-keys ks :application)
      (record-layer-install-keys
       (server-handshake-record-layer hs)
       :read key iv
       cipher-suite))
    (setf (server-handshake-state hs) :connected)))

;;;; Session Resumption (NewSessionTicket)

(defconstant +default-ticket-lifetime+ 86400
  "Default ticket lifetime: 24 hours in seconds.")

(defun generate-new-session-ticket (hs)
  "Generate a NewSessionTicket message for session resumption."
  (let* ((cipher-suite (server-handshake-selected-cipher-suite hs))
         (resumption-master-secret (server-handshake-resumption-master-secret hs))
         (ticket-lifetime +default-ticket-lifetime+)
         (age-add (random (expt 2 32)))  ; Random 32-bit value for obfuscation
         ;; Generate unique nonce for this ticket
         (nonce-counter (server-handshake-ticket-nonce-counter hs))
         (nonce (octet-vector (ldb (byte 8 0) nonce-counter)))
         (creation-time (get-internal-real-time))
         ;; Encrypt session state into ticket
         (ticket-data (encrypt-session-ticket
                       resumption-master-secret
                       cipher-suite
                       nonce
                       creation-time)))
    ;; Increment nonce counter for next ticket
    (incf (server-handshake-ticket-nonce-counter hs))
    ;; Create the NewSessionTicket structure
    ;; We also include the age-add in the ticket for later validation
    ;; by prepending it to the encrypted ticket data
    (let ((full-ticket (concat-octet-vectors
                        (encode-uint32-be age-add)
                        (encode-uint32-be ticket-lifetime)
                        ticket-data)))
      (make-new-session-ticket
       :ticket-lifetime ticket-lifetime
       :ticket-age-add age-add
       :ticket-nonce nonce
       :ticket full-ticket
       :extensions nil))))

(defun send-new-session-ticket (hs)
  "Send a NewSessionTicket message to enable session resumption.
   This should be called after the handshake completes."
  (when (server-handshake-resumption-master-secret hs)
    (let* ((nst (generate-new-session-ticket hs))
           (nst-bytes (serialize-new-session-ticket nst))
           (message (wrap-handshake-message +handshake-new-session-ticket+ nst-bytes)))
      ;; Send (encrypted with server application keys)
      (record-layer-write-handshake (server-handshake-record-layer hs) message))))

;;;; Handshake Message Reading

(defun server-read-handshake-message (hs &key update-transcript)
  "Read the next handshake message from the record layer.
   Returns (VALUES message raw-bytes)."
  (multiple-value-bind (content-type data)
      (record-layer-read (server-handshake-record-layer hs))
    ;; Handle alerts
    (when (= content-type +content-type-alert+)
      (process-alert data))
    ;; Skip change_cipher_spec (compatibility)
    (when (= content-type +content-type-change-cipher-spec+)
      (return-from server-read-handshake-message
        (server-read-handshake-message hs :update-transcript update-transcript)))
    ;; Must be handshake
    (unless (= content-type +content-type-handshake+)
      (error 'tls-handshake-error
             :message (format nil "Expected handshake, got content type ~D" content-type)
             :state (server-handshake-state hs)))
    (let ((hash-length (when (server-handshake-selected-cipher-suite hs)
                         (cipher-suite-hash-length
                          (server-handshake-selected-cipher-suite hs)))))
      (values (parse-handshake-message data :hash-length hash-length)
              data))))

;;;; Main Handshake Orchestration

(defun perform-server-handshake (record-layer certificate-chain private-key
                                  &key alpn-protocols verify-mode trust-store
                                       cipher-suites sni-callback)
  "Perform the TLS 1.3 server handshake.
   Returns the server-handshake structure on success.

   SNI-CALLBACK, if provided, is called with the client's requested hostname.
   It should return (VALUES certificate-chain private-key) to use for that host,
   or NIL to use the default certificate/key. This enables virtual hosting."
  (let ((hs (make-server-handshake
             :record-layer record-layer
             :certificate-chain certificate-chain
             :private-key private-key
             :alpn-protocols alpn-protocols
             :verify-mode (or verify-mode +verify-none+)
             :trust-store trust-store
             :cipher-suites (or cipher-suites
                                (list +tls-chacha20-poly1305-sha256+
                                      +tls-aes-256-gcm-sha384+
                                      +tls-aes-128-gcm-sha256+))
             :sni-callback sni-callback)))
    ;; State machine loop
    (loop
      (case (server-handshake-state hs)
        (:start
         (setf (server-handshake-state hs) :wait-client-hello))

        (:wait-client-hello
         (multiple-value-bind (message raw-bytes)
             (server-read-handshake-message hs)
           (unless (= (handshake-message-type message) +handshake-client-hello+)
             (error 'tls-handshake-error
                    :message "Expected ClientHello"
                    :state :wait-client-hello))
           (process-client-hello hs message raw-bytes)))

        (:send-server-hello
         (send-server-hello hs))

        (:send-encrypted-extensions
         (send-encrypted-extensions hs))

        (:send-certificate-request
         (send-certificate-request hs))

        (:send-certificate
         (send-certificate hs))

        (:send-certificate-verify
         (send-certificate-verify hs))

        (:send-finished
         (send-server-finished hs))

        (:wait-client-certificate
         (multiple-value-bind (message raw-bytes)
             (server-read-handshake-message hs)
           (unless (= (handshake-message-type message) +handshake-certificate+)
             (error 'tls-handshake-error
                    :message "Expected client Certificate"
                    :state :wait-client-certificate))
           (process-client-certificate hs message raw-bytes)))

        (:wait-client-certificate-verify
         (multiple-value-bind (message raw-bytes)
             (server-read-handshake-message hs)
           (unless (= (handshake-message-type message) +handshake-certificate-verify+)
             (error 'tls-handshake-error
                    :message "Expected client CertificateVerify"
                    :state :wait-client-certificate-verify))
           (process-client-certificate-verify hs message raw-bytes)))

        (:wait-client-finished
         (multiple-value-bind (message raw-bytes)
             (server-read-handshake-message hs)
           (unless (= (handshake-message-type message) +handshake-finished+)
             (error 'tls-handshake-error
                    :message "Expected client Finished"
                    :state :wait-client-finished))
           (process-client-finished hs message raw-bytes)))

        (:connected
         ;; Send NewSessionTicket for session resumption
         (send-new-session-ticket hs)
         (return hs))

        (t
         (error 'tls-handshake-error
                :message (format nil "Unknown state: ~A" (server-handshake-state hs))
                :state (server-handshake-state hs)))))))
