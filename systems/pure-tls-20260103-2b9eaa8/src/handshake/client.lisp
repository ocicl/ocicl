;;; client.lisp --- TLS 1.3 Client Handshake
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements the TLS 1.3 client handshake state machine.

(in-package #:pure-tls)

;;;; Client Handshake State

(defstruct client-handshake
  "Client handshake state."
  ;; Configuration
  (hostname nil :type (or null string))
  (alpn-protocols nil :type list)
  (verify-mode +verify-required+ :type fixnum)
  ;; Trust store for certificate verification
  (trust-store nil)
  ;; Cipher suites we support (in preference order)
  ;; ChaCha20-Poly1305 is preferred when available because it provides
  ;; better side-channel resistance than AES-GCM in pure software implementations.
  (cipher-suites (list +tls-chacha20-poly1305-sha256+
                       +tls-aes-256-gcm-sha384+
                       +tls-aes-128-gcm-sha256+)
                 :type list)
  ;; Key exchange state
  (key-exchange nil)
  ;; Selected cipher suite
  (selected-cipher-suite nil)
  ;; Key schedule
  (key-schedule nil)
  ;; Record layer
  (record-layer nil)
  ;; Handshake transcript (raw bytes for hashing)
  (transcript nil :type (or null octet-vector))
  ;; Buffer for handshake messages (multiple messages may arrive in one record)
  (message-buffer nil :type (or null octet-vector))
  ;; Server's certificate (parsed X.509)
  (peer-certificate nil)
  ;; Full certificate chain (list of parsed certificates)
  (peer-certificate-chain nil :type list)
  ;; Selected ALPN protocol
  (selected-alpn nil :type (or null string))
  ;; Client random (for SSLKEYLOGFILE)
  (client-random nil :type (or null octet-vector))
  ;; HelloRetryRequest handling
  (hello-retry-count 0 :type fixnum)  ; Prevent infinite HRR loops
  (hrr-cookie nil :type (or null octet-vector))  ; Cookie from HRR
  (hrr-selected-group nil)  ; Group requested by server in HRR
  ;; Session resumption (PSK)
  (offered-psk nil)  ; session-ticket if we offered a PSK
  (psk-accepted nil :type boolean)  ; T if server accepted our PSK
  (resumption-master-secret nil :type (or null octet-vector))  ; For ticket derivation
  ;; State
  (state :start))

(defun client-handshake-update-transcript (hs message-bytes)
  "Add message bytes to the handshake transcript."
  (setf (client-handshake-transcript hs)
        (if (client-handshake-transcript hs)
            (concat-octet-vectors (client-handshake-transcript hs) message-bytes)
            message-bytes))
  ;; Also update the key schedule's transcript hash
  (when (client-handshake-key-schedule hs)
    (key-schedule-update-transcript (client-handshake-key-schedule hs) message-bytes)))

;;;; ClientHello Generation

(defun generate-client-hello (hs &key requested-group)
  "Generate a ClientHello message.
   REQUESTED-GROUP, if provided, specifies the key exchange group to use
   (used when responding to HelloRetryRequest)."
  (let* ((random (random-bytes 32))
         (session-id (random-bytes 32))  ; Legacy, but some servers expect it
         ;; Generate key share for requested group (from HRR) or preferred group
         (key-share-group (or requested-group *preferred-group*))
         (key-exchange (generate-key-exchange key-share-group))
         ;; Build extensions
         (extensions (list
                      ;; supported_versions (required for TLS 1.3)
                      (make-tls-extension
                       :type +extension-supported-versions+
                       :data (make-supported-versions-ext :versions (list +tls-1.3+)))
                      ;; supported_groups
                      (make-tls-extension
                       :type +extension-supported-groups+
                       :data (make-supported-groups-ext :groups *supported-groups*))
                      ;; signature_algorithms
                      (make-tls-extension
                       :type +extension-signature-algorithms+
                       :data (make-signature-algorithms-ext
                              :algorithms (supported-signature-algorithms)))
                      ;; key_share
                      (make-tls-extension
                       :type +extension-key-share+
                       :data (make-key-share-ext
                              :client-shares
                              (list (make-key-share-entry
                                     :group (key-exchange-group key-exchange)
                                     :key-exchange (key-exchange-public-key key-exchange))))))))
    ;; Add SNI if hostname provided
    (when (client-handshake-hostname hs)
      (push (make-tls-extension
             :type +extension-server-name+
             :data (make-server-name-ext
                    :host-name (client-handshake-hostname hs)))
            extensions))
    ;; Add ALPN if protocols provided
    (when (client-handshake-alpn-protocols hs)
      (push (make-tls-extension
             :type +extension-application-layer-protocol-negotiation+
             :data (make-alpn-ext
                    :protocol-list (client-handshake-alpn-protocols hs)))
            extensions))
    ;; Add cookie if responding to HelloRetryRequest
    (when (client-handshake-hrr-cookie hs)
      (push (make-tls-extension
             :type +extension-cookie+
             :data (client-handshake-hrr-cookie hs))
            extensions))
    ;; Check for cached session ticket for resumption
    (let ((ticket (and (client-handshake-hostname hs)
                       (session-ticket-cache-get (client-handshake-hostname hs)))))
      (when ticket
        ;; Add psk_key_exchange_modes extension (required when offering PSK)
        (push (make-tls-extension
               :type +extension-psk-key-exchange-modes+
               :data (make-psk-key-exchange-modes-ext
                      :modes (list +psk-dhe-ke+)))  ; We require DHE with PSK
              extensions)
        ;; Store that we're offering this ticket
        (setf (client-handshake-offered-psk hs) ticket)))
    ;; Store key exchange for later
    (setf (client-handshake-key-exchange hs) key-exchange)
    ;; Store client random for SSLKEYLOGFILE
    (setf (client-handshake-client-random hs) random)
    ;; Build ClientHello (extensions in reverse order, PSK will be added separately)
    (let ((hello (make-client-hello
                  :legacy-version +tls-1.2+
                  :random random
                  :legacy-session-id session-id
                  :cipher-suites (client-handshake-cipher-suites hs)
                  :legacy-compression-methods (octet-vector 0)
                  :extensions (nreverse extensions))))
      ;; If offering PSK, add pre_shared_key extension with binder
      (when (client-handshake-offered-psk hs)
        (setf hello (add-psk-extension-to-client-hello hs hello)))
      hello)))

(defun add-psk-extension-to-client-hello (hs hello)
  "Add pre_shared_key extension with binder to ClientHello.
   The binder is computed over the partial ClientHello message.
   Returns a new ClientHello with the PSK extension added."
  (let* ((ticket (client-handshake-offered-psk hs))
         (cipher-suite (session-ticket-cipher-suite ticket))
         ;; Derive PSK from resumption master secret
         (psk (derive-resumption-psk (session-ticket-resumption-master-secret ticket)
                                     (session-ticket-nonce ticket)
                                     cipher-suite))
         ;; Build PSK identity
         (obfuscated-age (compute-obfuscated-ticket-age ticket))
         (identity (make-psk-identity
                    :identity (session-ticket-identity ticket)
                    :obfuscated-ticket-age obfuscated-age))
         ;; Create pre_shared_key extension with placeholder binder
         (hash-len (ironclad:digest-length (cipher-suite-digest cipher-suite)))
         (placeholder-binder (make-octet-vector hash-len))
         (psk-ext (make-tls-extension
                   :type +extension-pre-shared-key+
                   :data (make-pre-shared-key-ext
                          :identities (list identity)
                          :binders (list placeholder-binder)))))
    ;; Add PSK extension to ClientHello (must be last)
    (setf (client-hello-extensions hello)
          (append (client-hello-extensions hello) (list psk-ext)))
    ;; Serialize to compute partial transcript
    (let* ((hello-bytes (serialize-client-hello hello))
           (binders-len (pre-shared-key-ext-binders-length
                         (tls-extension-data psk-ext)))
           ;; Transcript for binder = CH without binders
           (partial-hello (subseq hello-bytes 0 (- (length hello-bytes) binders-len)))
           (partial-message (wrap-handshake-message +handshake-client-hello+ partial-hello))
           (transcript-hash (ironclad:digest-sequence
                             (cipher-suite-digest cipher-suite)
                             partial-message))
           ;; Compute actual binder
           (binder (compute-binder psk transcript-hash cipher-suite)))
      ;; Update the binder in the extension
      (setf (pre-shared-key-ext-binders (tls-extension-data psk-ext))
            (list binder))
      hello)))

(defun send-client-hello (hs &key requested-group)
  "Send ClientHello message.
   REQUESTED-GROUP is used when responding to HelloRetryRequest."
  (let* ((hello (generate-client-hello hs :requested-group requested-group))
         (hello-bytes (serialize-client-hello hello))
         (message (wrap-handshake-message +handshake-client-hello+ hello-bytes)))
    ;; Update transcript
    (client-handshake-update-transcript hs message)
    ;; Send
    (record-layer-write-handshake (client-handshake-record-layer hs) message)
    (setf (client-handshake-state hs) :wait-server-hello)))

;;;; HelloRetryRequest Processing

(defun make-message-hash (transcript cipher-suite)
  "Create the message_hash synthetic message per RFC 8446 Section 4.4.1.
   This replaces ClientHello1 in the transcript when processing HelloRetryRequest."
  (let* ((digest (cipher-suite-digest cipher-suite))
         (hash-len (ironclad:digest-length digest))
         (ch1-hash (ironclad:digest-sequence digest transcript)))
    ;; message_hash = handshake_type(254) + length(hash-len) + Hash(CH1)
    (concat-octet-vectors
     (octet-vector 254)  ; message_hash handshake type
     (encode-uint24 hash-len)
     ch1-hash)))

(defun process-hello-retry-request (hs server-hello hrr-raw-bytes)
  "Process a HelloRetryRequest and send a new ClientHello.
   Returns T to indicate the handshake should continue waiting for ServerHello."
  ;; Check for infinite HRR loop
  (when (>= (client-handshake-hello-retry-count hs) 1)
    (error 'tls-handshake-error
           :message "Received multiple HelloRetryRequests"
           :state :wait-server-hello))
  (incf (client-handshake-hello-retry-count hs))

  (let ((extensions (server-hello-extensions server-hello)))
    ;; Get the selected cipher suite (needed for hash algorithm)
    (let ((cipher-suite (server-hello-cipher-suite server-hello)))
      (unless (member cipher-suite (client-handshake-cipher-suites hs))
        (error 'tls-handshake-error
               :message "Server selected unsupported cipher suite in HRR"
               :state :wait-server-hello))
      (setf (client-handshake-selected-cipher-suite hs) cipher-suite))

    ;; Extract requested key share group
    (let ((ks-ext (find-extension extensions +extension-key-share+)))
      (when ks-ext
        (let ((selected-group (key-share-ext-selected-group (tls-extension-data ks-ext))))
          (when selected-group
            ;; Verify we support this group
            (unless (member selected-group *supported-groups*)
              (error 'tls-handshake-error
                     :message "Server requested unsupported key exchange group"
                     :state :wait-server-hello))
            (setf (client-handshake-hrr-selected-group hs) selected-group)))))

    ;; Extract cookie if present
    (let ((cookie-ext (find-extension extensions +extension-cookie+)))
      (when cookie-ext
        (setf (client-handshake-hrr-cookie hs)
              (tls-extension-data cookie-ext))))

    ;; Replace transcript with message_hash per RFC 8446 Section 4.4.1
    ;; The transcript becomes: message_hash || HelloRetryRequest
    (let ((message-hash (make-message-hash
                         (client-handshake-transcript hs)
                         (client-handshake-selected-cipher-suite hs))))
      (setf (client-handshake-transcript hs)
            (concat-octet-vectors message-hash hrr-raw-bytes)))

    ;; Send new ClientHello with requested group and cookie
    (send-client-hello hs :requested-group (client-handshake-hrr-selected-group hs))))

;;;; ServerHello Processing

(defun process-server-hello (hs message raw-bytes)
  "Process ServerHello message.
   RAW-BYTES are the raw message bytes (needed for transcript handling)."
  (let* ((server-hello (handshake-message-body message))
         (extensions (server-hello-extensions server-hello)))
    ;; Check for HelloRetryRequest
    (when (hello-retry-request-p server-hello)
      (process-hello-retry-request hs server-hello raw-bytes)
      (return-from process-server-hello nil))
    ;; Normal ServerHello - update transcript now
    (client-handshake-update-transcript hs raw-bytes)
    ;; Verify supported_versions extension
    (let ((sv-ext (find-extension extensions +extension-supported-versions+)))
      (unless sv-ext
        (error 'tls-handshake-error
               :message "Missing supported_versions extension"
               :state :wait-server-hello))
      (unless (= (supported-versions-ext-selected-version (tls-extension-data sv-ext))
                 +tls-1.3+)
        (error 'tls-handshake-error
               :message "Server did not select TLS 1.3"
               :state :wait-server-hello)))
    ;; Get selected cipher suite
    (let ((cipher-suite (server-hello-cipher-suite server-hello)))
      (unless (member cipher-suite (client-handshake-cipher-suites hs))
        (error 'tls-handshake-error
               :message "Server selected unsupported cipher suite"
               :state :wait-server-hello))
      (setf (client-handshake-selected-cipher-suite hs) cipher-suite))
    ;; Process key_share extension
    (let ((ks-ext (find-extension extensions +extension-key-share+)))
      (unless ks-ext
        (error 'tls-handshake-error
               :message "Missing key_share extension"
               :state :wait-server-hello))
      (let* ((ks-data (tls-extension-data ks-ext))
             (server-share (key-share-ext-server-share ks-data))
             (server-group (key-share-entry-group server-share))
             (server-public (key-share-entry-key-exchange server-share)))
        ;; Verify server used our offered group
        (unless (= server-group (key-exchange-group (client-handshake-key-exchange hs)))
          (error 'tls-handshake-error
                 :message "Server used different key exchange group"
                 :state :wait-server-hello))
        ;; Check for PSK acceptance
        (let ((psk-ext (find-extension extensions +extension-pre-shared-key+)))
          (when (and psk-ext (client-handshake-offered-psk hs))
            ;; Server accepted our PSK - parse the extension
            (let* ((psk-data (parse-pre-shared-key-extension
                              (tls-extension-data psk-ext)
                              :server-hello-p t))
                   (selected-id (pre-shared-key-ext-selected-identity psk-data)))
              ;; We only offered one PSK, so selected must be 0
              (unless (zerop selected-id)
                (error 'tls-handshake-error
                       :message "Server selected invalid PSK identity"
                       :state :wait-server-hello))
              (setf (client-handshake-psk-accepted hs) t))))
        ;; Compute shared secret
        (let ((shared-secret (compute-shared-secret
                              (client-handshake-key-exchange hs)
                              server-public)))
          ;; Initialize key schedule (with PSK if accepted)
          (let* ((ks (make-key-schedule-state (client-handshake-selected-cipher-suite hs)))
                 (psk (when (client-handshake-psk-accepted hs)
                        (let ((ticket (client-handshake-offered-psk hs)))
                          (derive-resumption-psk
                           (session-ticket-resumption-master-secret ticket)
                           (session-ticket-nonce ticket)
                           (session-ticket-cipher-suite ticket))))))
            (key-schedule-init ks psk)
            (key-schedule-derive-handshake-secret ks shared-secret)
            ;; Derive handshake traffic secrets from transcript so far
            (key-schedule-derive-handshake-traffic-secrets
             ks (client-handshake-transcript hs))
            (setf (client-handshake-key-schedule hs) ks)
            ;; Store client random in key schedule and log secrets for Wireshark
            (setf (key-schedule-client-random ks) (client-handshake-client-random hs))
            (keylog-write-handshake-secrets ks)
            ;; Feed existing transcript (ClientHello + ServerHello) into the key schedule's
            ;; incremental transcript hash for later use in Finished verification
            (key-schedule-update-transcript ks (client-handshake-transcript hs))
            ;; Install server handshake keys for reading
            (multiple-value-bind (key iv)
                (key-schedule-derive-read-keys ks :handshake)
              (record-layer-install-keys
               (client-handshake-record-layer hs)
               :read key iv
               (client-handshake-selected-cipher-suite hs)))))))
    (setf (client-handshake-state hs) :wait-encrypted-extensions)))

;;;; Encrypted Extensions Processing

(defun process-encrypted-extensions (hs message)
  "Process EncryptedExtensions message."
  (let* ((ee (handshake-message-body message))
         (extensions (encrypted-extensions-extensions ee)))
    ;; Process ALPN if present
    (let ((alpn-ext (find-extension extensions +extension-application-layer-protocol-negotiation+)))
      (when alpn-ext
        (let* ((alpn-data (tls-extension-data alpn-ext))
               (protocols (alpn-ext-protocol-list alpn-data)))
          (when protocols
            (setf (client-handshake-selected-alpn hs) (first protocols))))))
    ;; In PSK mode with resumption, server skips Certificate/CertificateVerify
    ;; Otherwise, expect Certificate next
    (setf (client-handshake-state hs) :wait-cert-or-finished)))

;;;; Certificate Processing

(defun process-certificate (hs message)
  "Process Certificate message."
  (let* ((cert-msg (handshake-message-body message))
         (cert-list (certificate-message-certificate-list cert-msg)))
    (when (null cert-list)
      (when (= (client-handshake-verify-mode hs) +verify-required+)
        (error 'tls-certificate-error
               :message "Server did not provide a certificate")))
    ;; Parse and store the entire certificate chain
    (when cert-list
      (let ((parsed-chain
              (mapcar (lambda (entry)
                        (parse-certificate (certificate-entry-cert-data entry)))
                      cert-list)))
        ;; Store the full chain
        (setf (client-handshake-peer-certificate-chain hs) parsed-chain)
        ;; Store the leaf certificate (first in chain) for easy access
        (setf (client-handshake-peer-certificate hs) (first parsed-chain))))
    (setf (client-handshake-state hs) :wait-certificate-verify)))

;;;; CertificateVerify Processing

(defun make-certificate-verify-content (transcript-hash &optional client-p)
  "Construct the content that is signed in CertificateVerify.
   Per RFC 8446 Section 4.4.3, this is:
   - 64 bytes of 0x20 (space)
   - The context string ('TLS 1.3, server CertificateVerify' or 'TLS 1.3, client CertificateVerify')
   - A single 0 byte
   - The transcript hash
   If CLIENT-P is true, uses the client context string; otherwise uses server."
  (let* ((context-string (if client-p
                             "TLS 1.3, client CertificateVerify"
                             "TLS 1.3, server CertificateVerify"))
         (content-length (+ 64 (length context-string) 1 (length transcript-hash)))
         (content (make-octet-vector content-length))
         (pos 0))
    ;; 64 spaces (0x20)
    (dotimes (i 64)
      (setf (aref content pos) #x20)
      (incf pos))
    ;; Context string
    (loop for char across context-string do
      (setf (aref content pos) (char-code char))
      (incf pos))
    ;; Single 0 byte
    (setf (aref content pos) 0)
    (incf pos)
    ;; Transcript hash
    (replace content transcript-hash :start1 pos)
    content))

(defun verify-certificate-verify-signature (cert algorithm signature content)
  "Verify the CertificateVerify signature using the certificate's public key.
   Returns T on success, signals an error on failure."
  (let* ((public-key-info (x509-certificate-subject-public-key-info cert))
         (key-algorithm (getf public-key-info :algorithm))
         (public-key-bytes (getf public-key-info :public-key)))
    ;; Determine hash algorithm from signature algorithm
    (multiple-value-bind (hash-algo sig-type)
        (signature-algorithm-params algorithm)
      (unless sig-type
        (error 'tls-handshake-error
               :message (format nil "Unsupported signature algorithm: ~X" algorithm)))
      ;; Verify based on key type
      (cond
        ;; RSA-PSS signatures
        ((member sig-type '(:rsa-pss-rsae :rsa-pss-pss))
         (verify-rsa-pss-signature public-key-bytes content signature hash-algo))
        ;; RSA PKCS#1 v1.5 signatures
        ((eq sig-type :rsa-pkcs1)
         (verify-rsa-pkcs1-signature public-key-bytes content signature hash-algo))
        ;; ECDSA signatures
        ((eq sig-type :ecdsa)
         (verify-ecdsa-signature public-key-bytes content signature hash-algo key-algorithm))
        ;; Ed25519 (handles its own hashing internally)
        ((eq sig-type :ed25519)
         (verify-ed25519-signature public-key-bytes content signature))
        (t
         (error 'tls-handshake-error
                :message (format nil "Unsupported signature type: ~A" sig-type)))))))

(defun signature-algorithm-params (algorithm)
  "Return (hash-algorithm signature-type) for a TLS signature algorithm code."
  (case algorithm
    (#.+sig-rsa-pkcs1-sha256+ (values :sha256 :rsa-pkcs1))
    (#.+sig-rsa-pkcs1-sha384+ (values :sha384 :rsa-pkcs1))
    (#.+sig-rsa-pkcs1-sha512+ (values :sha512 :rsa-pkcs1))
    (#.+sig-rsa-pss-rsae-sha256+ (values :sha256 :rsa-pss-rsae))
    (#.+sig-rsa-pss-rsae-sha384+ (values :sha384 :rsa-pss-rsae))
    (#.+sig-rsa-pss-rsae-sha512+ (values :sha512 :rsa-pss-rsae))
    (#.+sig-rsa-pss-pss-sha256+ (values :sha256 :rsa-pss-pss))
    (#.+sig-rsa-pss-pss-sha384+ (values :sha384 :rsa-pss-pss))
    (#.+sig-rsa-pss-pss-sha512+ (values :sha512 :rsa-pss-pss))
    (#.+sig-ecdsa-secp256r1-sha256+ (values :sha256 :ecdsa))
    (#.+sig-ecdsa-secp384r1-sha384+ (values :sha384 :ecdsa))
    (#.+sig-ecdsa-secp521r1-sha512+ (values :sha512 :ecdsa))
    (#.+sig-ed25519+ (values nil :ed25519))
    (t (values nil nil))))

(defun verify-rsa-pss-signature (public-key-der content signature hash-algo)
  "Verify an RSA-PSS signature."
  (handler-case
      (let ((public-key (ironclad:make-public-key :rsa
                          :n (parse-rsa-public-key-n public-key-der)
                          :e (parse-rsa-public-key-e public-key-der))))
        (ironclad:verify-signature public-key content signature
                                   :pss hash-algo))
    (error (e)
      (error 'tls-handshake-error
             :message (format nil "RSA-PSS signature verification failed: ~A" e)))))

(defun verify-rsa-pkcs1-signature (public-key-der content signature hash-algo)
  "Verify an RSA PKCS#1 v1.5 signature."
  (handler-case
      (let ((public-key (ironclad:make-public-key :rsa
                          :n (parse-rsa-public-key-n public-key-der)
                          :e (parse-rsa-public-key-e public-key-der))))
        (ironclad:verify-signature public-key content signature
                                   :pkcs1v15 hash-algo))
    (error (e)
      (error 'tls-handshake-error
             :message (format nil "RSA PKCS#1 signature verification failed: ~A" e)))))

(defun verify-ecdsa-signature (public-key-der content signature hash-algo key-algorithm)
  "Verify an ECDSA signature."
  (handler-case
      (let* ((curve (ecdsa-curve-from-algorithm key-algorithm))
             ;; Parse the ECDSA signature (DER encoded r and s)
             (parsed-sig (parse-ecdsa-signature signature))
             (r (getf parsed-sig :r))
             (s (getf parsed-sig :s))
             ;; Get coordinate size for the curve
             (coord-size (ecase curve
                           (:secp256r1 32)
                           (:secp384r1 48)
                           (:secp521r1 66)))
             ;; Convert r and s to byte arrays for ironclad:make-signature
             (r-bytes (ironclad:integer-to-octets r :n-bits (* 8 coord-size)))
             (s-bytes (ironclad:integer-to-octets s :n-bits (* 8 coord-size)))
             ;; Hash the content
             (hash (ironclad:digest-sequence hash-algo content))
             ;; Make the public key
             (public-key (make-ecdsa-public-key curve public-key-der)))
        ;; Use ironclad:make-signature with the curve name and byte arrays
        (ironclad:verify-signature public-key hash
                                   (ironclad:make-signature curve :r r-bytes :s s-bytes)))
    (error (e)
      (error 'tls-handshake-error
             :message (format nil "ECDSA signature verification failed: ~A" e)))))

(defun verify-ed25519-signature (public-key-der content signature)
  "Verify an Ed25519 signature."
  (handler-case
      (let ((public-key (ironclad:make-public-key :ed25519 :y public-key-der)))
        (ironclad:verify-signature public-key content signature))
    (error (e)
      (error 'tls-handshake-error
             :message (format nil "Ed25519 signature verification failed: ~A" e)))))

(defun ecdsa-curve-from-algorithm (key-algorithm)
  "Determine the ECDSA curve from the key algorithm OID."
  (cond
    ((member key-algorithm '(:ecdsa-p256 :secp256r1 :prime256v1)) :secp256r1)
    ((member key-algorithm '(:ecdsa-p384 :secp384r1)) :secp384r1)
    ((member key-algorithm '(:ecdsa-p521 :secp521r1)) :secp521r1)
    (t :secp256r1)))  ; Default to P-256

;;;; Signature Generation (for server CertificateVerify and client auth)

(defun signature-algorithm-for-key (private-key)
  "Determine the TLS signature algorithm code for a private key.
   Returns the signature algorithm ID for TLS."
  (let ((key-type (type-of private-key)))
    (cond
      ;; RSA keys - use RSA-PSS with SHA-256
      ((subtypep key-type 'ironclad::rsa-private-key)
       +sig-rsa-pss-rsae-sha256+)
      ;; ECDSA keys - determine curve
      ((subtypep key-type 'ironclad::secp256r1-private-key)
       +sig-ecdsa-secp256r1-sha256+)
      ((subtypep key-type 'ironclad::secp384r1-private-key)
       +sig-ecdsa-secp384r1-sha384+)
      ((subtypep key-type 'ironclad::secp521r1-private-key)
       +sig-ecdsa-secp521r1-sha512+)
      ;; Ed25519
      ((subtypep key-type 'ironclad::ed25519-private-key)
       +sig-ed25519+)
      (t
       (error 'tls-handshake-error
              :message (format nil "Unsupported private key type: ~A" key-type))))))

(defun sign-data (content private-key)
  "Sign CONTENT with PRIVATE-KEY.
   Returns the signature bytes."
  (let ((key-type (type-of private-key)))
    (cond
      ;; RSA keys - use RSA-PSS with SHA-256
      ((subtypep key-type 'ironclad::rsa-private-key)
       (ironclad:sign-message private-key content :pss :sha256))
      ;; ECDSA keys
      ((or (subtypep key-type 'ironclad::secp256r1-private-key)
           (subtypep key-type 'ironclad::secp384r1-private-key)
           (subtypep key-type 'ironclad::secp521r1-private-key))
       (let* ((hash-algo (cond
                           ((subtypep key-type 'ironclad::secp256r1-private-key) :sha256)
                           ((subtypep key-type 'ironclad::secp384r1-private-key) :sha384)
                           ((subtypep key-type 'ironclad::secp521r1-private-key) :sha512)))
              (hash (ironclad:digest-sequence hash-algo content))
              (raw-sig (ironclad:sign-message private-key hash)))
         ;; ECDSA signatures from Ironclad need to be converted to DER format
         (encode-ecdsa-signature-der raw-sig)))
      ;; Ed25519
      ((subtypep key-type 'ironclad::ed25519-private-key)
       (ironclad:sign-message private-key content))
      (t
       (error 'tls-handshake-error
              :message (format nil "Unsupported private key type for signing: ~A" key-type))))))

(defun encode-ecdsa-signature-der (raw-sig)
  "Encode an ECDSA signature (from Ironclad) to DER format.
   RAW-SIG is an Ironclad signature object containing R and S values."
  ;; Extract R and S from the signature
  (let* ((r-bytes (ironclad::signature-element raw-sig :r))
         (s-bytes (ironclad::signature-element raw-sig :s))
         (r-int (ironclad:octets-to-integer r-bytes))
         (s-int (ironclad:octets-to-integer s-bytes)))
    ;; Encode as DER SEQUENCE { INTEGER r, INTEGER s }
    (encode-der-sequence
     (list (encode-der-integer r-int)
           (encode-der-integer s-int)))))

(defun encode-der-integer (value)
  "Encode an integer as DER INTEGER."
  (let* ((bytes (if (zerop value)
                    (octet-vector 0)
                    (ironclad:integer-to-octets value)))
         ;; Add leading zero if high bit is set (to prevent interpretation as negative)
         (bytes (if (and (plusp (length bytes))
                         (>= (aref bytes 0) #x80))
                    (concat-octet-vectors (octet-vector 0) bytes)
                    bytes))
         (len (length bytes)))
    (concat-octet-vectors
     (octet-vector #x02)  ; INTEGER tag
     (encode-der-length len)
     bytes)))

(defun encode-der-length (len)
  "Encode a DER length."
  (cond
    ((< len 128)
     (octet-vector len))
    ((< len 256)
     (octet-vector #x81 len))
    (t
     (octet-vector #x82 (ldb (byte 8 8) len) (ldb (byte 8 0) len)))))

(defun encode-der-sequence (elements)
  "Encode a list of DER elements as a SEQUENCE."
  (let* ((content (apply #'concat-octet-vectors elements))
         (len (length content)))
    (concat-octet-vectors
     (octet-vector #x30)  ; SEQUENCE tag
     (encode-der-length len)
     content)))

(defun make-ecdsa-public-key (curve public-key-bytes)
  "Create an ECDSA public key from raw bytes.
   PUBLIC-KEY-BYTES should be the full encoded point (04 || X || Y)."
  ;; Ironclad's secp256r1/secp384r1/secp521r1 make-public-key expects :y to be
  ;; the full encoded public key bytes (04 || X || Y), not separate coordinates.
  ;; It will decode internally using ec-decode-point.
  (when (and (plusp (length public-key-bytes))
             (= (aref public-key-bytes 0) 4))  ; Uncompressed point format
    (ironclad:make-public-key curve :y public-key-bytes)))

(defun parse-ecdsa-signature (signature)
  "Parse a DER-encoded ECDSA signature into r and s values."
  (let ((parsed (parse-der signature)))
    (when (asn1-sequence-p parsed)
      (let ((children (asn1-children parsed)))
        (when (>= (length children) 2)
          (list :r (asn1-node-value (first children))
                :s (asn1-node-value (second children))))))))

(defun parse-rsa-public-key-n (public-key-der)
  "Extract the modulus N from an RSA public key."
  (let ((parsed (parse-der public-key-der)))
    (when (asn1-sequence-p parsed)
      (asn1-node-value (first (asn1-children parsed))))))

(defun parse-rsa-public-key-e (public-key-der)
  "Extract the exponent E from an RSA public key."
  (let ((parsed (parse-der public-key-der)))
    (when (asn1-sequence-p parsed)
      (asn1-node-value (second (asn1-children parsed))))))

(defun process-certificate-verify (hs message)
  "Process CertificateVerify message."
  (let* ((cert-verify (handshake-message-body message))
         (algorithm (certificate-verify-algorithm cert-verify))
         (signature (certificate-verify-signature cert-verify))
         (cert (client-handshake-peer-certificate hs)))
    ;; Must have a certificate to verify
    (unless cert
      (error 'tls-handshake-error
             :message "No certificate to verify CertificateVerify signature"))
    ;; Get the transcript hash at this point (excludes CertificateVerify)
    (let* ((ks (client-handshake-key-schedule hs))
           (transcript-hash (key-schedule-transcript-hash-value ks))
           (content (make-certificate-verify-content transcript-hash)))
      ;; Verify the signature
      (verify-certificate-verify-signature cert algorithm signature content))
    (setf (client-handshake-state hs) :wait-finished)))

;;;; Finished Processing

(defun process-server-finished (hs message raw-bytes)
  "Process server Finished message.
   RAW-BYTES are the serialized message bytes for updating transcript after verification."
  (let* ((finished (handshake-message-body message))
         (received-verify-data (finished-message-verify-data finished))
         (ks (client-handshake-key-schedule hs))
         (cipher-suite (client-handshake-selected-cipher-suite hs))
         ;; Compute expected verify_data
         ;; Note: transcript at this point excludes this Finished message (that's critical!)
         (transcript-hash (key-schedule-transcript-hash-value ks))
         (expected-verify-data
           (compute-finished-verify-data
            (key-schedule-server-handshake-traffic-secret ks)
            transcript-hash
            cipher-suite)))
    ;; Verify
    (unless (constant-time-equal received-verify-data expected-verify-data)
      (error 'tls-handshake-error
             :message "Server Finished verification failed"
             :state :wait-finished))
    ;; NOW update transcript with server Finished (after verification)
    (client-handshake-update-transcript hs raw-bytes)
    ;; Derive master secret and application traffic secrets
    (key-schedule-derive-master-secret ks)
    ;; Need transcript including server Finished for app secrets
    (key-schedule-derive-application-traffic-secrets
     ks (client-handshake-transcript hs))
    ;; Log application secrets for Wireshark
    (keylog-write-application-secrets ks)
    ;; Install server application keys for reading
    (multiple-value-bind (key iv)
        (key-schedule-derive-read-keys ks :application)
      (record-layer-install-keys
       (client-handshake-record-layer hs)
       :read key iv cipher-suite))
    (setf (client-handshake-state hs) :send-finished)))

;;;; Client Finished

(defun send-client-finished (hs)
  "Send client Finished message."
  (let* ((ks (client-handshake-key-schedule hs))
         (cipher-suite (client-handshake-selected-cipher-suite hs))
         ;; Install client handshake keys for writing (before sending Finished)
         (_  (multiple-value-bind (key iv)
                 (key-schedule-derive-write-keys ks :handshake)
               (record-layer-install-keys
                (client-handshake-record-layer hs)
                :write key iv cipher-suite)))
         ;; Compute verify_data
         (transcript-hash (key-schedule-transcript-hash-value ks))
         (verify-data (compute-finished-verify-data
                       (key-schedule-client-handshake-traffic-secret ks)
                       transcript-hash
                       cipher-suite))
         (finished (make-finished-message :verify-data verify-data))
         (finished-bytes (serialize-finished finished))
         (message (wrap-handshake-message +handshake-finished+ finished-bytes)))
    (declare (ignore _))
    ;; Update transcript (for resumption master secret)
    (client-handshake-update-transcript hs message)
    ;; Send
    (record-layer-write-handshake (client-handshake-record-layer hs) message)
    ;; Derive resumption master secret (for session tickets)
    (key-schedule-derive-resumption-master-secret ks (client-handshake-transcript hs))
    (setf (client-handshake-resumption-master-secret hs)
          (key-schedule-resumption-master-secret ks))
    ;; Install client application keys for writing
    (multiple-value-bind (key iv)
        (key-schedule-derive-write-keys ks :application)
      (record-layer-install-keys
       (client-handshake-record-layer hs)
       :write key iv cipher-suite))
    (setf (client-handshake-state hs) :connected)))

;;;; Main Handshake Loop

(defun handshake-buffer-has-complete-message-p (buffer)
  "Check if the buffer contains at least one complete handshake message."
  (when (and buffer (>= (length buffer) 4))
    ;; Parse the length from the header
    (let ((msg-length (decode-uint24 buffer 1)))
      (>= (length buffer) (+ 4 msg-length)))))

(defun handshake-buffer-extract-message (buffer)
  "Extract one handshake message from the buffer.
   Returns (VALUES message-bytes remaining-buffer)."
  (let* ((msg-length (decode-uint24 buffer 1))
         (total-length (+ 4 msg-length))
         (message (subseq buffer 0 total-length))
         (remaining (if (> (length buffer) total-length)
                        (subseq buffer total-length)
                        nil)))
    (values message remaining)))

(defun read-handshake-message (hs &key (update-transcript t))
  "Read and parse a handshake message from the record layer.
   Handles multiple messages per TLS record by buffering.
   If UPDATE-TRANSCRIPT is nil, the transcript is not updated (caller must do it).
   Returns (VALUES message raw-bytes) where raw-bytes are the serialized message bytes."
  ;; Read more data if needed
  (loop while (not (handshake-buffer-has-complete-message-p
                    (client-handshake-message-buffer hs)))
        do (multiple-value-bind (content-type data)
               (record-layer-read (client-handshake-record-layer hs))
             ;; Handle alerts
             (when (= content-type +content-type-alert+)
               (process-alert data))
             ;; Skip change_cipher_spec (compatibility) - just continue loop
             (unless (= content-type +content-type-change-cipher-spec+)
               ;; Must be handshake
               (unless (= content-type +content-type-handshake+)
                 (error 'tls-handshake-error
                        :message (format nil "Expected handshake, got content type ~D" content-type)))
               ;; Append to buffer
               (setf (client-handshake-message-buffer hs)
                     (if (client-handshake-message-buffer hs)
                         (concat-octet-vectors (client-handshake-message-buffer hs) data)
                         data)))))
  ;; Extract one message from buffer
  (multiple-value-bind (message-bytes remaining)
      (handshake-buffer-extract-message (client-handshake-message-buffer hs))
    (setf (client-handshake-message-buffer hs) remaining)
    ;; Optionally update transcript with raw message bytes
    (when update-transcript
      (client-handshake-update-transcript hs message-bytes))
    ;; Parse the message and return both parsed message and raw bytes
    (values (parse-handshake-message message-bytes
                                     :hash-length (when (client-handshake-selected-cipher-suite hs)
                                                    (cipher-suite-hash-length
                                                     (client-handshake-selected-cipher-suite hs))))
            message-bytes)))

(defun perform-client-handshake (record-layer &key hostname alpn-protocols
                                                   (verify-mode +verify-required+)
                                                   trust-store)
  "Perform the TLS 1.3 client handshake.
   Returns a CLIENT-HANDSHAKE structure on success.
   TRUST-STORE is used for certificate chain verification when verify-mode is +verify-required+."
  (let ((hs (make-client-handshake
             :hostname hostname
             :alpn-protocols alpn-protocols
             :verify-mode verify-mode
             :trust-store trust-store
             :record-layer record-layer)))
    ;; Send ClientHello
    (send-client-hello hs)
    ;; Process server messages
    (loop
      (case (client-handshake-state hs)
        (:wait-server-hello
         (let* ((raw-message (read-raw-handshake-message hs))
                (message (parse-handshake-message raw-message)))
           (unless (= (handshake-message-type message) +handshake-server-hello+)
             (error 'tls-handshake-error
                    :message "Expected ServerHello"
                    :state :wait-server-hello))
           ;; DON'T update transcript here - process-server-hello handles it
           ;; (transcript handling differs for HRR vs regular ServerHello)
           (process-server-hello hs message raw-message)))
        (:wait-encrypted-extensions
         (let ((message (read-handshake-message hs)))
           (unless (= (handshake-message-type message) +handshake-encrypted-extensions+)
             (error 'tls-handshake-error
                    :message "Expected EncryptedExtensions"
                    :state :wait-encrypted-extensions))
           (process-encrypted-extensions hs message)))
        (:wait-cert-or-finished
         ;; Don't update transcript automatically - we need to handle Finished specially
         (multiple-value-bind (message raw-bytes)
             (read-handshake-message hs :update-transcript nil)
           (case (handshake-message-type message)
             (#.+handshake-certificate+
              ;; Update transcript now for Certificate
              (client-handshake-update-transcript hs raw-bytes)
              (process-certificate hs message))
             (#.+handshake-finished+
              ;; Server sent Finished without certificate (PSK mode)
              ;; This is NOT allowed when verify-mode is +verify-required+
              (when (= (client-handshake-verify-mode hs) +verify-required+)
                (error 'tls-certificate-error
                       :message "Server did not provide a certificate but verification is required"))
              ;; Don't update transcript yet - process-server-finished will do it after verification
              (setf (client-handshake-state hs) :wait-finished)
              (process-server-finished hs message raw-bytes))
             (t (error 'tls-handshake-error
                       :message "Expected Certificate or Finished"
                       :state :wait-cert-or-finished)))))
        (:wait-certificate-verify
         (let ((message (read-handshake-message hs)))
           (unless (= (handshake-message-type message) +handshake-certificate-verify+)
             (error 'tls-handshake-error
                    :message "Expected CertificateVerify"
                    :state :wait-certificate-verify))
           (process-certificate-verify hs message)))
        (:wait-finished
         ;; Don't update transcript yet - we need to verify first, THEN update
         (multiple-value-bind (message raw-bytes)
             (read-handshake-message hs :update-transcript nil)
           (unless (= (handshake-message-type message) +handshake-finished+)
             (error 'tls-handshake-error
                    :message "Expected Finished"
                    :state :wait-finished))
           (process-server-finished hs message raw-bytes)))
        (:send-finished
         (send-client-finished hs))
        (:connected
         (return hs))
        (t
         (error 'tls-handshake-error
                :message (format nil "Unknown state: ~A" (client-handshake-state hs))))))))

(defun read-raw-handshake-message (hs)
  "Read a raw handshake message before encryption is established."
  (multiple-value-bind (content-type data)
      (record-layer-read (client-handshake-record-layer hs))
    ;; Handle alerts
    (when (= content-type +content-type-alert+)
      (process-alert data))
    ;; Must be handshake
    (unless (= content-type +content-type-handshake+)
      (error 'tls-handshake-error
             :message (format nil "Expected handshake, got content type ~D" content-type)))
    data))
