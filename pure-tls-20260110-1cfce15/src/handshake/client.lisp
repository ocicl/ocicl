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
  ;; Skip hostname verification (for SNI-only hostname)
  (skip-hostname-verify nil :type boolean)
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
  (offered-key-share-groups nil :type list)  ; Groups we've offered key shares for
  (ccs-sent nil :type boolean)  ; T if CCS was already sent (for HRR flow)
  ;; Saved from CH1 for reuse in CH2 (RFC 8446 Section 4.1.2)
  (legacy-session-id nil :type (or null octet-vector))
  ;; Saved GREASE values for CH2
  (grease-cipher nil)
  (grease-version nil)
  (grease-group nil)
  (grease-key-share-bytes nil :type (or null octet-vector))  ; Random bytes for GREASE key share
  (grease-ext-type nil)  ; GREASE extension type
  (grease-ext-data nil :type (or null octet-vector))  ; GREASE extension data
  ;; Session resumption (PSK)
  (offered-psk nil)  ; session-ticket if we offered a PSK
  (psk-accepted nil :type boolean)  ; T if server accepted our PSK
  (resumption-master-secret nil :type (or null octet-vector))  ; For ticket derivation
  ;; Client authentication (mTLS)
  (certificate-requested nil :type boolean)  ; T if server sent CertificateRequest
  (cert-request-context nil :type (or null octet-vector))  ; Context from CertificateRequest
  (cert-request-signature-algorithms nil :type list)  ; signature_algorithms from CertificateRequest
  ;; Client certificate/key for mTLS (when server requests client auth)
  (client-certificate nil)  ; Parsed X.509 certificate (or raw DER)
  (client-private-key nil)  ; Private key for signing CertificateVerify
  (client-certificate-chain nil :type list)  ; Additional chain certificates
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
  ;; RFC 8446 Section 4.1.2: In CH2 after HRR, random and session-id MUST be same as CH1
  (let* ((is-ch2 (not (null (client-handshake-client-random hs))))
         (random (if is-ch2
                     (client-handshake-client-random hs)
                     (let ((r (random-bytes 32)))
                       (setf (client-handshake-client-random hs) r)
                       r)))
         (session-id (if is-ch2
                         (client-handshake-legacy-session-id hs)
                         (let ((s (random-bytes 32)))
                           (setf (client-handshake-legacy-session-id hs) s)
                           s)))
         ;; Key share handling:
         ;; - CH1: generate new key share for preferred group
         ;; - CH2 with HRR key_share: generate new key share for requested group
         ;; - CH2 without HRR key_share: reuse key share from CH1
         (key-share-group (or requested-group *preferred-group*))
         (key-exchange (if (and is-ch2 (null requested-group))
                           ;; HRR didn't request a new group - reuse CH1's key share
                           (client-handshake-key-exchange hs)
                           ;; Generate new key share
                           (generate-key-exchange key-share-group)))
         ;; Track which groups we've offered (for HRR validation) - only for new groups
         (_ (unless (and is-ch2 (null requested-group))
              (push key-share-group (client-handshake-offered-key-share-groups hs))))
         ;; Build extensions
         ;; GREASE values for protocol robustness (RFC 8701)
         ;; Must be same in CH2 as in CH1
         (grease-version (or (client-handshake-grease-version hs)
                             (setf (client-handshake-grease-version hs)
                                   (random-grease-value *grease-extension-values*))))
         (grease-group (or (client-handshake-grease-group hs)
                           (setf (client-handshake-grease-group hs)
                                 (random-grease-value *grease-group-values*))))
         (grease-cipher (or (client-handshake-grease-cipher hs)
                            (setf (client-handshake-grease-cipher hs)
                                  (random-grease-value *grease-cipher-suite-values*))))
         ;; Prepend GREASE cipher suite to the list
         (cipher-suites-with-grease (cons grease-cipher (client-handshake-cipher-suites hs)))
         (extensions (list
                      ;; supported_versions (required for TLS 1.3)
                      ;; Include GREASE version for robustness testing
                      (make-tls-extension
                       :type +extension-supported-versions+
                       :data (make-supported-versions-ext :versions (list grease-version +tls-1.3+)))
                      ;; supported_groups (include GREASE group)
                      (make-tls-extension
                       :type +extension-supported-groups+
                       :data (make-supported-groups-ext :groups (cons grease-group *supported-groups*)))
                      ;; signature_algorithms
                      (make-tls-extension
                       :type +extension-signature-algorithms+
                       :data (make-signature-algorithms-ext
                              :algorithms (supported-signature-algorithms-tls13)))
                      ;; key_share extension
                      ;; RFC 8446 Section 4.1.2: If HRR includes key_share, CH2 contains
                      ;; only the requested group (no GREASE). Otherwise include GREASE + real.
                      (make-tls-extension
                       :type +extension-key-share+
                       :data (make-key-share-ext
                              :client-shares
                              (if requested-group
                                  ;; CH2 after HRR with key_share: only the requested group
                                  (list (make-key-share-entry
                                         :group (get-key-exchange-group key-exchange)
                                         :key-exchange (get-key-exchange-public-key key-exchange)))
                                  ;; CH1 or CH2 without HRR key_share: include GREASE + real
                                  (list
                               ;; GREASE key share entry (RFC 8701 §3.1)
                               ;; Save/reuse for CH2
                               (make-key-share-entry
                                :group grease-group
                                :key-exchange (or (client-handshake-grease-key-share-bytes hs)
                                                  (setf (client-handshake-grease-key-share-bytes hs)
                                                        (random-bytes 1))))
                                   ;; Real key share
                                   (make-key-share-entry
                                    :group (get-key-exchange-group key-exchange)
                                    :key-exchange (get-key-exchange-public-key key-exchange)))))))))
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
    ;; Add GREASE extension (RFC 8701) for protocol robustness
    ;; This helps prevent protocol ossification by ensuring servers
    ;; properly ignore unknown extension types
    ;; Must be same in CH2 as CH1
    (push (make-tls-extension
           :type (or (client-handshake-grease-ext-type hs)
                     (setf (client-handshake-grease-ext-type hs)
                           (random-grease-value *grease-extension-values*)))
           :data (make-grease-ext
                  :data (or (client-handshake-grease-ext-data hs)
                            (setf (client-handshake-grease-ext-data hs)
                                  (random-bytes 1)))))
          extensions)
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
    ;; Build ClientHello (extensions in reverse order, PSK will be added separately)
    (let ((hello (make-client-hello
                  :legacy-version +tls-1.2+
                  :random random
                  :legacy-session-id session-id
                  :cipher-suites cipher-suites-with-grease
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
    ;; Send ClientHello
    (record-layer-write-handshake (client-handshake-record-layer hs) message)
    ;; Note: CCS for middlebox compatibility is sent later, in send-client-finished,
    ;; after we confirm TLS 1.3 was negotiated
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
  ;; Check for infinite HRR loop - RFC 8446 Section 4.1.4
  (when (>= (client-handshake-hello-retry-count hs) 1)
    (record-layer-write-alert (client-handshake-record-layer hs)
                              +alert-level-fatal+ +alert-unexpected-message+)
    (error 'tls-handshake-error
           :message ":UNEXPECTED_MESSAGE: Received multiple HelloRetryRequests"
           :state :wait-server-hello))
  (incf (client-handshake-hello-retry-count hs))
  ;; RFC 8446 Section 4.1.3: legacy_compression_method MUST be 0
  (unless (zerop (server-hello-legacy-compression-method server-hello))
    (record-layer-write-alert (client-handshake-record-layer hs)
                              +alert-level-fatal+ +alert-decode-error+)
    (error 'tls-handshake-error
           :message ":DECODE_ERROR: HelloRetryRequest legacy_compression_method must be 0"
           :state :wait-server-hello))

  (let ((extensions (server-hello-extensions server-hello)))
    ;; RFC 8446 Section 4.1.4: HRR can only contain supported_versions, cookie, and key_share
    ;; Reject any unknown extensions with unsupported_extension
    (dolist (ext extensions)
      (let ((ext-type (tls-extension-type ext)))
        (unless (member ext-type (list +extension-supported-versions+
                                       +extension-cookie+
                                       +extension-key-share+))
          (record-layer-write-alert (client-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-unsupported-extension+)
          (error 'tls-handshake-error
                 :message (format nil ":UNEXPECTED_EXTENSION: Unknown extension ~D in HelloRetryRequest"
                                 ext-type)
                 :state :wait-server-hello))))

    ;; Get the selected cipher suite (needed for hash algorithm)
    (let ((cipher-suite (server-hello-cipher-suite server-hello)))
      (unless (member cipher-suite (client-handshake-cipher-suites hs))
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-illegal-parameter+)
        (error 'tls-handshake-error
               :message ":ILLEGAL_PARAMETER: Server selected unsupported cipher suite in HRR"
               :state :wait-server-hello))
      (setf (client-handshake-selected-cipher-suite hs) cipher-suite))

    ;; Extract requested key share group
    (let ((ks-ext (find-extension extensions +extension-key-share+)))
      (when ks-ext
        (let ((selected-group (key-share-ext-selected-group (tls-extension-data ks-ext))))
          (when selected-group
            ;; Verify we support this group
            (unless (member selected-group *supported-groups*)
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+ +alert-illegal-parameter+)
              (error 'tls-handshake-error
                     :message ":WRONG_CURVE: Server requested unsupported key exchange group"
                     :state :wait-server-hello))
            ;; RFC 8446 Section 4.1.4: HRR requesting a group we already offered is unnecessary
            (when (member selected-group (client-handshake-offered-key-share-groups hs))
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+ +alert-illegal-parameter+)
              (error 'tls-handshake-error
                     :message ":WRONG_CURVE: HelloRetryRequest for already-offered key share group"
                     :state :wait-server-hello))
            (setf (client-handshake-hrr-selected-group hs) selected-group)))))

    ;; Extract cookie if present
    (let ((cookie-ext (find-extension extensions +extension-cookie+)))
      (when cookie-ext
        (let ((cookie-data (tls-extension-data cookie-ext)))
          ;; Cookie extension structure: opaque cookie<1..2^16-1>
          ;; So we have 2-byte length prefix + cookie bytes
          ;; Cookie must have at least 1 byte (length > 0)
          (when (< (length cookie-data) 3)  ; Need at least 2 bytes length + 1 byte cookie
            (record-layer-write-alert (client-handshake-record-layer hs)
                                      +alert-level-fatal+ +alert-decode-error+)
            (error 'tls-decode-error
                   :message ":DECODE_ERROR: Empty cookie in HelloRetryRequest"))
          ;; Check the length field itself
          (let ((cookie-len (+ (* (aref cookie-data 0) 256) (aref cookie-data 1))))
            (when (zerop cookie-len)
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+ +alert-decode-error+)
              (error 'tls-decode-error
                     :message ":DECODE_ERROR: Empty cookie in HelloRetryRequest")))
          (setf (client-handshake-hrr-cookie hs) cookie-data))))

    ;; RFC 8446 Section 4.2.8: HRR must result in a change to ClientHello
    ;; An HRR with no key_share and no cookie doesn't cause any change
    (unless (or (client-handshake-hrr-selected-group hs)
                (client-handshake-hrr-cookie hs))
      (record-layer-write-alert (client-handshake-record-layer hs)
                                +alert-level-fatal+ +alert-illegal-parameter+)
      (error 'tls-handshake-error
             :message ":EMPTY_HELLO_RETRY_REQUEST: HelloRetryRequest would not result in any change"
             :state :wait-server-hello))

    ;; Replace transcript with message_hash per RFC 8446 Section 4.4.1
    ;; The transcript becomes: message_hash || HelloRetryRequest
    (let ((message-hash (make-message-hash
                         (client-handshake-transcript hs)
                         (client-handshake-selected-cipher-suite hs))))
      (setf (client-handshake-transcript hs)
            (concat-octet-vectors message-hash hrr-raw-bytes)))

    ;; Send CCS for middlebox compatibility before ClientHello2
    ;; RFC 8446 Appendix D.4: "before its second flight"
    (unless (client-handshake-ccs-sent hs)
      (record-layer-write-change-cipher-spec (client-handshake-record-layer hs))
      (setf (client-handshake-ccs-sent hs) t))

    ;; Send new ClientHello with requested group and cookie
    (send-client-hello hs :requested-group (client-handshake-hrr-selected-group hs))))

;;;; ServerHello Processing

(defun valid-server-hello-extension-p (ext-type)
  "Return T if ext-type is allowed in TLS 1.3 ServerHello.
   Per RFC 8446 Section 4.2, only these extensions are valid in ServerHello:
   - supported_versions (required)
   - key_share (required for key exchange)
   - pre_shared_key (if PSK mode)"
  (member ext-type (list +extension-supported-versions+
                         +extension-key-share+
                         +extension-pre-shared-key+)))

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
    ;; RFC 8446 Section 4.2: Validate that only allowed extensions are present
    ;; Extensions that should appear in EncryptedExtensions are forbidden here
    (dolist (ext extensions)
      (let ((ext-type (tls-extension-type ext)))
        (unless (valid-server-hello-extension-p ext-type)
          (record-layer-write-alert (client-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-unsupported-extension+)
          (error 'tls-handshake-error
                 :message (format nil ":UNEXPECTED_EXTENSION: Extension ~D not allowed in ServerHello"
                                 ext-type)
                 :state :wait-server-hello))))
    ;; RFC 8446 Section 4.1.3: legacy_compression_method MUST be 0
    (unless (zerop (server-hello-legacy-compression-method server-hello))
      (record-layer-write-alert (client-handshake-record-layer hs)
                                +alert-level-fatal+ +alert-decode-error+)
      (error 'tls-handshake-error
             :message ":DECODE_ERROR: ServerHello legacy_compression_method must be 0"
             :state :wait-server-hello))
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
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-illegal-parameter+)
        (error 'tls-handshake-error
               :message ":WRONG_CIPHER_RETURNED: Server selected unsupported cipher suite"
               :state :wait-server-hello))
      ;; RFC 8446 Section 4.1.4: Cipher suite in ServerHello must match HRR
      (when (and (> (client-handshake-hello-retry-count hs) 0)
                 (client-handshake-selected-cipher-suite hs)
                 (not (= cipher-suite (client-handshake-selected-cipher-suite hs))))
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-illegal-parameter+)
        (error 'tls-handshake-error
               :message ":WRONG_CIPHER_RETURNED: ServerHello cipher suite differs from HelloRetryRequest"
               :state :wait-server-hello))
      (setf (client-handshake-selected-cipher-suite hs) cipher-suite))
    ;; Process key_share extension
    (let ((ks-ext (find-extension extensions +extension-key-share+)))
      (unless ks-ext
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-illegal-parameter+)
        (error 'tls-handshake-error
               :message ":MISSING_KEY_SHARE: Missing key_share extension in ServerHello"
               :state :wait-server-hello))
      (let* ((ks-data (tls-extension-data ks-ext))
             (server-share (key-share-ext-server-share ks-data))
             (server-group (key-share-entry-group server-share))
             (server-public (key-share-entry-key-exchange server-share)))
        ;; Verify server used our offered group
        (unless (= server-group (get-key-exchange-group (client-handshake-key-exchange hs)))
          (record-layer-write-alert (client-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-illegal-parameter+)
          (error 'tls-handshake-error
                 :message ":WRONG_CURVE: Server used different key exchange group"
                 :state :wait-server-hello))
        ;; Validate server key_share length
        (let ((expected-len (key-exchange-server-share-length server-group)))
          (when (and (plusp expected-len)
                     (/= (length server-public) expected-len))
            (record-layer-write-alert (client-handshake-record-layer hs)
                                      +alert-level-fatal+ +alert-illegal-parameter+)
            (error 'tls-handshake-error
                   :message (format nil ":BAD_ECPOINT: Invalid server key_share length for ~A: ~D (expected ~D)"
                                   (named-group-name server-group) (length server-public) expected-len)
                   :state :wait-server-hello)))
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
                (key-schedule-derive-server-traffic-keys ks :handshake)
              (record-layer-install-keys
               (client-handshake-record-layer hs)
               :read key iv
               (client-handshake-selected-cipher-suite hs)))
            ;; Install client handshake keys for writing
            ;; This is done early so that any alerts during the encrypted handshake
            ;; phase are also encrypted (required by TLS 1.3)
            (multiple-value-bind (key iv)
                (key-schedule-derive-client-traffic-keys ks :handshake)
              (record-layer-install-keys
               (client-handshake-record-layer hs)
               :write key iv
               (client-handshake-selected-cipher-suite hs)))
            ;; Send CCS immediately after installing handshake keys, before any
            ;; encrypted records. RFC 8446 Appendix D.4 requires CCS to be sent
            ;; before other encrypted messages.
            (unless (client-handshake-ccs-sent hs)
              (record-layer-write-change-cipher-spec (client-handshake-record-layer hs))
              (setf (client-handshake-ccs-sent hs) t))))))
    (setf (client-handshake-state hs) :wait-encrypted-extensions)))

;;;; Encrypted Extensions Processing

(defun process-encrypted-extensions (hs message)
  "Process EncryptedExtensions message."
  (let* ((ee (handshake-message-body message))
         (extensions (encrypted-extensions-extensions ee)))
    ;; Validate extensions: only allow those permitted in EncryptedExtensions.
    ;; RFC 8446 restricts EE to extensions not in ServerHello and only those
    ;; defined for EE. We currently support ALPN and server_name (empty).
    (let ((seen (make-hash-table :test 'eql)))
      (dolist (ext extensions)
        (let ((ext-type (tls-extension-type ext)))
          (when (gethash ext-type seen)
            (record-layer-write-alert (client-handshake-record-layer hs)
                                      +alert-level-fatal+
                                      +alert-illegal-parameter+)
            (error 'tls-decode-error
                   :message ":DECODE_ERROR: Duplicate extension in EncryptedExtensions"))
          (setf (gethash ext-type seen) t)
          (unless (member ext-type (list +extension-application-layer-protocol-negotiation+
                                         +extension-server-name+))
            (record-layer-write-alert (client-handshake-record-layer hs)
                                      +alert-level-fatal+
                                      +alert-unsupported-extension+)
            ;; Use different error messages for known vs unknown extensions
            ;; - Known extensions in wrong context → :ERROR_PARSING_EXTENSION:
            ;; - Unknown/custom extensions → :UNEXPECTED_EXTENSION:
            (error 'tls-handshake-error
                   :message (format nil "~A Extension ~D not allowed in EncryptedExtensions"
                                    (if (known-extension-p ext-type)
                                        ":ERROR_PARSING_EXTENSION:"
                                        ":UNEXPECTED_EXTENSION:")
                                    ext-type)))
          (when (= ext-type +extension-server-name+)
            ;; RFC 8446: Server MUST NOT send server_name unless client sent it
            (unless (client-handshake-hostname hs)
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+
                                        +alert-unsupported-extension+)
              (error 'tls-decode-error
                     :message ":UNEXPECTED_EXTENSION: Unsolicited server_name extension"))
            (let ((sni (tls-extension-data ext)))
              (when (and (server-name-ext-p sni)
                         (server-name-ext-host-name sni))
                (record-layer-write-alert (client-handshake-record-layer hs)
                                          +alert-level-fatal+
                                          +alert-illegal-parameter+)
                (error 'tls-decode-error
                       :message ":ILLEGAL_PARAMETER: server_name in EncryptedExtensions must be empty")))))))
    ;; Process ALPN if present
    (let ((alpn-ext (find-extension extensions
                                    +extension-application-layer-protocol-negotiation+)))
      (when alpn-ext
        (let* ((alpn-data (tls-extension-data alpn-ext))
               (protocols (alpn-ext-protocol-list alpn-data)))
          ;; RFC 7301: Server MUST select exactly one protocol
          (when (or (null protocols) (/= (length protocols) 1))
            (record-layer-write-alert (client-handshake-record-layer hs)
                                      +alert-level-fatal+
                                      +alert-illegal-parameter+)
            (error 'tls-decode-error
                   :message ":ILLEGAL_PARAMETER: ALPN in EncryptedExtensions must select exactly one protocol"))
          (let* ((selected (first protocols))
                 (offered (client-handshake-alpn-protocols hs)))
            ;; Check for empty protocol name
            (when (or (null selected) (zerop (length selected)))
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+
                                        +alert-illegal-parameter+)
              (error 'tls-decode-error
                     :message ":ILLEGAL_PARAMETER: ALPN selected empty protocol name"))
            (when (null offered)
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+
                                        +alert-unsupported-extension+)
              (error 'tls-decode-error
                     :message ":UNEXPECTED_EXTENSION: ALPN returned but not offered"))
            (unless (member selected offered :test #'string=)
              (record-layer-write-alert (client-handshake-record-layer hs)
                                        +alert-level-fatal+
                                        +alert-illegal-parameter+)
              (error 'tls-decode-error
                     :message ":ILLEGAL_PARAMETER: ALPN selected protocol not offered by client"))
            (setf (client-handshake-selected-alpn hs) selected)))))
    ;; In PSK mode with resumption, server skips Certificate/CertificateVerify
    ;; Otherwise, expect Certificate next
    (setf (client-handshake-state hs) :wait-cert-or-finished)))

;;;; CertificateRequest Processing

(defun process-certificate-request (hs message)
  "Process CertificateRequest message from server.
   This indicates the server wants client authentication (mTLS).
   We currently don't support sending client certificates, so we'll send
   an empty Certificate message later."
  (let ((cert-req (handshake-message-body message)))
    ;; RFC 8446 Section 4.3.2: certificate_request_context MUST be zero length
    ;; during the initial handshake (non-empty only for post-handshake auth)
    (let ((context (certificate-request-certificate-request-context cert-req)))
      (when (and context (plusp (length context)))
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-decode-error+)
        (error 'tls-decode-error
               :message ":DECODE_ERROR: CertificateRequest has non-empty context during handshake")))
    ;; Store the certificate_request_context
    ;; This must be echoed back in our Certificate message
    (setf (client-handshake-cert-request-context hs)
          (certificate-request-certificate-request-context cert-req))
    ;; Store signature_algorithms preferences (TLS 1.3 requires this extension)
    (let ((sig-ext (find-extension (certificate-request-extensions cert-req)
                                   +extension-signature-algorithms+)))
      (unless sig-ext
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-decode-error+)
        (error 'tls-decode-error
               :message ":DECODE_ERROR: CertificateRequest missing signature_algorithms extension"))
      (let ((algorithms (signature-algorithms-ext-algorithms (tls-extension-data sig-ext))))
        (when (null algorithms)
          (record-layer-write-alert (client-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-decode-error+)
          (error 'tls-decode-error
                 :message ":DECODE_ERROR: CertificateRequest has empty signature_algorithms list"))
        ;; Check if we have any algorithms in common with the server
        (let ((common (intersection algorithms (supported-signature-algorithms-tls13))))
          (when (null common)
            (record-layer-write-alert (client-handshake-record-layer hs)
                                      +alert-level-fatal+ +alert-handshake-failure+)
            (error 'tls-handshake-error
                   :message ":NO_COMMON_SIGNATURE_ALGORITHMS: No common signature algorithms with server")))
        (setf (client-handshake-cert-request-signature-algorithms hs) algorithms)))
    ;; Mark that client auth was requested
    (setf (client-handshake-certificate-requested hs) t)))

;;;; Certificate Processing

(defun process-certificate (hs message)
  "Process Certificate message."
  (let* ((cert-msg (handshake-message-body message))
         (cert-list (certificate-message-certificate-list cert-msg)))
    (when (null cert-list)
      (when (= (client-handshake-verify-mode hs) +verify-required+)
        (error 'tls-certificate-error
               :message "Server did not provide a certificate")))
    ;; Validate per-certificate extensions
    ;; RFC 8446 Section 4.4.2: Only status_request and signed_certificate_timestamp
    ;; are valid, and only if the client requested them in ClientHello
    (dolist (entry cert-list)
      (dolist (ext (certificate-entry-extensions entry))
        (let ((ext-type (tls-extension-type ext)))
          (cond
            ;; status_request (OCSP) - we don't request it, so reject
            ((= ext-type +extension-status-request+)
             (let ((ocsp-data (tls-extension-data ext)))
               ;; Even if we requested OCSP, empty response is invalid
               (when (or (null ocsp-data) (zerop (length ocsp-data)))
                 (record-layer-write-alert (client-handshake-record-layer hs)
                                           +alert-level-fatal+ +alert-decode-error+)
                 (error 'tls-handshake-error
                        :message ":DECODE_ERROR: Empty OCSP response in certificate")))
             ;; We don't request OCSP, so this is unsolicited
             (record-layer-write-alert (client-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unsupported-extension+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_EXTENSION: Unsolicited OCSP response in certificate"))
            ;; signed_certificate_timestamp (SCT) - we don't request it, so reject
            ;; But first validate the format (RFC 6962 Section 3.3)
            ((= ext-type +extension-signed-certificate-timestamp+)
             (let ((sct-data (tls-extension-data ext)))
               ;; Validate SCT list format before rejecting as unsolicited
               (when (or (null sct-data) (< (length sct-data) 2))
                 (record-layer-write-alert (client-handshake-record-layer hs)
                                           +alert-level-fatal+ +alert-decode-error+)
                 (error 'tls-handshake-error
                        :message ":DECODE_ERROR: Invalid SCT list in certificate"))
               ;; Parse SCT list: uint16 list_length followed by SCT entries
               (let* ((list-len (decode-uint16 sct-data 0))
                      (expected-len (+ 2 list-len)))
                 (unless (= (length sct-data) expected-len)
                   (record-layer-write-alert (client-handshake-record-layer hs)
                                             +alert-level-fatal+ +alert-decode-error+)
                   (error 'tls-handshake-error
                          :message ":DECODE_ERROR: SCT list length mismatch"))
                 ;; Empty SCT list is invalid
                 (when (zerop list-len)
                   (record-layer-write-alert (client-handshake-record-layer hs)
                                             +alert-level-fatal+ +alert-decode-error+)
                   (error 'tls-handshake-error
                          :message ":DECODE_ERROR: Empty SCT list in certificate"))
                 ;; Validate each SCT entry is non-empty
                 (let ((pos 2))
                   (loop while (< pos (length sct-data))
                         do (when (> (+ pos 2) (length sct-data))
                              (record-layer-write-alert (client-handshake-record-layer hs)
                                                        +alert-level-fatal+ +alert-decode-error+)
                              (error 'tls-handshake-error
                                     :message ":DECODE_ERROR: Truncated SCT in list"))
                            (let ((sct-len (decode-uint16 sct-data pos)))
                              (when (zerop sct-len)
                                (record-layer-write-alert (client-handshake-record-layer hs)
                                                          +alert-level-fatal+ +alert-decode-error+)
                                (error 'tls-handshake-error
                                       :message ":DECODE_ERROR: Empty SCT entry in list"))
                              (incf pos (+ 2 sct-len)))))))
             ;; We don't request SCT, so this is unsolicited
             (record-layer-write-alert (client-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unsupported-extension+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_EXTENSION: Unsolicited SCT in certificate"))
            ;; Any other extension is unknown/forbidden
            (t
             (record-layer-write-alert (client-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unsupported-extension+)
             (error 'tls-handshake-error
                    :message (format nil ":UNEXPECTED_EXTENSION: Unknown extension ~D in certificate"
                                    ext-type)))))))
    ;; Parse and store the entire certificate chain
    (when cert-list
      (let ((parsed-chain
              (loop for entry in cert-list
                    for i from 0
                    collect (handler-case
                                (parse-certificate (certificate-entry-cert-data entry))
                              (error (e)
                                ;; Re-throw with proper error message for BoringSSL tests
                                (error 'tls-decode-error
                                       :message (format nil ":CANNOT_PARSE_LEAF_CERT: Certificate ~D: ~A"
                                                       i e)))))))
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

(defun signature-key-type-from-public-algorithm (key-algorithm)
  "Return the key type symbol for a certificate public key algorithm."
  (cond
    ((member key-algorithm '(:rsa-encryption :rsassa-pss :rsa-pss :rsa)) :rsa)
    ((member key-algorithm '(:secp256r1 :secp384r1 :secp521r1 :prime256v1)) :ecdsa)
    ((eql key-algorithm :ed25519) :ed25519)
    ((eql key-algorithm :ed448) :ed448)
    (t :unknown)))

(defun signature-key-type-from-private-key (private-key)
  "Return the key type symbol for a private key."
  (let ((key-type (type-of private-key)))
    (cond
      ((subtypep key-type 'ironclad::rsa-private-key) :rsa)
      ((subtypep key-type 'ironclad::secp256r1-private-key) :ecdsa-p256)
      ((subtypep key-type 'ironclad::secp384r1-private-key) :ecdsa-p384)
      ((subtypep key-type 'ironclad::secp521r1-private-key) :ecdsa-p521)
      ((subtypep key-type 'ironclad::ed25519-private-key) :ed25519)
      (t :unknown))))

(defun signature-required-key-type (sig-type)
  "Return the key type required by a signature type."
  (case sig-type
    ((:rsa-pss-rsae :rsa-pss-pss :rsa-pkcs1) :rsa)
    (:ecdsa :ecdsa)
    (:ed25519 :ed25519)
    (:ed448 :ed448)
    (otherwise :unknown)))

(defun signature-algorithm-allowed-p (algorithm)
  "Return T when ALGORITHM is permitted for TLS 1.3."
  (member algorithm (supported-signature-algorithms-tls13) :test #'eql))

(defun signature-algorithms-for-private-key (private-key)
  "Return the TLS 1.3 signature algorithms compatible with PRIVATE-KEY.
   For RSA keys, only rsae algorithms are returned since standard RSA keys
   (rsaEncryption OID) cannot use rsa_pss_pss_* algorithms - those require
   id-RSASSA-PSS OID in the certificate."
  (case (signature-key-type-from-private-key private-key)
    ;; Standard RSA keys (rsaEncryption OID) only support rsae algorithms
    (:rsa (list +sig-rsa-pss-rsae-sha256+
                +sig-rsa-pss-rsae-sha384+
                +sig-rsa-pss-rsae-sha512+))
    (:ecdsa-p256 (list +sig-ecdsa-secp256r1-sha256+))
    (:ecdsa-p384 (list +sig-ecdsa-secp384r1-sha384+))
    (:ecdsa-p521 (list +sig-ecdsa-secp521r1-sha512+))
    (:ed25519 (list +sig-ed25519+))
    (otherwise nil)))

(defun select-signature-algorithm-for-key (private-key peer-algorithms)
  "Select a signature algorithm based on PRIVATE-KEY and PEER-ALGORITHMS."
  (let* ((allowed (signature-algorithms-for-private-key private-key))
         (choice (when peer-algorithms
                   (find-if (lambda (alg) (member alg allowed)) peer-algorithms))))
    (or choice
        (error 'tls-handshake-error
               :message ":NO_COMMON_SIGNATURE_ALGORITHMS:"))))

(defun signature-algorithm-expected-curve (algorithm)
  "Return the expected ECDSA curve for a signature algorithm, or NIL if not ECDSA."
  (case algorithm
    (#.+sig-ecdsa-secp256r1-sha256+ :secp256r1)
    (#.+sig-ecdsa-secp384r1-sha384+ :secp384r1)
    (#.+sig-ecdsa-secp521r1-sha512+ :secp521r1)
    (otherwise nil)))

(defun key-algorithm-curve (key-algorithm)
  "Return the curve for an EC key algorithm, or NIL if not EC."
  (cond
    ((member key-algorithm '(:ecdsa-p256 :secp256r1 :prime256v1 :ec-public-key)) :secp256r1)
    ((member key-algorithm '(:ecdsa-p384 :secp384r1)) :secp384r1)
    ((member key-algorithm '(:ecdsa-p521 :secp521r1)) :secp521r1)
    (t nil)))

(defun verify-certificate-verify-signature (cert algorithm signature content
                                           &key allowed-algorithms)
  "Verify the CertificateVerify signature using the certificate's public key.
   Returns T on success, signals an error on failure."
  (let* ((public-key-info (x509-certificate-subject-public-key-info cert))
         (key-algorithm (getf public-key-info :algorithm))
         (key-type (signature-key-type-from-public-algorithm key-algorithm))
         (public-key-bytes (getf public-key-info :public-key))
         (key-usage (certificate-key-usage cert)))
    ;; RFC 8446 Section 4.4.2.2: Certificate MUST allow digitalSignature key usage
    ;; if the keyUsage extension is present
    (when (and key-usage (not (member :digital-signature key-usage)))
      (error 'tls-handshake-error
             :message ":KEY_USAGE_BIT_INCORRECT: Certificate lacks digitalSignature key usage"))
    (unless (signature-algorithm-allowed-p algorithm)
      (error 'tls-handshake-error
             :message ":WRONG_SIGNATURE_TYPE: unsupported signature algorithm"))
    (when (and allowed-algorithms (not (member algorithm allowed-algorithms)))
      (error 'tls-handshake-error
             :message ":WRONG_SIGNATURE_TYPE: signature algorithm not permitted"))
    ;; Determine hash algorithm from signature algorithm
    (multiple-value-bind (hash-algo sig-type)
        (signature-algorithm-params algorithm)
      (unless sig-type
        (error 'tls-handshake-error
               :message (format nil "Unsupported signature algorithm: ~X" algorithm)))
      (let ((required-key-type (signature-required-key-type sig-type)))
        (unless (and (not (eql key-type :unknown))
                     (eql required-key-type key-type))
          (error 'tls-handshake-error
                 :message ":WRONG_SIGNATURE_TYPE: signature type mismatch")))
      ;; For ECDSA, verify that certificate curve matches signature algorithm curve
      (when (eql sig-type :ecdsa)
        (let ((expected-curve (signature-algorithm-expected-curve algorithm))
              (cert-curve (key-algorithm-curve key-algorithm)))
          (when (and expected-curve cert-curve
                     (not (eql expected-curve cert-curve)))
            (error 'tls-handshake-error
                   :message ":WRONG_SIGNATURE_TYPE: ECDSA curve mismatch"))))
      ;; Verify based on key type
      (cond
        ;; RSA-PSS signatures
        ((member sig-type '(:rsa-pss-rsae :rsa-pss-pss))
         (verify-rsa-pss-signature public-key-bytes content signature hash-algo))
        ;; RSA PKCS#1 v1.5 signatures
        ((eql sig-type :rsa-pkcs1)
         (verify-rsa-pkcs1-signature public-key-bytes content signature hash-algo))
        ;; ECDSA signatures
        ((eql sig-type :ecdsa)
         (verify-ecdsa-signature public-key-bytes content signature hash-algo key-algorithm))
        ;; Ed25519 (handles its own hashing internally)
        ((eql sig-type :ed25519)
         (verify-ed25519-signature public-key-bytes content signature))
        (t
         (error 'tls-handshake-error
                :message (format nil "Unsupported signature type: ~A" sig-type)))))))

(defun signature-algorithm-params (algorithm)
  "Return (hash-algorithm signature-type) for a TLS signature algorithm code."
  (case algorithm
    (#.+sig-rsa-pkcs1-md5+ (values :md5 :rsa-pkcs1))
    (#.+sig-rsa-pkcs1-sha1+ (values :sha1 :rsa-pkcs1))
    (#.+sig-ecdsa-sha1+ (values :sha1 :ecdsa))
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
    (otherwise (values nil nil))))

(defun verify-rsa-pss-signature (public-key-der content signature hash-algo)
  "Verify an RSA-PSS signature."
  (handler-case
      (let ((public-key (ironclad:make-public-key :rsa
                          :n (parse-rsa-public-key-n public-key-der)
                          :e (parse-rsa-public-key-e public-key-der))))
        (unless (ironclad:verify-signature public-key content signature
                                           :pss hash-algo)
          (error 'tls-handshake-error
                 :message ":BAD_SIGNATURE:")))
    (error (e)
      (declare (ignore e))
      (error 'tls-handshake-error
             :message ":BAD_SIGNATURE:"))))

(defun verify-rsa-pkcs1-signature (public-key-der content signature hash-algo)
  "Verify an RSA PKCS#1 v1.5 signature."
  (handler-case
      (let ((public-key (ironclad:make-public-key :rsa
                          :n (parse-rsa-public-key-n public-key-der)
                          :e (parse-rsa-public-key-e public-key-der))))
        (unless (ironclad:verify-signature public-key content signature
                                           :pkcs1v15 hash-algo)
          (error 'tls-handshake-error
                 :message ":BAD_SIGNATURE:")))
    (error (e)
      (declare (ignore e))
      (error 'tls-handshake-error
             :message ":BAD_SIGNATURE:"))))

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
        (unless (ironclad:verify-signature public-key hash
                                           (ironclad:make-signature curve :r r-bytes :s s-bytes))
          (error 'tls-handshake-error
                 :message ":BAD_SIGNATURE:")))
    (error (e)
      (declare (ignore e))
      (error 'tls-handshake-error
             :message ":BAD_SIGNATURE:"))))

(defun verify-ed25519-signature (public-key-der content signature)
  "Verify an Ed25519 signature."
  (handler-case
      (let ((public-key (ironclad:make-public-key :ed25519 :y public-key-der)))
        (unless (ironclad:verify-signature public-key content signature)
          (error 'tls-handshake-error
                 :message ":BAD_SIGNATURE:")))
    (error (e)
      (declare (ignore e))
      (error 'tls-handshake-error
             :message ":BAD_SIGNATURE:"))))

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

(defun sign-data-with-algorithm (content private-key algorithm)
  "Sign CONTENT with PRIVATE-KEY using TLS signature algorithm ALGORITHM."
  (multiple-value-bind (hash-algo sig-type)
      (signature-algorithm-params algorithm)
    (unless sig-type
      (error 'tls-handshake-error
             :message (format nil "Unsupported signature algorithm: ~X" algorithm)))
    (unless (signature-algorithm-allowed-p algorithm)
      (error 'tls-handshake-error
             :message ":WRONG_SIGNATURE_TYPE: unsupported signature algorithm"))
    (let* ((key-type (signature-key-type-from-private-key private-key))
           (required-key-type (signature-required-key-type sig-type))
           (normalized-key-type (case key-type
                                  ((:ecdsa-p256 :ecdsa-p384 :ecdsa-p521) :ecdsa)
                                  (otherwise key-type))))
      (unless (and (not (eql normalized-key-type :unknown))
                   (eql required-key-type normalized-key-type))
        (error 'tls-handshake-error
               :message ":WRONG_SIGNATURE_TYPE: signature type mismatch"))
      (cond
        ;; RSA-PSS signatures
        ((member sig-type '(:rsa-pss-rsae :rsa-pss-pss))
         (ironclad:sign-message private-key content :pss hash-algo))
        ;; RSA PKCS#1 v1.5 signatures
        ((eql sig-type :rsa-pkcs1)
         (ironclad:sign-message private-key content hash-algo))
        ;; ECDSA signatures
        ((eql sig-type :ecdsa)
         (let* ((hash (ironclad:digest-sequence hash-algo content))
                (raw-sig (ironclad:sign-message private-key hash)))
           (encode-ecdsa-signature-der raw-sig)))
        ;; Ed25519 signatures
        ((eql sig-type :ed25519)
         (ironclad:sign-message private-key content))
        (t
         (error 'tls-handshake-error
                :message (format nil "Unsupported signature type: ~A" sig-type)))))))

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
   RAW-SIG is a byte array containing R and S values concatenated.
   For P-256: 64 bytes (32 bytes R + 32 bytes S)
   For P-384: 96 bytes (48 bytes R + 48 bytes S)
   For P-521: 132 bytes (66 bytes R + 66 bytes S)"
  ;; Extract R and S from the concatenated signature
  (let* ((half-len (/ (length raw-sig) 2))
         (r-bytes (subseq raw-sig 0 half-len))
         (s-bytes (subseq raw-sig half-len))
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

(defun process-certificate-verify (hs message raw-bytes)
  "Process CertificateVerify message.
   RAW-BYTES are the serialized message bytes for updating transcript after verification.
   Verifies the signature AND performs full chain/hostname verification
   when verify-mode is +verify-peer+ or +verify-required+."
  (let* ((cert-verify (handshake-message-body message))
         (algorithm (certificate-verify-algorithm cert-verify))
         (signature (certificate-verify-signature cert-verify))
         (cert (client-handshake-peer-certificate hs))
         (chain (client-handshake-peer-certificate-chain hs))
         (verify-mode (client-handshake-verify-mode hs))
         (hostname (client-handshake-hostname hs))
         (trust-store (client-handshake-trust-store hs)))
    ;; Must have a certificate to verify
    (unless cert
      (error 'tls-handshake-error
             :message ":PEER_DID_NOT_RETURN_A_CERTIFICATE:"))
    ;; Get the transcript hash at this point (excludes CertificateVerify)
    (let* ((ks (client-handshake-key-schedule hs))
           (transcript-hash (key-schedule-transcript-hash-value ks))
           (content (make-certificate-verify-content transcript-hash)))
      ;; Verify the signature (proves server possesses the private key)
      (verify-certificate-verify-signature
       cert algorithm signature content
       :allowed-algorithms (supported-signature-algorithms-tls13)))
    ;; NOW update transcript with CertificateVerify (after verification)
    (client-handshake-update-transcript hs raw-bytes)
    ;; Now perform full certificate verification if required
    ;; This prevents bypass when using perform-client-handshake directly
    (when (member verify-mode (list +verify-peer+ +verify-required+))
      ;; Verify hostname matches certificate (unless skip-hostname-verify is set)
      (when (and hostname (not (client-handshake-skip-hostname-verify hs)))
        (verify-hostname cert hostname))
      ;; Verify certificate chain
      ;; Pass nil for hostname when skip-hostname-verify is set to avoid
      ;; platform-specific hostname verification (macOS/Windows)
      (when chain
        (let ((trusted-roots (when trust-store
                               (trust-store-certificates trust-store)))
              (verify-hostname (if (client-handshake-skip-hostname-verify hs)
                                   nil
                                   hostname)))
          (verify-certificate-chain chain trusted-roots
                                    (get-universal-time) verify-hostname))))
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
      ;; Send decrypt_error alert before signaling error
      (handler-case
          (record-layer-write-alert (client-handshake-record-layer hs)
                                    +alert-level-fatal+
                                    +alert-decrypt-error+)
        (error () nil))
      (error 'tls-handshake-error
             :message ":DIGEST_CHECK_FAILED: Server Finished verification failed"
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
        (key-schedule-derive-server-traffic-keys ks :application)
      (record-layer-install-keys
       (client-handshake-record-layer hs)
       :read key iv cipher-suite))
    ;; Check for trailing data after server Finished
    ;; The handshake message buffer should be empty at this point
    (when (client-handshake-message-buffer hs)
      (let ((extra-len (length (client-handshake-message-buffer hs))))
        (record-layer-write-alert (client-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-unexpected-message+)
        (error 'tls-handshake-error
               :message (format nil ":EXCESS_HANDSHAKE_DATA: ~D extra bytes after server Finished"
                               extra-len)
               :state :wait-finished)))
    (setf (client-handshake-state hs) :send-finished)))

;;;; Client Finished

(defun send-empty-client-certificate (hs)
  "Send an empty Certificate message when server requested client auth.
   In TLS 1.3, if we have no certificate to send, we must still send
   an empty Certificate message with the request context."
  (let* ((context (or (client-handshake-cert-request-context hs)
                      (make-octet-vector 0)))
         ;; Build Certificate message: context_len(1) + context + cert_list_len(3)
         ;; Empty certificate list has 0 length
         (context-len (length context))
         (message-data (concat-octet-vectors
                        (octet-vector context-len)
                        context
                        (octet-vector 0 0 0)))  ; 3-byte length = 0 (empty cert list)
         (message (wrap-handshake-message +handshake-certificate+ message-data)))
    ;; Update transcript
    (client-handshake-update-transcript hs message)
    ;; Send
    (record-layer-write-handshake (client-handshake-record-layer hs) message)))

(defun send-client-certificate (hs)
  "Send the client's Certificate message with actual certificate(s).
   Uses the certificate and chain from the handshake state."
  (let* ((context (or (client-handshake-cert-request-context hs)
                      (make-octet-vector 0)))
         ;; Build certificate entries (client cert first, then chain certs)
         (client-cert (client-handshake-client-certificate hs))
         (chain-certs (client-handshake-client-certificate-chain hs))
         ;; Combine: client cert + chain
         (all-certs (cons client-cert chain-certs))
         (cert-entries
           (mapcar (lambda (cert)
                     ;; Handle both raw DER bytes and X509-CERTIFICATE objects
                     (let ((cert-der (if (x509-certificate-p cert)
                                         (x509-certificate-raw-der cert)
                                         cert)))
                       (make-certificate-entry
                        :cert-data cert-der
                        :extensions nil)))
                   all-certs))
         (cert-msg (make-certificate-message
                    :certificate-request-context context
                    :certificate-list cert-entries))
         (cert-bytes (serialize-certificate-message cert-msg))
         (message (wrap-handshake-message +handshake-certificate+ cert-bytes)))
    ;; Update transcript
    (client-handshake-update-transcript hs message)
    ;; Send
    (record-layer-write-handshake (client-handshake-record-layer hs) message)))

(defun send-client-certificate-verify (hs)
  "Send the client's CertificateVerify message.
   Signs the transcript hash with the client's private key."
  (let* ((ks (client-handshake-key-schedule hs))
         (transcript-hash (key-schedule-transcript-hash-value ks))
         ;; Build the content to sign (per RFC 8446 Section 4.4.3)
         ;; Note: t = client context string
         (content (make-certificate-verify-content transcript-hash t))
         ;; Sign with client's private key
         (private-key (client-handshake-client-private-key hs))
         (algorithm (select-signature-algorithm-for-key
                     private-key
                     (client-handshake-cert-request-signature-algorithms hs)))
         (signature (sign-data-with-algorithm content private-key algorithm))
         (cv (make-certificate-verify :algorithm algorithm :signature signature))
         (cv-bytes (serialize-certificate-verify cv))
         (message (wrap-handshake-message +handshake-certificate-verify+ cv-bytes)))
    ;; Update transcript
    (client-handshake-update-transcript hs message)
    ;; Send
    (record-layer-write-handshake (client-handshake-record-layer hs) message)))

(defun send-client-finished (hs)
  "Send client Finished message.
   If client auth was requested, sends Certificate (and CertificateVerify if we have a cert)."
  (let* ((ks (client-handshake-key-schedule hs))
         (cipher-suite (client-handshake-selected-cipher-suite hs)))
    ;; Send dummy CCS for middlebox compatibility (RFC 8446 Appendix D.4)
    ;; Skip if already sent (e.g., after HRR)
    (unless (client-handshake-ccs-sent hs)
      (record-layer-write-change-cipher-spec (client-handshake-record-layer hs))
      (setf (client-handshake-ccs-sent hs) t))
    ;; Client handshake write keys were already installed after ServerHello processing
    ;; If server requested client auth, send Certificate (+ CertificateVerify if we have one)
    (when (client-handshake-certificate-requested hs)
      (if (and (client-handshake-client-certificate hs)
               (client-handshake-client-private-key hs))
          ;; We have a certificate and key - send them
          (progn
            (send-client-certificate hs)
            (send-client-certificate-verify hs))
          ;; No certificate available - send empty Certificate
          (send-empty-client-certificate hs)))
    ;; Compute verify_data using current transcript
    (let* ((transcript-hash (key-schedule-transcript-hash-value ks))
           (verify-data (compute-finished-verify-data
                         (key-schedule-client-handshake-traffic-secret ks)
                         transcript-hash
                         cipher-suite))
           (finished (make-finished-message :verify-data verify-data))
           (finished-bytes (serialize-finished finished))
           (message (wrap-handshake-message +handshake-finished+ finished-bytes)))
      ;; Update transcript (for resumption master secret)
      (client-handshake-update-transcript hs message)
      ;; Send
      (record-layer-write-handshake (client-handshake-record-layer hs) message))
    ;; Derive resumption master secret (for session tickets)
    (key-schedule-derive-resumption-master-secret ks (client-handshake-transcript hs))
    (setf (client-handshake-resumption-master-secret hs)
          (key-schedule-resumption-master-secret ks))
    ;; Install client application keys for writing
    (multiple-value-bind (key iv)
        (key-schedule-derive-client-traffic-keys ks :application)
      (record-layer-install-keys
       (client-handshake-record-layer hs)
       :write key iv cipher-suite))
    (setf (client-handshake-state hs) :connected)))

;;;; Main Handshake Loop
;;;
;;; Note: handshake-buffer-has-complete-message-p and handshake-buffer-extract-message
;;; are defined in messages.lisp and shared by both client and server.

(defun read-handshake-message (hs &key (update-transcript t))
  "Read and parse a handshake message from the record layer.
   Handles multiple messages per TLS record by buffering.
   If UPDATE-TRANSCRIPT is nil, the transcript is not updated (caller must do it).
   Returns (VALUES message raw-bytes) where raw-bytes are the serialized message bytes."
  ;; Read more data if needed
  (loop while (not (handshake-buffer-has-complete-message-p
                    (client-handshake-message-buffer hs)))
        do (handler-case
               (multiple-value-bind (content-type data)
                   (record-layer-read (client-handshake-record-layer hs))
                 ;; Handle alerts
                 (when (= content-type +content-type-alert+)
                   (process-alert data (client-handshake-record-layer hs)))
                 ;; Skip change_cipher_spec (compatibility) - just continue loop
                 (unless (= content-type +content-type-change-cipher-spec+)
                   ;; Must be handshake
                   (unless (= content-type +content-type-handshake+)
                     (error 'tls-handshake-error
                            :message (format nil ":UNEXPECTED_MESSAGE: Expected handshake, got content type ~D" content-type)))
                   ;; Append to buffer
                   (setf (client-handshake-message-buffer hs)
                         (if (client-handshake-message-buffer hs)
                             (concat-octet-vectors (client-handshake-message-buffer hs) data)
                             data))))
             ;; Handle record overflow - send alert and re-signal
             (tls-record-overflow (e)
               (handler-case
                   (record-layer-write-alert (client-handshake-record-layer hs)
                                             +alert-level-fatal+
                                             +alert-record-overflow+)
                 (error () nil))
               (error e))))
  ;; Extract one message from buffer
  (multiple-value-bind (message-bytes remaining)
      (handshake-buffer-extract-message (client-handshake-message-buffer hs))
    (setf (client-handshake-message-buffer hs) remaining)
    ;; Optionally update transcript with raw message bytes
    (when update-transcript
      (client-handshake-update-transcript hs message-bytes))
    ;; Parse the message with proper error handling for alerts
    (handler-bind
        ((tls-handshake-error
           (lambda (e)
             ;; Send appropriate alert based on error type
             (ignore-errors
               (let ((msg (tls-error-message e)))
                 (record-layer-write-alert (client-handshake-record-layer hs)
                                           +alert-level-fatal+
                                           (cond
                                             ;; Duplicate of KNOWN extension type → illegal_parameter
                                             ;; Duplicate of UNKNOWN extension type (like 65535) → decode_error
                                             ((and (search ":DUPLICATE_EXTENSION:" msg)
                                                   (not (search "65535" msg)))
                                              +alert-illegal-parameter+)
                                             (t +alert-decode-error+)))))))
         (tls-decode-error
           (lambda (e)
             ;; Send appropriate alert based on error type
             (ignore-errors
               (record-layer-write-alert (client-handshake-record-layer hs)
                                         +alert-level-fatal+
                                         (if (search ":UNSUPPORTED_EXTENSION:" (tls-error-message e))
                                             +alert-unsupported-extension+
                                             +alert-decode-error+))))))
      (values (parse-handshake-message message-bytes
                                       :hash-length (when (client-handshake-selected-cipher-suite hs)
                                                      (cipher-suite-hash-length
                                                       (client-handshake-selected-cipher-suite hs))))
              message-bytes))))

(defun perform-client-handshake (record-layer &key hostname alpn-protocols
                                                   (verify-mode +verify-required+)
                                                   trust-store
                                                   skip-hostname-verify
                                                   client-certificate
                                                   client-private-key
                                                   client-certificate-chain)
  "Perform the TLS 1.3 client handshake.
   Returns a CLIENT-HANDSHAKE structure on success.
   TRUST-STORE is used for certificate chain verification when verify-mode is +verify-required+.
   SKIP-HOSTNAME-VERIFY when true skips hostname verification (for SNI-only hostname).
   CLIENT-CERTIFICATE and CLIENT-PRIVATE-KEY are used for client authentication (mTLS)
   when the server requests it. CLIENT-CERTIFICATE-CHAIN provides additional chain certificates."
  (let ((hs (make-client-handshake
             :hostname hostname
             :alpn-protocols alpn-protocols
             :verify-mode verify-mode
             :trust-store trust-store
             :skip-hostname-verify skip-hostname-verify
             :client-certificate client-certificate
             :client-private-key client-private-key
             :client-certificate-chain client-certificate-chain
             :record-layer record-layer)))
    ;; Send ClientHello
    (send-client-hello hs)
    ;; Process server messages
    (loop
      (case (client-handshake-state hs)
        (:wait-server-hello
         (handler-bind
             ((tls-handshake-error
                (lambda (e)
                  ;; Send appropriate alert based on error type
                  (ignore-errors
                    (let ((msg (tls-error-message e)))
                      (record-layer-write-alert (client-handshake-record-layer hs)
                                                +alert-level-fatal+
                                                (cond
                                                  ;; Duplicate of KNOWN extension → illegal_parameter
                                                  ;; Duplicate of UNKNOWN extension (65535) → decode_error
                                                  ((and (search ":DUPLICATE_EXTENSION:" msg)
                                                        (not (search "65535" msg)))
                                                   +alert-illegal-parameter+)
                                                  (t +alert-decode-error+)))))))
              (tls-crypto-error
                (lambda (e)
                  ;; Send illegal_parameter for key exchange errors
                  (ignore-errors
                    (record-layer-write-alert (client-handshake-record-layer hs)
                                              +alert-level-fatal+
                                              +alert-illegal-parameter+)))))
           (let* ((raw-message (read-raw-handshake-message hs))
                  (message (parse-handshake-message raw-message)))
             (unless (= (handshake-message-type message) +handshake-server-hello+)
               (record-layer-write-alert (client-handshake-record-layer hs)
                                         +alert-level-fatal+ +alert-unexpected-message+)
               (error 'tls-handshake-error
                      :message ":UNEXPECTED_MESSAGE: Expected ServerHello"
                      :state :wait-server-hello))
             ;; DON'T update transcript here - process-server-hello handles it
             ;; (transcript handling differs for HRR vs regular ServerHello)
             (process-server-hello hs message raw-message)
             ;; After ServerHello (not HRR), encryption is installed.
             ;; Any leftover plaintext data in the buffer is excess handshake data.
             (when (and (eql (client-handshake-state hs) :wait-encrypted-extensions)
                        (client-handshake-message-buffer hs))
               (record-layer-write-alert (client-handshake-record-layer hs)
                                         +alert-level-fatal+ +alert-unexpected-message+)
               (error 'tls-handshake-error
                      :message ":EXCESS_HANDSHAKE_DATA: Unexpected plaintext data after ServerHello"
                      :state :wait-server-hello)))))
        (:wait-encrypted-extensions
         ;; Wrap in handler-bind to send alerts for TLS 1.2-only extension errors
         ;; (e.g., EMS, NPN, renegotiation_info sent in TLS 1.3)
         ;; Other errors (like ALPN empty protocol) are handled by inner handler
         (handler-bind
             ((tls-handshake-error
                (lambda (e)
                  ;; Only send illegal_parameter for TLS 1.2-only extension errors
                  ;; Other errors are already handled by the inner handler in read-handshake-message
                  (let ((msg (tls-error-message e)))
                    (when (search ":TLS12_ONLY_EXTENSION:" msg)
                      (ignore-errors
                        (record-layer-write-alert (client-handshake-record-layer hs)
                                                  +alert-level-fatal+
                                                  +alert-illegal-parameter+)))))))
           (let ((message (read-handshake-message hs)))
             (unless (= (handshake-message-type message) +handshake-encrypted-extensions+)
               (record-layer-write-alert (client-handshake-record-layer hs)
                                         +alert-level-fatal+ +alert-unexpected-message+)
               (error 'tls-handshake-error
                      :message ":UNEXPECTED_MESSAGE: Expected EncryptedExtensions"
                      :state :wait-encrypted-extensions))
             (process-encrypted-extensions hs message))))
        (:wait-cert-or-finished
         ;; Don't update transcript automatically - we need to handle Finished specially
         ;; Wrap in handler-bind to catch decode errors (e.g., garbage certificates)
         (handler-bind
             ((tls-decode-error
                (lambda (e)
                  ;; Send decode_error alert for certificate parsing errors
                  (ignore-errors
                    (record-layer-write-alert (client-handshake-record-layer hs)
                                              +alert-level-fatal+
                                              +alert-decode-error+)))))
           (multiple-value-bind (message raw-bytes)
               (read-handshake-message hs :update-transcript nil)
             (case (handshake-message-type message)
               (#.+handshake-certificate-request+
                ;; Server is requesting client certificate (mTLS)
                ;; Update transcript and remember we need to send a cert later
                (client-handshake-update-transcript hs raw-bytes)
                (process-certificate-request hs message)
                ;; Stay in this state, waiting for server's Certificate
                )
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
               (otherwise
                (record-layer-write-alert (client-handshake-record-layer hs)
                                          +alert-level-fatal+ +alert-unexpected-message+)
                (error 'tls-handshake-error
                       :message ":UNEXPECTED_MESSAGE: Expected CertificateRequest, Certificate or Finished"
                       :state :wait-cert-or-finished))))))
        (:wait-certificate-verify
         ;; Don't update transcript yet - we need to verify signature first, THEN update
         (multiple-value-bind (message raw-bytes)
             (read-handshake-message hs :update-transcript nil)
           (unless (= (handshake-message-type message) +handshake-certificate-verify+)
             (record-layer-write-alert (client-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unexpected-message+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_MESSAGE: Expected CertificateVerify"
                    :state :wait-certificate-verify))
           (process-certificate-verify hs message raw-bytes)))
        (:wait-finished
         ;; Don't update transcript yet - we need to verify first, THEN update
         (multiple-value-bind (message raw-bytes)
             (read-handshake-message hs :update-transcript nil)
           (unless (= (handshake-message-type message) +handshake-finished+)
             (record-layer-write-alert (client-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unexpected-message+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_MESSAGE: Expected Finished"
                    :state :wait-finished))
           (process-server-finished hs message raw-bytes)))
        (:send-finished
         (send-client-finished hs))
        (:connected
         (return hs))
        (otherwise
         (error 'tls-handshake-error
                :message (format nil "Unknown state: ~A" (client-handshake-state hs))))))))

(defun read-raw-handshake-message (hs)
  "Read a raw handshake message before encryption is established.
   Handles handshake messages fragmented across multiple TLS records."
  ;; Read more data if needed until we have a complete message
  (loop while (not (handshake-buffer-has-complete-message-p
                    (client-handshake-message-buffer hs)))
        do (handler-case
               (multiple-value-bind (content-type data)
                   (record-layer-read (client-handshake-record-layer hs))
                 ;; Handle alerts
                 (when (= content-type +content-type-alert+)
                   (process-alert data (client-handshake-record-layer hs)))
                 ;; Skip change_cipher_spec (compatibility) - just continue loop
                 (unless (= content-type +content-type-change-cipher-spec+)
                   ;; Must be handshake
                   (unless (= content-type +content-type-handshake+)
                     (error 'tls-handshake-error
                            :message (format nil ":UNEXPECTED_MESSAGE: Expected handshake, got content type ~D" content-type)))
                   ;; Append to buffer
                   (setf (client-handshake-message-buffer hs)
                         (if (client-handshake-message-buffer hs)
                             (concat-octet-vectors (client-handshake-message-buffer hs) data)
                             data))))
             ;; Handle record overflow - send alert and re-signal
             (tls-record-overflow (e)
               (handler-case
                   (record-layer-write-alert (client-handshake-record-layer hs)
                                             +alert-level-fatal+
                                             +alert-record-overflow+)
                 (error () nil))
               (error e))))
  ;; Extract one message from buffer
  (multiple-value-bind (message-bytes remaining)
      (handshake-buffer-extract-message (client-handshake-message-buffer hs))
    (setf (client-handshake-message-buffer hs) remaining)
    message-bytes))
