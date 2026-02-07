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
  ;; Certificate provider for dynamic cert selection (e.g., ACME TLS-ALPN-01)
  ;; Called with (hostname alpn-list) before certificate selection
  ;; Should return (values certificate-chain private-key alpn-protocol) or NIL
  ;; If alpn-protocol is returned, it overrides normal ALPN negotiation
  (certificate-provider nil)
  ;; Cipher suites we support (in preference order)
  ;; ChaCha20-Poly1305 is preferred for better side-channel resistance
  (cipher-suites (list +tls-chacha20-poly1305-sha256+
                       +tls-aes-256-gcm-sha384+
                       +tls-aes-128-gcm-sha256+)
                 :type list)
  ;; Key exchange state
  (key-exchange nil)
  ;; Selected key exchange group
  (selected-group nil)
  ;; Server's key share bytes for ServerHello (public key or ct||pk for hybrid)
  (server-share-bytes nil :type (or null octet-vector))
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
  ;; Client's signature algorithm preferences (from ClientHello)
  (client-signature-algorithms nil :type list)
  ;; Whether we requested a client certificate
  (certificate-requested nil :type boolean)
  ;; signature_algorithms sent in CertificateRequest (for mTLS verification)
  (cert-request-signature-algorithms nil :type list)
  ;; Session resumption (PSK)
  (psk-accepted nil :type boolean)  ; T if we accepted client's PSK
  (accepted-psk nil :type (or null octet-vector))  ; The PSK we accepted
  (selected-psk-index nil)  ; Index of selected PSK identity
  (resumption-master-secret nil :type (or null octet-vector))  ; For ticket generation
  (ticket-nonce-counter 0 :type fixnum)  ; Counter for ticket nonces
  ;; Message buffer for reassembling fragmented handshake messages
  (message-buffer nil :type (or null octet-vector))
  ;; HelloRetryRequest state
  (hello-retry-sent nil :type boolean)  ; T if we've sent HRR
  (hrr-selected-group nil)              ; Group we requested in HRR
  (first-client-hello-hash nil :type (or null octet-vector))  ; Hash of CH1 for transcript
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
                         (let* (;; Use helper that includes 2-byte length prefix + all binder data
                                (binders-len (pre-shared-key-ext-binders-length psk-ext))
                                ;; Truncated hello is everything except the binders portion
                                (truncated-len (- (length raw-client-hello)
                                                  binders-len))
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
  (hs-log "~&[HS] process-client-hello: starting~%")
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
    (hs-log "~&[HS] process-client-hello: hostname=~A~%" (server-handshake-client-hostname hs))
    ;; Extract client ALPN list for certificate provider
    (let* ((alpn-ext (find-extension extensions +extension-application-layer-protocol-negotiation+))
           (client-alpn-list (when alpn-ext
                               (alpn-ext-protocol-list (tls-extension-data alpn-ext)))))
      (hs-log "~&[HS] process-client-hello: ALPN=~A~%" client-alpn-list)
      ;; Call certificate provider if available (for ACME TLS-ALPN-01, etc.)
      ;; Certificate provider can override certificate AND ALPN selection
      (when (server-handshake-certificate-provider hs)
        (hs-log "~&[HS] process-client-hello: calling cert provider~%")
        (multiple-value-bind (cert-chain priv-key selected-alpn)
            (funcall (server-handshake-certificate-provider hs)
                     (server-handshake-client-hostname hs)
                     client-alpn-list)
          (hs-log "~&[HS] process-client-hello: provider returned chain=~A key=~A alpn=~A~%"
                  (and cert-chain t) (and priv-key t) selected-alpn)
          (when cert-chain
            (setf (server-handshake-certificate-chain hs) cert-chain)
            (when priv-key
              (setf (server-handshake-private-key hs) priv-key))
            (when selected-alpn
              ;; Provider selected ALPN - skip normal ALPN negotiation later
              (setf (server-handshake-selected-alpn hs) selected-alpn))))))
    ;; Call SNI callback if provided (for virtual hosting)
    ;; Only if certificate-provider didn't already set a certificate
    (when (and (server-handshake-sni-callback hs)
               (server-handshake-client-hostname hs)
               (null (server-handshake-certificate-chain hs)))
      (multiple-value-bind (cert-chain priv-key)
          (funcall (server-handshake-sni-callback hs)
                   (server-handshake-client-hostname hs))
        (cond
          ;; Callback returned :reject - send unrecognized_name alert
          ((eq cert-chain :reject)
           (record-layer-write-alert (server-handshake-record-layer hs)
                                     +alert-level-fatal+ +alert-unrecognized-name+)
           (error 'tls-alert-error
                  :level +alert-level-fatal+
                  :description +alert-unrecognized-name+))
          ;; Callback returned certificate chain - use it
          (cert-chain
           (setf (server-handshake-certificate-chain hs) cert-chain)
           (when priv-key
             (setf (server-handshake-private-key hs) priv-key))))))
    ;; Verify we have a certificate after SNI callback
    (unless (server-handshake-certificate-chain hs)
      (hs-log "~&[HS] process-client-hello: NO CERTIFICATE - erroring~%")
      (error 'tls-handshake-error
             :message "No certificate available for this hostname"
             :state :wait-client-hello))
    (hs-log "~&[HS] process-client-hello: have certificate, checking supported_versions~%")
    ;; Check for supported_versions extension (required for TLS 1.3)
    ;; If missing or doesn't include TLS 1.3, send protocol_version alert
    ;; This helps scanners like TLS-Anvil understand we only support TLS 1.3
    ;; Must check this BEFORE cipher suite selection to give correct error
    (let ((sv-ext (find-extension extensions +extension-supported-versions+)))
      (unless sv-ext
        (record-layer-write-alert (server-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-protocol-version+)
        (error 'tls-handshake-error
               :message "Missing supported_versions extension (TLS 1.2 not supported)"
               :state :wait-client-hello))
      (unless (member +tls-1.3+ (supported-versions-ext-versions (tls-extension-data sv-ext)))
        (record-layer-write-alert (server-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-protocol-version+)
        (error 'tls-handshake-error
               :message "Client does not support TLS 1.3"
               :state :wait-client-hello)))
    ;; RFC 8446 Section 4.2: Servers MUST ignore unrecognized extensions.
    ;; TLS 1.2-only extensions (renegotiation_info, extended_master_secret, NPN)
    ;; are simply skipped - we don't reject the ClientHello for having them.
    ;; The client might be offering TLS 1.2 fallback support.
    ;; However, certain extensions MUST be rejected in a non-QUIC TLS server:
    ;; - QUIC transport parameters (RFC 9001, extension 57) are only valid in QUIC
    ;; - Legacy QUIC params (65445) are in private-use range, just ignore them
    ;; - ALPS (17513) is ignored per RFC 8446 - server ignores unknown extensions
    (when (find-extension extensions +extension-quic-transport-parameters+)
      (record-layer-write-alert (server-handshake-record-layer hs)
                                +alert-level-fatal+ +alert-unsupported-extension+)
      (error 'tls-handshake-error
             :message ":UNEXPECTED_EXTENSION: QUIC transport parameters not allowed in TLS"
             :state :wait-client-hello))
    ;; RFC 8446 Section 4.1.2: legacy_compression_methods MUST contain exactly
    ;; one byte set to zero (null compression)
    (let ((compression-methods (client-hello-legacy-compression-methods client-hello)))
      (unless (and compression-methods
                   (= (length compression-methods) 1)
                   (= (aref compression-methods 0) 0))
        (record-layer-write-alert (server-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-illegal-parameter+)
        (error 'tls-handshake-error
               :message ":INVALID_COMPRESSION_LIST: TLS 1.3 requires legacy_compression_methods to be [0]"
               :state :wait-client-hello)))
    ;; Select cipher suite (first mutually supported)
    (hs-log "~&[HS] process-client-hello: selecting cipher suite~%")
    (let ((client-suites (client-hello-cipher-suites client-hello))
          (server-suites (server-handshake-cipher-suites hs)))
      (hs-log "~&[HS] process-client-hello: client-suites=~A server-suites=~A~%"
              client-suites server-suites)
      (let ((selected (find-if (lambda (s) (member s client-suites))
                               server-suites)))
        (unless selected
          (hs-log "~&[HS] process-client-hello: NO COMMON CIPHER SUITE~%")
          (record-layer-write-alert (server-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-handshake-failure+)
          (error 'tls-handshake-error
                 :message ":HANDSHAKE_FAILURE_ON_CLIENT_HELLO: No common cipher suite"
                 :state :wait-client-hello))
        (hs-log "~&[HS] process-client-hello: selected cipher=~A~%" selected)
        (setf (server-handshake-selected-cipher-suite hs) selected)))
    ;; Capture client's signature algorithm preferences (may be nil if missing)
    (let ((sig-ext (find-extension extensions +extension-signature-algorithms+)))
      (when sig-ext
        (setf (server-handshake-client-signature-algorithms hs)
              (signature-algorithms-ext-algorithms (tls-extension-data sig-ext)))))
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
      ;; Process key_share extension (may trigger HelloRetryRequest)
      (hs-log "~&[HS] process-client-hello: processing key_share~%")
      (let* ((ks-ext (find-extension extensions +extension-key-share+))
             (sg-ext (find-extension extensions +extension-supported-groups+))
             (our-groups (list +group-x25519-mlkem768+ +group-x25519+ +group-secp256r1+ +group-secp384r1+))
             (client-shares (when ks-ext
                              (key-share-ext-client-shares (tls-extension-data ks-ext))))
             ;; Find first share for a group we support
             (selected-share (find-if (lambda (share)
                                        (member (key-share-entry-group share) our-groups))
                                      client-shares)))
        (hs-log "~&[HS] process-client-hello: ks-ext=~A client-shares=~A selected-share=~A~%"
                (and ks-ext t) (length client-shares) (and selected-share t))
        (cond
          ;; Happy path: client offered a key share we can use
          (selected-share
           (let* ((group (key-share-entry-group selected-share))
                  (client-public (key-share-entry-key-exchange selected-share))
                  (expected-len (key-exchange-public-key-length group)))
             ;; Validate key_share length
             (when (and (plusp expected-len)
                        (/= (length client-public) expected-len))
               (record-layer-write-alert (server-handshake-record-layer hs)
                                         +alert-level-fatal+ +alert-illegal-parameter+)
               (error 'tls-handshake-error
                      :message (format nil ":ILLEGAL_PARAMETER: Invalid key_share length for ~A: ~D (expected ~D)"
                                      (named-group-name group) (length client-public) expected-len)
                      :state :wait-client-hello))
             ;; Store selected group
             (setf (server-handshake-selected-group hs) group)
             ;; Compute shared secret - different path for hybrid vs regular key exchange
             (hs-log "~&[HS] process-client-hello: computing shared secret for group ~A~%" group)
             (let ((shared-secret
                     (cond
                       ;; Hybrid X25519MLKEM768: use hybrid-server-encaps
                       ((= group +group-x25519-mlkem768+)
                        (multiple-value-bind (ss server-share)
                            (hybrid-server-encaps client-public)
                          (setf (server-handshake-server-share-bytes hs) server-share)
                          ss))
                       ;; Regular key exchange: generate keys and compute shared secret
                       (t
                        (let ((key-exchange (generate-key-exchange group)))
                          (setf (server-handshake-key-exchange hs) key-exchange)
                          (setf (server-handshake-server-share-bytes hs)
                                (key-exchange-public-key key-exchange))
                          (compute-shared-secret key-exchange client-public))))))
               (hs-log "~&[HS] process-client-hello: shared secret computed, initializing key schedule~%")
               ;; Initialize key schedule
               (let ((ks (make-key-schedule-state (server-handshake-selected-cipher-suite hs))))
                 ;; Use PSK if accepted, otherwise nil (for regular handshake)
                 (key-schedule-init ks accepted-psk)
                 (key-schedule-derive-handshake-secret ks shared-secret)
                 (setf (server-handshake-key-schedule hs) ks)
                 (hs-log "~&[HS] process-client-hello: key schedule initialized~%")))))
          ;; Need HelloRetryRequest: no usable key_share
          (t
           ;; RFC 8446: Only one HRR allowed
           (when (server-handshake-hello-retry-sent hs)
             (record-layer-write-alert (server-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-illegal-parameter+)
             ;; Differentiate between no key share vs wrong curve
             (error 'tls-handshake-error
                    :message (if client-shares
                                 ":WRONG_CURVE: Second ClientHello has key_share for wrong group"
                                 ":MISSING_KEY_SHARE: Second ClientHello missing requested key_share")
                    :state :wait-client-hello))
           ;; Find a group from supported_groups that we also support
           (let* ((client-groups (when sg-ext
                                   (supported-groups-ext-groups (tls-extension-data sg-ext))))
                  (common-group (find-if (lambda (g) (member g our-groups))
                                         client-groups)))
             (unless common-group
               ;; No common group at all - fatal error
               (record-layer-write-alert (server-handshake-record-layer hs)
                                         +alert-level-fatal+ +alert-handshake-failure+)
               (error 'tls-handshake-error
                      :message ":NO_SHARED_GROUP: No common key exchange group"
                      :state :wait-client-hello))
             ;; Request the client to retry with this group
             (setf (server-handshake-hrr-selected-group hs) common-group)
             (setf (server-handshake-state hs) :send-hello-retry-request)
             ;; Return early - don't process ALPN etc. yet
             (return-from process-client-hello nil))))))
    ;; Handle ALPN per RFC 7301
    ;; Skip if certificate-provider already selected ALPN
    (hs-log "~&[HS] process-client-hello: handling ALPN, selected-alpn=~A~%"
            (server-handshake-selected-alpn hs))
    (unless (server-handshake-selected-alpn hs)
      ;; alpn-protocols can be:
      ;;   nil - server doesn't care about ALPN
      ;;   (:none) - server requires ALPN but supports no protocols (always fail)
      ;;   list of strings - server supports these protocols
      (let ((alpn-ext (find-extension extensions +extension-application-layer-protocol-negotiation+))
            (server-protos (server-handshake-alpn-protocols hs)))
        (cond
          ;; Server doesn't care about ALPN - ignore client ALPN extension
          ((null server-protos)
           nil)
          ;; Server requires ALPN but has no protocols - fail if client sends ALPN
          ((equal server-protos '(:none))
           (when alpn-ext
             (record-layer-write-alert (server-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-no-application-protocol+)
             (error 'tls-alert-error
                    :level +alert-level-fatal+
                    :description +alert-no-application-protocol+)))
          ;; Server has protocols - try to negotiate
          (t
           (when alpn-ext
             (let* ((client-protos (alpn-ext-protocol-list (tls-extension-data alpn-ext)))
                    (selected (find-if (lambda (p) (member p client-protos :test #'string=))
                                       server-protos)))
               (if selected
                   (setf (server-handshake-selected-alpn hs) selected)
                   ;; No match - send fatal alert per RFC 7301
                   (progn
                     (record-layer-write-alert (server-handshake-record-layer hs)
                                             +alert-level-fatal+ +alert-no-application-protocol+)
                   (error 'tls-alert-error
                          :level +alert-level-fatal+
                          :description +alert-no-application-protocol+)))))))))
    (hs-log "~&[HS] process-client-hello: COMPLETE - transitioning to :send-server-hello~%")
    (setf (server-handshake-state hs) :send-server-hello)))

;;;; Server Message Generation

(defun generate-server-hello (hs)
  "Generate the ServerHello message."
  (let* ((random (random-bytes 32))
         (extensions
           (list
            ;; supported_versions extension (required)
            (make-tls-extension
             :type +extension-supported-versions+
             :data (make-supported-versions-ext :selected-version +tls-1.3+))
            ;; key_share extension (uses pre-computed server share bytes)
            (make-tls-extension
             :type +extension-key-share+
             :data (make-key-share-ext
                    :server-share (make-key-share-entry
                                   :group (server-handshake-selected-group hs)
                                   :key-exchange (server-handshake-server-share-bytes hs)))))))
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

(defun generate-hello-retry-request (hs)
  "Generate a HelloRetryRequest message.
   RFC 8446 Section 4.1.4: HRR is a ServerHello with special random value."
  (let ((extensions
          (list
           ;; supported_versions extension (required)
           (make-tls-extension
            :type +extension-supported-versions+
            :data (make-supported-versions-ext :selected-version +tls-1.3+))
           ;; key_share extension with selected_group (NOT a full key share)
           ;; For HRR, we send just the NamedGroup we want the client to use
           (make-tls-extension
            :type +extension-key-share+
            :data (make-key-share-ext
                   :selected-group (server-handshake-hrr-selected-group hs))))))
    (make-server-hello
     :legacy-version +tls-1.2+
     :random +hello-retry-request-random+  ; Magic value indicates HRR
     :legacy-session-id-echo (or (server-handshake-client-session-id hs)
                                  (make-octet-vector 0))
     :cipher-suite (server-handshake-selected-cipher-suite hs)
     :legacy-compression-method 0
     :extensions extensions)))

(defun send-hello-retry-request (hs)
  "Send HelloRetryRequest and update transcript per RFC 8446 Section 4.4.1.
   The transcript becomes: message_hash || HelloRetryRequest"
  (let* ((hrr (generate-hello-retry-request hs))
         (hrr-bytes (serialize-server-hello hrr))
         (message (wrap-handshake-message +handshake-server-hello+ hrr-bytes))
         (cipher-suite (server-handshake-selected-cipher-suite hs))
         (digest (cipher-suite-digest cipher-suite))
         (hash-len (ironclad:digest-length digest)))
    ;; Compute hash of first ClientHello (stored in transcript)
    (let ((ch1-hash (ironclad:digest-sequence digest (server-handshake-transcript hs))))
      ;; Store for potential debugging
      (setf (server-handshake-first-client-hello-hash hs) ch1-hash)
      ;; Create message_hash synthetic message per RFC 8446 Section 4.4.1
      ;; message_hash = handshake_type(254) + length(hash-len) + Hash(CH1)
      (let ((message-hash (concat-octet-vectors
                           (octet-vector 254)  ; message_hash handshake type
                           (encode-uint24 hash-len)
                           ch1-hash)))
        ;; Replace transcript with message_hash || HRR
        (setf (server-handshake-transcript hs)
              (concat-octet-vectors message-hash message))))
    ;; Send HRR (unencrypted, like ServerHello)
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Send dummy CCS for middlebox compatibility
    (record-layer-write-change-cipher-spec (server-handshake-record-layer hs))
    ;; Mark that we've sent HRR
    (setf (server-handshake-hello-retry-sent hs) t)
    ;; Go back to waiting for a new ClientHello
    (setf (server-handshake-state hs) :wait-client-hello)))

(defun send-server-hello (hs)
  "Send the ServerHello message."
  (hs-log "~&[HS] send-server-hello: generating~%")
  (let* ((hello (generate-server-hello hs))
         (hello-bytes (serialize-server-hello hello))
         (message (wrap-handshake-message +handshake-server-hello+ hello-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    ;; Send ServerHello (unencrypted)
    (hs-log "~&[HS] send-server-hello: sending ServerHello~%")
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Send dummy CCS for middlebox compatibility (RFC 8446 Appendix D.4)
    ;; This must be sent immediately after ServerHello, before encrypted messages
    (hs-log "~&[HS] send-server-hello: sending CCS~%")
    (record-layer-write-change-cipher-spec (server-handshake-record-layer hs))
    ;; Derive handshake traffic secrets from transcript (ClientHello + ServerHello)
    (hs-log "~&[HS] send-server-hello: deriving secrets~%")
    (let ((ks (server-handshake-key-schedule hs)))
      (key-schedule-derive-handshake-traffic-secrets
       ks (server-handshake-transcript hs))
      ;; Store client random in key schedule and log secrets for Wireshark
      (setf (key-schedule-client-random ks) (server-handshake-client-random hs))
      (keylog-write-handshake-secrets ks)
      ;; Feed transcript into key schedule for Finished verification
      (key-schedule-update-transcript ks (server-handshake-transcript hs))
      ;; Install server handshake write keys
      (hs-log "~&[HS] send-server-hello: installing keys~%")
      (multiple-value-bind (key iv)
          (key-schedule-derive-server-traffic-keys ks :handshake)
        (record-layer-install-keys
         (server-handshake-record-layer hs)
         :write key iv
         (server-handshake-selected-cipher-suite hs)))
      ;; Install client handshake read keys
      (multiple-value-bind (key iv)
          (key-schedule-derive-client-traffic-keys ks :handshake)
        (record-layer-install-keys
         (server-handshake-record-layer hs)
         :read key iv
         (server-handshake-selected-cipher-suite hs))))
    (hs-log "~&[HS] send-server-hello: done~%")
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
  (hs-log "~&[HS] send-encrypted-extensions: generating~%")
  (let* ((ee (generate-encrypted-extensions hs))
         (ee-bytes (serialize-encrypted-extensions ee))
         (message (wrap-handshake-message +handshake-encrypted-extensions+ ee-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send (encrypted with server handshake keys)
    (hs-log "~&[HS] send-encrypted-extensions: sending~%")
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    (hs-log "~&[HS] send-encrypted-extensions: done~%")
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
                   :algorithms (supported-signature-algorithms-tls13))))))
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
    (setf (server-handshake-cert-request-signature-algorithms hs)
          (let ((sig-ext (find-extension (certificate-request-extensions req)
                                         +extension-signature-algorithms+)))
            (when sig-ext
              (signature-algorithms-ext-algorithms (tls-extension-data sig-ext)))))
    (setf (server-handshake-state hs) :send-certificate)))

(defun generate-certificate-message (hs)
  "Generate the server's Certificate message."
  (let ((cert-entries
          (mapcar (lambda (cert)
                    ;; Handle both raw DER bytes and X509-CERTIFICATE objects
                    (let ((cert-der (if (x509-certificate-p cert)
                                        (x509-certificate-raw-der cert)
                                        cert)))
                      (make-certificate-entry
                       :cert-data cert-der
                       :extensions nil)))
                  (server-handshake-certificate-chain hs))))
    (make-certificate-message
     :certificate-request-context (make-octet-vector 0)
     :certificate-list cert-entries)))

(defun send-certificate (hs)
  "Send the server's Certificate message."
  (hs-log "~&[HS] send-certificate: generating~%")
  (let* ((cert-msg (generate-certificate-message hs))
         (cert-bytes (serialize-certificate-message cert-msg))
         (message (wrap-handshake-message +handshake-certificate+ cert-bytes)))
    ;; Update transcript
    (server-handshake-update-transcript hs message)
    (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
    ;; Send
    (hs-log "~&[HS] send-certificate: sending (~A bytes)~%" (length cert-bytes))
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    (hs-log "~&[HS] send-certificate: done~%")
    (setf (server-handshake-state hs) :send-certificate-verify)))

(defun send-certificate-verify (hs)
  "Send the CertificateVerify message."
  (hs-log "~&[HS] send-certificate-verify: building content~%")
  (let* ((ks (server-handshake-key-schedule hs))
         (transcript-hash (key-schedule-transcript-hash-value ks))
         ;; Build the content to sign (per RFC 8446 Section 4.4.3)
         (content (make-certificate-verify-content transcript-hash nil))) ; nil = server
    ;; Sign with server's private key
    (hs-log "~&[HS] send-certificate-verify: key type=~A~%" (type-of (server-handshake-private-key hs)))
    (hs-log "~&[HS] send-certificate-verify: client sig algs=~A~%" (server-handshake-client-signature-algorithms hs))
    (let* ((algorithm (select-signature-algorithm-for-key
                       (server-handshake-private-key hs)
                       (server-handshake-client-signature-algorithms hs))))
      (hs-log "~&[HS] send-certificate-verify: selected alg=~A, signing...~%" algorithm)
      (let* ((signature (sign-data-with-algorithm content (server-handshake-private-key hs) algorithm))
             (cv (make-certificate-verify :algorithm algorithm :signature signature))
             (cv-bytes (serialize-certificate-verify cv))
             (message (wrap-handshake-message +handshake-certificate-verify+ cv-bytes)))
        (hs-log "~&[HS] send-certificate-verify: sig len=~A~%" (length signature))
        ;; Update transcript
        (server-handshake-update-transcript hs message)
        (key-schedule-update-transcript (server-handshake-key-schedule hs) message)
        ;; Send
        (hs-log "~&[HS] send-certificate-verify: sending~%")
        (record-layer-write-handshake (server-handshake-record-layer hs) message)
        (hs-log "~&[HS] send-certificate-verify: done~%")
        (setf (server-handshake-state hs) :send-finished)))))

(defun send-server-finished (hs)
  "Send the server's Finished message."
  (hs-log "~&[HS] send-server-finished: computing verify data~%")
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
    (hs-log "~&[HS] send-server-finished: sending~%")
    (record-layer-write-handshake (server-handshake-record-layer hs) message)
    ;; Derive master secret and application secrets
    (hs-log "~&[HS] send-server-finished: deriving app secrets~%")
    (key-schedule-derive-master-secret ks)
    (key-schedule-derive-application-traffic-secrets ks (server-handshake-transcript hs))
    ;; Log application secrets for Wireshark
    (keylog-write-application-secrets ks)
    ;; Install server application write keys
    (hs-log "~&[HS] send-server-finished: installing app keys~%")
    (multiple-value-bind (key iv)
        (key-schedule-derive-server-traffic-keys ks :application)
      (record-layer-install-keys
       (server-handshake-record-layer hs)
       :write key iv
       cipher-suite))
    (hs-log "~&[HS] send-server-finished: done, waiting for client~%")
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
            (progn
              (record-layer-write-alert (server-handshake-record-layer hs)
                                        +alert-level-fatal+ +alert-certificate-required+)
              (error 'tls-certificate-error
                     :message ":PEER_DID_NOT_RETURN_A_CERTIFICATE: Client certificate required but not provided"))
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
  "Process the client's CertificateVerify message.
   Verifies both the signature AND the certificate chain when verification is enabled."
  (let* ((cv (handshake-message-body message))
         (ks (server-handshake-key-schedule hs))
         ;; Get transcript hash BEFORE adding this message
         (transcript-hash (key-schedule-transcript-hash-value ks))
         ;; Build expected content (per RFC 8446 Section 4.4.3)
         (content (make-certificate-verify-content transcript-hash t)) ; t = client
         (cert (server-handshake-peer-certificate hs))
         (algorithm (certificate-verify-algorithm cv))
         (signature (certificate-verify-signature cv))
         (verify-mode (server-handshake-verify-mode hs))
         (trust-store (server-handshake-trust-store hs)))
    ;; Verify signature (proves client possesses the private key)
    (verify-certificate-verify-signature
     cert algorithm signature content
     :allowed-algorithms (server-handshake-cert-request-signature-algorithms hs))
    ;; Update transcript AFTER verification
    (server-handshake-update-transcript hs raw-bytes)
    (key-schedule-update-transcript ks raw-bytes)
    ;; Verify certificate chain when verification mode requires it
    ;; +verify-peer+ with no trust store = just verify signature (proof of key possession)
    ;; +verify-required+ with no trust store = FAIL with unknown_ca (cannot verify without CA)
    ;; +verify-peer+ or +verify-required+ with trust store = verify signature AND chain
    (when (= verify-mode +verify-required+)
      (unless trust-store
        ;; Cannot verify client certificate without a trust store
        (record-layer-write-alert (server-handshake-record-layer hs)
                                  +alert-level-fatal+ +alert-unknown-ca+)
        (error 'tls-verification-error
               :message "Cannot verify client certificate: no trusted CA configured"
               :reason :unknown-ca)))
    (when (and (member verify-mode (list +verify-peer+ +verify-required+))
               trust-store)
      ;; Full chain verification when trust store is available
      ;; Map verification failures to appropriate TLS alerts
      (handler-case
          (verify-certificate-chain
           (server-handshake-peer-certificate-chain hs)
           (trust-store-certificates trust-store)
           (get-universal-time) nil
           :request-context (record-layer-request-context (server-handshake-record-layer hs)))
        (tls-verification-error (e)
          ;; Map verification reason to appropriate alert
          (let ((alert-code (case (tls-verification-error-reason e)
                              (:unknown-ca +alert-unknown-ca+)
                              (:name-mismatch +alert-bad-certificate+)
                              (t +alert-certificate-unknown+))))
            (record-layer-write-alert (server-handshake-record-layer hs)
                                      +alert-level-fatal+ alert-code)
            (error e)))
        (tls-certificate-expired (e)
          (record-layer-write-alert (server-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-certificate-expired+)
          (error e))
        (tls-certificate-not-yet-valid (e)
          (record-layer-write-alert (server-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-bad-certificate+)
          (error e))
        (tls-certificate-error (e)
          (record-layer-write-alert (server-handshake-record-layer hs)
                                    +alert-level-fatal+ +alert-bad-certificate+)
          (error e))))
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
      ;; Send decrypt_error alert before signaling error
      (handler-case
          (record-layer-write-alert (server-handshake-record-layer hs)
                                    +alert-level-fatal+
                                    +alert-decrypt-error+)
        (error () nil))
      (error 'tls-handshake-error
             :message ":DIGEST_CHECK_FAILED: Client Finished verification failed"
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
        (key-schedule-derive-client-traffic-keys ks :application)
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
      ;; Note: GREASE extension temporarily removed for debugging
      (make-new-session-ticket
       :ticket-lifetime ticket-lifetime
       :ticket-age-add age-add
       :ticket-nonce nonce
       :ticket full-ticket
       :extensions nil))))

(defun send-new-session-ticket (hs)
  "Send a NewSessionTicket message to enable session resumption.
   This should be called after the handshake completes.
   Silently ignores write errors if the client has already closed."
  (when (server-handshake-resumption-master-secret hs)
    (let* ((nst (generate-new-session-ticket hs))
           (nst-bytes (serialize-new-session-ticket nst))
           (message (wrap-handshake-message +handshake-new-session-ticket+ nst-bytes)))
      ;; Send (encrypted with server application keys)
      ;; Ignore errors - client may have already closed the connection
      (handler-case
          (record-layer-write-handshake (server-handshake-record-layer hs) message)
        (error () nil)))))

;;;; Handshake Message Reading

(defun server-read-handshake-message (hs &key update-transcript)
  "Read and parse a handshake message from the record layer.
   Handles handshake messages that are fragmented across multiple TLS records
   by buffering data until a complete message is available.
   Returns (VALUES message raw-bytes) where raw-bytes are the serialized message bytes."
  ;; Read more data if needed until we have a complete message
  (loop while (not (handshake-buffer-has-complete-message-p
                    (server-handshake-message-buffer hs)))
        do (handler-case
               (multiple-value-bind (content-type data)
                   (record-layer-read (server-handshake-record-layer hs))
                 ;; Handle alerts
                 (when (= content-type +content-type-alert+)
                   (process-alert data (server-handshake-record-layer hs)))
                 ;; Skip change_cipher_spec (compatibility) - just continue loop
                 (unless (= content-type +content-type-change-cipher-spec+)
                   ;; Must be handshake
                   (unless (= content-type +content-type-handshake+)
                     (error 'tls-handshake-error
                            :message (format nil ":UNEXPECTED_MESSAGE: Expected handshake, got content type ~D" content-type)
                            :state (server-handshake-state hs)))
                   ;; Append to buffer
                   (setf (server-handshake-message-buffer hs)
                         (if (server-handshake-message-buffer hs)
                             (concat-octet-vectors (server-handshake-message-buffer hs) data)
                             data))))
             ;; Handle record overflow - send alert and re-signal
             (tls-record-overflow (e)
               (handler-case
                   (record-layer-write-alert (server-handshake-record-layer hs)
                                             +alert-level-fatal+
                                             +alert-record-overflow+)
                 (error () nil))
               (error e))))
  ;; Extract one message from buffer
  (multiple-value-bind (message-bytes remaining)
      (handshake-buffer-extract-message (server-handshake-message-buffer hs))
    (setf (server-handshake-message-buffer hs) remaining)
    ;; Optionally update transcript with raw message bytes
    (when update-transcript
      (server-handshake-update-transcript hs message-bytes))
    ;; Parse the message and return both parsed message and raw bytes
    (let ((hash-length (when (server-handshake-selected-cipher-suite hs)
                         (cipher-suite-hash-length
                          (server-handshake-selected-cipher-suite hs)))))
      (values (parse-handshake-message message-bytes :hash-length hash-length)
              message-bytes))))

;;;; Main Handshake Orchestration

(defvar *handshake-debug* nil
  "When T, log detailed handshake progress for debugging.")

(defun hs-log (format-string &rest args)
  "Log handshake debug message."
  (when *handshake-debug*
    (apply #'format t format-string args)
    (force-output)))

(defun perform-server-handshake (record-layer certificate-chain private-key
                                  &key alpn-protocols verify-mode trust-store
                                       cipher-suites sni-callback certificate-provider)
  "Perform the TLS 1.3 server handshake.
   Returns the server-handshake structure on success.

   SNI-CALLBACK, if provided, is called with the client's requested hostname.
   It should return (VALUES certificate-chain private-key) to use for that host,
   or NIL to use the default certificate/key. This enables virtual hosting.

   CERTIFICATE-PROVIDER, if provided, is called with (hostname alpn-list) before
   certificate selection. It should return (VALUES cert-chain key selected-alpn)
   to override both certificate and ALPN selection. Useful for ACME TLS-ALPN-01."
  (hs-log "~&[HS] Starting server handshake~%")
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
             :sni-callback sni-callback
             :certificate-provider certificate-provider)))
    ;; State machine loop
    (loop
      ;; Check request context for deadline/cancellation
      (let ((record-layer (server-handshake-record-layer hs)))
        (when record-layer
          (check-tls-context)))
      (hs-log "~&[HS] State: ~A~%" (server-handshake-state hs))
      (case (server-handshake-state hs)
        (:start
         (setf (server-handshake-state hs) :wait-client-hello))

        (:wait-client-hello
         (handler-case
             (multiple-value-bind (message raw-bytes)
                 (server-read-handshake-message hs)
               (unless (= (handshake-message-type message) +handshake-client-hello+)
                 (record-layer-write-alert (server-handshake-record-layer hs)
                                           +alert-level-fatal+ +alert-unexpected-message+)
                 (error 'tls-handshake-error
                        :message ":UNEXPECTED_MESSAGE: Expected ClientHello"
                        :state :wait-client-hello))
               (process-client-hello hs message raw-bytes))
           ;; Catch decode errors and send decode_error alert
           (tls-decode-error (e)
             (ignore-errors
               (record-layer-write-alert (server-handshake-record-layer hs)
                                         +alert-level-fatal+ +alert-decode-error+))
             (error e))
           ;; Catch extension parsing errors and send appropriate alert
           (tls-handshake-error (e)
             (let ((msg (tls-error-message e)))
               ;; Check for specific error types and send appropriate alerts
               (cond
                 ((search ":DUPLICATE_KEY_SHARE:" msg)
                  (record-layer-write-alert (server-handshake-record-layer hs)
                                            +alert-level-fatal+ +alert-illegal-parameter+))
                 ((search ":DUPLICATE_EXTENSION:" msg)
                  (record-layer-write-alert (server-handshake-record-layer hs)
                                            +alert-level-fatal+ +alert-illegal-parameter+)))
               (error e)))))

        (:send-hello-retry-request
         (send-hello-retry-request hs))

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
             (record-layer-write-alert (server-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unexpected-message+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_MESSAGE: Expected client Certificate"
                    :state :wait-client-certificate))
           (process-client-certificate hs message raw-bytes)))

        (:wait-client-certificate-verify
         (multiple-value-bind (message raw-bytes)
             (server-read-handshake-message hs)
           (unless (= (handshake-message-type message) +handshake-certificate-verify+)
             (record-layer-write-alert (server-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unexpected-message+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_MESSAGE: Expected client CertificateVerify"
                    :state :wait-client-certificate-verify))
           (process-client-certificate-verify hs message raw-bytes)))

        (:wait-client-finished
         (multiple-value-bind (message raw-bytes)
             (server-read-handshake-message hs)
           (unless (= (handshake-message-type message) +handshake-finished+)
             (record-layer-write-alert (server-handshake-record-layer hs)
                                       +alert-level-fatal+ +alert-unexpected-message+)
             (error 'tls-handshake-error
                    :message ":UNEXPECTED_MESSAGE: Expected client Finished"
                    :state :wait-client-finished))
           (process-client-finished hs message raw-bytes)))

        (:connected
         ;; Send NewSessionTicket for session resumption
         (send-new-session-ticket hs)
         (return hs))

        (otherwise
         (error 'tls-handshake-error
                :message (format nil "Unknown state: ~A" (server-handshake-state hs))
                :state (server-handshake-state hs)))))))
