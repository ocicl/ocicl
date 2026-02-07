;;; streams.lisp --- TLS Stream Implementation
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements TLS streams using Gray streams.

(in-package #:pure-tls)

;;;; TLS Stream Class

(defclass tls-stream (trivial-gray-stream-mixin
                      fundamental-binary-input-stream
                      fundamental-binary-output-stream)
  ((underlying-stream
    :initarg :stream
    :reader tls-stream-underlying-stream
    :documentation "The underlying TCP stream.")
   (record-layer
    :accessor tls-stream-record-layer
    :documentation "The TLS record layer.")
   (handshake
    :accessor tls-stream-handshake
    :documentation "The handshake state (after completion).")
   (input-buffer
    :initform (make-octet-vector 0)
    :accessor tls-stream-input-buffer
    :documentation "Buffer for decrypted data not yet read.")
   (input-position
    :initform 0
    :accessor tls-stream-input-position
    :documentation "Current position in input buffer.")
   (output-buffer
    :accessor tls-stream-output-buffer
    :documentation "Buffer for data to be encrypted and sent.")
   (output-position
    :initform 0
    :accessor tls-stream-output-position
    :documentation "Current position in output buffer.")
   (closed
    :initform nil
    :accessor tls-stream-closed-p
    :documentation "Whether the stream has been closed.")
   (close-callback
    :initarg :close-callback
    :initform nil
    :accessor tls-stream-close-callback)
   (warning-alert-count
    :initform 0
    :accessor tls-stream-warning-alert-count
    :documentation "Count of consecutive warning alerts received. Reset on app data.")
   (key-update-count
    :initform 0
    :accessor tls-stream-key-update-count
    :documentation "Count of KeyUpdate messages received. Used to prevent DoS.")
   (empty-record-count
    :initform 0
    :accessor tls-stream-empty-record-count
    :documentation "Count of consecutive empty records. Used to prevent DoS."))
  (:documentation "A TLS-encrypted stream."))

(defclass tls-client-stream (tls-stream)
  ()
  (:documentation "A TLS client stream."))

(defclass tls-server-stream (tls-stream)
  ()
  (:documentation "A TLS server stream."))

(defmethod initialize-instance :after ((stream tls-stream) &key
                                                            (buffer-size *default-buffer-size*)
                                                            &allow-other-keys)
  (setf (tls-stream-output-buffer stream)
        (make-octet-vector buffer-size)))

;;;; Stream Methods

(defmethod stream-element-type ((stream tls-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream tls-stream))
  (not (tls-stream-closed-p stream)))

(defmethod close ((stream tls-stream) &key abort)
  (unless (tls-stream-closed-p stream)
    ;; Flush pending output unless aborting
    (unless abort
      (force-output stream))
    ;; Send close_notify alert and flush to ensure it's sent before closing
    (unless abort
      (handler-case
          (progn
            (record-layer-write-alert (tls-stream-record-layer stream)
                                      +alert-level-warning+
                                      +alert-close-notify+)
            (force-output (tls-stream-underlying-stream stream)))
        (error () nil)))  ; Ignore errors during shutdown
    ;; Close underlying stream (ignore errors - peer may have already closed)
    (handler-case
        (close (tls-stream-underlying-stream stream) :abort abort)
      (error () nil))
    ;; Mark as closed
    (setf (tls-stream-closed-p stream) t)
    ;; Call close callback
    (when (tls-stream-close-callback stream)
      (funcall (tls-stream-close-callback stream) stream)))
  t)

;;;; Input Methods

(defun tls-stream-get-key-schedule (stream)
  "Get the key schedule from the stream's handshake."
  (let ((hs (tls-stream-handshake stream)))
    (typecase hs
      (client-handshake (client-handshake-key-schedule hs))
      (server-handshake (server-handshake-key-schedule hs)))))

(defun tls-stream-get-cipher-suite (stream)
  "Get the cipher suite from the stream's handshake."
  (let ((hs (tls-stream-handshake stream)))
    (typecase hs
      (client-handshake (client-handshake-selected-cipher-suite hs))
      (server-handshake (server-handshake-selected-cipher-suite hs)))))

(defun tls-stream-is-client-p (stream)
  "Check if this is a client stream."
  (client-handshake-p (tls-stream-handshake stream)))

(defun tls-stream-process-new-session-ticket (stream nst)
  "Process a NewSessionTicket message, caching it for session resumption."
  (let ((hs (tls-stream-handshake stream)))
    ;; Only process on client side
    (when (client-handshake-p hs)
      (let ((resumption-secret (client-handshake-resumption-master-secret hs))
            (cipher-suite (client-handshake-selected-cipher-suite hs))
            (hostname (client-handshake-hostname hs)))
        (when (and resumption-secret hostname)
          (process-new-session-ticket nst resumption-secret cipher-suite hostname))))))

(defun tls-stream-process-key-update (stream key-update)
  "Process a KeyUpdate message, updating read keys and responding if requested."
  ;; Check for too many key updates (DoS prevention)
  (incf (tls-stream-key-update-count stream))
  (when (> (tls-stream-key-update-count stream) +max-key-updates+)
    (error 'tls-error :message ":TOO_MANY_KEY_UPDATES:"))
  ;; RFC 8446 Section 4.6.3: request_update must be 0 or 1
  ;; "If an implementation receives any other value, it MUST terminate
  ;;  the connection with an illegal_parameter alert."
  (let ((request-update (key-update-request-update key-update)))
    (unless (or (= request-update +key-update-not-requested+)
                (= request-update +key-update-requested+))
      (record-layer-write-alert (tls-stream-record-layer stream)
                                +alert-level-fatal+
                                +alert-illegal-parameter+)
      (error 'tls-error
             :message (format nil "Invalid KeyUpdate request_update value: ~D" request-update))))
  (let* ((ks (tls-stream-get-key-schedule stream))
         (cipher-suite (tls-stream-get-cipher-suite stream))
         (is-client (tls-stream-is-client-p stream)))
    ;; Update the sender's traffic secret (our read keys)
    ;; For client: sender is server, so update server application traffic secret
    ;; For server: sender is client, so update client application traffic secret
    (if is-client
        ;; We're client, received from server - update server app secret
        (setf (key-schedule-server-application-traffic-secret ks)
              (key-schedule-update-traffic-secret
               (key-schedule-server-application-traffic-secret ks)
               cipher-suite))
        ;; We're server, received from client - update client app secret
        (setf (key-schedule-client-application-traffic-secret ks)
              (key-schedule-update-traffic-secret
               (key-schedule-client-application-traffic-secret ks)
               cipher-suite)))
    ;; Install new read keys (from the sender's traffic secret)
    (multiple-value-bind (key iv)
        (if is-client
            (key-schedule-derive-server-traffic-keys ks :application)  ; client reads server
            (key-schedule-derive-client-traffic-keys ks :application)) ; server reads client
      (record-layer-install-keys (tls-stream-record-layer stream)
                                 :read key iv cipher-suite))
    ;; If update was requested, send our own KeyUpdate
    (when (= (key-update-request-update key-update) +key-update-requested+)
      (tls-stream-send-key-update stream :request-update nil))))

(defun tls-stream-send-key-update (stream &key (request-update nil))
  "Send a KeyUpdate message and update our write keys.
   REQUEST-UPDATE if true, asks the peer to also update their keys."
  (let* ((ks (tls-stream-get-key-schedule stream))
         (cipher-suite (tls-stream-get-cipher-suite stream))
         (is-client (tls-stream-is-client-p stream))
         (msg (make-key-update
               :request-update (if request-update
                                   +key-update-requested+
                                   +key-update-not-requested+)))
         (msg-bytes (serialize-key-update msg))
         (handshake-msg (wrap-handshake-message +handshake-key-update+ msg-bytes)))
    ;; Send the KeyUpdate message and flush immediately
    ;; This ensures the peer receives our response before we try to read more
    (record-layer-write (tls-stream-record-layer stream)
                        +content-type-handshake+ handshake-msg)
    (force-output (tls-stream-underlying-stream stream))
    ;; Update our traffic secret (write keys)
    (if is-client
        ;; We're client, updating our write keys
        (setf (key-schedule-client-application-traffic-secret ks)
              (key-schedule-update-traffic-secret
               (key-schedule-client-application-traffic-secret ks)
               cipher-suite))
        ;; We're server, updating our write keys
        (setf (key-schedule-server-application-traffic-secret ks)
              (key-schedule-update-traffic-secret
               (key-schedule-server-application-traffic-secret ks)
               cipher-suite)))
    ;; Install new write keys (from our own traffic secret)
    (multiple-value-bind (key iv)
        (if is-client
            (key-schedule-derive-client-traffic-keys ks :application)  ; client writes with client keys
            (key-schedule-derive-server-traffic-keys ks :application)) ; server writes with server keys
      (record-layer-install-keys (tls-stream-record-layer stream)
                                 :write key iv cipher-suite))))

(defun tls-stream-fill-buffer (stream)
  "Read more data from the record layer into the input buffer."
  (handler-case
      (multiple-value-bind (content-type data)
          (record-layer-read (tls-stream-record-layer stream))
        (case content-type
          (#.+content-type-application-data+
           ;; Check for empty records (DoS prevention)
           (cond ((zerop (length data)) 
                 (incf (tls-stream-empty-record-count stream))
                 (when (> (tls-stream-empty-record-count stream) +max-empty-records+)
                   (error 'tls-error :message ":TOO_MANY_EMPTY_FRAGMENTS:"))
                 ;; Recursively try for more data
                 (tls-stream-fill-buffer stream))
      (t 
                 ;; Reset counters on non-empty application data
                 (setf (tls-stream-warning-alert-count stream) 0)
                 (setf (tls-stream-empty-record-count stream) 0)
                 (setf (tls-stream-input-buffer stream) data)
                 (setf (tls-stream-input-position stream) 0))))
          (#.+content-type-alert+
           ;; process-alert will error on most alerts, but returns nil for
           ;; user_canceled warnings which should be ignored
           (process-alert data (tls-stream-record-layer stream))
           ;; If we get here, the alert was ignored (user_canceled)
           ;; Track consecutive warning alerts to prevent DoS
           (incf (tls-stream-warning-alert-count stream))
           (when (> (tls-stream-warning-alert-count stream) +max-warning-alerts+)
             (record-layer-write-alert (tls-stream-record-layer stream)
                                       +alert-level-fatal+
                                       +alert-unexpected-message+)
             (error 'tls-error :message ":TOO_MANY_WARNING_ALERTS:"))
           ;; Recursively try for more data
           (tls-stream-fill-buffer stream))
          (#.+content-type-handshake+
           ;; Post-handshake messages (e.g., NewSessionTicket, KeyUpdate)
           (when (zerop (length data))
             (record-layer-write-alert (tls-stream-record-layer stream)
                                       +alert-level-fatal+
                                       +alert-decode-error+)
             (error 'tls-decode-error
                    :message ":DECODE_ERROR: Zero-length handshake record"))
           (let ((msg (parse-handshake-message data)))
             (case (handshake-message-type msg)
               (#.+handshake-key-update+
                (tls-stream-process-key-update stream (handshake-message-body msg)))
               (#.+handshake-new-session-ticket+
                ;; NewSessionTicket - cache for session resumption
                (tls-stream-process-new-session-ticket stream (handshake-message-body msg)))
               (otherwise
                ;; Unknown post-handshake message - ignore
                nil)))
           ;; Recursively try to get more data
           (tls-stream-fill-buffer stream))
          (otherwise
           (record-layer-write-alert (tls-stream-record-layer stream)
                                     +alert-level-fatal+
                                     +alert-unexpected-message+)
           (error 'tls-error :message (format nil ":UNEXPECTED_RECORD: Unexpected content type: ~D" content-type)))))
    ;; Handle record overflow - send alert and re-signal
    (tls-record-overflow (e)
      (handler-case
          (record-layer-write-alert (tls-stream-record-layer stream)
                                    +alert-level-fatal+
                                    +alert-record-overflow+)
        (error () nil))  ; Ignore errors during alert send
      (error e))
    ;; Handle MAC verification failure - send alert and re-signal
    (tls-mac-error (e)
      (handler-case
          (record-layer-write-alert (tls-stream-record-layer stream)
                                    +alert-level-fatal+
                                    +alert-bad-record-mac+)
        (error () nil))  ; Ignore errors during alert send
      (error e))))

(defun tls-stream-buffer-remaining (stream)
  "Return the number of bytes remaining in the input buffer."
  (- (length (tls-stream-input-buffer stream))
     (tls-stream-input-position stream)))

(defmethod stream-read-byte ((stream tls-stream))
  ;; Check request context for deadline/cancellation
  (let ((record-layer (tls-stream-record-layer stream)))
    (when record-layer
      (check-tls-context)))
  (when (tls-stream-closed-p stream)
    (return-from stream-read-byte :eof))
  ;; Refill buffer if empty
  (when (zerop (tls-stream-buffer-remaining stream))
    (handler-case
        (tls-stream-fill-buffer stream)
      (tls-connection-closed ()
        (return-from stream-read-byte :eof))))
  ;; Read from buffer
  (if (plusp (tls-stream-buffer-remaining stream))
      (prog1 (aref (tls-stream-input-buffer stream)
                   (tls-stream-input-position stream))
        (incf (tls-stream-input-position stream)))
      :eof))

(defmethod stream-read-sequence ((stream tls-stream) sequence start end &key)
  ;; Check request context for deadline/cancellation
  (let ((record-layer (tls-stream-record-layer stream)))
    (when record-layer
      (check-tls-context)))
  (when (tls-stream-closed-p stream)
    (return-from stream-read-sequence start))
  (let ((pos start)
        (first-read t))  ; Track if this is the first read
    (loop while (< pos end)
          do (progn
               ;; Refill buffer if needed
               (when (zerop (tls-stream-buffer-remaining stream))
                 ;; After first successful read, don't block if no data available
                 ;; This prevents deadlock when peer is waiting for response
                 (when (and (not first-read)
                            (not (listen (tls-stream-underlying-stream stream))))
                   (return-from stream-read-sequence pos))
                 (handler-case
                     (tls-stream-fill-buffer stream)
                   (tls-connection-closed ()
                     (return-from stream-read-sequence pos))))
               ;; Copy from buffer
               (let* ((remaining (tls-stream-buffer-remaining stream))
                      (to-copy (min remaining (- end pos))))
                 (when (zerop to-copy)
                   (return-from stream-read-sequence pos))
                 (replace sequence (tls-stream-input-buffer stream)
                          :start1 pos
                          :end1 (+ pos to-copy)
                          :start2 (tls-stream-input-position stream))
                 (incf pos to-copy)
                 (incf (tls-stream-input-position stream) to-copy)
                 (setf first-read nil))))  ; Mark that we've read some data
    pos))

(defmethod stream-listen ((stream tls-stream))
  (or (plusp (tls-stream-buffer-remaining stream))
      (listen (tls-stream-underlying-stream stream))))

;;;; Output Methods

(defmethod stream-write-byte ((stream tls-stream) byte)
  (when (tls-stream-closed-p stream)
    (error 'tls-error :message "Cannot write to closed stream"))
  (let ((buf (tls-stream-output-buffer stream))
        (pos (tls-stream-output-position stream)))
    (setf (aref buf pos) byte)
    (incf (tls-stream-output-position stream))
    ;; Flush if buffer is full
    (when (= (tls-stream-output-position stream) (length buf))
      (force-output stream)))
  byte)

(defmethod stream-write-sequence ((stream tls-stream) sequence start end &key)
  (when (tls-stream-closed-p stream)
    (error 'tls-error :message "Cannot write to closed stream"))
  (loop while (< start end)
        do (let* ((buf (tls-stream-output-buffer stream))
                  (pos (tls-stream-output-position stream))
                  (space (- (length buf) pos))
                  (to-copy (min space (- end start))))
             (replace buf sequence
                      :start1 pos
                      :start2 start
                      :end2 (+ start to-copy))
             (incf (tls-stream-output-position stream) to-copy)
             (incf start to-copy)
             ;; Flush if buffer is full
             (when (= (tls-stream-output-position stream) (length buf))
               (force-output stream))))
  sequence)

(defmethod stream-force-output ((stream tls-stream))
  (when (plusp (tls-stream-output-position stream))
    (let ((data (subseq (tls-stream-output-buffer stream)
                        0 (tls-stream-output-position stream))))
      (record-layer-write-application-data
       (tls-stream-record-layer stream) data)
      (setf (tls-stream-output-position stream) 0)))
  (force-output (tls-stream-underlying-stream stream)))

(defmethod stream-finish-output ((stream tls-stream))
  (stream-force-output stream))

;;;; Stream Accessors

(defun tls-peer-certificate (stream)
  "Return the peer's certificate, if available.
   Returns an x509-certificate structure (already parsed)."
  (let ((hs (tls-stream-handshake stream)))
    (when hs
      (typecase hs
        (client-handshake (client-handshake-peer-certificate hs))
        (server-handshake (server-handshake-peer-certificate hs))))))

(defun tls-peer-certificate-chain (stream)
  "Return the peer's full certificate chain, if available.
   Returns a list of x509-certificate structures (leaf first)."
  (let ((hs (tls-stream-handshake stream)))
    (when hs
      (typecase hs
        (client-handshake (client-handshake-peer-certificate-chain hs))
        (server-handshake (server-handshake-peer-certificate-chain hs))))))

(defun tls-selected-alpn (stream)
  "Return the negotiated ALPN protocol, if any."
  (let ((hs (tls-stream-handshake stream)))
    (when hs
      (typecase hs
        (client-handshake (client-handshake-selected-alpn hs))
        (server-handshake (server-handshake-selected-alpn hs))))))

(defun tls-cipher-suite (stream)
  "Return the negotiated cipher suite."
  (let ((hs (tls-stream-handshake stream)))
    (when hs
      (typecase hs
        (client-handshake (client-handshake-selected-cipher-suite hs))
        (server-handshake (server-handshake-selected-cipher-suite hs))))))

(defun tls-version (stream)
  "Return the TLS version (always 1.3 for this implementation)."
  (declare (ignore stream))
  +tls-1.3+)

(defun tls-client-hostname (stream)
  "Return the client's SNI hostname (server-side only)."
  (let ((hs (tls-stream-handshake stream)))
    (when (server-handshake-p hs)
      (server-handshake-client-hostname hs))))

(defun tls-request-key-update (stream &key (request-peer-update t))
  "Request a TLS 1.3 key update on STREAM.
   This updates the sending keys immediately and optionally requests
   the peer to also update their keys.

   REQUEST-PEER-UPDATE - If true (default), the peer must respond with
                         their own KeyUpdate message. If false, only our
                         sending keys are updated."
  (tls-stream-send-key-update stream :request-update request-peer-update))

(defun tls-ech-accepted-p (stream)
  "Return T if ECH (Encrypted Client Hello) was used and accepted by the server.
   Returns NIL if ECH was not used, was rejected, or this is a server stream."
  (let ((hs (tls-stream-handshake stream)))
    (when (client-handshake-p hs)
      (client-handshake-ech-accepted hs))))

;;;; Stream Creation

(defun make-tls-client-stream (socket &key
                                        hostname
                                        sni-hostname
                                        (context (ensure-default-context))
                                        (verify (tls-context-verify-mode context))
                                        alpn-protocols
                                        client-certificate
                                        client-key
                                        ech-configs
                                        (ech-enabled t)
                                        close-callback
                                        external-format
                                        (buffer-size *default-buffer-size*)
                                        max-send-fragment
                                        request-context)
  "Create a TLS client stream over SOCKET.

   SOCKET - The underlying TCP stream or socket.
   HOSTNAME - Server hostname for SNI and verification.
   SNI-HOSTNAME - Override hostname for SNI only (no verification).
   CONTEXT - TLS context for configuration.
   VERIFY - Certificate verification mode.
   ALPN-PROTOCOLS - List of ALPN protocol names to offer.
   CLIENT-CERTIFICATE - Certificate for client authentication (mTLS).
   CLIENT-KEY - Private key for client authentication (mTLS).
   ECH-CONFIGS - ECH configurations for Encrypted Client Hello (from DNS or manual).
                 Can be raw bytes (ECHConfigList) or parsed ECH-CONFIG structures.
   ECH-ENABLED - Enable ECH when configs available (default T).
   CLOSE-CALLBACK - Function called when stream is closed.
   EXTERNAL-FORMAT - If non-NIL, wrap in a flexi-stream.
   BUFFER-SIZE - Size of I/O buffers.
   MAX-SEND-FRAGMENT - Maximum plaintext size for outgoing records.
   REQUEST-CONTEXT - Optional cl-cancel context for timeout/cancellation support.

   Returns the TLS stream, or a flexi-stream if EXTERNAL-FORMAT specified."
  (let* ((stream (make-instance 'tls-client-stream
                                :stream socket
                                :close-callback close-callback
                                :buffer-size buffer-size))
         (record-layer (make-record-layer socket
                                          :max-send-fragment (or max-send-fragment
                                                                 +max-record-size+)
                                          :request-context request-context))
         ;; Set up automatic socket closure on context cancellation
         (cancel-monitor (setup-close-on-cancel request-context socket))
         (trust-store (tls-context-trust-store context))
         ;; SNI uses sni-hostname if provided, otherwise hostname
         (sni-name (or sni-hostname hostname))
         ;; Load client certificate chain from file if path provided
         ;; Handle: list of certs, single cert object, file path, or nil
         (loaded-certs (cond
                         ((null client-certificate) nil)
                         ((listp client-certificate) client-certificate)
                         ((x509-certificate-p client-certificate) (list client-certificate))
                         ((stringp client-certificate) (load-certificate-chain client-certificate))
                         ((pathnamep client-certificate) (load-certificate-chain client-certificate))
                         (t nil)))
         ;; Split: first cert is the client cert, rest are chain certs
         (client-cert (when loaded-certs (first loaded-certs)))
         (chain-certs (when loaded-certs (rest loaded-certs)))
         ;; Load client private key from file if path provided
         (private-key (cond
                        ((or (null client-key) (stringp client-key) (pathnamep client-key))
                         (let ((key-source (or client-key
                                               (when (stringp client-certificate) client-certificate)
                                               (when (pathnamep client-certificate) client-certificate))))
                           (when key-source
                             (load-private-key key-source))))
                        (t client-key))))  ; Already an Ironclad key object
    (setf (tls-stream-record-layer stream) record-layer)
    ;; Perform handshake (CertificateVerify is verified during handshake)
    ;; Skip hostname verification if only sni-hostname is provided (no hostname)
    ;; Parse ECH configs if raw bytes provided
    (let ((parsed-ech-configs
            (when ech-configs
              (if (and (typep ech-configs '(simple-array (unsigned-byte 8) (*)))
                       (> (length ech-configs) 2))
                  ;; Raw ECHConfigList bytes - parse them
                  (parse-ech-config-list ech-configs)
                  ;; Already parsed or list of configs
                  (if (listp ech-configs)
                      ech-configs
                      (list ech-configs))))))
      (let ((hs (perform-client-handshake
                  record-layer
                  :hostname sni-name
                  :alpn-protocols (or alpn-protocols
                                      (tls-context-alpn-protocols context))
                  :verify-mode verify
                  :trust-store trust-store
                  :skip-hostname-verify (and sni-hostname (null hostname))
                  :client-certificate client-cert
                  :client-private-key private-key
                  :client-certificate-chain chain-certs
                  :ech-configs parsed-ech-configs
                  :ech-enabled ech-enabled)))
        (setf (tls-stream-handshake stream) hs)
        ;; Verify certificate chain and hostname if verification enabled
        (when (and (member verify (list +verify-peer+ +verify-required+))
                   (client-handshake-peer-certificate hs))
          (let ((cert (client-handshake-peer-certificate hs))
                (chain (client-handshake-peer-certificate-chain hs)))
            ;; Verify hostname - only if hostname (not just sni-hostname) was provided
            (when hostname
              (verify-hostname cert hostname))
            ;; Verify certificate chain for both +verify-peer+ and +verify-required+
            ;; (+verify-peer+ means "verify if presented" - servers always present certs)
            (when chain
              ;; On Windows/macOS with native verification enabled, verify even without trust-store
              ;; (they use their own trusted root stores)
              (let ((trusted-roots (when trust-store
                                     (trust-store-certificates trust-store))))
                (verify-certificate-chain chain trusted-roots
                                          (get-universal-time) hostname)))))))
    ;; Wrap with flexi-stream if external-format specified
    (if external-format
        (flexi-streams:make-flexi-stream stream :external-format external-format)
        stream)))

(defun make-tls-server-stream (socket &key
                                        (context (ensure-default-context))
                                        certificate
                                        key
                                        (verify +verify-none+)
                                        trust-store
                                        alpn-protocols
                                        sni-callback
                                        certificate-provider
                                        close-callback
                                        external-format
                                        (buffer-size *default-buffer-size*)
                                        max-send-fragment
                                        request-context)
  "Create a TLS server stream over SOCKET.

   SOCKET - The underlying TCP stream or socket.
   CONTEXT - TLS context for configuration.
   CERTIFICATE - Certificate chain (list of x509-certificate) or path to PEM file.
   KEY - Private key (Ironclad key object) or path to PEM file.
   VERIFY - Client certificate verification mode (+verify-none+, +verify-peer+, +verify-required+).
   TRUST-STORE - Trust store for verifying client certificates.
   ALPN-PROTOCOLS - List of ALPN protocol names the server supports.
   SNI-CALLBACK - Function called with the client's requested hostname.
                  Should return (VALUES certificate-chain private-key) for that host,
                  or NIL to use the default certificate/key.
   CERTIFICATE-PROVIDER - Function called with (hostname alpn-list) before cert selection.
                          Should return (VALUES cert-chain key selected-alpn) to override
                          certificate and ALPN, or NIL to use defaults. For ACME TLS-ALPN-01.
   CLOSE-CALLBACK - Function called when stream is closed.
   EXTERNAL-FORMAT - If non-NIL, wrap in a flexi-stream.
   BUFFER-SIZE - Size of I/O buffers.
   MAX-SEND-FRAGMENT - Maximum plaintext size for outgoing records.
   REQUEST-CONTEXT - Optional cl-cancel context for timeout/cancellation support.

   Returns the TLS stream, or a flexi-stream if EXTERNAL-FORMAT specified."
  (let* ((stream (make-instance 'tls-server-stream
                                :stream socket
                                :close-callback close-callback
                                :buffer-size buffer-size))
         (record-layer (make-record-layer socket
                                          :max-send-fragment (or max-send-fragment
                                                                 +max-record-size+)
                                          :request-context request-context))
         ;; Set up automatic socket closure on context cancellation
         (cancel-monitor (setup-close-on-cancel request-context socket))
         ;; Get certificate chain (from parameter, context, or file)
         (cert-chain (cond
                       ((listp certificate) certificate)
                       ((stringp certificate) (load-certificate-chain certificate))
                       ((pathnamep certificate) (load-certificate-chain certificate))
                       (t (tls-context-certificate-chain context))))
         ;; Get private key (from parameter, context, or file)
         (private-key (cond
                        ((or (null key) (stringp key) (pathnamep key))
                         (let ((key-source (or key
                                               (when (stringp certificate) certificate)
                                               (when (pathnamep certificate) certificate))))
                           (if key-source
                               (load-private-key key-source)
                               (tls-context-private-key context))))
                        (t key)))  ; Already an Ironclad key object
         ;; Get trust store for client certificate verification
         ;; Use explicit trust-store parameter if provided
         ;; Do NOT fall back to context trust store - server-side client cert verification
         ;; should only use what's explicitly provided. This allows +verify-required+ to
         ;; require a certificate without verifying its chain (e.g., for testing).
         (client-trust-store trust-store)
         ;; Get ALPN protocols
         (alpn (or alpn-protocols (tls-context-alpn-protocols context))))
    ;; Validate we have certificate and key
    (unless cert-chain
      (error 'tls-error :message "Server requires a certificate chain"))
    (unless private-key
      (error 'tls-error :message "Server requires a private key"))
    (setf (tls-stream-record-layer stream) record-layer)
    ;; Perform server handshake
    (let ((hs (perform-server-handshake
               record-layer
               cert-chain
               private-key
               :alpn-protocols alpn
               :verify-mode verify
               :trust-store client-trust-store
               :sni-callback sni-callback
               :certificate-provider certificate-provider)))
      (setf (tls-stream-handshake stream) hs))
    ;; Wrap with flexi-stream if external-format specified
    (if external-format
        (flexi-streams:make-flexi-stream stream :external-format external-format)
        stream)))

;;;; Convenience Macros

(defmacro with-tls-client-stream ((var socket &rest args) &body body)
  "Execute BODY with VAR bound to a TLS client stream over SOCKET.
   The stream is automatically closed when BODY exits (normally or abnormally).

   ARGS are passed to MAKE-TLS-CLIENT-STREAM (e.g., :hostname, :verify).

   Example:
     (with-tls-client-stream (tls socket :hostname \"example.com\")
       (write-sequence request tls)
       (read-response tls))"
  `(let ((,var (make-tls-client-stream ,socket ,@args)))
     (unwind-protect
         (unquote-splicing body)
       (close ,var))))

(defmacro with-tls-server-stream ((var socket &rest args) &body body)
  "Execute BODY with VAR bound to a TLS server stream over SOCKET.
   The stream is automatically closed when BODY exits (normally or abnormally).

   ARGS are passed to MAKE-TLS-SERVER-STREAM (e.g., :certificate, :key).

   Example:
     (with-tls-server-stream (tls client-socket :certificate cert :key key)
       (handle-request tls))"
  `(let ((,var (make-tls-server-stream ,socket ,@args)))
     (unwind-protect
         (unquote-splicing body)
       (close ,var))))
