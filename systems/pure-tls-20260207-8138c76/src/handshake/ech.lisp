;;; ech.lisp --- Encrypted Client Hello (ECH) implementation
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements TLS Encrypted Client Hello per RFC 9639.
;;; ECH encrypts the ClientHello including SNI to protect privacy
;;; from network observers.

(in-package #:pure-tls)

;;;; ECH Config Structures (RFC 9639 Section 4)

(defstruct ech-hpke-cipher-suite
  "HPKE cipher suite for ECH."
  (kdf-id 0 :type (unsigned-byte 16))
  (aead-id 0 :type (unsigned-byte 16)))

(defstruct ech-config-contents
  "ECHConfigContents structure (RFC 9639 Section 4)."
  (key-config nil)    ; HpkeKeyConfig
  (maximum-name-length 0 :type (unsigned-byte 8))
  (public-name "" :type string)
  (extensions #() :type octet-vector))

(defstruct ech-hpke-key-config
  "HpkeKeyConfig structure."
  (config-id 0 :type (unsigned-byte 8))
  (kem-id 0 :type (unsigned-byte 16))
  (public-key #() :type octet-vector)
  (cipher-suites nil :type list))  ; list of ech-hpke-cipher-suite

(defstruct ech-config
  "ECHConfig structure (RFC 9639 Section 4).
   version: Should be 0xfe0d for the current spec
   contents: ECHConfigContents structure"
  (version 0 :type (unsigned-byte 16))
  (contents nil))

;;;; ECH Config Parsing

(defun parse-ech-hpke-cipher-suites (data offset count)
  "Parse HPKE cipher suites from ECHConfig.
   Returns (values cipher-suite-list new-offset)."
  (let ((suites nil))
    (dotimes (i count)
      (let ((kdf-id (logior (ash (aref data offset) 8)
                            (aref data (1+ offset))))
            (aead-id (logior (ash (aref data (+ offset 2)) 8)
                             (aref data (+ offset 3)))))
        (push (make-ech-hpke-cipher-suite :kdf-id kdf-id :aead-id aead-id) suites)
        (incf offset 4)))
    (values (nreverse suites) offset)))

(defun parse-ech-config-contents (data offset length)
  "Parse ECHConfigContents from data.
   Returns (values ech-config-contents new-offset)."
  (let* ((end (+ offset length))
         ;; HpkeKeyConfig
         (config-id (aref data offset))
         (kem-id (logior (ash (aref data (1+ offset)) 8)
                         (aref data (+ offset 2))))
         ;; public_key<1..2^16-1>
         (pk-len (logior (ash (aref data (+ offset 3)) 8)
                         (aref data (+ offset 4))))
         (pk-start (+ offset 5))
         (public-key (subseq data pk-start (+ pk-start pk-len)))
         (pos (+ pk-start pk-len))
         ;; cipher_suites<4..2^16-2>
         (cs-len (logior (ash (aref data pos) 8)
                         (aref data (1+ pos)))))
    (incf pos 2)
    (unless (zerop (mod cs-len 4))
      (error 'tls-handshake-error
             :message "ECH config: invalid cipher suite list length"))
    (multiple-value-bind (cipher-suites new-pos)
        (parse-ech-hpke-cipher-suites data pos (/ cs-len 4))
      (setf pos new-pos)
      ;; maximum_name_length
      (let ((max-name-len (aref data pos)))
        (incf pos)
        ;; public_name<1..255>
        (let* ((pn-len (aref data pos))
               (public-name (octets-to-string (subseq data (1+ pos) (+ pos 1 pn-len)))))
          (incf pos (1+ pn-len))
          ;; extensions<0..2^16-1>
          (let* ((ext-len (logior (ash (aref data pos) 8)
                                  (aref data (1+ pos))))
                 (extensions (subseq data (+ pos 2) (+ pos 2 ext-len))))
            (incf pos (+ 2 ext-len))
            (unless (<= pos end)
              (error 'tls-handshake-error
                     :message "ECH config: data extends beyond length"))
            (values
             (make-ech-config-contents
              :key-config (make-ech-hpke-key-config
                           :config-id config-id
                           :kem-id kem-id
                           :public-key public-key
                           :cipher-suites cipher-suites)
              :maximum-name-length max-name-len
              :public-name public-name
              :extensions extensions)
             pos)))))))

(defun parse-ech-config (data offset)
  "Parse a single ECHConfig from data.
   Returns (values ech-config new-offset) or NIL if version is unknown."
  (let* ((version (logior (ash (aref data offset) 8)
                          (aref data (1+ offset))))
         ;; length of contents
         (length (logior (ash (aref data (+ offset 2)) 8)
                         (aref data (+ offset 3))))
         (contents-start (+ offset 4)))
    (if (= version +ech-version+)
        ;; Known version - parse contents
        (multiple-value-bind (contents new-pos)
            (parse-ech-config-contents data contents-start length)
          (declare (ignore new-pos))
          (values (make-ech-config :version version :contents contents)
                  (+ contents-start length)))
        ;; Unknown version - skip
        (values nil (+ contents-start length)))))

(defun parse-ech-config-list (data)
  "Parse ECHConfigList from raw bytes (e.g., from DNS HTTPS record).
   DATA should be the raw ECHConfigList bytes.
   Returns a list of ech-config structures."
  (when (< (length data) 2)
    (error 'tls-handshake-error
           :message "ECH config list too short"))
  (let* ((total-len (logior (ash (aref data 0) 8)
                            (aref data 1)))
         (offset 2)
         (end (+ 2 total-len))
         (configs nil))
    (unless (<= end (length data))
      (error 'tls-handshake-error
             :message "ECH config list length exceeds data"))
    (loop while (< offset end) do
      (multiple-value-bind (config new-offset)
          (parse-ech-config data offset)
        (when config
          (push config configs))
        (setf offset new-offset)))
    (nreverse configs)))

;;;; ECH Config Serialization

(defun serialize-ech-hpke-cipher-suites (cipher-suites)
  "Serialize HPKE cipher suites to bytes."
  (let ((result (make-octet-vector (* 4 (length cipher-suites)))))
    (loop for suite in cipher-suites
          for i from 0 by 4 do
      (let ((kdf-id (ech-hpke-cipher-suite-kdf-id suite))
            (aead-id (ech-hpke-cipher-suite-aead-id suite)))
        (setf (aref result i) (ldb (byte 8 8) kdf-id))
        (setf (aref result (1+ i)) (ldb (byte 8 0) kdf-id))
        (setf (aref result (+ i 2)) (ldb (byte 8 8) aead-id))
        (setf (aref result (+ i 3)) (ldb (byte 8 0) aead-id))))
    result))

(defun serialize-ech-config-contents (contents)
  "Serialize ECHConfigContents to bytes."
  (let* ((key-config (ech-config-contents-key-config contents))
         (public-key (ech-hpke-key-config-public-key key-config))
         (cipher-suites-bytes (serialize-ech-hpke-cipher-suites
                               (ech-hpke-key-config-cipher-suites key-config)))
         (public-name-bytes (string-to-octets (ech-config-contents-public-name contents)))
         (extensions (ech-config-contents-extensions contents)))
    (concat-octet-vectors
     ;; config_id (1 byte)
     (octet-vector (ech-hpke-key-config-config-id key-config))
     ;; kem_id (2 bytes)
     (octet-vector (ldb (byte 8 8) (ech-hpke-key-config-kem-id key-config))
                   (ldb (byte 8 0) (ech-hpke-key-config-kem-id key-config)))
     ;; public_key<1..2^16-1>
     (octet-vector (ldb (byte 8 8) (length public-key))
                   (ldb (byte 8 0) (length public-key)))
     public-key
     ;; cipher_suites<4..2^16-2>
     (octet-vector (ldb (byte 8 8) (length cipher-suites-bytes))
                   (ldb (byte 8 0) (length cipher-suites-bytes)))
     cipher-suites-bytes
     ;; maximum_name_length (1 byte)
     (octet-vector (ech-config-contents-maximum-name-length contents))
     ;; public_name<1..255>
     (octet-vector (length public-name-bytes))
     public-name-bytes
     ;; extensions<0..2^16-1>
     (octet-vector (ldb (byte 8 8) (length extensions))
                   (ldb (byte 8 0) (length extensions)))
     extensions)))

(defun serialize-ech-config (config)
  "Serialize a single ECHConfig to bytes."
  (let* ((contents-bytes (serialize-ech-config-contents (ech-config-contents config)))
         (version (ech-config-version config)))
    (concat-octet-vectors
     ;; version (2 bytes)
     (octet-vector (ldb (byte 8 8) version)
                   (ldb (byte 8 0) version))
     ;; length (2 bytes)
     (octet-vector (ldb (byte 8 8) (length contents-bytes))
                   (ldb (byte 8 0) (length contents-bytes)))
     ;; contents
     contents-bytes)))

(defun serialize-ech-config-list (configs)
  "Serialize a list of ECHConfig structures to ECHConfigList bytes."
  (let* ((config-bytes (apply #'concat-octet-vectors
                              (mapcar #'serialize-ech-config configs)))
         (total-len (length config-bytes)))
    (concat-octet-vectors
     (octet-vector (ldb (byte 8 8) total-len)
                   (ldb (byte 8 0) total-len))
     config-bytes)))

;;;; ECH Config Selection

(defun ech-cipher-suite-supported-p (cipher-suite)
  "Check if we support this HPKE cipher suite."
  (and (= (ech-hpke-cipher-suite-kdf-id cipher-suite) +hpke-kdf-hkdf-sha256+)
       (member (ech-hpke-cipher-suite-aead-id cipher-suite)
               (list +hpke-aead-aes-128-gcm+ +hpke-aead-chacha20-poly1305+))))

(defun select-ech-config (configs)
  "Select the best ECH config from a list.
   Returns (values config cipher-suite) or NIL if none supported."
  (dolist (config configs)
    (let ((contents (ech-config-contents config)))
      (when contents
        (let ((key-config (ech-config-contents-key-config contents)))
          ;; Check KEM support (we only support X25519)
          (when (= (ech-hpke-key-config-kem-id key-config) +hpke-kem-x25519-sha256+)
            ;; Find first supported cipher suite
            (dolist (cs (ech-hpke-key-config-cipher-suites key-config))
              (when (ech-cipher-suite-supported-p cs)
                (return-from select-ech-config
                  (values config cs)))))))))
  nil)

;;;; ECH ClientHello Extension Structures

(defstruct ech-client-hello-outer
  "ECH extension for ClientHelloOuter."
  (cipher-suite nil :type (or null ech-hpke-cipher-suite))
  (config-id 0 :type (unsigned-byte 8))
  (enc #() :type octet-vector)
  (payload #() :type octet-vector))

(defstruct ech-client-hello-inner
  "ECH extension for ClientHelloInner (empty).")

;;;; Inner ClientHello Construction

(defun compute-ech-padding (inner-client-hello maximum-name-length server-name-len)
  "Compute padding needed for ClientHelloInner.
   Returns the number of padding bytes needed.

   Per RFC 9639 Section 6.1.2:
   1. First pad based on maximum_name_length to hide server name length:
      - If SNI present: add max(0, L - D) where L = max_name_len, D = SNI length
      - If no SNI: add L + 9 bytes
   2. Then round up to a multiple of 32 bytes:
      - N = 31 - ((len - 1) mod 32)"
  (let* ((current-len (length inner-client-hello))
         ;; Step 1: Server name padding
         (sni-padding (if (zerop server-name-len)
                          ;; No SNI: add L + 9 bytes
                          (+ maximum-name-length 9)
                          ;; Has SNI: add max(0, L - D) bytes
                          (max 0 (- maximum-name-length server-name-len))))
         (len-after-sni-padding (+ current-len sni-padding))
         ;; Step 2: Round up to multiple of 32 bytes
         ;; N = 31 - ((len - 1) mod 32)
         (round-padding (- 31 (mod (1- len-after-sni-padding) 32)))
         ;; Total padding needed
         (total-padding (+ sni-padding round-padding)))
    total-padding))

;;;; ECH AAD Computation

(defun compute-ech-aad (client-hello-outer ech-extension-offset ech-payload-offset)
  "Compute AAD for HPKE encryption of ClientHelloInner.
   AAD is ClientHelloOuter with the ECH payload replaced by zeros.
   CLIENT-HELLO-OUTER is the serialized outer ClientHello.
   ECH-EXTENSION-OFFSET is the start of the ECH extension value.
   ECH-PAYLOAD-OFFSET is the start of the payload within the extension."
  (let* ((aad (copy-seq client-hello-outer))
         (payload-len-offset (- ech-payload-offset 2))
         (payload-len (logior (ash (aref aad payload-len-offset) 8)
                              (aref aad (1+ payload-len-offset)))))
    ;; Zero out the payload
    (fill aad 0 :start ech-payload-offset :end (+ ech-payload-offset payload-len))
    aad))

;;;; GREASE ECH

(defun generate-grease-ech-extension ()
  "Generate a GREASE ECH extension for use when ECH is not available.
   This helps prevent ossification by sending fake ECH data."
  (let* ((config-id (random 256))
         ;; Random 32-byte enc (like X25519 public key)
         (enc (random-bytes 32))
         ;; Random payload (typical encrypted inner CH size)
         (payload (random-bytes (+ 128 (random 128)))))
    (make-ech-client-hello-outer
     :cipher-suite (make-ech-hpke-cipher-suite
                    :kdf-id +hpke-kdf-hkdf-sha256+
                    :aead-id +hpke-aead-aes-128-gcm+)
     :config-id config-id
     :enc enc
     :payload payload)))

;;;; ECH Accept Confirmation

(defun compute-ech-accept-confirmation (inner-random transcript-hash)
  "Compute the ECH accept confirmation value.
   This is placed in ServerHello.random[24..31] to signal ECH acceptance.
   Returns 8 bytes.

   Per RFC 9639 Section 7.2:
   accept_confirmation = HKDF-Expand-Label(
       HKDF-Extract(0, ClientHelloInner.random),
       \"ech accept confirmation\",
       transcript_ech_conf,
       8)

   Where:
   - 0 is a string of Hash.length zero bytes (salt for HKDF-Extract)
   - ClientHelloInner.random is the 32-byte random from the inner ClientHello
   - transcript_ech_conf is the transcript hash up through modified ServerHello"
  (let* ((hash-len 32)  ; SHA-256 output length
         ;; Step 1: HKDF-Extract(salt=zeros, ikm=inner_random)
         (zero-salt (make-octet-vector hash-len))
         (prk (hkdf-extract zero-salt inner-random)))
    ;; Step 2: HKDF-Expand-Label with label "ech accept confirmation"
    ;; Note: hkdf-expand-label prepends "tls13 " to make "tls13 ech accept confirmation"
    (hkdf-expand-label prk "ech accept confirmation" transcript-hash +ech-accept-confirmation-length+)))

;;;; EncodedClientHelloInner (RFC 9639 Section 5.1)
;;;
;;; EncodedClientHelloInner is similar to ClientHello but:
;;; - legacy_session_id is replaced with a zero-length field
;;; - Extensions may use "outer_extensions" to reference outer CH extensions

(defun encode-client-hello-inner (client-hello maximum-name-length)
  "Encode a ClientHello as EncodedClientHelloInner.
   This zeroes the legacy_session_id and pads to hide the server name length.
   Returns the encoded bytes."
  (let ((buf (make-tls-write-buffer)))
    ;; ProtocolVersion legacy_version = 0x0303
    (write-buffer-append-uint16 buf (client-hello-legacy-version client-hello))
    ;; Random random (32 bytes)
    (write-buffer-append buf (client-hello-random client-hello))
    ;; opaque legacy_session_id<0..32> = empty for inner
    (write-buffer-append-vector8 buf (make-octet-vector 0))
    ;; CipherSuite cipher_suites<2..2^16-2>
    (let ((suites-buf (make-tls-write-buffer)))
      (dolist (suite (client-hello-cipher-suites client-hello))
        (write-buffer-append-uint16 suites-buf suite))
      (write-buffer-append-vector16 buf (write-buffer-contents suites-buf)))
    ;; opaque legacy_compression_methods<1..2^8-1>
    (write-buffer-append-vector8 buf (or (client-hello-legacy-compression-methods client-hello)
                                          (octet-vector 0)))
    ;; Extension extensions<8..2^16-1>
    (let ((ext-buf (make-tls-write-buffer)))
      (dolist (ext (client-hello-extensions client-hello))
        (write-buffer-append ext-buf (serialize-extension ext)))
      (write-buffer-append-vector16 buf (write-buffer-contents ext-buf)))
    ;; Compute padding needed
    (let* ((encoded (write-buffer-contents buf))
           (server-name-len (let ((sni-ext (find-extension (client-hello-extensions client-hello)
                                                            +extension-server-name+)))
                              (if (and sni-ext (server-name-ext-host-name (tls-extension-data sni-ext)))
                                  (length (server-name-ext-host-name (tls-extension-data sni-ext)))
                                  0)))
           (padding-needed (compute-ech-padding encoded maximum-name-length server-name-len)))
      (if (zerop padding-needed)
          encoded
          (concat-octet-vectors encoded (make-octet-vector padding-needed))))))

;;;; ECH Encryption

(defun encrypt-client-hello-inner (encoded-inner config cipher-suite &optional aad)
  "Encrypt EncodedClientHelloInner using HPKE.
   CONFIG is the ECHConfig to use.
   CIPHER-SUITE is the selected HPKE cipher suite.
   AAD is the ClientHelloOuterAAD (outer CH with payload zeroed). If nil, empty AAD is used.
   Returns (values enc ciphertext hpke-context)."
  (let* ((contents (ech-config-contents config))
         (key-config (ech-config-contents-key-config contents))
         (pk-r (ech-hpke-key-config-public-key key-config))
         ;; info = "tls ech" || 0x00 || ECHConfig
         (info (concat-octet-vectors
                (string-to-octets "tls ech")
                (octet-vector 0)
                (serialize-ech-config config))))
    (multiple-value-bind (ctx enc)
        (hpke-setup-base-s pk-r info
                           :kem-id (ech-hpke-key-config-kem-id key-config)
                           :kdf-id (ech-hpke-cipher-suite-kdf-id cipher-suite)
                           :aead-id (ech-hpke-cipher-suite-aead-id cipher-suite))
      ;; AAD is ClientHelloOuterAAD per RFC 9639 Section 5.1
      (let ((ciphertext (hpke-context-seal ctx (or aad (make-octet-vector 0)) encoded-inner)))
        (values enc ciphertext ctx)))))

;;;; ECH Extension Building for ClientHelloOuter

(defun build-ech-outer-extension (config cipher-suite enc payload)
  "Build the ECH extension for ClientHelloOuter."
  (make-tls-extension
   :type +extension-ech+
   :data (make-ech-ext
          :type :outer
          :config-id (ech-hpke-key-config-config-id
                      (ech-config-contents-key-config (ech-config-contents config)))
          :cipher-suite cipher-suite
          :enc enc
          :payload payload)))

(defun build-ech-inner-extension ()
  "Build the ECH extension marker for ClientHelloInner."
  (make-tls-extension
   :type +extension-ech+
   :data (make-ech-ext :type :inner)))

;;;; ClientHelloInner Generation Helper

(defun add-ech-inner-marker (client-hello)
  "Add the ECH inner marker extension to a ClientHello.
   Returns a new ClientHello with the extension added."
  (make-client-hello
   :legacy-version (client-hello-legacy-version client-hello)
   :random (client-hello-random client-hello)
   :legacy-session-id (client-hello-legacy-session-id client-hello)
   :cipher-suites (client-hello-cipher-suites client-hello)
   :legacy-compression-methods (client-hello-legacy-compression-methods client-hello)
   :extensions (cons (build-ech-inner-extension)
                     (client-hello-extensions client-hello))))

;;;; Outer ClientHello SNI Replacement

(defun replace-sni-for-outer (extensions public-name)
  "Replace the SNI in extensions with the public_name from ECHConfig.
   Returns a new list of extensions."
  (let ((new-exts nil))
    (dolist (ext extensions)
      (if (= (tls-extension-type ext) +extension-server-name+)
          ;; Replace with public_name
          (push (make-tls-extension
                 :type +extension-server-name+
                 :data (make-server-name-ext :host-name public-name))
                new-exts)
          (push ext new-exts)))
    ;; If no SNI was present, add one
    (unless (find +extension-server-name+ new-exts :key #'tls-extension-type)
      (push (make-tls-extension
             :type +extension-server-name+
             :data (make-server-name-ext :host-name public-name))
            new-exts))
    (nreverse new-exts)))
