;;; hpke.lisp --- HPKE (Hybrid Public Key Encryption) implementation for ECH
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements RFC 9180 HPKE Base mode using:
;;; - DHKEM(X25519, HKDF-SHA256)
;;; - HKDF-SHA256
;;; - AES-128-GCM
;;;
;;; This implementation is specifically for TLS ECH support.

(in-package #:pure-tls)

;;;; HPKE Constants

(defconstant +hpke-x25519-nsecret+ 32
  "X25519 shared secret length in bytes")
(defconstant +hpke-x25519-nenc+ 32
  "X25519 encapsulated key length in bytes")
(defconstant +hpke-x25519-npk+ 32
  "X25519 public key length in bytes")
(defconstant +hpke-x25519-nsk+ 32
  "X25519 private key length in bytes")

(defconstant +hpke-sha256-nh+ 32
  "SHA-256 output length in bytes")

(defconstant +hpke-aes-128-gcm-nk+ 16
  "AES-128-GCM key length in bytes")
(defconstant +hpke-aes-128-gcm-nn+ 12
  "AES-128-GCM nonce length in bytes")
(defconstant +hpke-aes-128-gcm-nt+ 16
  "AES-128-GCM tag length in bytes")

;;;; Suite IDs for HPKE labeled functions

(defun hpke-kem-suite-id (kem-id)
  "Build KEM suite_id: 'KEM' || I2OSP(kem_id, 2)"
  (concat-octet-vectors
   (string-to-octets "KEM")
   (octet-vector (ldb (byte 8 8) kem-id)
                 (ldb (byte 8 0) kem-id))))

(defun hpke-suite-id (kem-id kdf-id aead-id)
  "Build HPKE suite_id: 'HPKE' || I2OSP(kem_id, 2) || I2OSP(kdf_id, 2) || I2OSP(aead_id, 2)"
  (concat-octet-vectors
   (string-to-octets "HPKE")
   (octet-vector (ldb (byte 8 8) kem-id)
                 (ldb (byte 8 0) kem-id))
   (octet-vector (ldb (byte 8 8) kdf-id)
                 (ldb (byte 8 0) kdf-id))
   (octet-vector (ldb (byte 8 8) aead-id)
                 (ldb (byte 8 0) aead-id))))

;;;; Labeled Extract and Expand (RFC 9180 Section 4)

(defun hpke-labeled-extract (salt label ikm suite-id &key (digest :sha256))
  "HPKE LabeledExtract function.
   labeled_ikm = concat('HPKE-v1', suite_id, label, ikm)
   return HKDF-Extract(salt, labeled_ikm)"
  (let ((labeled-ikm (concat-octet-vectors
                      (string-to-octets "HPKE-v1")
                      suite-id
                      (string-to-octets label)
                      ikm)))
    (hkdf-extract salt labeled-ikm :digest digest)))

(defun hpke-labeled-expand (prk label info len suite-id &key (digest :sha256))
  "HPKE LabeledExpand function.
   labeled_info = concat(I2OSP(L, 2), 'HPKE-v1', suite_id, label, info)
   return HKDF-Expand(prk, labeled_info, L)"
  (let ((labeled-info (concat-octet-vectors
                       (octet-vector (ldb (byte 8 8) len)
                                     (ldb (byte 8 0) len))
                       (string-to-octets "HPKE-v1")
                       suite-id
                       (string-to-octets label)
                       info)))
    (hkdf-expand prk labeled-info len :digest digest)))

;;;; DHKEM(X25519, HKDF-SHA256) - RFC 9180 Section 4.1

(defun hpke-extract-and-expand (dh kem-context suite-id)
  "ExtractAndExpand for DHKEM.
   eae_prk = LabeledExtract('', 'eae_prk', dh)
   shared_secret = LabeledExpand(eae_prk, 'shared_secret', kem_context, Nsecret)"
  (let ((eae-prk (hpke-labeled-extract #() "eae_prk" dh suite-id)))
    (hpke-labeled-expand eae-prk "shared_secret" kem-context
                         +hpke-x25519-nsecret+ suite-id)))

(defun hpke-x25519-encap (pk-r)
  "DHKEM(X25519) Encap - encapsulate to recipient's public key.
   pk-r should be 32 bytes (X25519 public key).
   Returns (values shared-secret enc) where enc is the ephemeral public key."
  (let ((suite-id (hpke-kem-suite-id +hpke-kem-x25519-sha256+)))
    ;; Generate ephemeral key pair
    (multiple-value-bind (sk-e pk-e-key)
        (ironclad:generate-key-pair :curve25519)
      (let* ((pk-e (ironclad:curve25519-key-y pk-e-key))
             ;; Compute DH shared secret
             (pk-r-key (ironclad:make-public-key :curve25519 :y pk-r))
             (dh (ironclad:diffie-hellman sk-e pk-r-key))
             ;; kem_context = enc || pkR
             (kem-context (concat-octet-vectors pk-e pk-r))
             ;; Extract shared secret
             (shared-secret (hpke-extract-and-expand dh kem-context suite-id)))
        (values shared-secret pk-e)))))

(defun hpke-x25519-decap (enc sk-r)
  "DHKEM(X25519) Decap - decapsulate using recipient's private key.
   enc is the sender's ephemeral public key (32 bytes).
   sk-r is the recipient's private key (Ironclad private key object).
   Returns the shared secret."
  (let ((suite-id (hpke-kem-suite-id +hpke-kem-x25519-sha256+)))
    ;; Get recipient's public key from private key
    ;; Ironclad stores the public key Y in both private and public key objects
    (let* ((pk-r-bytes (ironclad:curve25519-key-y sk-r))
           ;; Compute DH shared secret
           (pk-e (ironclad:make-public-key :curve25519 :y enc))
           (dh (ironclad:diffie-hellman sk-r pk-e))
           ;; kem_context = enc || pkR
           (kem-context (concat-octet-vectors enc pk-r-bytes))
           ;; Extract shared secret
           (shared-secret (hpke-extract-and-expand dh kem-context suite-id)))
      shared-secret)))

;;;; HPKE Key Schedule (RFC 9180 Section 5.1)

(defstruct hpke-context
  "HPKE encryption context."
  (key nil :type (or null octet-vector))
  (base-nonce nil :type (or null octet-vector))
  (exporter-secret nil :type (or null octet-vector))
  (seq 0 :type (unsigned-byte 64))
  (kem-id 0 :type fixnum)
  (kdf-id 0 :type fixnum)
  (aead-id 0 :type fixnum))

(defun hpke-key-schedule (shared-secret info kem-id kdf-id aead-id mode
                          &key (psk #()) (psk-id #()))
  "HPKE KeySchedule function.
   Returns an hpke-context structure.
   MODE should be +hpke-mode-base+ for base mode."
  (declare (ignore psk psk-id))  ; Only base mode for now
  (let* ((suite-id (hpke-suite-id kem-id kdf-id aead-id))
         ;; Nk, Nn depend on AEAD
         (nk (case aead-id
               (#.+hpke-aead-aes-128-gcm+ +hpke-aes-128-gcm-nk+)
               (#.+hpke-aead-aes-256-gcm+ 32)
               (#.+hpke-aead-chacha20-poly1305+ 32)
               (otherwise (error 'tls-crypto-error
                                 :operation "HPKE key schedule"
                                 :message (format nil "Unsupported AEAD: ~X" aead-id)))))
         (nn (case aead-id
               (#.+hpke-aead-aes-128-gcm+ +hpke-aes-128-gcm-nn+)
               (#.+hpke-aead-aes-256-gcm+ 12)
               (#.+hpke-aead-chacha20-poly1305+ 12)
               (otherwise 12)))
         (nh +hpke-sha256-nh+)
         ;; For base mode: psk_id_hash and info_hash use empty psk_id
         (psk-id-hash (hpke-labeled-extract #() "psk_id_hash" #() suite-id))
         (info-hash (hpke-labeled-extract #() "info_hash" info suite-id))
         ;; ks_context = mode || psk_id_hash || info_hash
         (ks-context (concat-octet-vectors
                      (octet-vector mode)
                      psk-id-hash
                      info-hash))
         ;; secret = LabeledExtract(shared_secret, "secret", psk)
         ;; For base mode, psk is empty
         (secret (hpke-labeled-extract shared-secret "secret" #() suite-id))
         ;; Derive key, base_nonce, exporter_secret
         (key (hpke-labeled-expand secret "key" ks-context nk suite-id))
         (base-nonce (hpke-labeled-expand secret "base_nonce" ks-context nn suite-id))
         (exporter-secret (hpke-labeled-expand secret "exp" ks-context nh suite-id)))
    (make-hpke-context :key key
                       :base-nonce base-nonce
                       :exporter-secret exporter-secret
                       :seq 0
                       :kem-id kem-id
                       :kdf-id kdf-id
                       :aead-id aead-id)))

;;;; HPKE Sender/Receiver Setup (RFC 9180 Section 5.1)

(defun hpke-setup-base-s (pk-r info &key
                                      (kem-id +hpke-kem-x25519-sha256+)
                                      (kdf-id +hpke-kdf-hkdf-sha256+)
                                      (aead-id +hpke-aead-aes-128-gcm+))
  "SetupBaseS - Sender setup for HPKE base mode.
   pk-r is the recipient's public key (32 bytes for X25519).
   info is the context info (can be empty).
   Returns (values context enc) where enc is the encapsulated key."
  (unless (= kem-id +hpke-kem-x25519-sha256+)
    (error 'tls-crypto-error
           :operation "HPKE setup"
           :message (format nil "Unsupported KEM: ~X" kem-id)))
  (multiple-value-bind (shared-secret enc)
      (hpke-x25519-encap pk-r)
    (let ((context (hpke-key-schedule shared-secret info
                                      kem-id kdf-id aead-id
                                      +hpke-mode-base+)))
      (values context enc))))

(defun hpke-setup-base-r (enc sk-r info &key
                                          (kem-id +hpke-kem-x25519-sha256+)
                                          (kdf-id +hpke-kdf-hkdf-sha256+)
                                          (aead-id +hpke-aead-aes-128-gcm+))
  "SetupBaseR - Receiver setup for HPKE base mode.
   enc is the encapsulated key from sender (32 bytes for X25519).
   sk-r is the recipient's private key (Ironclad private key object).
   info is the context info (must match sender's).
   Returns the HPKE context."
  (unless (= kem-id +hpke-kem-x25519-sha256+)
    (error 'tls-crypto-error
           :operation "HPKE setup"
           :message (format nil "Unsupported KEM: ~X" kem-id)))
  (let ((shared-secret (hpke-x25519-decap enc sk-r)))
    (hpke-key-schedule shared-secret info
                       kem-id kdf-id aead-id
                       +hpke-mode-base+)))

;;;; HPKE Encryption/Decryption (RFC 9180 Section 5.2)

(defun hpke-compute-nonce (ctx)
  "Compute the nonce for the current sequence number.
   nonce = base_nonce XOR I2OSP(seq, Nn)"
  (let* ((base-nonce (hpke-context-base-nonce ctx))
         (seq (hpke-context-seq ctx))
         (nn (length base-nonce))
         (nonce (copy-seq base-nonce)))
    ;; XOR seq (as big-endian) into the nonce
    (loop for i from (1- nn) downto 0
          for shift from 0 by 8
          while (< shift 64)
          do (setf (aref nonce i)
                   (logxor (aref nonce i)
                           (ldb (byte 8 shift) seq))))
    nonce))

(defun hpke-increment-seq (ctx)
  "Increment sequence number. Returns NIL if overflow would occur."
  (let ((max-seq (1- (ash 1 (* 8 (length (hpke-context-base-nonce ctx)))))))
    (when (>= (hpke-context-seq ctx) max-seq)
      (error 'tls-crypto-error
             :operation "HPKE"
             :message "Sequence number overflow"))
    (incf (hpke-context-seq ctx))))

(defun hpke-context-seal (ctx aad pt)
  "Encrypt a message using the HPKE context.
   Returns ciphertext || tag."
  (let* ((key (hpke-context-key ctx))
         (nonce (hpke-compute-nonce ctx))
         (aead-id (hpke-context-aead-id ctx))
         (ct (case aead-id
               (#.+hpke-aead-aes-128-gcm+
                (aes-gcm-encrypt key nonce pt aad))
               (#.+hpke-aead-aes-256-gcm+
                (aes-gcm-encrypt key nonce pt aad))
               (#.+hpke-aead-chacha20-poly1305+
                (chacha20-poly1305-encrypt key nonce pt aad))
               (otherwise
                (error 'tls-crypto-error
                       :operation "HPKE seal"
                       :message (format nil "Unsupported AEAD: ~X" aead-id))))))
    (hpke-increment-seq ctx)
    ct))

(defun hpke-context-open (ctx aad ct)
  "Decrypt a message using the HPKE context.
   Returns plaintext or signals error on authentication failure."
  (let* ((key (hpke-context-key ctx))
         (nonce (hpke-compute-nonce ctx))
         (aead-id (hpke-context-aead-id ctx))
         (pt (case aead-id
               (#.+hpke-aead-aes-128-gcm+
                (aes-gcm-decrypt key nonce ct aad))
               (#.+hpke-aead-aes-256-gcm+
                (aes-gcm-decrypt key nonce ct aad))
               (#.+hpke-aead-chacha20-poly1305+
                (chacha20-poly1305-decrypt key nonce ct aad))
               (otherwise
                (error 'tls-crypto-error
                       :operation "HPKE open"
                       :message (format nil "Unsupported AEAD: ~X" aead-id))))))
    (hpke-increment-seq ctx)
    pt))

;;;; Single-Shot API (RFC 9180 Section 6)

(defun hpke-seal (pk-r info aad pt &key
                                     (kem-id +hpke-kem-x25519-sha256+)
                                     (kdf-id +hpke-kdf-hkdf-sha256+)
                                     (aead-id +hpke-aead-aes-128-gcm+))
  "Single-shot HPKE seal (encrypt).
   pk-r is recipient's public key.
   info is context info.
   aad is additional authenticated data.
   pt is plaintext.
   Returns (values enc ct) where enc is encapsulated key and ct is ciphertext."
  (multiple-value-bind (ctx enc)
      (hpke-setup-base-s pk-r info
                         :kem-id kem-id :kdf-id kdf-id :aead-id aead-id)
    (let ((ct (hpke-context-seal ctx aad pt)))
      (values enc ct))))

(defun hpke-open (enc sk-r info aad ct &key
                                         (kem-id +hpke-kem-x25519-sha256+)
                                         (kdf-id +hpke-kdf-hkdf-sha256+)
                                         (aead-id +hpke-aead-aes-128-gcm+))
  "Single-shot HPKE open (decrypt).
   enc is encapsulated key from sender.
   sk-r is recipient's private key.
   info is context info (must match sender's).
   aad is additional authenticated data.
   ct is ciphertext.
   Returns plaintext."
  (let ((ctx (hpke-setup-base-r enc sk-r info
                                :kem-id kem-id :kdf-id kdf-id :aead-id aead-id)))
    (hpke-context-open ctx aad ct)))

;;;; Exporter (RFC 9180 Section 5.3)

(defun hpke-context-export (ctx exporter-context len)
  "Export a secret from the HPKE context.
   Used for deriving additional keys, e.g., ECH accept confirmation."
  (let ((suite-id (hpke-suite-id (hpke-context-kem-id ctx)
                                 (hpke-context-kdf-id ctx)
                                 (hpke-context-aead-id ctx))))
    (hpke-labeled-expand (hpke-context-exporter-secret ctx)
                         "sec" exporter-context len suite-id)))

;;;; X25519 Key Generation Helper

(defun hpke-generate-x25519-keypair ()
  "Generate an X25519 key pair for HPKE.
   Returns (values private-key public-key-bytes)."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :curve25519)
    (let ((public-key-bytes (ironclad:curve25519-key-y public-key)))
      (values private-key public-key-bytes))))
