;;; aead.lisp --- AEAD cipher implementations for TLS 1.3
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements AEAD ciphers used in TLS 1.3:
;;; - AES-128-GCM
;;; - ChaCha20-Poly1305

(in-package #:pure-tls)

;;;; AEAD Cipher Interface

(defstruct aead-cipher
  "Abstract AEAD cipher state."
  (key nil :type (or null octet-vector))
  (implicit-nonce nil :type (or null octet-vector))
  (sequence-number 0 :type (unsigned-byte 64))
  (cipher-suite 0 :type fixnum))

(defun make-aead (cipher-suite key iv)
  "Create an AEAD cipher for the given cipher suite."
  (make-aead-cipher :key (copy-seq key)
                    :implicit-nonce (copy-seq iv)
                    :sequence-number 0
                    :cipher-suite cipher-suite))

(defun aead-compute-nonce (cipher)
  "Compute the per-record nonce by XORing sequence number with implicit nonce.

   In TLS 1.3, the nonce is computed as:
   nonce = implicit_iv XOR padded_sequence_number"
  (let* ((iv (aead-cipher-implicit-nonce cipher))
         (seq (aead-cipher-sequence-number cipher))
         (nonce (copy-seq iv)))
    ;; XOR the sequence number (as 8 bytes, big-endian) with the last 8 bytes of IV
    (loop for i from 0 below 8
          for shift from 56 downto 0 by 8
          for idx from (- (length nonce) 8)
          do (setf (aref nonce idx)
                   (logxor (aref nonce idx)
                           (ldb (byte 8 shift) seq))))
    nonce))

(defun aead-increment-sequence (cipher)
  "Increment the sequence number. Must be called after each encrypt/decrypt."
  (incf (aead-cipher-sequence-number cipher))
  (when (>= (aead-cipher-sequence-number cipher) (ash 1 64))
    (error 'tls-crypto-error
           :operation "AEAD"
           :message "Sequence number overflow")))

;;;; AES-GCM Implementation

(defun aes-gcm-encrypt (key nonce plaintext aad)
  "Encrypt using AES-GCM.

   KEY       - 16 or 32 byte encryption key (AES-128 or AES-256).
   NONCE     - 12 byte nonce.
   PLAINTEXT - Data to encrypt.
   AAD       - Additional authenticated data.

   Returns ciphertext with 16-byte authentication tag appended."
  (let ((key-len (length key)))
    (unless (or (= key-len 16) (= key-len 32))
      (error 'tls-crypto-error
             :operation "AES-GCM encrypt"
             :message (format nil "Invalid key length ~D (must be 16 or 32)" key-len))))
  (let* ((mode (ironclad:make-authenticated-encryption-mode
                :gcm :cipher-name :aes :key key
                :initialization-vector nonce))
         (ciphertext (make-octet-vector (length plaintext)))
         (tag (make-octet-vector 16)))
    ;; Process AAD
    (ironclad:process-associated-data mode aad)
    ;; Encrypt
    (ironclad:encrypt mode plaintext ciphertext)
    ;; Get tag
    (ironclad:produce-tag mode :tag tag)
    ;; Return ciphertext || tag
    (concat-octet-vectors ciphertext tag)))

(defun aes-gcm-decrypt (key nonce ciphertext-with-tag aad)
  "Decrypt using AES-GCM.

   KEY               - 16 or 32 byte encryption key (AES-128 or AES-256).
   NONCE             - 12 byte nonce.
   CIPHERTEXT-WITH-TAG - Ciphertext with 16-byte tag appended.
   AAD               - Additional authenticated data.

   Returns plaintext, or signals TLS-MAC-ERROR if authentication fails."
  (let ((key-len (length key)))
    (unless (or (= key-len 16) (= key-len 32))
      (error 'tls-crypto-error
             :operation "AES-GCM decrypt"
             :message (format nil "Invalid key length ~D (must be 16 or 32)" key-len))))
  (when (< (length ciphertext-with-tag) 16)
    (error 'tls-mac-error))
  (let* ((ct-len (- (length ciphertext-with-tag) 16))
         (ciphertext (subseq ciphertext-with-tag 0 ct-len))
         (tag (subseq ciphertext-with-tag ct-len))
         (mode (ironclad:make-authenticated-encryption-mode
                :gcm :cipher-name :aes :key key
                :initialization-vector nonce))
         (plaintext (make-octet-vector ct-len))
         (computed-tag (make-octet-vector 16)))
    ;; Process AAD
    (ironclad:process-associated-data mode aad)
    ;; Decrypt
    (ironclad:decrypt mode ciphertext plaintext)
    ;; Get computed tag
    (ironclad:produce-tag mode :tag computed-tag)
    ;; Verify tag (constant-time comparison)
    (unless (constant-time-equal tag computed-tag)
      (error 'tls-mac-error))
    plaintext))

;;;; ChaCha20-Poly1305 Implementation
;;;
;;; Implements AEAD_CHACHA20_POLY1305 per RFC 8439
;;; Since Ironclad doesn't provide a combined AEAD mode for ChaCha20-Poly1305,
;;; we implement it using the separate ChaCha and Poly1305 primitives.

(defun chacha20-poly1305-pad16 (len)
  "Return number of zero padding bytes needed to align LEN to 16 bytes."
  (mod (- 16 (mod len 16)) 16))

(defun chacha20-poly1305-encrypt (key nonce plaintext aad)
  "Encrypt using ChaCha20-Poly1305 per RFC 8439.

   KEY       - 32 byte encryption key.
   NONCE     - 12 byte nonce.
   PLAINTEXT - Data to encrypt.
   AAD       - Additional authenticated data.

   Returns ciphertext with 16-byte authentication tag appended."
  ;; Step 1: Generate Poly1305 one-time key using ChaCha20 with counter=0
  ;; The first 32 bytes of ChaCha20 keystream become the Poly1305 key
  (let* ((poly-key-block (make-octet-vector 64))
         (zeros (make-octet-vector 64))
         ;; Use a dedicated cipher for the Poly1305 key block (counter=0).
         (poly-cipher (ironclad:make-cipher :chacha :key key
                                            :initialization-vector nonce
                                            :mode :stream))
         ;; Use a separate cipher for payload encryption and explicitly
         ;; advance to block counter=1 by consuming one 64-byte block.
         (data-cipher (ironclad:make-cipher :chacha :key key
                                            :initialization-vector nonce
                                            :mode :stream)))
    (ironclad:encrypt poly-cipher zeros poly-key-block)
    (let ((skip (make-octet-vector 64)))
      (ironclad:encrypt data-cipher zeros skip))
    (let ((poly-key (subseq poly-key-block 0 32)))

      ;; Step 2: Encrypt plaintext using ChaCha20 starting at counter=1
      (let ((ciphertext (make-octet-vector (length plaintext))))
        (ironclad:encrypt data-cipher plaintext ciphertext)

        ;; Step 3: Construct the Poly1305 input per RFC 8439 Section 2.8
        ;; AAD || pad16(AAD) || ciphertext || pad16(ciphertext) || len(AAD) || len(ciphertext)
        (let* ((aad-len (length aad))
               (ct-len (length ciphertext))
               (aad-pad (chacha20-poly1305-pad16 aad-len))
               (ct-pad (chacha20-poly1305-pad16 ct-len))
               ;; Lengths are 64-bit little-endian
               (mac-input (concat-octet-vectors
                           aad
                           (make-octet-vector aad-pad)
                           ciphertext
                           (make-octet-vector ct-pad)
                           (encode-uint64-le aad-len)
                           (encode-uint64-le ct-len)))
               (mac (ironclad:make-mac :poly1305 poly-key)))
          (ironclad:update-mac mac mac-input)
          (let ((tag (ironclad:produce-mac mac)))
            ;; Return ciphertext || tag
            (concat-octet-vectors ciphertext tag)))))))

(defun chacha20-poly1305-decrypt (key nonce ciphertext-with-tag aad)
  "Decrypt using ChaCha20-Poly1305 per RFC 8439.

   KEY               - 32 byte encryption key.
   NONCE             - 12 byte nonce.
   CIPHERTEXT-WITH-TAG - Ciphertext with 16-byte tag appended.
   AAD               - Additional authenticated data.

   Returns plaintext, or signals TLS-MAC-ERROR if authentication fails."
  (when (< (length ciphertext-with-tag) 16)
    (error 'tls-mac-error))
  (let* ((ct-len (- (length ciphertext-with-tag) 16))
         (ciphertext (subseq ciphertext-with-tag 0 ct-len))
         (tag (subseq ciphertext-with-tag ct-len)))

    ;; Step 1: Generate Poly1305 one-time key using ChaCha20 with counter=0
    (let* ((poly-key-block (make-octet-vector 64))
           (zeros (make-octet-vector 64))
           ;; Use a dedicated cipher for the Poly1305 key block (counter=0).
           (poly-cipher (ironclad:make-cipher :chacha :key key
                                              :initialization-vector nonce
                                              :mode :stream))
           ;; Use a separate cipher for payload decryption and explicitly
           ;; advance to block counter=1 by consuming one 64-byte block.
           (data-cipher (ironclad:make-cipher :chacha :key key
                                              :initialization-vector nonce
                                              :mode :stream)))
      (ironclad:encrypt poly-cipher zeros poly-key-block)
      (let ((skip (make-octet-vector 64)))
        (ironclad:encrypt data-cipher zeros skip))
      (let ((poly-key (subseq poly-key-block 0 32)))

        ;; Step 2: Verify the Poly1305 tag before decrypting (verify-then-decrypt)
        (let* ((aad-len (length aad))
               (aad-pad (chacha20-poly1305-pad16 aad-len))
               (ct-pad (chacha20-poly1305-pad16 ct-len))
               (mac-input (concat-octet-vectors
                           aad
                           (make-octet-vector aad-pad)
                           ciphertext
                           (make-octet-vector ct-pad)
                           (encode-uint64-le aad-len)
                           (encode-uint64-le ct-len)))
               (mac (ironclad:make-mac :poly1305 poly-key)))
          (ironclad:update-mac mac mac-input)
          (let ((computed-tag (ironclad:produce-mac mac)))
            ;; Constant-time comparison to prevent timing attacks
            (unless (constant-time-equal tag computed-tag)
              (error 'tls-mac-error))))

        ;; Step 3: Decrypt the ciphertext using ChaCha20 starting at counter=1
        (let ((plaintext (make-octet-vector ct-len)))
          (ironclad:encrypt data-cipher ciphertext plaintext)  ; XOR-based, same as decrypt
          plaintext)))))

(defun encode-uint64-le (n)
  "Encode a 64-bit unsigned integer as 8 bytes in little-endian order."
  (octet-vector (ldb (byte 8 0) n)
                (ldb (byte 8 8) n)
                (ldb (byte 8 16) n)
                (ldb (byte 8 24) n)
                (ldb (byte 8 32) n)
                (ldb (byte 8 40) n)
                (ldb (byte 8 48) n)
                (ldb (byte 8 56) n)))

;;;; Unified AEAD Operations

(defun aead-encrypt (cipher plaintext aad)
  "Encrypt plaintext using the AEAD cipher.
   Returns ciphertext with authentication tag."
  (let* ((key (aead-cipher-key cipher))
         (nonce (aead-compute-nonce cipher))
         (suite (aead-cipher-cipher-suite cipher))
         (result (case suite
                   (#.+tls-aes-128-gcm-sha256+
                    (aes-gcm-encrypt key nonce plaintext aad))
                   (#.+tls-aes-256-gcm-sha384+
                    (aes-gcm-encrypt key nonce plaintext aad))
                   (#.+tls-chacha20-poly1305-sha256+
                    (chacha20-poly1305-encrypt key nonce plaintext aad))
                   (otherwise (error 'tls-crypto-error
                             :operation "AEAD encrypt"
                             :message (format nil "Unsupported cipher suite: ~X" suite))))))
    (aead-increment-sequence cipher)
    result))

(defun aead-decrypt (cipher ciphertext aad)
  "Decrypt ciphertext using the AEAD cipher.
   Returns plaintext or signals TLS-MAC-ERROR on authentication failure."
  (let* ((key (aead-cipher-key cipher))
         (nonce (aead-compute-nonce cipher))
         (suite (aead-cipher-cipher-suite cipher))
         (result (case suite
                   (#.+tls-aes-128-gcm-sha256+
                    (aes-gcm-decrypt key nonce ciphertext aad))
                   (#.+tls-aes-256-gcm-sha384+
                    (aes-gcm-decrypt key nonce ciphertext aad))
                   (#.+tls-chacha20-poly1305-sha256+
                    (chacha20-poly1305-decrypt key nonce ciphertext aad))
                   (otherwise (error 'tls-crypto-error
                             :operation "AEAD decrypt"
                             :message (format nil "Unsupported cipher suite: ~X" suite))))))
    (aead-increment-sequence cipher)
    result))

;;;; TLS 1.3 Record Encryption/Decryption
;;;
;;; Record padding support per RFC 8446 Section 5.4:
;;; "The padding octets all have value zero, and any receiver MAY
;;;  remove any such trailing zero octets."
;;;
;;; Padding policies help mitigate traffic analysis by hiding true message lengths.

(defparameter *record-padding-policy* nil
  "Record padding policy. Options:
   NIL - No padding (default)
   :BLOCK-256 - Pad to next 256-byte boundary
   :BLOCK-1024 - Pad to next 1024-byte boundary
   :FIXED-4096 - Pad all records to 4096 bytes (max that fits in typical MTU)
   (function) - Custom function taking plaintext-length, returns target length")

(defun compute-padded-length (plaintext-length)
  "Compute the target length for a record based on padding policy.
   Returns the target length for the inner plaintext (before content type byte)."
  (let ((policy *record-padding-policy*))
    (cond
      ((null policy) plaintext-length)
      ((eql policy :block-256)
       (* 256 (ceiling (1+ plaintext-length) 256)))  ; +1 for content type
      ((eql policy :block-1024)
       (* 1024 (ceiling (1+ plaintext-length) 1024)))
      ((eql policy :fixed-4096)
       (min 4096 (max plaintext-length 4096)))
      ((functionp policy)
       (funcall policy plaintext-length))
      (t plaintext-length))))

(defun tls13-encrypt-record (cipher content-type plaintext &optional (padding-policy *record-padding-policy*))
  "Encrypt a TLS 1.3 record.

   The plaintext is padded with the inner content type and optional zeros,
   then encrypted with AAD being the record header.

   PADDING-POLICY overrides *record-padding-policy* for this record.

   Returns the encrypted record payload (ciphertext + tag)."
  (let ((*record-padding-policy* padding-policy))
    ;; Build inner plaintext: content || content_type || zeros (padding)
    (let* ((content-len (length plaintext))
           (target-len (compute-padded-length content-len))
           (padding-len (max 0 (- target-len content-len)))
           ;; Ensure we don't exceed max record size
           (actual-padding (min padding-len
                                (- +max-record-size+ content-len +aead-tag-length+ 1)))
           (inner (if (plusp actual-padding)
                      (concat-octet-vectors
                       plaintext
                       (octet-vector content-type)
                       (make-octet-vector actual-padding))  ; zeros for padding
                      (concat-octet-vectors
                       plaintext
                       (octet-vector content-type))))
           ;; AAD is the record header for the outer record
           ;; TLSCiphertext header: content_type(1) || legacy_version(2) || length(2)
           (encrypted-len (+ (length inner) +aead-tag-length+))
           (aad (octet-vector +content-type-application-data+  ; outer type
                              #x03 #x03                         ; legacy TLS 1.2 version
                              (ldb (byte 8 8) encrypted-len)
                              (ldb (byte 8 0) encrypted-len))))
      (aead-encrypt cipher inner aad))))

(defun tls13-decrypt-record (cipher ciphertext record-header)
  "Decrypt a TLS 1.3 record.

   CIPHERTEXT is the encrypted payload (including tag).
   RECORD-HEADER is the 5-byte TLS record header (used as AAD).

   Returns (VALUES plaintext content-type) where content-type is the
   inner content type extracted from the decrypted data.

   All decryption failures signal TLS-MAC-ERROR to avoid oracles.
   Per RFC 8446, all failures should appear as 'bad_record_mac'."
  (let* ((inner (aead-decrypt cipher ciphertext record-header))
         ;; Find the content type (last non-zero byte)
         (content-type-pos (position-if-not #'zerop inner :from-end t)))
    ;; Missing content type is also reported as MAC error to avoid oracle
    (unless content-type-pos
      (error 'tls-mac-error))
    ;; Check inner plaintext size - RFC 8446 Section 5.4:
    ;; The inner plaintext (content + type + padding) must not exceed 2^14 + 1 bytes.
    ;; This ensures content + padding <= 16384, allowing for the type byte.
    (when (> (length inner) (1+ +max-record-size+))
      (error 'tls-record-overflow
             :size (length inner)
             :message ":DATA_LENGTH_TOO_LONG:"))
    (values (subseq inner 0 content-type-pos)
            (aref inner content-type-pos))))
