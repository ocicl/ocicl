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
  (let* ((pt-len (length plaintext))
         (mode (ironclad:make-authenticated-encryption-mode
                :gcm :cipher-name :aes :key key
                :initialization-vector nonce))
         ;; Allocate output with room for ciphertext + 16-byte tag
         (output (make-octet-vector (+ pt-len 16)))
         (tag (make-octet-vector 16)))
    ;; Process AAD
    (ironclad:process-associated-data mode aad)
    ;; Encrypt
    (ironclad:encrypt mode plaintext output)
    ;; Get tag and place at end of output
    (ironclad:produce-tag mode :tag tag)
    (replace output tag :start1 pt-len)
    output))

(defun aes-gcm-decrypt (key nonce ciphertext-with-tag aad &key output)
  "Decrypt using AES-GCM.

   KEY               - 16 or 32 byte encryption key (AES-128 or AES-256).
   NONCE             - 12 byte nonce.
   CIPHERTEXT-WITH-TAG - Ciphertext with 16-byte tag appended.
   AAD               - Additional authenticated data.
   OUTPUT            - Optional destination buffer for the plaintext.  When
                       supplied it must hold at least (- (length
                       CIPHERTEXT-WITH-TAG) 16) bytes; the decrypted plaintext
                       is written into its prefix and OUTPUT is returned,
                       letting callers reuse a pooled buffer.

   Returns plaintext, or signals TLS-MAC-ERROR if authentication fails."
  (let ((key-len (length key)))
    (unless (or (= key-len 16) (= key-len 32))
      (error 'tls-crypto-error
             :operation "AES-GCM decrypt"
             :message (format nil "Invalid key length ~D (must be 16 or 32)" key-len))))
  (when (< (length ciphertext-with-tag) 16)
    (error 'tls-mac-error))
  (let* ((ct-len (- (length ciphertext-with-tag) 16))
         ;; The trailing 16 bytes are the tag; only 16 bytes, so copying them
         ;; out is cheap.  The ciphertext body is left in place and bounded via
         ;; :ciphertext-end below to avoid a full-record subseq copy.
         (tag (subseq ciphertext-with-tag ct-len))
         (mode (ironclad:make-authenticated-encryption-mode
                :gcm :cipher-name :aes :key key
                :initialization-vector nonce))
         (plaintext (or output (make-octet-vector ct-len)))
         (computed-tag (make-octet-vector 16)))
    ;; Process AAD
    (ironclad:process-associated-data mode aad)
    ;; Decrypt only the ciphertext portion, leaving the tag untouched.
    (ironclad:decrypt mode ciphertext-with-tag plaintext :ciphertext-end ct-len)
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
    ;; Advance data cipher past block 0, reusing poly-key-block as the skip
    ;; buffer: both ciphers share key/nonce, so this writes the identical
    ;; block-0 keystream and the Poly1305 key bytes are preserved.
    (ironclad:encrypt data-cipher zeros poly-key-block)
    (let* ((poly-key (subseq poly-key-block 0 32))
           (pt-len (length plaintext)))

      ;; Step 2: Encrypt into output buffer sized for ciphertext + 16-byte tag.
      ;; ironclad:encrypt processes (length plaintext) bytes, so the oversized
      ;; output is safe; the last 16 bytes are filled with the tag below.
      (let ((output (make-octet-vector (+ pt-len 16))))
        (ironclad:encrypt data-cipher plaintext output)

        ;; Step 3: Compute Poly1305 MAC incrementally per RFC 8439 Section 2.8
        ;; Avoids allocating a single concatenated mac-input vector.
        (let* ((aad-len (length aad))
               (ct-len pt-len)
               (mac (ironclad:make-mac :poly1305 poly-key)))
          ;; AAD || pad16(AAD)
          (ironclad:update-mac mac aad)
          (let ((aad-pad (chacha20-poly1305-pad16 aad-len)))
            (when (plusp aad-pad)
              (ironclad:update-mac mac (make-octet-vector aad-pad))))
          ;; ciphertext || pad16(ciphertext)
          (ironclad:update-mac mac output :end ct-len)
          (let ((ct-pad (chacha20-poly1305-pad16 ct-len)))
            (when (plusp ct-pad)
              (ironclad:update-mac mac (make-octet-vector ct-pad))))
          ;; len(AAD) || len(ciphertext) as 64-bit LE
          (ironclad:update-mac mac (encode-uint64-le aad-len))
          (ironclad:update-mac mac (encode-uint64-le ct-len))
          ;; Place tag at end of output
          (let ((tag (ironclad:produce-mac mac)))
            (replace output tag :start1 ct-len))
          output)))))

(defun chacha20-poly1305-decrypt (key nonce ciphertext-with-tag aad &key output)
  "Decrypt using ChaCha20-Poly1305 per RFC 8439.

   KEY               - 32 byte encryption key.
   NONCE             - 12 byte nonce.
   CIPHERTEXT-WITH-TAG - Ciphertext with 16-byte tag appended.
   AAD               - Additional authenticated data.
   OUTPUT            - Optional destination buffer for the plaintext (see
                       AES-GCM-DECRYPT); reused instead of allocating when
                       supplied.

   Returns plaintext, or signals TLS-MAC-ERROR if authentication fails."
  (when (< (length ciphertext-with-tag) 16)
    (error 'tls-mac-error))
  (let* ((ct-len (- (length ciphertext-with-tag) 16))
         ;; Only the trailing 16-byte tag is copied out; the ciphertext body is
         ;; MAC'd and decrypted in place via bounds below.
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
      ;; Advance data cipher past block 0, reusing poly-key-block as the skip
      ;; buffer: both ciphers share key/nonce, so this rewrites the identical
      ;; block-0 keystream and the Poly1305 key bytes are preserved.
      (ironclad:encrypt data-cipher zeros poly-key-block)
      (let ((poly-key (subseq poly-key-block 0 32)))

        ;; Step 2: Verify the Poly1305 tag before decrypting (verify-then-decrypt).
        ;; Compute the MAC incrementally per RFC 8439 Section 2.8 to avoid
        ;; allocating a single concatenated mac-input vector (matches the
        ;; encrypt path).
        (let* ((aad-len (length aad))
               (mac (ironclad:make-mac :poly1305 poly-key)))
          ;; AAD || pad16(AAD)
          (ironclad:update-mac mac aad)
          (let ((aad-pad (chacha20-poly1305-pad16 aad-len)))
            (when (plusp aad-pad)
              (ironclad:update-mac mac (make-octet-vector aad-pad))))
          ;; ciphertext || pad16(ciphertext)
          (ironclad:update-mac mac ciphertext-with-tag :end ct-len)
          (let ((ct-pad (chacha20-poly1305-pad16 ct-len)))
            (when (plusp ct-pad)
              (ironclad:update-mac mac (make-octet-vector ct-pad))))
          ;; len(AAD) || len(ciphertext) as 64-bit LE
          (ironclad:update-mac mac (encode-uint64-le aad-len))
          (ironclad:update-mac mac (encode-uint64-le ct-len))
          (let ((computed-tag (ironclad:produce-mac mac)))
            ;; Constant-time comparison to prevent timing attacks
            (unless (constant-time-equal tag computed-tag)
              (error 'tls-mac-error))))

        ;; Step 3: Decrypt the ciphertext using ChaCha20 starting at counter=1.
        ;; :plaintext-end bounds the input to the ciphertext body, skipping the
        ;; trailing tag without a subseq copy.
        (let ((plaintext (or output (make-octet-vector ct-len))))
          (ironclad:encrypt data-cipher ciphertext-with-tag plaintext
                            :plaintext-end ct-len)  ; XOR-based, same as decrypt
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

(defun aead-decrypt (cipher ciphertext aad &key output)
  "Decrypt ciphertext using the AEAD cipher.
   OUTPUT, when supplied, is a destination buffer the plaintext is written
   into (see AES-GCM-DECRYPT) so callers can reuse a pooled buffer.
   Returns plaintext or signals TLS-MAC-ERROR on authentication failure."
  (let* ((key (aead-cipher-key cipher))
         (nonce (aead-compute-nonce cipher))
         (suite (aead-cipher-cipher-suite cipher))
         (result (case suite
                   (#.+tls-aes-128-gcm-sha256+
                    (aes-gcm-decrypt key nonce ciphertext aad :output output))
                   (#.+tls-aes-256-gcm-sha384+
                    (aes-gcm-decrypt key nonce ciphertext aad :output output))
                   (#.+tls-chacha20-poly1305-sha256+
                    (chacha20-poly1305-decrypt key nonce ciphertext aad :output output))
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

(defun tls13-encrypt-record (cipher content-type plaintext
                             &key (padding-policy *record-padding-policy*)
                                  (start 0) (end (length plaintext)))
  "Encrypt a TLS 1.3 record.

   The plaintext is padded with the inner content type and optional zeros,
   then encrypted with AAD being the record header.

   PADDING-POLICY overrides *record-padding-policy* for this record.
   START/END bound the region of PLAINTEXT to encrypt, letting callers pass a
   slice of a larger buffer without a subseq copy.

   Returns the encrypted record payload (ciphertext + tag)."
  (let ((*record-padding-policy* padding-policy))
    ;; Build inner plaintext: content || content_type || zeros (padding)
    ;; Allocate a single buffer and fill in place to avoid copying plaintext.
    (let* ((content-len (- end start))
           (target-len (compute-padded-length content-len))
           (padding-len (max 0 (- target-len content-len)))
           ;; Ensure we don't exceed max record size.  The cap goes negative
           ;; for a near-full record (content-len > 16367), so clamp at zero:
           ;; inner-len must never drop below content-len + 1.
           (actual-padding (max 0 (min padding-len
                                       (- +max-record-size+ content-len +aead-tag-length+ 1))))
           (inner-len (+ content-len 1 actual-padding))
           (inner (make-octet-vector inner-len)))
      (replace inner plaintext :start2 start :end2 end)  ; content
      (setf (aref inner content-len) content-type)  ; content_type byte
      ;; padding zeros already filled by make-octet-vector
      ;; AAD is the record header for the outer record
      ;; TLSCiphertext header: content_type(1) || legacy_version(2) || length(2)
      (let* ((encrypted-len (+ inner-len +aead-tag-length+))
             (aad (octet-vector +content-type-application-data+  ; outer type
                                #x03 #x03                         ; legacy TLS 1.2 version
                                (ldb (byte 8 8) encrypted-len)
                                (ldb (byte 8 0) encrypted-len))))
        (aead-encrypt cipher inner aad)))))

(defun tls13-decrypt-record (cipher ciphertext record-header)
  "Decrypt a TLS 1.3 record.

   CIPHERTEXT is the encrypted payload (including tag).
   RECORD-HEADER is the 5-byte TLS record header (used as AAD).

   Returns (VALUES plaintext content-type) where content-type is the
   inner content type extracted from the decrypted data.

   All decryption failures signal TLS-MAC-ERROR to avoid oracles.
   Per RFC 8446, all failures should appear as 'bad_record_mac'."
  ;; Decrypt into a pooled scratch buffer (recycled by the enclosing
  ;; WITH-BUFFER-CONTEXT in RECORD-LAYER-READ) so the exact-sized plaintext
  ;; returned below is the only per-record heap allocation, rather than one
  ;; full-record buffer from the AEAD plus a second from the trim.  SCRATCH may
  ;; be larger than the plaintext (tier-sized), so all length reasoning uses
  ;; CT-LEN, not (length inner).
  (let* ((ct-len (max 0 (- (length ciphertext) +aead-tag-length+)))
         (scratch (buffer-pool-allocate *buffer-pool* ct-len))
         (inner (aead-decrypt cipher ciphertext record-header :output scratch))
         ;; Find the content type (last non-zero byte within the CT-LEN prefix)
         (content-type-pos (position-if-not #'zerop inner :end ct-len :from-end t)))
    ;; Missing content type is also reported as MAC error to avoid oracle
    (unless content-type-pos
      (error 'tls-mac-error))
    ;; Check inner plaintext size - RFC 8446 Section 5.4:
    ;; The inner plaintext (content + type + padding) must not exceed 2^14 + 1 bytes.
    ;; This ensures content + padding <= 16384, allowing for the type byte.
    (when (> ct-len (1+ +max-record-size+))
      (error 'tls-record-overflow
             :size ct-len
             :message ":DATA_LENGTH_TOO_LONG:"))
    (let ((plaintext (make-octet-vector content-type-pos))
          (content-type (aref inner content-type-pos)))
      (replace plaintext inner :end2 content-type-pos)
      ;; Wipe the decrypted plaintext from the pooled scratch buffer before it
      ;; is recycled, so cleartext does not linger in a shared buffer.
      (fill inner 0 :end ct-len)
      (values plaintext content-type))))
