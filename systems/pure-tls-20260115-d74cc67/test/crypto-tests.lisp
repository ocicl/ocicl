;;; test/crypto-tests.lisp --- Cryptographic primitive tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Tests for HKDF, AEAD, and key derivation using RFC test vectors.

(in-package #:pure-tls/test)

(def-suite crypto-tests
  :description "Tests for cryptographic primitives")

(in-suite crypto-tests)

;;;; Helper Functions

(defun hex-to-bytes (hex-string)
  "Convert a hex string to a byte vector."
  (let* ((clean (remove-if (lambda (c) (member c '(#\Space #\Newline #\Tab))) hex-string))
         (len (/ (length clean) 2))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len
          do (setf (aref result i)
                   (parse-integer clean :start (* i 2) :end (* (1+ i) 2) :radix 16)))
    result))

(defun bytes-equal (a b)
  "Compare two byte vectors."
  (and (= (length a) (length b))
       (every #'= a b)))

;;;; HKDF Tests (RFC 5869 Test Vectors)

(test hkdf-sha256-test-case-1
  "RFC 5869 Test Case 1: Basic test case with SHA-256"
  (let ((ikm (hex-to-bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
        (salt (hex-to-bytes "000102030405060708090a0b0c"))
        (info (hex-to-bytes "f0f1f2f3f4f5f6f7f8f9"))
        (expected-prk (hex-to-bytes
                       "077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5"))
        (expected-okm (hex-to-bytes
                       "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865")))
    ;; Test HKDF-Extract
    (let ((prk (pure-tls::hkdf-extract salt ikm :digest :sha256)))
      (is (bytes-equal prk expected-prk)
          "HKDF-Extract should produce correct PRK"))
    ;; Test HKDF-Expand
    (let* ((prk (pure-tls::hkdf-extract salt ikm :digest :sha256))
           (okm (pure-tls::hkdf-expand prk info 42 :digest :sha256)))
      (is (bytes-equal okm expected-okm)
          "HKDF-Expand should produce correct OKM"))))

(test hkdf-sha256-test-case-2
  "RFC 5869 Test Case 2: Longer inputs/outputs"
  (let ((ikm (hex-to-bytes
              "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f"))
        (salt (hex-to-bytes
               "606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf"))
        (info (hex-to-bytes
               "b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"))
        (expected-okm (hex-to-bytes
                       "b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87")))
    (let* ((prk (pure-tls::hkdf-extract salt ikm :digest :sha256))
           (okm (pure-tls::hkdf-expand prk info 82 :digest :sha256)))
      (is (bytes-equal okm expected-okm)
          "HKDF with longer inputs should work correctly"))))

(test hkdf-sha256-test-case-3
  "RFC 5869 Test Case 3: Zero-length salt and info"
  (let ((ikm (hex-to-bytes "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"))
        (salt (make-array 0 :element-type '(unsigned-byte 8)))
        (info (make-array 0 :element-type '(unsigned-byte 8)))
        (expected-okm (hex-to-bytes
                       "8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8")))
    (let* ((prk (pure-tls::hkdf-extract salt ikm :digest :sha256))
           (okm (pure-tls::hkdf-expand prk info 42 :digest :sha256)))
      (is (bytes-equal okm expected-okm)
          "HKDF with zero-length salt/info should work"))))

;;;; AES-GCM Tests (NIST Test Vectors)

(test aes-128-gcm-encrypt-decrypt
  "Test AES-128-GCM encryption and decryption"
  (let ((key (hex-to-bytes "feffe9928665731c6d6a8f9467308308"))
        (nonce (hex-to-bytes "cafebabefacedbaddecaf888"))
        (plaintext (hex-to-bytes "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"))
        (aad (make-array 0 :element-type '(unsigned-byte 8)))
        (expected-ciphertext (hex-to-bytes "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091473f5985"))
        (expected-tag (hex-to-bytes "4d5c2af327cd64a62cf35abd2ba6fab4")))
    ;; Test encryption
    (let ((result (pure-tls::aes-gcm-encrypt key nonce plaintext aad)))
      (is (= (length result) (+ (length plaintext) 16))
          "Ciphertext should include 16-byte tag")
      (is (bytes-equal (subseq result 0 (length plaintext)) expected-ciphertext)
          "Ciphertext should match expected")
      (is (bytes-equal (subseq result (length plaintext)) expected-tag)
          "Tag should match expected"))
    ;; Test decryption
    (let* ((ciphertext-with-tag (concatenate '(vector (unsigned-byte 8))
                                              expected-ciphertext expected-tag))
           (decrypted (pure-tls::aes-gcm-decrypt key nonce ciphertext-with-tag aad)))
      (is (bytes-equal decrypted plaintext)
          "Decrypted text should match original plaintext"))))

(test aes-256-gcm-encrypt-decrypt
  "Test AES-256-GCM encryption and decryption"
  (let ((key (hex-to-bytes "0000000000000000000000000000000000000000000000000000000000000000"))
        (nonce (hex-to-bytes "000000000000000000000000"))
        (plaintext (make-array 0 :element-type '(unsigned-byte 8)))
        (aad (make-array 0 :element-type '(unsigned-byte 8)))
        (expected-tag (hex-to-bytes "530f8afbc74536b9a963b4f1c4cb738b")))
    (let ((result (pure-tls::aes-gcm-encrypt key nonce plaintext aad)))
      (is (= (length result) 16)
          "Empty plaintext should produce only tag")
      (is (bytes-equal result expected-tag)
          "Tag for empty message should match"))))

(test aes-gcm-authentication-failure
  "Test that AES-GCM rejects tampered ciphertext"
  (let ((key (hex-to-bytes "feffe9928665731c6d6a8f9467308308"))
        (nonce (hex-to-bytes "cafebabefacedbaddecaf888"))
        (plaintext (hex-to-bytes "d9313225f88406e5a55909c5aff5269a"))
        (aad (make-array 0 :element-type '(unsigned-byte 8))))
    (let* ((ciphertext (pure-tls::aes-gcm-encrypt key nonce plaintext aad))
           ;; Tamper with the ciphertext
           (tampered (copy-seq ciphertext)))
      (setf (aref tampered 0) (logxor (aref tampered 0) #xff))
      (signals pure-tls::tls-mac-error
        (pure-tls::aes-gcm-decrypt key nonce tampered aad)))))

;;;; ChaCha20-Poly1305 Tests (RFC 8439 Test Vectors)

(test chacha20-poly1305-rfc8439-section-2-8-2
  "RFC 8439 Section 2.8.2 AEAD test vector"
  (let ((key (hex-to-bytes
              "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
        (nonce (hex-to-bytes "070000004041424344454647"))
        (aad (hex-to-bytes "50515253c0c1c2c3c4c5c6c7"))
        (plaintext (flexi-streams:string-to-octets
                    "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
                    :external-format :utf-8))
        (expected-ciphertext (hex-to-bytes
                              "d31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116"))
        (expected-tag (hex-to-bytes "1ae10b594f09e26a7e902ecbd0600691")))
    ;; Test encryption
    (let ((result (pure-tls::chacha20-poly1305-encrypt key nonce plaintext aad)))
      (is (= (length result) (+ (length plaintext) 16))
          "Result should include ciphertext and 16-byte tag")
      (is (bytes-equal (subseq result 0 (length plaintext)) expected-ciphertext)
          "Ciphertext should match RFC 8439 expected value")
      (is (bytes-equal (subseq result (length plaintext)) expected-tag)
          "Tag should match RFC 8439 expected value"))
    ;; Test decryption
    (let* ((ciphertext-with-tag (concatenate '(vector (unsigned-byte 8))
                                              expected-ciphertext expected-tag))
           (decrypted (pure-tls::chacha20-poly1305-decrypt key nonce ciphertext-with-tag aad)))
      (is (bytes-equal decrypted plaintext)
          "Decrypted text should match original"))))

(test chacha20-poly1305-authentication-failure
  "Test that ChaCha20-Poly1305 rejects tampered ciphertext"
  (let ((key (hex-to-bytes
              "808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"))
        (nonce (hex-to-bytes "070000004041424344454647"))
        (aad (hex-to-bytes "50515253c0c1c2c3c4c5c6c7"))
        (plaintext (flexi-streams:string-to-octets "Test message" :external-format :utf-8)))
    (let* ((ciphertext (pure-tls::chacha20-poly1305-encrypt key nonce plaintext aad))
           (tampered (copy-seq ciphertext)))
      (setf (aref tampered 0) (logxor (aref tampered 0) #xff))
      (signals pure-tls::tls-mac-error
        (pure-tls::chacha20-poly1305-decrypt key nonce tampered aad)))))

;;;; TLS 1.3 Key Derivation Tests (RFC 8448)

(test tls13-hkdf-expand-label
  "Test TLS 1.3 HKDF-Expand-Label with derived label"
  ;; Test vector from RFC 8448 Simple 1-RTT Handshake
  ;; Derive-Secret(Secret, Label, Messages) = HKDF-Expand-Label(Secret, Label, Transcript-Hash(Messages), Hash.length)
  ;; For "derived" with empty messages: context = SHA-256("") = e3b0c442...
  (let* ((secret (hex-to-bytes
                  "33ad0a1c607ec03b09e6cd9893680ce210adf300aa1f2660e1b22e10f170f92a"))
         ;; SHA-256 of empty message
         (empty-hash (hex-to-bytes
                      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
         ;; derived = Derive-Secret(., "derived", "") = HKDF-Expand-Label(., "derived", SHA-256(""), 32)
         (expected (hex-to-bytes
                    "6f2615a108c702c5678f54fc9dbab69716c076189c48250cebeac3576c3611ba")))
    (let ((result (pure-tls::hkdf-expand-label secret "derived"
                                               empty-hash
                                               32 :digest :sha256)))
      (is (bytes-equal result expected)
          "HKDF-Expand-Label 'derived' should match RFC 8448"))))

;;;; Constant-Time Comparison Tests

(test constant-time-equal-same
  "Test constant-time comparison with equal values"
  (let ((a (hex-to-bytes "0102030405060708"))
        (b (hex-to-bytes "0102030405060708")))
    (is (pure-tls:constant-time-equal a b)
        "Equal byte vectors should compare equal")))

(test constant-time-equal-different
  "Test constant-time comparison with different values"
  (let ((a (hex-to-bytes "0102030405060708"))
        (b (hex-to-bytes "0102030405060709")))
    (is (not (pure-tls:constant-time-equal a b))
        "Different byte vectors should not compare equal")))

(test constant-time-equal-different-length
  "Test constant-time comparison with different lengths"
  (let ((a (hex-to-bytes "01020304"))
        (b (hex-to-bytes "0102030405060708")))
    (is (not (pure-tls:constant-time-equal a b))
        "Vectors of different length should not compare equal")))

;;;; Random Bytes Tests

(test random-bytes-length
  "Test that random-bytes returns correct length"
  (dotimes (i 5)
    (let* ((len (+ 16 (* i 16)))
           (bytes (pure-tls:random-bytes len)))
      (is (= (length bytes) len)
          "random-bytes should return requested length"))))

(test random-bytes-uniqueness
  "Test that random-bytes produces unique values"
  (let ((a (pure-tls:random-bytes 32))
        (b (pure-tls:random-bytes 32)))
    (is (not (bytes-equal a b))
        "Two random byte vectors should be different (with overwhelming probability)")))

;;;; ML-KEM-768 Tests

(test ml-kem-768-roundtrip
  "Test ML-KEM-768 key generation, encapsulation, and decapsulation"
  (multiple-value-bind (ek dk)
      (pure-tls::ml-kem-768-keygen)
    ;; Check key sizes
    (is (= (length ek) 1184)
        "Encapsulation key should be 1184 bytes")
    (is (= (length dk) 2400)
        "Decapsulation key should be 2400 bytes")
    ;; Test encapsulation
    (multiple-value-bind (ss ct)
        (pure-tls::ml-kem-768-encaps ek)
      (is (= (length ss) 32)
          "Shared secret should be 32 bytes")
      (is (= (length ct) 1088)
          "Ciphertext should be 1088 bytes")
      ;; Test decapsulation
      (let ((ss-dec (pure-tls::ml-kem-768-decaps dk ct)))
        (is (= (length ss-dec) 32)
            "Decapsulated shared secret should be 32 bytes")
        (is (bytes-equal ss ss-dec)
            "Decapsulated shared secret should match encapsulated one")))))

(test ml-kem-768-deterministic
  "Test ML-KEM-768 with deterministic randomness"
  (let ((d (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (z (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x43))
        (m (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x44)))
    (multiple-value-bind (ek dk)
        (pure-tls::ml-kem-768-keygen-internal d z)
      (multiple-value-bind (ss ct)
          (pure-tls::ml-kem-768-encaps-internal ek m)
        (let ((ss-dec (pure-tls::ml-kem-768-decaps dk ct)))
          (is (bytes-equal ss ss-dec)
              "Deterministic encaps/decaps should produce same shared secret"))))))

(test ml-kem-768-implicit-rejection
  "Test ML-KEM-768 implicit rejection with modified ciphertext"
  (multiple-value-bind (ek dk)
      (pure-tls::ml-kem-768-keygen)
    (multiple-value-bind (ss ct)
        (pure-tls::ml-kem-768-encaps ek)
      ;; Modify the ciphertext
      (let ((bad-ct (copy-seq ct)))
        (setf (aref bad-ct 0) (logxor (aref bad-ct 0) #xff))
        (let ((bad-ss (pure-tls::ml-kem-768-decaps dk bad-ct)))
          ;; Implicit rejection: different shared secret, not an error
          (is (= (length bad-ss) 32)
              "Implicit rejection should still return 32 bytes")
          (is (not (bytes-equal ss bad-ss))
              "Modified ciphertext should produce different shared secret"))))))

(test ml-kem-ntt-roundtrip
  "Test NTT forward and inverse transforms"
  (let ((poly (pure-tls::make-ml-kem-poly)))
    ;; Initialize with some values
    (dotimes (i 256)
      (setf (aref poly i) (mod i pure-tls::+ml-kem-q+)))
    (let ((original (copy-seq poly)))
      ;; Apply NTT and inverse NTT
      (pure-tls::ntt-forward poly)
      (pure-tls::ntt-inverse poly)
      ;; Should get back the original
      (is (every #'= original poly)
          "NTT inverse should restore original polynomial"))))

(test hybrid-x25519-ml-kem-768-roundtrip
  "Test hybrid X25519+ML-KEM-768 key exchange"
  ;; Client generates hybrid key exchange
  (let ((client-kex (pure-tls::make-hybrid-x25519-ml-kem-768)))
    ;; Get client's public key share
    (let ((client-share (pure-tls::hybrid-public-key client-kex)))
      (is (= (length client-share) 1216)
          "Client share should be 1216 bytes (32 + 1184)")
      ;; Server performs encapsulation
      (multiple-value-bind (server-ss server-share)
          (pure-tls::hybrid-server-encaps client-share)
        (is (= (length server-ss) 64)
            "Server shared secret should be 64 bytes")
        (is (= (length server-share) 1120)
            "Server share should be 1120 bytes (32 + 1088)")
        ;; Client computes shared secret
        (let ((client-ss (pure-tls::hybrid-compute-shared-secret client-kex server-share)))
          (is (= (length client-ss) 64)
              "Client shared secret should be 64 bytes")
          (is (bytes-equal server-ss client-ss)
              "Client and server should derive same shared secret"))))))

(test ml-kem-poly-arithmetic
  "Test polynomial addition and subtraction"
  (let ((a (pure-tls::make-ml-kem-poly))
        (b (pure-tls::make-ml-kem-poly)))
    ;; Set some values
    (dotimes (i 256)
      (setf (aref a i) (mod (* i 13) pure-tls::+ml-kem-q+))
      (setf (aref b i) (mod (* i 7) pure-tls::+ml-kem-q+)))
    ;; Test a + b - b = a
    (let* ((sum (pure-tls::poly-add a b))
           (diff (pure-tls::poly-sub sum b)))
      (is (every #'= a diff)
          "a + b - b should equal a"))))

;;;; Test Runner

(defun run-crypto-tests ()
  "Run all cryptographic tests."
  (run! 'crypto-tests))
