;;; test/ml-dsa-tests.lisp --- ML-DSA post-quantum signature tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Tests for ML-DSA-65 (FIPS 204) post-quantum digital signatures.

(in-package #:pure-tls/test)

(def-suite ml-dsa-tests
  :description "Tests for ML-DSA-65 post-quantum signatures")

(in-suite ml-dsa-tests)

;;;; ML-DSA-65 Key Generation Tests

(test ml-dsa-65-keygen-sizes
  "Test ML-DSA-65 key generation produces correct sizes"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      ;; Check public key size (FIPS 204: 1952 bytes for ML-DSA-65)
      (is (= (length pk) 1952)
          "Public key should be 1952 bytes")
      ;; Check secret key size (FIPS 204: 4032 bytes for ML-DSA-65)
      (is (= (length sk) 4032)
          "Secret key should be 4032 bytes"))))

(test ml-dsa-65-keygen-deterministic
  "Test ML-DSA-65 key generation is deterministic"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk1 sk1)
        (pure-tls::ml-dsa-65-keygen seed)
      (multiple-value-bind (pk2 sk2)
          (pure-tls::ml-dsa-65-keygen seed)
        (is (bytes-equal pk1 pk2)
            "Same seed should produce same public key")
        (is (bytes-equal sk1 sk2)
            "Same seed should produce same secret key")))))

(test ml-dsa-65-keygen-different-seeds
  "Test ML-DSA-65 key generation produces different keys for different seeds"
  (let ((seed1 (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x11))
        (seed2 (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x22)))
    (multiple-value-bind (pk1 sk1)
        (pure-tls::ml-dsa-65-keygen seed1)
      (declare (ignore sk1))
      (multiple-value-bind (pk2 sk2)
          (pure-tls::ml-dsa-65-keygen seed2)
        (declare (ignore sk2))
        (is (not (bytes-equal pk1 pk2))
            "Different seeds should produce different public keys")))))

;;;; ML-DSA-65 Signature Tests

(test ml-dsa-65-signature-size
  "Test ML-DSA-65 signature has correct size"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      (declare (ignore pk))
      (let* ((msg (pure-tls::string-to-octets "Test message"))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer)))
        ;; Check signature size (FIPS 204: 3309 bytes for ML-DSA-65)
        (is (= (length sig) 3309)
            "Signature should be 3309 bytes")))))

(test ml-dsa-65-sign-verify-roundtrip
  "Test ML-DSA-65 sign and verify roundtrip"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      (let* ((msg (pure-tls::string-to-octets "Hello, post-quantum world!"))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer)))
        (is (pure-tls::ml-dsa-65-verify pk msg sig)
            "Valid signature should verify")))))

(test ml-dsa-65-verify-different-messages
  "Test ML-DSA-65 verification with different message lengths"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      ;; Empty message
      (let* ((msg (make-array 0 :element-type '(unsigned-byte 8)))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer)))
        (is (pure-tls::ml-dsa-65-verify pk msg sig)
            "Empty message signature should verify"))
      ;; Short message
      (let* ((msg (pure-tls::string-to-octets "Hi"))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer)))
        (is (pure-tls::ml-dsa-65-verify pk msg sig)
            "Short message signature should verify"))
      ;; Long message
      (let* ((msg (make-array 10000 :element-type '(unsigned-byte 8)
                              :initial-element #xAB))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer)))
        (is (pure-tls::ml-dsa-65-verify pk msg sig)
            "Long message signature should verify")))))

(test ml-dsa-65-deterministic-signing
  "Test ML-DSA-65 hedged signing with explicit randomness"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (rnd (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x99)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      (declare (ignore pk))
      (let* ((msg (pure-tls::string-to-octets "Test message"))
             (sig1 (pure-tls::ml-dsa-65-sign sk msg rnd))
             (sig2 (pure-tls::ml-dsa-65-sign sk msg rnd)))
        (is (bytes-equal sig1 sig2)
            "Same randomness should produce same signature")))))

;;;; ML-DSA-65 Signature Rejection Tests

(test ml-dsa-65-reject-modified-signature
  "Test ML-DSA-65 rejects modified signature"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      (let* ((msg (pure-tls::string-to-octets "Test message"))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer))
             (bad-sig (copy-seq sig)))
        ;; Flip a bit in the signature
        (setf (aref bad-sig 100) (logxor (aref bad-sig 100) #xff))
        (is (not (pure-tls::ml-dsa-65-verify pk msg bad-sig))
            "Modified signature should not verify")))))

(test ml-dsa-65-reject-modified-message
  "Test ML-DSA-65 rejects signature for modified message"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      (let* ((msg (pure-tls::string-to-octets "Original message"))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer))
             (bad-msg (pure-tls::string-to-octets "Modified message")))
        (is (not (pure-tls::ml-dsa-65-verify pk bad-msg sig))
            "Signature for different message should not verify")))))

(test ml-dsa-65-reject-wrong-public-key
  "Test ML-DSA-65 rejects signature with wrong public key"
  (let ((seed1 (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (seed2 (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x43))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk1 sk1)
        (pure-tls::ml-dsa-65-keygen seed1)
      (declare (ignore pk1))
      (multiple-value-bind (pk2 sk2)
          (pure-tls::ml-dsa-65-keygen seed2)
        (declare (ignore sk2))
        (let* ((msg (pure-tls::string-to-octets "Test message"))
               (sig (pure-tls::ml-dsa-65-sign sk1 msg randomizer)))
          (is (not (pure-tls::ml-dsa-65-verify pk2 msg sig))
              "Signature should not verify with wrong public key"))))))

(test ml-dsa-65-reject-truncated-signature
  "Test ML-DSA-65 rejects truncated signature"
  (let ((seed (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x42))
        (randomizer (make-array 32 :element-type '(unsigned-byte 8) :initial-element #x55)))
    (multiple-value-bind (pk sk)
        (pure-tls::ml-dsa-65-keygen seed)
      (let* ((msg (pure-tls::string-to-octets "Test message"))
             (sig (pure-tls::ml-dsa-65-sign sk msg randomizer))
             (truncated-sig (subseq sig 0 3000)))
        (is (not (pure-tls::ml-dsa-65-verify pk msg truncated-sig))
            "Truncated signature should not verify")))))

;;;; ML-DSA Polynomial Arithmetic Tests

(test ml-dsa-poly-mul-schoolbook
  "Test ML-DSA schoolbook polynomial multiplication"
  ;; Test that multiplication is consistent
  (let ((a (pure-tls::make-ml-dsa-poly))
        (b (pure-tls::make-ml-dsa-poly)))
    ;; Set some non-trivial values
    (dotimes (i 256)
      (setf (aref a i) (mod (* i 17) pure-tls::+ml-dsa-q+))
      (setf (aref b i) (mod (* i 23) pure-tls::+ml-dsa-q+)))
    ;; Test that a * b gives consistent results
    (let ((c1 (pure-tls::ml-dsa-poly-mul-schoolbook a b))
          (c2 (pure-tls::ml-dsa-poly-mul-schoolbook a b)))
      (is (every #'= c1 c2)
          "Polynomial multiplication should be deterministic"))))

(test ml-dsa-poly-add-sub
  "Test ML-DSA polynomial addition and subtraction"
  (let ((a (pure-tls::make-ml-dsa-poly))
        (b (pure-tls::make-ml-dsa-poly)))
    ;; Set values
    (dotimes (i 256)
      (setf (aref a i) (mod (* i 13) pure-tls::+ml-dsa-q+))
      (setf (aref b i) (mod (* i 7) pure-tls::+ml-dsa-q+)))
    ;; Copy a to compare later
    (let ((a-orig (copy-seq a)))
      ;; Test (a + b) - b = a
      (let* ((sum (pure-tls::ml-dsa-poly-add a b))
             (diff (pure-tls::ml-dsa-poly-sub sum b)))
        ;; Reduce to canonical form for comparison
        (dotimes (i 256)
          (setf (aref diff i) (mod (aref diff i) pure-tls::+ml-dsa-q+)))
        (is (every #'= a-orig diff)
            "(a + b) - b should equal a")))))

(test ml-dsa-mod-q-signed
  "Test ML-DSA signed modular reduction"
  ;; Test that mod-q-signed produces values in (-(q-1)/2, (q-1)/2]
  ;; Per FIPS 204, this maps [0, q) to (-(q-1)/2, (q-1)/2]
  (let ((half-q (floor pure-tls::+ml-dsa-q+ 2)))
    ;; Test small positive
    (is (= (pure-tls::ml-dsa-mod-q-signed 100) 100)
        "Small positive should remain unchanged")
    ;; Test value just under q/2
    (is (= (pure-tls::ml-dsa-mod-q-signed (1- half-q)) (1- half-q))
        "Value just under q/2 should remain positive")
    ;; Test value at q/2 (boundary - stays positive)
    (is (= (pure-tls::ml-dsa-mod-q-signed half-q) half-q)
        "Value at q/2 should remain positive")
    ;; Test value just over q/2 (should become negative)
    (is (< (pure-tls::ml-dsa-mod-q-signed (1+ half-q)) 0)
        "Value over q/2 should become negative")))

;;;; ML-DSA Constants Tests

(test ml-dsa-65-constants
  "Test ML-DSA-65 parameter constants are correct"
  ;; FIPS 204 Table 1 parameters for ML-DSA-65
  (is (= pure-tls::+ml-dsa-q+ 8380417)
      "ML-DSA modulus q should be 8380417")
  (is (= pure-tls::+ml-dsa-65-k+ 6)
      "ML-DSA-65 k should be 6")
  (is (= pure-tls::+ml-dsa-65-l+ 5)
      "ML-DSA-65 l should be 5")
  (is (= pure-tls::+ml-dsa-65-eta+ 4)
      "ML-DSA-65 eta should be 4")
  (is (= pure-tls::+ml-dsa-65-tau+ 49)
      "ML-DSA-65 tau should be 49")
  (is (= pure-tls::+ml-dsa-65-beta+ 196)
      "ML-DSA-65 beta should be tau*eta = 196")
  (is (= pure-tls::+ml-dsa-65-gamma1+ (ash 1 19))
      "ML-DSA-65 gamma1 should be 2^19")
  (is (= pure-tls::+ml-dsa-65-gamma2+ (floor (1- pure-tls::+ml-dsa-q+) 32))
      "ML-DSA-65 gamma2 should be (q-1)/32")
  (is (= pure-tls::+ml-dsa-65-omega+ 55)
      "ML-DSA-65 omega should be 55")
  (is (= pure-tls::+ml-dsa-65-d+ 13)
      "ML-DSA-65 d should be 13"))

;;;; ML-DSA TLS Integration Tests

(test ml-dsa-65-signature-algorithm-constant
  "Test ML-DSA-65 TLS signature algorithm constant"
  (is (= pure-tls::+sig-mldsa65+ #x0905)
      "ML-DSA-65 signature algorithm should be 0x0905"))

(test ml-dsa-65-in-supported-algorithms
  "Test ML-DSA-65 is in supported signature algorithms"
  (is (member pure-tls::+sig-mldsa65+ (pure-tls::supported-signature-algorithms-tls13))
      "ML-DSA-65 should be in supported signature algorithms list"))

;;;; Test Runner

(defun run-ml-dsa-tests ()
  "Run all ML-DSA tests."
  (run! 'ml-dsa-tests))
