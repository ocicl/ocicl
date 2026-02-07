;;; key-exchange.lisp --- Key exchange implementations for TLS 1.3
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements key exchange algorithms for TLS 1.3:
;;; - X25519 (Curve25519 ECDH)
;;; - secp256r1 (P-256 ECDH)

(in-package #:pure-tls)

;;;; Key Exchange Interface

(defstruct key-exchange
  "Key exchange state."
  (group 0 :type fixnum)
  (private-key nil)
  (public-key nil :type (or null octet-vector)))

;;;; X25519 Implementation

(defun make-x25519-key-exchange ()
  "Generate a new X25519 key pair for key exchange."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :curve25519)
    (let ((public-key-bytes (ironclad:curve25519-key-y public-key)))
      (make-key-exchange :group +group-x25519+
                         :private-key private-key
                         :public-key public-key-bytes))))

(defun x25519-compute-shared-secret (key-exchange peer-public-key)
  "Compute the X25519 shared secret from our private key and peer's public key.
Rejects all-zero shared secrets per RFC 7748 §6.1 and RFC 8446 §7.4.2."
  ;; X25519 public keys MUST be exactly 32 bytes (RFC 7748 Section 5)
  (unless (= (length peer-public-key) 32)
    (error 'tls-crypto-error
           :operation "X25519 key exchange"
           :message (format nil ":BAD_ECPOINT: Invalid X25519 key length: ~D (expected 32)"
                           (length peer-public-key))))
  (let* ((private-key (key-exchange-private-key key-exchange))
         (peer-key (ironclad:make-public-key :curve25519 :y peer-public-key))
         (shared-secret (ironclad:diffie-hellman private-key peer-key)))
    ;; RFC 8446 §7.4.2: For X25519, implementations MUST check whether the
    ;; computed Diffie-Hellman shared secret is the all-zero value and abort.
    (when (every #'zerop shared-secret)
      (error 'tls-crypto-error
             :operation "X25519 key exchange"
             :message ":BAD_ECPOINT: Invalid shared secret (all zeros) - possible small-subgroup attack"))
    shared-secret))

;;;; secp256r1 (P-256) Implementation

(defun make-secp256r1-key-exchange ()
  "Generate a new secp256r1 (P-256) key pair for key exchange."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :secp256r1)
    ;; The public key Y slot contains the uncompressed point (04 || x || y)
    (let ((public-key-bytes (slot-value public-key 'ironclad::y)))
      (make-key-exchange :group +group-secp256r1+
                         :private-key private-key
                         :public-key public-key-bytes))))

;; secp256r1 curve parameters (NIST P-256)
(defconstant +secp256r1-p+
  #xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
  "The prime p for secp256r1")

(defconstant +secp256r1-a+
  #xffffffff00000001000000000000000000000000fffffffffffffffffffffffc
  "The curve parameter a for secp256r1 (equals p - 3)")

(defconstant +secp256r1-b+
  #x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
  "The curve parameter b for secp256r1")

(defun secp256r1-point-on-curve-p (x y)
  "Check if point (x, y) lies on the secp256r1 curve.
   Curve equation: y² ≡ x³ + ax + b (mod p)"
  (let* ((p +secp256r1-p+)
         (a +secp256r1-a+)
         (b +secp256r1-b+)
         ;; Compute y² mod p
         (y-squared (mod (* y y) p))
         ;; Compute x³ + ax + b mod p
         (x-cubed (mod (* x x x) p))
         (ax (mod (* a x) p))
         (rhs (mod (+ x-cubed ax b) p)))
    (= y-squared rhs)))

(defun validate-secp256r1-public-key (peer-public-key)
  "Validate a secp256r1 public key per RFC 8446 §4.2.8.2.
   Returns (values x y) if valid, signals error otherwise."
  ;; Check format: must be uncompressed (04 || x || y), exactly 65 bytes
  (unless (= (length peer-public-key) 65)
    (error 'tls-crypto-error
           :operation "secp256r1 key exchange"
           :message (format nil ":BAD_ECPOINT: Invalid key length: ~D (expected 65)"
                           (length peer-public-key))))
  (unless (= (aref peer-public-key 0) #x04)
    (error 'tls-crypto-error
           :operation "secp256r1 key exchange"
           :message ":BAD_ECPOINT: Invalid key format: expected uncompressed point (0x04 prefix)"))
  ;; Extract x and y coordinates (big-endian, 32 bytes each)
  (let ((x (ironclad:octets-to-integer peer-public-key :start 1 :end 33))
        (y (ironclad:octets-to-integer peer-public-key :start 33 :end 65)))
    ;; Check coordinates are in valid range [1, p-1]
    (unless (and (< 0 x +secp256r1-p+)
                 (< 0 y +secp256r1-p+))
      (error 'tls-crypto-error
             :operation "secp256r1 key exchange"
             :message ":BAD_ECPOINT: Point coordinates out of range"))
    ;; Check point is on curve
    (unless (secp256r1-point-on-curve-p x y)
      (error 'tls-crypto-error
             :operation "secp256r1 key exchange"
             :message ":BAD_ECPOINT: Point not on curve - possible invalid curve attack"))
    (values x y)))

(defun secp256r1-compute-shared-secret (key-exchange peer-public-key)
  "Compute the secp256r1 shared secret from our private key and peer's public key.
   Validates that the peer's public key is a valid point on the curve.
   Returns the x-coordinate of the shared point (32 bytes) per RFC 8446 Section 7.4.2."
  ;; Validate peer public key (signals error if invalid)
  (validate-secp256r1-public-key peer-public-key)
  ;; Proceed with ECDH
  (let* ((private-key (key-exchange-private-key key-exchange))
         (peer-key (ironclad:make-public-key :secp256r1 :y peer-public-key))
         (shared-point (ironclad:diffie-hellman private-key peer-key)))
    ;; Ironclad returns the full uncompressed point (0x04 || x || y)
    ;; TLS 1.3 requires only the x-coordinate as the shared secret
    (subseq shared-point 1 33)))

;;;; secp384r1 (P-384) Implementation

(defun make-secp384r1-key-exchange ()
  "Generate a new secp384r1 (P-384) key pair for key exchange."
  (multiple-value-bind (private-key public-key)
      (ironclad:generate-key-pair :secp384r1)
    ;; The public key Y slot contains the uncompressed point (04 || x || y)
    (let ((public-key-bytes (slot-value public-key 'ironclad::y)))
      (make-key-exchange :group +group-secp384r1+
                         :private-key private-key
                         :public-key public-key-bytes))))

;; secp384r1 curve parameters (NIST P-384)
(defconstant +secp384r1-p+
  #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff
  "The prime p for secp384r1")

(defconstant +secp384r1-a+
  #xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000fffffffc
  "The curve parameter a for secp384r1 (equals p - 3)")

(defconstant +secp384r1-b+
  #xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
  "The curve parameter b for secp384r1")

(defun secp384r1-point-on-curve-p (x y)
  "Check if point (x, y) lies on the secp384r1 curve.
   Curve equation: y² ≡ x³ + ax + b (mod p)"
  (let* ((p +secp384r1-p+)
         (a +secp384r1-a+)
         (b +secp384r1-b+)
         ;; Compute y² mod p
         (y-squared (mod (* y y) p))
         ;; Compute x³ + ax + b mod p
         (x-cubed (mod (* x x x) p))
         (ax (mod (* a x) p))
         (rhs (mod (+ x-cubed ax b) p)))
    (= y-squared rhs)))

(defun validate-secp384r1-public-key (peer-public-key)
  "Validate a secp384r1 public key per RFC 8446 §4.2.8.2.
   Returns (values x y) if valid, signals error otherwise."
  ;; Check format: must be uncompressed (04 || x || y), exactly 97 bytes
  (unless (= (length peer-public-key) 97)
    (error 'tls-crypto-error
           :operation "secp384r1 key exchange"
           :message (format nil ":BAD_ECPOINT: Invalid key length: ~D (expected 97)"
                           (length peer-public-key))))
  (unless (= (aref peer-public-key 0) #x04)
    (error 'tls-crypto-error
           :operation "secp384r1 key exchange"
           :message ":BAD_ECPOINT: Invalid key format: expected uncompressed point (0x04 prefix)"))
  ;; Extract x and y coordinates (big-endian, 48 bytes each)
  (let ((x (ironclad:octets-to-integer peer-public-key :start 1 :end 49))
        (y (ironclad:octets-to-integer peer-public-key :start 49 :end 97)))
    ;; Check coordinates are in valid range [1, p-1]
    (unless (and (< 0 x +secp384r1-p+)
                 (< 0 y +secp384r1-p+))
      (error 'tls-crypto-error
             :operation "secp384r1 key exchange"
             :message ":BAD_ECPOINT: Point coordinates out of range"))
    ;; Check point is on curve
    (unless (secp384r1-point-on-curve-p x y)
      (error 'tls-crypto-error
             :operation "secp384r1 key exchange"
             :message ":BAD_ECPOINT: Point not on curve - possible invalid curve attack"))
    (values x y)))

(defun secp384r1-compute-shared-secret (key-exchange peer-public-key)
  "Compute the secp384r1 shared secret from our private key and peer's public key.
   Validates that the peer's public key is a valid point on the curve.
   Returns the x-coordinate of the shared point (48 bytes) per RFC 8446 Section 7.4.2."
  ;; Validate peer public key (signals error if invalid)
  (validate-secp384r1-public-key peer-public-key)
  ;; Proceed with ECDH
  (let* ((private-key (key-exchange-private-key key-exchange))
         (peer-key (ironclad:make-public-key :secp384r1 :y peer-public-key))
         (shared-point (ironclad:diffie-hellman private-key peer-key)))
    ;; Ironclad returns the full uncompressed point (0x04 || x || y)
    ;; TLS 1.3 requires only the x-coordinate as the shared secret
    (subseq shared-point 1 49)))

;;;; Generic Key Exchange Operations

(defun generate-key-exchange (group)
  "Generate a key exchange for the specified named group."
  (case group
    (#.+group-x25519+
     (make-x25519-key-exchange))
    (#.+group-secp256r1+
     (make-secp256r1-key-exchange))
    (#.+group-secp384r1+
     (make-secp384r1-key-exchange))
    (#.+group-x25519-mlkem768+
     (make-hybrid-x25519-ml-kem-768))
    (otherwise
     (error 'tls-crypto-error
            :operation "key exchange"
            :message (format nil "Unsupported group: ~X" group)))))

(defun compute-shared-secret (key-exchange peer-public-key)
  "Compute the shared secret using the key exchange and peer's public key."
  ;; Handle hybrid key exchange separately
  (when (hybrid-key-exchange-p key-exchange)
    (return-from compute-shared-secret
      (hybrid-compute-shared-secret key-exchange peer-public-key)))
  (let ((group (key-exchange-group key-exchange)))
    (case group
      (#.+group-x25519+
       (x25519-compute-shared-secret key-exchange peer-public-key))
      (#.+group-secp256r1+
       (secp256r1-compute-shared-secret key-exchange peer-public-key))
      (#.+group-secp384r1+
       (secp384r1-compute-shared-secret key-exchange peer-public-key))
      (otherwise
       (error 'tls-crypto-error
              :operation "compute shared secret"
              :message (format nil "Unsupported group: ~X" group))))))

(defun key-exchange-public-key-length (group)
  "Return the client's public key length in bytes for the given named group.
For hybrid, this is what the client sends (encapsulation key + ECDH public key)."
  (case group
    (#.+group-x25519+ 32)
    (#.+group-x448+ 56)
    (#.+group-secp256r1+ 65)  ; uncompressed point
    (#.+group-secp384r1+ 97)  ; uncompressed point
    (#.+group-secp521r1+ 133) ; uncompressed point
    (#.+group-x25519-mlkem768+ 1216)  ; 1184 + 32 (ML-KEM ek + X25519)
    (otherwise 0)))

(defun key-exchange-server-share-length (group)
  "Return the server's key_share length in bytes for the given named group.
For hybrid, this is what the server sends (ciphertext + ECDH public key)."
  (case group
    (#.+group-x25519+ 32)
    (#.+group-x448+ 56)
    (#.+group-secp256r1+ 65)  ; uncompressed point
    (#.+group-secp384r1+ 97)  ; uncompressed point
    (#.+group-secp521r1+ 133) ; uncompressed point
    (#.+group-x25519-mlkem768+ 1120)  ; 1088 + 32 (ML-KEM ct + X25519)
    (otherwise 0)))

(defun named-group-name (group)
  "Return the human-readable name for a named group."
  (case group
    (#.+group-x25519+ "x25519")
    (#.+group-x448+ "x448")
    (#.+group-secp256r1+ "secp256r1")
    (#.+group-secp384r1+ "secp384r1")
    (#.+group-secp521r1+ "secp521r1")
    (#.+group-ffdhe2048+ "ffdhe2048")
    (#.+group-ffdhe3072+ "ffdhe3072")
    (#.+group-ffdhe4096+ "ffdhe4096")
    (#.+group-x25519-mlkem768+ "X25519MLKEM768")
    (#.+group-x25519-kyber768-draft00+ "X25519Kyber768Draft00")
    (#.+group-secp256r1-kyber768-draft00+ "SecP256r1Kyber768Draft00")
    (otherwise (format nil "unknown(~X)" group))))

;;;; Generic Key Exchange Accessors (works with both regular and hybrid)

(defun get-key-exchange-public-key (kex)
  "Get the public key from any type of key exchange."
  (if (hybrid-key-exchange-p kex)
      (hybrid-public-key kex)
      (key-exchange-public-key kex)))

(defun get-key-exchange-group (kex)
  "Get the named group from any type of key exchange."
  (if (hybrid-key-exchange-p kex)
      +group-x25519-mlkem768+
      (key-exchange-group kex)))

;;;; Supported Groups for ClientHello

(defparameter *supported-groups*
  (list +group-x25519-mlkem768+ +group-x25519+ +group-secp256r1+ +group-secp384r1+)
  "List of named groups we support, in preference order.
X25519MLKEM768 is preferred for post-quantum security.")

(defparameter *preferred-group* +group-x25519-mlkem768+
  "The preferred named group to offer in ClientHello key_share.")

;;;; Signature Algorithms

(defun supported-signature-algorithms ()
  "Return the list of supported signature algorithms for certificate verification."
  (list +sig-ecdsa-secp256r1-sha256+
        +sig-rsa-pss-rsae-sha256+
        +sig-rsa-pkcs1-sha256+
        +sig-ecdsa-secp384r1-sha384+
        +sig-rsa-pss-rsae-sha384+
        +sig-rsa-pkcs1-sha384+
        +sig-rsa-pss-rsae-sha512+
        +sig-rsa-pkcs1-sha512+
        +sig-ed25519+
        +sig-ed448+))

(defun supported-signature-algorithms-tls13 ()
  "Return the list of supported TLS 1.3 signature algorithms."
  (list +sig-ecdsa-secp256r1-sha256+
        +sig-rsa-pss-rsae-sha256+
        +sig-rsa-pss-pss-sha256+
        +sig-ecdsa-secp384r1-sha384+
        +sig-rsa-pss-rsae-sha384+
        +sig-rsa-pss-pss-sha384+
        +sig-ecdsa-secp521r1-sha512+
        +sig-rsa-pss-rsae-sha512+
        +sig-rsa-pss-pss-sha512+
        +sig-ed25519+
        +sig-ed448+
        ;; Post-quantum signatures (FIPS 204)
        +sig-mldsa65+))

(defun signature-algorithm-name (sig-alg)
  "Return the human-readable name for a signature algorithm."
  (case sig-alg
    (#.+sig-rsa-pkcs1-md5+ "rsa_pkcs1_md5")
    (#.+sig-rsa-pkcs1-sha1+ "rsa_pkcs1_sha1")
    (#.+sig-ecdsa-sha1+ "ecdsa_sha1")
    (#.+sig-rsa-pkcs1-sha256+ "rsa_pkcs1_sha256")
    (#.+sig-rsa-pkcs1-sha384+ "rsa_pkcs1_sha384")
    (#.+sig-rsa-pkcs1-sha512+ "rsa_pkcs1_sha512")
    (#.+sig-ecdsa-secp256r1-sha256+ "ecdsa_secp256r1_sha256")
    (#.+sig-ecdsa-secp384r1-sha384+ "ecdsa_secp384r1_sha384")
    (#.+sig-ecdsa-secp521r1-sha512+ "ecdsa_secp521r1_sha512")
    (#.+sig-rsa-pss-rsae-sha256+ "rsa_pss_rsae_sha256")
    (#.+sig-rsa-pss-rsae-sha384+ "rsa_pss_rsae_sha384")
    (#.+sig-rsa-pss-rsae-sha512+ "rsa_pss_rsae_sha512")
    (#.+sig-ed25519+ "ed25519")
    (#.+sig-ed448+ "ed448")
    (#.+sig-mldsa44+ "mldsa44")
    (#.+sig-mldsa65+ "mldsa65")
    (#.+sig-mldsa87+ "mldsa87")
    (otherwise (format nil "unknown(~X)" sig-alg))))
