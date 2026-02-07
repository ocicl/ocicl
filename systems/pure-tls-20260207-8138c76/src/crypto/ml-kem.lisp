;;; ml-kem.lisp --- ML-KEM-768 implementation (FIPS 203)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements ML-KEM-768 (Module-Lattice-Based Key-Encapsulation Mechanism)
;;; for post-quantum hybrid key exchange in TLS 1.3.
;;;
;;; References:
;;; - FIPS 203: Module-Lattice-Based Key-Encapsulation Mechanism Standard
;;; - RFC TBD: Hybrid Key Exchange in TLS 1.3

(in-package #:pure-tls)

;;;; =========================================================================
;;;; ML-KEM-768 Parameters (FIPS 203, Table 2)
;;;; =========================================================================

(defconstant +ml-kem-n+ 256
  "Polynomial ring degree (number of coefficients).")

(defconstant +ml-kem-q+ 3329
  "Modulus for coefficient arithmetic.")

(defconstant +ml-kem-768-k+ 3
  "Module rank for ML-KEM-768.")

(defconstant +ml-kem-768-eta1+ 2
  "CBD parameter for key generation.")

(defconstant +ml-kem-768-eta2+ 2
  "CBD parameter for encryption.")

(defconstant +ml-kem-768-du+ 10
  "Compression bits for vector u.")

(defconstant +ml-kem-768-dv+ 4
  "Compression bits for polynomial v.")

;; Derived sizes
(defconstant +ml-kem-768-ek-size+ (+ (* 384 +ml-kem-768-k+) 32)
  "Encapsulation key size: 384*k + 32 = 1184 bytes.")

(defconstant +ml-kem-768-dk-size+ (+ (* 768 +ml-kem-768-k+) 96)
  "Decapsulation key size: 768*k + 96 = 2400 bytes.")

(defconstant +ml-kem-768-ct-size+ (+ (* 32 (+ (* +ml-kem-768-du+ +ml-kem-768-k+)
                                              +ml-kem-768-dv+)))
  "Ciphertext size: 32*(du*k + dv) = 1088 bytes.")

(defconstant +ml-kem-768-ss-size+ 32
  "Shared secret size: 32 bytes.")

;;;; =========================================================================
;;;; Precomputed NTT Constants
;;;; =========================================================================

;; Primitive 256th root of unity: 17 (since 17^128 ≡ -1 mod 3329)
(defconstant +ml-kem-root+ 17
  "Primitive 256th root of unity in Z_q.")

;; Bit-reversal helper
(defun bit-reverse-7 (x)
  "Reverse the lowest 7 bits of X."
  (declare (type (unsigned-byte 8) x)
           (optimize speed))
  (let ((result 0))
    (declare (type (unsigned-byte 8) result))
    (loop for i from 0 below 7
          do (setf result (logior (ash result 1) (logand x 1))
                   x (ash x -1)))
    result))

;; Precompute zetas (bit-reversed powers of root of unity)
(defun compute-ntt-zetas ()
  "Compute the 128 NTT zeta values (bit-reversed powers of root)."
  (let ((zetas (make-array 128 :element-type '(unsigned-byte 16))))
    (loop for i from 0 below 128
          for br = (bit-reverse-7 i)
          do (setf (aref zetas i)
                   (mod (expt-mod +ml-kem-root+ br +ml-kem-q+) +ml-kem-q+)))
    zetas))

(defun expt-mod (base exp modulus)
  "Compute BASE^EXP mod MODULUS using binary exponentiation."
  (declare (type integer base exp modulus)
           (optimize speed))
  (let ((result 1)
        (base (mod base modulus)))
    (loop while (> exp 0)
          do (when (oddp exp)
               (setf result (mod (* result base) modulus)))
             (setf exp (ash exp -1))
             (setf base (mod (* base base) modulus)))
    result))

;; Precomputed zetas: bit-reversed powers of 17 mod 3329
;; zetas[i] = 17^{BitRev_7(i)} mod 3329 for i = 0..127
;; As defined in FIPS 203 Section 4.3
(defparameter *ml-kem-zetas*
  #(1 1729 2580 3289 2642 630 1897 848 1062 1919 193 797 2786 3260 569
    1746 296 2447 1339 1476 3046 56 2240 1333 1426 2094 535 2882 2393
    2879 1974 821 289 331 3253 1756 1197 2304 2277 2055 650 1977 2513
    632 2865 33 1320 1915 2319 1435 807 452 1438 2868 1534 2402 2647
    2617 1481 648 2474 3110 1227 910 17 2761 583 2649 1637 723 2288
    1100 1409 2662 3281 233 756 2156 3015 3050 1703 1651 2789 1789
    1847 952 1461 2687 939 2308 2437 2388 733 2337 268 641 1584 2298
    2037 3220 375 2549 2090 1645 1063 319 2773 757 2099 561 2466 2594
    2804 1092 403 1026 1143 2150 2775 886 1722 1212 1874 1029 2110
    2935 885 2154)
  "Precomputed NTT zeta values (bit-reversed powers of 17 mod 3329).")

;; Inverse of 128 mod 3329 (for inverse NTT scaling)
(defconstant +ml-kem-ntt-scale+ 3303
  "Multiplicative inverse of 128 mod 3329 (for iNTT scaling).")

;;;; =========================================================================
;;;; Modular Arithmetic (Constant-Time)
;;;; =========================================================================
;;;;
;;;; These functions avoid data-dependent branches that could leak timing
;;;; information about secret values.

(declaim (inline ct-cond-sub-q ct-barrett-reduce ct-mod-q mod-q barrett-reduce))

(defun ct-cond-sub-q (x)
  "Constant-time conditional subtraction of q from x.
X must be in range [0, 2*q-1]. Returns x mod q without branching."
  (declare (type (unsigned-byte 16) x)
           (optimize speed))
  ;; (x - q + 2^16) >> 16 gives 1 if x >= q, 0 otherwise
  (let* ((diff (- x +ml-kem-q+))
         (cmp (ash (+ diff 65536) -16))
         (sub (* cmp +ml-kem-q+)))
    (- x sub)))

(defun ct-barrett-reduce (x)
  "Constant-time Barrett reduction for values up to 2^26. Returns x mod 3329.
Uses precomputed v = floor(2^26 / 3329) = 20158."
  (declare (type (unsigned-byte 32) x)
           (optimize speed))
  ;; v = floor(2^26 / 3329) = 20158
  ;; Note: 20159 would cause negative results for some inputs
  (let* ((v 20158)
         (t-val (ash (* x v) -26))
         (result (- x (* t-val +ml-kem-q+))))
    (ct-cond-sub-q result)))

(defun ct-mod-q (x)
  "Constant-time reduction of X modulo q = 3329."
  (declare (type integer x)
           (optimize speed))
  (if (< x 67108864)  ; 2^26
      (ct-barrett-reduce x)
      (mod x +ml-kem-q+)))  ; Fallback for precomputation only

(defun mod-q (x)
  "Reduce X modulo q = 3329. Uses constant-time reduction."
  (ct-mod-q x))

(defun barrett-reduce (x)
  "Barrett reduction. Uses constant-time reduction."
  (ct-barrett-reduce x))

(defun centered-mod (x)
  "Return X mod q in range [-(q-1)/2, (q-1)/2]."
  (declare (type integer x)
           (optimize speed))
  (let ((r (mod x +ml-kem-q+)))
    (if (> r (ash +ml-kem-q+ -1))
        (- r +ml-kem-q+)
        r)))

;;;; =========================================================================
;;;; Polynomial Type
;;;; =========================================================================

(deftype ml-kem-poly ()
  "A polynomial with 256 coefficients in Z_q."
  '(simple-array (unsigned-byte 16) (256)))

(defun make-ml-kem-poly ()
  "Create a new zero polynomial."
  (make-array 256 :element-type '(unsigned-byte 16) :initial-element 0))

(defun poly-copy (src)
  "Create a copy of polynomial SRC."
  (let ((dst (make-ml-kem-poly)))
    (replace dst src)
    dst))

;;;; =========================================================================
;;;; Polynomial Arithmetic
;;;; =========================================================================

(defun poly-add (a b)
  "Add polynomials A and B, returning a new polynomial."
  (declare (type ml-kem-poly a b)
           (optimize speed))
  (let ((result (make-ml-kem-poly)))
    (dotimes (i 256 result)
      (setf (aref result i)
            (ct-cond-sub-q (+ (aref a i) (aref b i)))))))

(defun poly-sub (a b)
  "Subtract polynomial B from A, returning a new polynomial."
  (declare (type ml-kem-poly a b)
           (optimize speed))
  (let ((result (make-ml-kem-poly)))
    (dotimes (i 256 result)
      (setf (aref result i)
            (ct-cond-sub-q (+ (- (aref a i) (aref b i)) +ml-kem-q+))))))

(defun poly-add! (dst src)
  "Add SRC to DST in-place."
  (declare (type ml-kem-poly dst src)
           (optimize speed))
  (dotimes (i 256 dst)
    (setf (aref dst i)
          (ct-cond-sub-q (+ (aref dst i) (aref src i))))))

;;;; =========================================================================
;;;; Number Theoretic Transform (NTT)
;;;; =========================================================================

(defun ntt-forward (poly)
  "Compute the NTT of POLY in-place (Cooley-Tukey butterfly)."
  (declare (type ml-kem-poly poly)
           (optimize speed))
  (let ((k 1))
    (loop for len = 128 then (ash len -1)
          while (>= len 2)
          do (loop for start from 0 below 256 by (ash len 1)
                   for zeta = (aref *ml-kem-zetas* k)
                   do (incf k)
                      (loop for j from start below (+ start len)
                            for t-val = (ct-barrett-reduce (* zeta (aref poly (+ j len))))
                            do (setf (aref poly (+ j len))
                                     (ct-cond-sub-q (+ (- (aref poly j) t-val) +ml-kem-q+)))
                               (setf (aref poly j)
                                     (ct-cond-sub-q (+ (aref poly j) t-val)))))))
  poly)

(defun ntt-inverse (poly)
  "Compute the inverse NTT of POLY in-place (Gentleman-Sande butterfly)."
  (declare (type ml-kem-poly poly)
           (optimize speed))
  (let ((k 127))
    (loop for len = 2 then (ash len 1)
          while (<= len 128)
          do (loop for start from 0 below 256 by (ash len 1)
                   for zeta = (aref *ml-kem-zetas* k)
                   do (decf k)
                      (loop for j from start below (+ start len)
                            for t-val = (aref poly j)
                            for u-val = (aref poly (+ j len))
                            do (setf (aref poly j)
                                     (ct-cond-sub-q (+ t-val u-val)))
                               ;; Note: (u - t) not (t - u)
                               (setf (aref poly (+ j len))
                                     (ct-barrett-reduce (* zeta (ct-cond-sub-q (+ (- u-val t-val) +ml-kem-q+)))))))))
  ;; Scale by n^{-1} = 3303
  (dotimes (i 256 poly)
    (setf (aref poly i)
          (ct-barrett-reduce (* (aref poly i) +ml-kem-ntt-scale+)))))

;;;; =========================================================================
;;;; NTT-domain Polynomial Multiplication
;;;; =========================================================================

(defun ntt-base-mul (a0 a1 b0 b1 zeta)
  "Base case multiplication for two degree-1 polynomials in NTT domain.
Computes (a0 + a1*X) * (b0 + b1*X) mod (X^2 - zeta)."
  (declare (type (unsigned-byte 16) a0 a1 b0 b1 zeta)
           (optimize speed))
  (values (ct-barrett-reduce (+ (* a0 b0) (* zeta (ct-barrett-reduce (* a1 b1)))))
          (ct-barrett-reduce (+ (* a1 b0) (* a0 b1)))))

(defun poly-mul-ntt (a b)
  "Multiply polynomials A and B in NTT domain, returning result in NTT domain.
Processes coefficients in groups of 4, using zetas[64..127]."
  (declare (type ml-kem-poly a b)
           (optimize speed))
  (let ((result (make-ml-kem-poly)))
    ;; Process 256 coefficients in groups of 4
    ;; Each group uses two base multiplications with +zeta and -zeta
    (loop for i from 0 below 64
          for zeta = (aref *ml-kem-zetas* (+ 64 i))
          for neg-zeta = (- +ml-kem-q+ zeta)
          for idx = (* 4 i)
          do ;; First pair uses +zeta
             (multiple-value-bind (r0 r1)
                 (ntt-base-mul (aref a idx)
                               (aref a (1+ idx))
                               (aref b idx)
                               (aref b (1+ idx))
                               zeta)
               (setf (aref result idx) r0)
               (setf (aref result (1+ idx)) r1))
             ;; Second pair uses -zeta
             (multiple-value-bind (r2 r3)
                 (ntt-base-mul (aref a (+ idx 2))
                               (aref a (+ idx 3))
                               (aref b (+ idx 2))
                               (aref b (+ idx 3))
                               neg-zeta)
               (setf (aref result (+ idx 2)) r2)
               (setf (aref result (+ idx 3)) r3)))
    result))

;;;; =========================================================================
;;;; Compression and Decompression
;;;; =========================================================================

(defun compress-coeff (x d)
  "Compress coefficient X using D bits.
Computes round(2^d * x / q) mod 2^d."
  (declare (type (unsigned-byte 16) x)
           (type (integer 1 12) d)
           (optimize speed))
  (let ((scale (ash 1 d)))
    ;; round(scale * x / q) = floor((scale * x + q/2) / q)
    (logand (floor (+ (* scale x) (ash +ml-kem-q+ -1)) +ml-kem-q+)
            (1- scale))))

(defun decompress-coeff (x d)
  "Decompress D-bit value X back to Z_q.
Computes round(q * x / 2^d)."
  (declare (type (unsigned-byte 16) x)
           (type (integer 1 12) d)
           (optimize speed))
  ;; round(q * x / 2^d) = floor((q * x + 2^(d-1)) / 2^d)
  (floor (+ (* +ml-kem-q+ x) (ash 1 (1- d))) (ash 1 d)))

(defun poly-compress (poly d)
  "Compress polynomial POLY using D bits per coefficient."
  (declare (type ml-kem-poly poly)
           (type (integer 1 12) d)
           (optimize speed))
  (let ((result (make-ml-kem-poly)))
    (dotimes (i 256 result)
      (setf (aref result i) (compress-coeff (aref poly i) d)))))

(defun poly-decompress (poly d)
  "Decompress polynomial POLY from D bits per coefficient."
  (declare (type ml-kem-poly poly)
           (type (integer 1 12) d)
           (optimize speed))
  (let ((result (make-ml-kem-poly)))
    (dotimes (i 256 result)
      (setf (aref result i) (decompress-coeff (aref poly i) d)))))

;;;; =========================================================================
;;;; Byte Encoding/Decoding
;;;; =========================================================================

(defun poly-to-bytes (poly)
  "Encode polynomial (12 bits per coefficient) to 384 bytes."
  (declare (type ml-kem-poly poly)
           (optimize speed))
  (let ((bytes (make-array 384 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 256 by 2
          for j from 0 by 3
          for c0 = (aref poly i)
          for c1 = (aref poly (1+ i))
          do (setf (aref bytes j) (logand c0 #xff))
             (setf (aref bytes (1+ j)) (logior (ash c0 -8) (ash (logand c1 #xf) 4)))
             (setf (aref bytes (+ j 2)) (ash c1 -4)))
    bytes))

(defun bytes-to-poly (bytes &optional (offset 0))
  "Decode 384 bytes to polynomial (12 bits per coefficient).
Reduces coefficients mod q for defense against malformed inputs."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum offset)
           (optimize speed))
  (let ((poly (make-ml-kem-poly)))
    (loop for i from 0 below 256 by 2
          for j from offset by 3
          do (let ((c0 (logior (aref bytes j)
                               (ash (logand (aref bytes (1+ j)) #xf) 8)))
                   (c1 (logior (ash (aref bytes (1+ j)) -4)
                               (ash (aref bytes (+ j 2)) 4))))
               ;; Reduce mod q for canonical representation
               (setf (aref poly i) (if (>= c0 +ml-kem-q+) (mod c0 +ml-kem-q+) c0))
               (setf (aref poly (1+ i)) (if (>= c1 +ml-kem-q+) (mod c1 +ml-kem-q+) c1))))
    poly))

(defun poly-compress-to-bytes (poly d)
  "Compress polynomial and encode to bytes."
  (declare (type ml-kem-poly poly)
           (type (integer 1 12) d)
           (optimize speed))
  (let* ((num-bytes (/ (* 256 d) 8))
         (bytes (make-array num-bytes :element-type '(unsigned-byte 8) :initial-element 0))
         (bit-pos 0))
    (dotimes (i 256)
      (let ((c (compress-coeff (aref poly i) d)))
        (loop for b from 0 below d
              for byte-idx = (floor bit-pos 8)
              for bit-idx = (mod bit-pos 8)
              do (setf (aref bytes byte-idx)
                       (logior (aref bytes byte-idx)
                               (ash (logand (ash c (- b)) 1) bit-idx)))
                 (incf bit-pos))))
    bytes))

(defun bytes-decompress-to-poly (bytes d &optional (offset 0))
  "Decode bytes and decompress to polynomial."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type (integer 1 12) d)
           (type fixnum offset)
           (optimize speed))
  (let ((poly (make-ml-kem-poly))
        (bit-pos (* offset 8)))
    (dotimes (i 256)
      (let ((c 0))
        (loop for b from 0 below d
              for byte-idx = (floor bit-pos 8)
              for bit-idx = (mod bit-pos 8)
              do (setf c (logior c (ash (logand (ash (aref bytes byte-idx) (- bit-idx)) 1) b)))
                 (incf bit-pos))
        (setf (aref poly i) (decompress-coeff c d))))
    poly))

;;;; =========================================================================
;;;; Centered Binomial Distribution (CBD)
;;;; =========================================================================

(defun cbd (bytes eta)
  "Sample polynomial from centered binomial distribution with parameter ETA.
   Supports eta=1,2,3 (ML-KEM) and eta=4 (ML-DSA-65)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type (integer 1 4) eta)
           (optimize speed))
  (let ((poly (make-ml-kem-poly))
        (bit-pos 0))
    (flet ((get-bit ()
             (let* ((byte-idx (floor bit-pos 8))
                    (bit-idx (logand bit-pos 7))
                    (bit (logand (ash (aref bytes byte-idx) (- bit-idx)) 1)))
               (incf bit-pos)
               bit)))
      (dotimes (i 256 poly)
        (let ((a 0) (b 0))
          (dotimes (j eta)
            (incf a (get-bit)))
          (dotimes (j eta)
            (incf b (get-bit)))
          (setf (aref poly i) (ct-cond-sub-q (+ (- a b) +ml-kem-q+))))))))

;;;; =========================================================================
;;;; XOF and Hash Functions
;;;; =========================================================================

(defun shake128-xof (seed length)
  "SHAKE128 extendable output function."
  (let ((digest (ironclad:make-digest :shake128 :output-length length)))
    (ironclad:update-digest digest seed)
    (ironclad:produce-digest digest)))

(defun shake256-xof (seed length)
  "SHAKE256 extendable output function."
  (let ((digest (ironclad:make-digest :shake256 :output-length length)))
    (ironclad:update-digest digest seed)
    (ironclad:produce-digest digest)))

(defun sha3-256-hash (&rest inputs)
  "SHA3-256 hash of concatenated inputs."
  (let ((digest (ironclad:make-digest :sha3/256)))
    (dolist (input inputs)
      (ironclad:update-digest digest input))
    (ironclad:produce-digest digest)))

(defun sha3-512-hash (&rest inputs)
  "SHA3-512 hash of concatenated inputs."
  (let ((digest (ironclad:make-digest :sha3)))
    (dolist (input inputs)
      (ironclad:update-digest digest input))
    (ironclad:produce-digest digest)))

;;;; =========================================================================
;;;; Matrix and Vector Operations
;;;; =========================================================================

(defun sample-ntt-poly (seed i j)
  "Sample a polynomial in NTT domain from XOF(seed || i || j).
Uses rejection sampling with sufficient buffer for worst-case rejection.
With 12-bit values and q=3329, acceptance rate is 3329/4096 ≈ 81.3%.
We need 256 valid samples. Buffer of 672 bytes gives 448 candidates.
P(failure) = P(Binomial(448, 0.813) < 256) ≈ 10^-96, negligible."
  (let* ((xof-input (make-array (+ (length seed) 2) :element-type '(unsigned-byte 8)))
         ;; 672 bytes = 224 iterations * 3 bytes = 448 candidate samples
         ;; Even with worst-case 81.3% acceptance, P(< 256 accepted) ≈ 0
         (xof-output (progn
                       (replace xof-input seed)
                       (setf (aref xof-input (length seed)) i)
                       (setf (aref xof-input (1+ (length seed))) j)
                       (shake128-xof xof-input 672)))
         (poly (make-ml-kem-poly))
         (n 0)
         (pos 0)
         (max-pos (- (length xof-output) 2)))
    (loop while (and (< n 256) (< pos max-pos))
          do (let* ((d1 (aref xof-output pos))
                    (d2 (aref xof-output (1+ pos)))
                    (d3 (aref xof-output (+ pos 2)))
                    (d (logior d1 (ash (logand d2 #x0f) 8)))
                    (e (logior (ash d2 -4) (ash d3 4))))
               (incf pos 3)
               (when (< d +ml-kem-q+)
                 (setf (aref poly n) d)
                 (incf n))
               (when (and (< n 256) (< e +ml-kem-q+))
                 (setf (aref poly n) e)
                 (incf n))))
    ;; Safety check - should never trigger in practice
    (when (< n 256)
      (error "sample-ntt-poly: insufficient samples after rejection sampling"))
    poly))

(defun sample-poly-cbd (seed eta n)
  "Sample polynomial from CBD using PRF(seed, n)."
  (let* ((prf-input (make-array (1+ (length seed)) :element-type '(unsigned-byte 8)))
         (prf-output (progn
                       (replace prf-input seed)
                       (setf (aref prf-input (length seed)) n)
                       (shake256-xof prf-input (* eta 64)))))
    (cbd prf-output eta)))

;;;; =========================================================================
;;;; K-PKE: IND-CPA Public Key Encryption
;;;; =========================================================================

(defun k-pke-keygen (d)
  "K-PKE key generation from seed D (32 bytes).
Returns (values public-key secret-key) as byte vectors."
  (declare (type (simple-array (unsigned-byte 8) (32)) d))
  (let* ((k +ml-kem-768-k+)
         ;; G(d) = (rho, sigma)
         (g-output (sha3-512-hash d))
         (rho (subseq g-output 0 32))
         (sigma (subseq g-output 32 64))
         ;; Generate matrix A in NTT domain
         (a-hat (make-array (list k k)))
         ;; Generate secret vector s
         (s (make-array k))
         (s-hat (make-array k))
         ;; Generate error vector e
         (e (make-array k))
         (e-hat (make-array k))
         ;; Public key t-hat = A * s + e (in NTT domain)
         (t-hat (make-array k)))
    ;; Sample A matrix
    (dotimes (i k)
      (dotimes (j k)
        (setf (aref a-hat i j) (sample-ntt-poly rho j i))))
    ;; Sample secret s and error e
    (dotimes (i k)
      (setf (aref s i) (sample-poly-cbd sigma +ml-kem-768-eta1+ i))
      (setf (aref s-hat i) (ntt-forward (poly-copy (aref s i))))
      (setf (aref e i) (sample-poly-cbd sigma +ml-kem-768-eta1+ (+ k i)))
      (setf (aref e-hat i) (ntt-forward (poly-copy (aref e i)))))
    ;; Compute t = A * s + e in NTT domain
    (dotimes (i k)
      (let ((ti (make-ml-kem-poly)))
        (dotimes (j k)
          (poly-add! ti (poly-mul-ntt (aref a-hat i j) (aref s-hat j))))
        (poly-add! ti (aref e-hat i))
        (setf (aref t-hat i) ti)))
    ;; Encode public key: t || rho
    (let ((pk (make-array +ml-kem-768-ek-size+ :element-type '(unsigned-byte 8))))
      (let ((pos 0))
        (dotimes (i k)
          (let ((encoded (poly-to-bytes (aref t-hat i))))
            (replace pk encoded :start1 pos)
            (incf pos 384)))
        (replace pk rho :start1 pos))
      ;; Encode secret key: s (in NTT domain)
      (let ((sk (make-array (* 384 k) :element-type '(unsigned-byte 8))))
        (let ((pos 0))
          (dotimes (i k)
            (let ((encoded (poly-to-bytes (aref s-hat i))))
              (replace sk encoded :start1 pos)
              (incf pos 384))))
        (values pk sk)))))

(defun k-pke-encrypt (pk m r)
  "K-PKE encryption of message M (32 bytes) using public key PK and randomness R.
Returns ciphertext as byte vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) pk m r))
  (let* ((k +ml-kem-768-k+)
         (du +ml-kem-768-du+)
         (dv +ml-kem-768-dv+)
         ;; Decode public key
         (t-hat (make-array k))
         (rho (subseq pk (* 384 k)))
         ;; Generate matrix A^T in NTT domain
         (a-hat-t (make-array (list k k)))
         ;; Sample r vector
         (r-vec (make-array k))
         (r-hat (make-array k))
         ;; Sample e1, e2
         (e1 (make-array k))
         (e2 (sample-poly-cbd r +ml-kem-768-eta2+ (* 2 k))))
    ;; Decode t from public key
    (dotimes (i k)
      (setf (aref t-hat i) (bytes-to-poly pk (* i 384))))
    ;; Sample A^T matrix (transpose)
    (dotimes (i k)
      (dotimes (j k)
        (setf (aref a-hat-t i j) (sample-ntt-poly rho i j))))
    ;; Sample r and e1
    (dotimes (i k)
      (setf (aref r-vec i) (sample-poly-cbd r +ml-kem-768-eta1+ i))
      (setf (aref r-hat i) (ntt-forward (poly-copy (aref r-vec i))))
      (setf (aref e1 i) (sample-poly-cbd r +ml-kem-768-eta2+ (+ k i))))
    ;; Compute u = A^T * r + e1
    (let ((u (make-array k)))
      (dotimes (i k)
        (let ((ui (make-ml-kem-poly)))
          (dotimes (j k)
            (poly-add! ui (ntt-inverse (poly-mul-ntt (aref a-hat-t i j)
                                                     (aref r-hat j)))))
          (poly-add! ui (aref e1 i))
          (setf (aref u i) ui)))
      ;; Compute v = t^T * r + e2 + decode(m)
      (let ((v (poly-copy e2)))
        ;; t^T * r
        (dotimes (i k)
          (poly-add! v (ntt-inverse (poly-mul-ntt (aref t-hat i) (aref r-hat i)))))
        ;; Add message (scaled by q/2)
        (dotimes (i 256)
          (let ((bit (logand (ash (aref m (floor i 8)) (- (logand i 7))) 1)))
            (setf (aref v i)
                  (ct-cond-sub-q (+ (aref v i) (* bit (ash (1+ +ml-kem-q+) -1)))))))
        ;; Encode ciphertext
        (let ((ct (make-array +ml-kem-768-ct-size+ :element-type '(unsigned-byte 8)))
              (pos 0))
          (dotimes (i k)
            (let ((compressed (poly-compress-to-bytes (aref u i) du)))
              (replace ct compressed :start1 pos)
              (incf pos (length compressed))))
          (let ((compressed (poly-compress-to-bytes v dv)))
            (replace ct compressed :start1 pos))
          ct)))))

(defun k-pke-decrypt (sk ct)
  "K-PKE decryption of ciphertext CT using secret key SK.
Returns 32-byte message."
  (declare (type (simple-array (unsigned-byte 8) (*)) sk ct))
  (let* ((k +ml-kem-768-k+)
         (du +ml-kem-768-du+)
         (dv +ml-kem-768-dv+)
         ;; Decode secret key (s in NTT domain)
         (s-hat (make-array k))
         ;; Decode ciphertext
         (u (make-array k))
         (u-bytes-per (* 32 du))
         (v-offset (* k u-bytes-per)))
    ;; Decode s from secret key
    (dotimes (i k)
      (setf (aref s-hat i) (bytes-to-poly sk (* i 384))))
    ;; Decode u from ciphertext
    (let ((pos 0))
      (dotimes (i k)
        (setf (aref u i) (bytes-decompress-to-poly ct du pos))
        (incf pos u-bytes-per)))
    ;; Decode v from ciphertext
    (let ((v (bytes-decompress-to-poly ct dv v-offset)))
      ;; Compute w = v - s^T * u
      (let ((w (poly-copy v)))
        (dotimes (i k)
          (let ((term (ntt-inverse (poly-mul-ntt (aref s-hat i)
                                                 (ntt-forward (poly-copy (aref u i)))))))
            (setf w (poly-sub w term))))
        ;; Decode message bits
        (let ((m (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
          (dotimes (i 256)
            (let* ((coeff (aref w i))
                   ;; Round to nearest 0 or q/2
                   (bit (if (< (abs (- coeff (ash +ml-kem-q+ -1)))
                               (abs (centered-mod coeff)))
                            1 0)))
              (setf (aref m (floor i 8))
                    (logior (aref m (floor i 8))
                            (ash bit (mod i 8))))))
          m)))))

;;;; =========================================================================
;;;; ML-KEM: Key Encapsulation Mechanism
;;;; =========================================================================

(defun ml-kem-768-keygen ()
  "Generate ML-KEM-768 key pair.
Returns (values encapsulation-key decapsulation-key)."
  (let* ((d (ironclad:random-data 32))
         (z (ironclad:random-data 32)))
    (ml-kem-768-keygen-internal d z)))

(defun ml-kem-768-keygen-internal (d z)
  "Internal keygen with explicit randomness for testing.
Returns (values encapsulation-key decapsulation-key)."
  (multiple-value-bind (ek-pke dk-pke)
      (k-pke-keygen d)
    (let* ((ek ek-pke)
           ;; dk = dk_pke || ek || H(ek) || z
           (h-ek (sha3-256-hash ek))
           (dk (make-array +ml-kem-768-dk-size+ :element-type '(unsigned-byte 8))))
      (let ((pos 0))
        (replace dk dk-pke :start1 pos)
        (incf pos (length dk-pke))
        (replace dk ek :start1 pos)
        (incf pos (length ek))
        (replace dk h-ek :start1 pos)
        (incf pos 32)
        (replace dk z :start1 pos))
      (values ek dk))))

(defun ml-kem-768-encaps (ek)
  "Encapsulate a shared secret using encapsulation key EK.
Returns (values shared-secret ciphertext)."
  (let ((m (ironclad:random-data 32)))
    (ml-kem-768-encaps-internal ek m)))

(defun ml-kem-768-encaps-internal (ek m)
  "Internal encapsulation with explicit randomness for testing."
  (let* (;; (K, r) = G(m || H(ek))
         (h-ek (sha3-256-hash ek))
         (g-input (make-array 64 :element-type '(unsigned-byte 8)))
         (g-output (progn
                     (replace g-input m)
                     (replace g-input h-ek :start1 32)
                     (sha3-512-hash g-input)))
         (k (subseq g-output 0 32))
         (r (subseq g-output 32 64))
         ;; c = Encrypt(ek, m, r)
         (c (k-pke-encrypt ek m r)))
    (values k c)))

(defun ml-kem-768-decaps (dk c)
  "Decapsulate shared secret from ciphertext C using decapsulation key DK.
Returns 32-byte shared secret."
  (let* ((k +ml-kem-768-k+)
         ;; Parse dk = dk_pke || ek || H(ek) || z
         (dk-pke-len (* 384 k))
         (ek-len +ml-kem-768-ek-size+)
         (dk-pke (subseq dk 0 dk-pke-len))
         (ek (subseq dk dk-pke-len (+ dk-pke-len ek-len)))
         (h-ek (subseq dk (+ dk-pke-len ek-len) (+ dk-pke-len ek-len 32)))
         (z (subseq dk (+ dk-pke-len ek-len 32)))
         ;; m' = Decrypt(dk_pke, c)
         (m-prime (k-pke-decrypt dk-pke c))
         ;; (K', r') = G(m' || H(ek))
         (g-input (make-array 64 :element-type '(unsigned-byte 8)))
         (g-output (progn
                     (replace g-input m-prime)
                     (replace g-input h-ek :start1 32)
                     (sha3-512-hash g-input)))
         (k-prime (subseq g-output 0 32))
         (r-prime (subseq g-output 32 64))
         ;; c' = Encrypt(ek, m', r')
         (c-prime (k-pke-encrypt ek m-prime r-prime)))
    ;; Constant-time comparison and selection
    (if (constant-time-equal c c-prime)
        k-prime
        ;; Implicit rejection: return J(z || c)
        (shake256-xof (concatenate '(vector (unsigned-byte 8)) z c) 32))))

(defun constant-time-equal (a b)
  "Constant-time comparison of byte vectors A and B."
  (declare (type (simple-array (unsigned-byte 8) (*)) a b)
           (optimize speed))
  (and (= (length a) (length b))
       (zerop (loop with diff of-type (unsigned-byte 8) = 0
                    for i from 0 below (length a)
                    do (setf diff (logior diff (logxor (aref a i) (aref b i))))
                    finally (return diff)))))

;;;; =========================================================================
;;;; TLS Hybrid Key Exchange: X25519MLKEM768 (FIPS 203)
;;;; Per draft-kwiatkowski-tls-ecdhe-mlkem, ML-KEM comes FIRST
;;;; =========================================================================

(defstruct hybrid-key-exchange
  "Hybrid key exchange state for X25519MLKEM768."
  (x25519 nil)          ; X25519 key exchange state
  (ml-kem-ek nil)       ; ML-KEM encapsulation key (for client, sent to server)
  (ml-kem-dk nil)       ; ML-KEM decapsulation key (for client, kept secret)
  (ml-kem-ct nil))      ; ML-KEM ciphertext (for server response)

(defun make-hybrid-x25519-ml-kem-768 ()
  "Generate hybrid X25519MLKEM768 key exchange for client."
  (let ((x25519-kex (make-x25519-key-exchange)))
    (multiple-value-bind (ek dk)
        (ml-kem-768-keygen)
      (make-hybrid-key-exchange :x25519 x25519-kex
                                :ml-kem-ek ek
                                :ml-kem-dk dk))))

(defun hybrid-public-key (kex)
  "Return the hybrid public key (ML-KEM ek || X25519) for ClientHello.
Per X25519MLKEM768 spec, ML-KEM comes first."
  (let ((ml-kem-ek (hybrid-key-exchange-ml-kem-ek kex))
        (x25519-pk (key-exchange-public-key (hybrid-key-exchange-x25519 kex))))
    (concatenate '(vector (unsigned-byte 8)) ml-kem-ek x25519-pk)))

(defun hybrid-compute-shared-secret (kex server-share)
  "Compute hybrid shared secret from server's key share.
SERVER-SHARE is ML-KEM ciphertext (1088 bytes) || X25519 public key (32 bytes).
Returns 64-byte shared secret (ML-KEM ss || X25519 ss)."
  (let* ((ml-kem-ct (subseq server-share 0 1088))
         (x25519-pk (subseq server-share 1088))
         (ml-kem-ss (ml-kem-768-decaps
                     (hybrid-key-exchange-ml-kem-dk kex)
                     ml-kem-ct))
         (x25519-ss (x25519-compute-shared-secret
                     (hybrid-key-exchange-x25519 kex)
                     x25519-pk)))
    (concatenate '(vector (unsigned-byte 8)) ml-kem-ss x25519-ss)))

(defun hybrid-server-encaps (client-share)
  "Server-side hybrid encapsulation.
CLIENT-SHARE is ML-KEM ek (1184 bytes) || X25519 public key (32 bytes).
Returns (values shared-secret server-share)."
  (let* ((ml-kem-ek (subseq client-share 0 1184))
         (x25519-client-pk (subseq client-share 1184))
         ;; X25519 key exchange
         (x25519-kex (make-x25519-key-exchange))
         (x25519-ss (x25519-compute-shared-secret x25519-kex x25519-client-pk))
         (x25519-server-pk (key-exchange-public-key x25519-kex)))
    ;; ML-KEM encapsulation
    (multiple-value-bind (ml-kem-ss ml-kem-ct)
        (ml-kem-768-encaps ml-kem-ek)
      (let ((shared-secret (concatenate '(vector (unsigned-byte 8))
                                        ml-kem-ss x25519-ss))
            (server-share (concatenate '(vector (unsigned-byte 8))
                                       ml-kem-ct x25519-server-pk)))
        (values shared-secret server-share)))))
