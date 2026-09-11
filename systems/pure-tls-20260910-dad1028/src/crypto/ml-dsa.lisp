;;; ml-dsa.lisp --- ML-DSA-65 implementation (FIPS 204)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Implements ML-DSA-65 (Module-Lattice-Based Digital Signature Algorithm)
;;; for post-quantum signature verification in TLS 1.3.
;;;
;;; References:
;;; - FIPS 204: Module-Lattice-Based Digital Signature Standard
;;; - draft-ietf-tls-mldsa: Use of ML-DSA in TLS 1.3

(in-package #:pure-tls)

;;;; =========================================================================
;;;; ML-DSA Parameters (FIPS 204)
;;;; =========================================================================

;; ML-DSA uses a different modulus than ML-KEM!
;; ML-DSA: q = 2^23 - 2^13 + 1 = 8380417
;; ML-KEM: q = 3329
;; Both use n = 256

(defconstant +ml-dsa-q+ 8380417
  "ML-DSA modulus: 2^23 - 2^13 + 1 = 8380417.")

(defconstant +ml-dsa-n+ 256
  "Polynomial degree (same as ML-KEM).")

;;;; =========================================================================
;;;; ML-DSA-65 Parameters (FIPS 204, Table 1)
;;;; =========================================================================

(defconstant +ml-dsa-65-k+ 6
  "Number of rows in matrix A (module rank).")

(defconstant +ml-dsa-65-l+ 5
  "Number of columns in matrix A.")

(defconstant +ml-dsa-65-eta+ 4
  "Maximum coefficient magnitude for secret key polynomials s1, s2.")

(defconstant +ml-dsa-65-tau+ 49
  "Number of ±1 coefficients in the challenge polynomial c.")

(defconstant +ml-dsa-65-beta+ 196
  "Bound for checking z coefficients: tau * eta = 49 * 4.")

(defconstant +ml-dsa-65-gamma1+ (ash 1 19)
  "Range for mask polynomial y: 2^19 = 524288.")

(defconstant +ml-dsa-65-gamma2+ (floor (1- +ml-dsa-q+) 32)
  "Low-order rounding range: (q-1)/32 = 261888.")

(defconstant +ml-dsa-65-omega+ 55
  "Maximum number of 1s in hint polynomial h.")

(defconstant +ml-dsa-65-d+ 13
  "Dropped bits from t for public key compression.")

;; Derived sizes (FIPS 204, Table 2)
(defconstant +ml-dsa-65-pk-size+ 1952
  "Public key size: 32 + 32*k*(bitlen(q-1) - d) = 32 + 32*6*10 = 1952 bytes.")

(defconstant +ml-dsa-65-sk-size+ 4032
  "Secret key size in bytes.")

(defconstant +ml-dsa-65-sig-size+ 3309
  "Signature size in bytes.")

(defconstant +ml-dsa-65-ctilde-bytes+ 48
  "Size of c_tilde (commitment hash) in bytes: lambda/4 = 192/4 = 48.")

;;;; =========================================================================
;;;; ML-DSA Polynomial Type and Basic Operations
;;;; =========================================================================

(deftype ml-dsa-poly ()
  "Polynomial with n=256 coefficients modulo ML-DSA q."
  '(simple-array (unsigned-byte 32) (256)))

(defun make-ml-dsa-poly ()
  "Create a new zero polynomial for ML-DSA."
  (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0))

(defun ml-dsa-poly ()
  "Create a new zero polynomial for ML-DSA."
  (make-ml-dsa-poly))

(defun ml-dsa-poly-vector (len)
  "Create a vector of LEN zero polynomials."
  (let ((v (make-array len)))
    (dotimes (i len v)
      (setf (aref v i) (ml-dsa-poly)))))

(defun ml-dsa-poly-copy (poly)
  "Create a copy of polynomial POLY."
  (let ((result (make-ml-dsa-poly)))
    (dotimes (i 256 result)
      (setf (aref result i) (aref poly i)))))

(defun ml-dsa-poly-add (a b)
  "Add two ML-DSA polynomials."
  (let ((result (make-ml-dsa-poly)))
    (dotimes (i 256 result)
      (setf (aref result i) (mod (+ (aref a i) (aref b i)) +ml-dsa-q+)))))

(defun ml-dsa-poly-add! (a b)
  "Add polynomial B to polynomial A, modifying A in place."
  (dotimes (i 256 a)
    (setf (aref a i) (mod (+ (aref a i) (aref b i)) +ml-dsa-q+))))

(defun ml-dsa-poly-sub (a b)
  "Subtract polynomial B from A."
  (let ((result (make-ml-dsa-poly)))
    (dotimes (i 256 result)
      (setf (aref result i) (mod (- (aref a i) (aref b i)) +ml-dsa-q+)))))

(defun ml-dsa-poly-mul-schoolbook (a b)
  "Multiply two ML-DSA polynomials using schoolbook method in Z_q[x]/(x^256+1).
   Note: This is O(n^2) and slow. For production, NTT should be used."
  (let ((result (make-ml-dsa-poly)))
    (dotimes (i 256)
      (dotimes (j 256)
        (let* ((k (mod (+ i j) 256))
               (prod (mod (* (aref a i) (aref b j)) +ml-dsa-q+)))
          (if (>= (+ i j) 256)
              ;; Reduce by x^256 = -1
              (setf (aref result k) (mod (- (aref result k) prod) +ml-dsa-q+))
              (setf (aref result k) (mod (+ (aref result k) prod) +ml-dsa-q+))))))
    result))

;;;; =========================================================================
;;;; Modular Arithmetic Extensions for ML-DSA
;;;; =========================================================================

(defun mod-pm (x q)
  "Reduce X modulo Q to centered representation in [-(q-1)/2, (q-1)/2]."
  (let ((r (mod x q)))
    (if (> r (ash q -1))
        (- r q)
        r)))

(defun ml-dsa-mod-q (x)
  "Reduce X modulo ML-DSA q=8380417."
  (mod x +ml-dsa-q+))

(defun ml-dsa-mod-q-signed (x)
  "Reduce X modulo ML-DSA q=8380417 to centered representation."
  (mod-pm x +ml-dsa-q+))

;;;; =========================================================================
;;;; Power2Round and Decompose (FIPS 204, Section 8.3)
;;;; =========================================================================

(defun power2round (r)
  "Decompose R into (r1, r0) where r = r1 * 2^d + r0.
   Returns (values r1 r0) with r0 in [-(2^(d-1)-1), 2^(d-1)]."
  (declare (type integer r))
  (let* ((r+ (mod r +ml-dsa-q+))
         (d +ml-dsa-65-d+)
         (r0 (mod-pm r+ (ash 1 d))) ; r mod 2^d, centered
         (r1 (ash (- r+ r0) (- d))))  ; (r - r0) / 2^d
    (values r1 r0)))

(defun decompose (r)
  "Decompose R into (r1, r0) for hint computation.
   FIPS 204 Algorithm 35.
   Returns (values r1 r0) with r0 in [-gamma2, gamma2]."
  (declare (type integer r))
  (let* ((r+ (mod r +ml-dsa-q+))
         (gamma2 +ml-dsa-65-gamma2+)
         (two-gamma2 (* 2 gamma2))
         (r0 (mod-pm r+ two-gamma2)))
    (cond
      ;; Case: r+ - r0 = q - 1, set r1 = 0, r0 = r0 - 1
      ((= (- r+ r0) (1- +ml-dsa-q+))
       (values 0 (1- r0)))
      ;; Normal case
      (t
       (values (floor (- r+ r0) two-gamma2) r0)))))

(defun highbits (r)
  "Return the high bits of R (r1 from decompose)."
  (multiple-value-bind (r1 r0) (decompose r)
    (declare (ignore r0))
    r1))

(defun lowbits (r)
  "Return the low bits of R (r0 from decompose)."
  (multiple-value-bind (r1 r0) (decompose r)
    (declare (ignore r1))
    r0))

;;;; =========================================================================
;;;; Hint Functions (FIPS 204, Section 8.4)
;;;; =========================================================================

(defun make-hint (z r)
  "Compute hint bit indicating whether adding Z to R changes highbits.
   FIPS 204 Algorithm 36.
   Returns 1 if highbits differ, 0 otherwise."
  (let ((r1 (highbits r))
        (v1 (highbits (+ r z))))
    (if (= r1 v1) 0 1)))

(defun use-hint (h r)
  "Recover high bits using hint H.
   FIPS 204 Algorithm 37.
   Returns the corrected high bits value."
  (let* ((gamma2 +ml-dsa-65-gamma2+)
         (m (floor (1- +ml-dsa-q+) (* 2 gamma2))))  ; m = 16 for ML-DSA-65
    (multiple-value-bind (r1 r0) (decompose r)
      (cond
        ((zerop h) r1)
        ((> r0 0) (mod (1+ r1) m))
        (t (mod (1- r1) m))))))

;;;; =========================================================================
;;;; Polynomial Encoding/Decoding (FIPS 204, Section 8.1)
;;;; =========================================================================

(defun encode-poly-eta (poly eta)
  "Encode polynomial with coefficients in [-eta, eta] to bytes.
   FIPS 204: For eta=4, each coefficient uses 4 bits."
  (declare (type ml-dsa-poly poly))
  (let* ((bits-per-coeff (if (= eta 2) 3 4))
         (bytes-out (/ (* 256 bits-per-coeff) 8))
         (result (make-octet-vector bytes-out))
         (bit-pos 0))
    (dotimes (i 256 result)
      (let* ((coeff (aref poly i))
             ;; Map from Z_q centered to [0, 2*eta]
             (mapped (- eta (ml-dsa-mod-q-signed coeff))))
        ;; Pack bits
        (dotimes (b bits-per-coeff)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf (aref result byte-idx)
                  (logior (aref result byte-idx)
                          (ash (logand (ash mapped (- b)) 1) bit-idx))))
          (incf bit-pos))))))

(defun decode-poly-eta (data eta)
  "Decode bytes to polynomial with coefficients in [-eta, eta]."
  (let* ((bits-per-coeff (if (= eta 2) 3 4))
         (poly (ml-dsa-poly))
         (bit-pos 0))
    (dotimes (i 256 poly)
      (let ((mapped 0))
        (dotimes (b bits-per-coeff)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf mapped
                  (logior mapped
                          (ash (logand (ash (aref data byte-idx) (- bit-idx)) 1) b))))
          (incf bit-pos))
        ;; Map from [0, 2*eta] back to centered coefficient
        (setf (aref poly i) (mod (- eta mapped) +ml-dsa-q+))))))

(defun encode-poly-t1 (poly)
  "Encode t1 polynomial (10 bits per coefficient) to bytes."
  (declare (type ml-dsa-poly poly))
  (let ((result (make-octet-vector 320))  ; 256 * 10 / 8 = 320
        (bit-pos 0))
    (dotimes (i 256 result)
      (let ((coeff (aref poly i)))
        (dotimes (b 10)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf (aref result byte-idx)
                  (logior (aref result byte-idx)
                          (ash (logand (ash coeff (- b)) 1) bit-idx))))
          (incf bit-pos))))))

(defun decode-poly-t1 (data &optional (offset 0))
  "Decode bytes to t1 polynomial (10 bits per coefficient)."
  (let ((poly (ml-dsa-poly))
        (bit-pos (* offset 8)))
    (dotimes (i 256 poly)
      (let ((coeff 0))
        (dotimes (b 10)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf coeff
                  (logior coeff
                          (ash (logand (ash (aref data byte-idx) (- bit-idx)) 1) b))))
          (incf bit-pos))
        (setf (aref poly i) coeff)))))

(defun encode-poly-z (poly)
  "Encode z polynomial (coefficients in [-gamma1+1, gamma1]) to bytes.
   For ML-DSA-65: gamma1 = 2^19, so 20 bits per coefficient."
  (declare (type ml-dsa-poly poly))
  (let ((result (make-octet-vector 640))  ; 256 * 20 / 8 = 640
        (bit-pos 0)
        (gamma1 +ml-dsa-65-gamma1+))
    (dotimes (i 256 result)
      (let* ((coeff (ml-dsa-mod-q-signed (aref poly i)))
             ;; Map to [0, 2*gamma1-1]
             (mapped (- gamma1 1 coeff)))
        (dotimes (b 20)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf (aref result byte-idx)
                  (logior (aref result byte-idx)
                          (ash (logand (ash mapped (- b)) 1) bit-idx))))
          (incf bit-pos))))))

(defun decode-poly-z (data &optional (offset 0))
  "Decode bytes to z polynomial."
  (let ((poly (ml-dsa-poly))
        (bit-pos (* offset 8))
        (gamma1 +ml-dsa-65-gamma1+))
    (dotimes (i 256 poly)
      (let ((mapped 0))
        (dotimes (b 20)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf mapped
                  (logior mapped
                          (ash (logand (ash (aref data byte-idx) (- bit-idx)) 1) b))))
          (incf bit-pos))
        ;; Map back from [0, 2*gamma1-1] to centered
        (setf (aref poly i) (mod (- gamma1 1 mapped) +ml-dsa-q+))))
    poly))

(defun encode-poly-t0 (poly)
  "Encode t0 polynomial (coefficients in [-2^(d-1), 2^(d-1)]) to bytes.
   For ML-DSA-65: d=13, so 13 bits per coefficient, 416 bytes per polynomial."
  (declare (type ml-dsa-poly poly))
  (let ((result (make-octet-vector 416))  ; 256 * 13 / 8 = 416
        (bit-pos 0)
        (half-range (ash 1 (1- +ml-dsa-65-d+))))  ; 2^12 = 4096
    (dotimes (i 256 result)
      (let* ((coeff (ml-dsa-mod-q-signed (aref poly i)))
             ;; Map from [-4096, 4096] to [0, 8191] = [0, 2^13-1]
             (mapped (- half-range coeff)))
        (dotimes (b 13)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf (aref result byte-idx)
                  (logior (aref result byte-idx)
                          (ash (logand (ash mapped (- b)) 1) bit-idx))))
          (incf bit-pos))))))

(defun decode-poly-t0 (data &optional (offset 0))
  "Decode bytes to t0 polynomial (13 bits per coefficient)."
  (let ((poly (ml-dsa-poly))
        (bit-pos (* offset 8))
        (half-range (ash 1 (1- +ml-dsa-65-d+))))  ; 2^12 = 4096
    (dotimes (i 256 poly)
      (let ((mapped 0))
        (dotimes (b 13)
          (let ((byte-idx (floor bit-pos 8))
                (bit-idx (mod bit-pos 8)))
            (setf mapped
                  (logior mapped
                          (ash (logand (ash (aref data byte-idx) (- bit-idx)) 1) b))))
          (incf bit-pos))
        ;; Map back from [0, 8191] to centered [-4096, 4096], then mod q
        (setf (aref poly i) (mod (- half-range mapped) +ml-dsa-q+))))
    poly))

;;;; =========================================================================
;;;; Sampling Functions (FIPS 204, Section 8.2)
;;;; =========================================================================

(defun sample-in-ball (seed)
  "Sample a polynomial c with exactly tau coefficients in {-1, 1}.
   FIPS 204 Algorithm 30.
   SEED is the c_tilde commitment (48 bytes for ML-DSA-65)."
  (let ((c (ml-dsa-poly))
        (tau +ml-dsa-65-tau+)
        ;; Use SHAKE256 to generate random bytes
        (xof (ironclad:make-digest :shake256 :output-length 1024)))
    (ironclad:update-digest xof seed)
    (let ((h (ironclad:produce-digest xof)))
      ;; First 8 bytes give sign bits
      (let ((signs (decode-uint64 h 0))
            (pos 8))
        ;; Sample tau positions
        (loop for i from (- 256 tau) below 256
              for j = (loop for byte = (aref h pos)
                            do (incf pos)
                            when (<= byte i) return byte)
              do ;; Swap c[i] and c[j]
                 (setf (aref c i) (aref c j))
                 ;; Set c[j] = ±1 based on sign bit
                 (setf (aref c j)
                       (if (zerop (logand signs 1))
                           1
                           (1- +ml-dsa-q+)))  ; -1 mod q
                 (setf signs (ash signs -1)))))
    c))

(defun ml-dsa-rej-uniform-poly (seed)
  "Sample a polynomial with uniform coefficients in [0, q-1] using rejection sampling.
   FIPS 204 Algorithm 24: RejNTTPoly.
   SEED is the 34-byte input (rho || indices).
   Returns polynomial in standard domain (not NTT)."
  (let ((poly (make-ml-dsa-poly))
        ;; Use SHAKE128 for matrix expansion per FIPS 204
        (xof (ironclad:make-digest :shake128 :output-length 4096))
        (j 0))  ; coefficient index
    (ironclad:update-digest xof seed)
    (let ((stream (ironclad:produce-digest xof))
          (pos 0))
      ;; Read 3 bytes at a time, reject if >= q
      (loop while (< j 256)
            do (when (>= (+ pos 3) (length stream))
                 ;; Need more random bytes - extend the stream
                 (setf xof (ironclad:make-digest :shake128 :output-length 8192))
                 (ironclad:update-digest xof seed)
                 (setf stream (ironclad:produce-digest xof))
                 (setf pos 0))
               ;; Read 3 bytes as little-endian, mask to 23 bits
               (let* ((b0 (aref stream pos))
                      (b1 (aref stream (1+ pos)))
                      (b2 (aref stream (+ pos 2)))
                      (coeff (logand (logior b0 (ash b1 8) (ash b2 16))
                                     #x7FFFFF)))  ; 23-bit mask
                 (incf pos 3)
                 (when (< coeff +ml-dsa-q+)
                   (setf (aref poly j) coeff)
                   (incf j)))))
    poly))

(defun expand-a (rho)
  "Expand seed RHO into k×l matrix A.
   FIPS 204 Algorithm 32: ExpandA.
   Note: Returns polynomials in standard domain for schoolbook multiplication."
  (let ((a (make-array (list +ml-dsa-65-k+ +ml-dsa-65-l+))))
    (dotimes (i +ml-dsa-65-k+ a)
      (dotimes (j +ml-dsa-65-l+)
        ;; Construct seed: rho || j || i (each index is 1 byte per FIPS 204)
        (let ((seed (make-octet-vector 34)))
          (replace seed rho)
          (setf (aref seed 32) j)
          (setf (aref seed 33) i)
          (setf (aref a i j) (ml-dsa-rej-uniform-poly seed)))))))

(defun ml-dsa-cbd (bytes eta)
  "Sample polynomial from centered binomial distribution with parameter ETA.
   Returns polynomial with coefficients in [-eta, eta] stored mod ML-DSA q.
   For eta=4, uses 8 bits per coefficient (256 bytes for 256 coefficients)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type (integer 1 4) eta))
  (let ((poly (make-ml-dsa-poly)))
    (dotimes (i 256 poly)
      (let* ((byte-offset (* i eta 2 (/ 1 8)))
             (a-bits 0)
             (b-bits 0))
        ;; Sum eta bits for a and eta bits for b
        (dotimes (j eta)
          (let* ((bit-idx-a (+ (* i 2 eta) j))
                 (bit-idx-b (+ (* i 2 eta) eta j))
                 (byte-idx-a (floor bit-idx-a 8))
                 (byte-idx-b (floor bit-idx-b 8))
                 (bit-pos-a (mod bit-idx-a 8))
                 (bit-pos-b (mod bit-idx-b 8)))
            (incf a-bits (logand (ash (aref bytes byte-idx-a) (- bit-pos-a)) 1))
            (incf b-bits (logand (ash (aref bytes byte-idx-b) (- bit-pos-b)) 1))))
        ;; Coefficient = a - b, stored mod q
        (setf (aref poly i) (mod (- a-bits b-bits) +ml-dsa-q+))))))

(defun sample-poly-cbd-eta (seed nonce)
  "Sample polynomial with coefficients in [-eta, eta] using CBD.
   FIPS 204 uses SHAKE256 to generate the random bits."
  (let* ((eta +ml-dsa-65-eta+)
         (input-bytes (* eta 64))  ; 256 bytes for eta=4
         (xof (ironclad:make-digest :shake256 :output-length input-bytes))
         (nonce-byte (make-octet-vector 1)))
    ;; SHAKE256(seed || nonce)
    (setf (aref nonce-byte 0) (mod nonce 256))
    (ironclad:update-digest xof seed)
    (ironclad:update-digest xof nonce-byte)
    (let ((h (ironclad:produce-digest xof)))
      (ml-dsa-cbd h eta))))

;;;; =========================================================================
;;;; Matrix/Vector Operations (using ML-DSA modulus)
;;;; =========================================================================

(defun ml-dsa-matrix-vector-mul (a s)
  "Compute A * s where A is k×l matrix, s is l-vector.
   Uses schoolbook multiplication with ML-DSA modulus.
   Returns k-vector."
  (let ((result (ml-dsa-poly-vector +ml-dsa-65-k+)))
    (dotimes (i +ml-dsa-65-k+ result)
      (dotimes (j +ml-dsa-65-l+)
        (let ((prod (ml-dsa-poly-mul-schoolbook (aref a i j) (aref s j))))
          (ml-dsa-poly-add! (aref result i) prod))))))

(defun ml-dsa-vector-add (a b)
  "Add two polynomial vectors element-wise using ML-DSA modulus."
  (let* ((len (length a))
         (result (ml-dsa-poly-vector len)))
    (dotimes (i len result)
      (setf (aref result i) (ml-dsa-poly-add (aref a i) (aref b i))))))

(defun ml-dsa-vector-sub (a b)
  "Subtract polynomial vector B from A element-wise using ML-DSA modulus."
  (let* ((len (length a))
         (result (ml-dsa-poly-vector len)))
    (dotimes (i len result)
      (setf (aref result i) (ml-dsa-poly-sub (aref a i) (aref b i))))))

(defun ml-dsa-scalar-poly-mul (c v)
  "Multiply each polynomial in vector V by scalar polynomial C.
   Uses schoolbook multiplication with ML-DSA modulus."
  (let* ((len (length v))
         (result (ml-dsa-poly-vector len)))
    (dotimes (i len result)
      (setf (aref result i) (ml-dsa-poly-mul-schoolbook c (aref v i))))))

;;;; =========================================================================
;;;; Key Generation (FIPS 204, Algorithm 1)
;;;; =========================================================================

(defun ml-dsa-65-keygen (&optional seed)
  "Generate ML-DSA-65 key pair.
   Returns (values public-key secret-key).
   If SEED is provided (32 bytes), key generation is deterministic."
  (let* ((xi (or seed (random-bytes 32)))
         ;; H(xi) -> rho || rho' || K (using SHAKE256)
         (xof (ironclad:make-digest :shake256 :output-length 128))
         (_ (ironclad:update-digest xof xi))
         (expanded (ironclad:produce-digest xof))
         (rho (subseq expanded 0 32))      ; Public seed for A
         (rho-prime (subseq expanded 32 96)) ; 64 bytes for signing
         (k-seed (subseq expanded 96 128))) ; Secret for rejection
    (declare (ignore _))
    ;; Expand A from rho
    (let ((a (expand-a rho)))
      ;; Sample secret vectors s1 (l polys), s2 (k polys)
      (let ((s1 (ml-dsa-poly-vector +ml-dsa-65-l+))
            (s2 (ml-dsa-poly-vector +ml-dsa-65-k+))
            (nonce 0))
        ;; Sample s1
        (dotimes (i +ml-dsa-65-l+)
          (setf (aref s1 i) (sample-poly-cbd-eta rho-prime nonce))
          (incf nonce))
        ;; Sample s2
        (dotimes (i +ml-dsa-65-k+)
          (setf (aref s2 i) (sample-poly-cbd-eta rho-prime nonce))
          (incf nonce))
        ;; Compute t = A*s1 + s2 using schoolbook multiplication
        (let* ((as1 (ml-dsa-matrix-vector-mul a s1))
               (t-vec (ml-dsa-vector-add as1 s2))
               ;; Split t into t1 (high bits) and t0 (low bits)
               (t1 (ml-dsa-poly-vector +ml-dsa-65-k+))
               (t0 (ml-dsa-poly-vector +ml-dsa-65-k+)))
          (dotimes (i +ml-dsa-65-k+)
            (let ((ti (aref t-vec i)))
              (dotimes (j 256)
                (multiple-value-bind (hi lo) (power2round (aref ti j))
                  (setf (aref (aref t1 i) j) hi)
                  (setf (aref (aref t0 i) j) (mod lo +ml-dsa-q+))))))
          ;; Encode public key: rho || t1
          (let ((pk (make-octet-vector +ml-dsa-65-pk-size+)))
            (replace pk rho)
            (let ((offset 32))
              (dotimes (i +ml-dsa-65-k+)
                (let ((encoded (encode-poly-t1 (aref t1 i))))
                  (replace pk encoded :start1 offset)
                  (incf offset 320))))
            ;; Compute tr = H(pk)
            (let* ((tr-xof (ironclad:make-digest :shake256 :output-length 64))
                   (_ (ironclad:update-digest tr-xof pk))
                   (tr (ironclad:produce-digest tr-xof)))
              (declare (ignore _))
              ;; Encode secret key: rho || K || tr || s1 || s2 || t0
              (let ((sk (make-octet-vector +ml-dsa-65-sk-size+)))
                (replace sk rho)  ; 32 bytes
                (replace sk k-seed :start1 32)  ; 32 bytes
                (replace sk tr :start1 64)  ; 64 bytes
                ;; Encode s1 (l * 128 bytes for eta=4)
                (let ((offset 128))
                  (dotimes (i +ml-dsa-65-l+)
                    (let ((encoded (encode-poly-eta (aref s1 i) +ml-dsa-65-eta+)))
                      (replace sk encoded :start1 offset)
                      (incf offset 128)))
                  ;; Encode s2 (k * 128 bytes)
                  (dotimes (i +ml-dsa-65-k+)
                    (let ((encoded (encode-poly-eta (aref s2 i) +ml-dsa-65-eta+)))
                      (replace sk encoded :start1 offset)
                      (incf offset 128)))
                  ;; Encode t0 (k * 416 bytes for d=13 bits)
                  (dotimes (i +ml-dsa-65-k+)
                    (let ((encoded (encode-poly-t0 (aref t0 i))))
                      (replace sk encoded :start1 offset)
                      (incf offset 416))))
                (values pk sk)))))))))

;;;; =========================================================================
;;;; Signature Verification (FIPS 204, Algorithm 3)
;;;; =========================================================================

(defun ml-dsa-65-verify (pk message signature)
  "Verify ML-DSA-65 signature.
   Returns T if signature is valid, NIL otherwise."
  (when (or (/= (length pk) +ml-dsa-65-pk-size+)
            (/= (length signature) +ml-dsa-65-sig-size+))
    (return-from ml-dsa-65-verify nil))
  ;; Parse public key: rho || t1
  (let ((rho (subseq pk 0 32))
        (t1 (ml-dsa-poly-vector +ml-dsa-65-k+)))
    ;; Decode t1 polynomials
    (let ((offset 32))
      (dotimes (i +ml-dsa-65-k+)
        (setf (aref t1 i) (decode-poly-t1 pk offset))
        (incf offset 320)))
    ;; Compute tr = H(pk)
    (let* ((tr-xof (ironclad:make-digest :shake256 :output-length 64))
           (_ (ironclad:update-digest tr-xof pk))
           (tr (ironclad:produce-digest tr-xof)))
      (declare (ignore _))
      ;; Parse signature: c_tilde || z || h
      (let ((c-tilde (subseq signature 0 +ml-dsa-65-ctilde-bytes+))
            (z (ml-dsa-poly-vector +ml-dsa-65-l+)))
        ;; Decode z polynomials
        (let ((offset +ml-dsa-65-ctilde-bytes+))
          (dotimes (i +ml-dsa-65-l+)
            (setf (aref z i) (decode-poly-z signature offset))
            (incf offset 640)))
        ;; Check z coefficients are within bounds
        (dotimes (i +ml-dsa-65-l+)
          (dotimes (j 256)
            (let ((coeff (abs (ml-dsa-mod-q-signed (aref (aref z i) j)))))
              (when (>= coeff (- +ml-dsa-65-gamma1+ +ml-dsa-65-beta+))
                (return-from ml-dsa-65-verify nil)))))
        ;; Expand A from rho
        (let ((a (expand-a rho)))
          ;; Compute c from c_tilde
          (let ((c (sample-in-ball c-tilde)))
            ;; Scale t1 by 2^d
            (let ((t1-scaled (ml-dsa-poly-vector +ml-dsa-65-k+)))
              (dotimes (i +ml-dsa-65-k+)
                (let ((t1i (ml-dsa-poly-copy (aref t1 i))))
                  (dotimes (j 256)
                    (setf (aref t1i j)
                          (mod (* (aref t1i j) (ash 1 +ml-dsa-65-d+)) +ml-dsa-q+)))
                  (setf (aref t1-scaled i) t1i)))
              ;; Compute w' = Az - c*t1*2^d using schoolbook multiplication
              (let* ((az (ml-dsa-matrix-vector-mul a z))
                     (ct1 (ml-dsa-scalar-poly-mul c t1-scaled))
                     (w-prime (ml-dsa-vector-sub az ct1)))
                ;; Decode hints from signature
                (let* ((hint-offset (+ +ml-dsa-65-ctilde-bytes+ (* +ml-dsa-65-l+ 640)))
                       (h (decode-hints signature hint-offset)))
                  (unless h
                    (return-from ml-dsa-65-verify nil))
                  ;; Compute w1' using hints
                  (let ((w1-prime (ml-dsa-poly-vector +ml-dsa-65-k+)))
                    (dotimes (i +ml-dsa-65-k+)
                      (dotimes (j 256)
                        (setf (aref (aref w1-prime i) j)
                              (use-hint (aref (aref h i) j)
                                        (aref (aref w-prime i) j)))))
                    ;; Compute mu = H(tr || message)
                    (let* ((mu-xof (ironclad:make-digest :shake256 :output-length 64))
                           (_ (progn
                                (ironclad:update-digest mu-xof tr)
                                (ironclad:update-digest mu-xof message)))
                           (mu (ironclad:produce-digest mu-xof)))
                      (declare (ignore _))
                      ;; Compute c_tilde' = H(mu || w1')
                      (let* ((c-xof (ironclad:make-digest :shake256
                                                          :output-length +ml-dsa-65-ctilde-bytes+))
                             (_ (progn
                                  (ironclad:update-digest c-xof mu)
                                  ;; Encode w1' and hash
                                  (dotimes (i +ml-dsa-65-k+)
                                    (let ((encoded (encode-poly-w1 (aref w1-prime i))))
                                      (ironclad:update-digest c-xof encoded)))))
                             (c-tilde-prime (ironclad:produce-digest c-xof)))
                        (declare (ignore _))
                        ;; Verify c_tilde = c_tilde'
                        (constant-time-equal c-tilde c-tilde-prime)))))))))))))

(defun encode-poly-w1 (poly)
  "Encode w1 polynomial for hashing.
   For ML-DSA-65: coefficients in [0, 15], 4 bits each."
  (let ((result (make-octet-vector 128)))  ; 256 * 4 / 8 = 128
    (dotimes (i 128 result)
      (setf (aref result i)
            (logior (aref poly (* 2 i))
                    (ash (aref poly (1+ (* 2 i))) 4))))))

(defun encode-hints (h)
  "Encode hint vector H for signature.
   FIPS 204 format: omega + k bytes.
   First omega bytes are indices, last k bytes are counts per polynomial."
  (let* ((omega +ml-dsa-65-omega+)
         (k +ml-dsa-65-k+)
         (result (make-octet-vector (+ omega k)))
         (idx 0))
    ;; Collect hint positions for each polynomial
    (dotimes (i k)
      (dotimes (j 256)
        (when (and (< idx omega)
                   (= (aref (aref h i) j) 1))
          (setf (aref result idx) j)
          (incf idx)))
      ;; Store count for this polynomial
      (setf (aref result (+ omega i)) idx))
    result))

(defun decode-hints (data offset)
  "Decode hints from signature bytes.
   Returns hint vector h (k arrays of 256 elements each) or NIL if invalid."
  (let* ((omega +ml-dsa-65-omega+)
         (k +ml-dsa-65-k+)
         (h (make-array k))
         (prev-count 0))
    ;; Initialize hint arrays
    (dotimes (i k)
      (setf (aref h i) (make-array 256 :initial-element 0)))
    ;; Decode hints from packed format
    (dotimes (i k)
      (let ((count (aref data (+ offset omega i))))
        ;; Validate count is monotonically increasing and bounded
        (when (or (< count prev-count) (> count omega))
          (return-from decode-hints nil))
        ;; Set hint bits for this polynomial
        (loop for j from prev-count below count
              for pos = (aref data (+ offset j))
              do (setf (aref (aref h i) pos) 1))
        (setf prev-count count)))
    h))

;;;; =========================================================================
;;;; Signing (FIPS 204, Algorithm 2)
;;;; =========================================================================

(defun ml-dsa-65-sign (sk message &optional randomizer)
  "Sign MESSAGE with secret key SK.
   RANDOMIZER if provided gives deterministic signing (32 bytes).
   Returns signature bytes or NIL on failure."
  ;; Parse secret key: rho || K || tr || s1 || s2 || t0
  (let* ((rho (subseq sk 0 32))
         (k-seed (subseq sk 32 64))
         (tr (subseq sk 64 128))
         ;; Decode s1, s2, t0 from sk
         (s1 (ml-dsa-poly-vector +ml-dsa-65-l+))
         (s2 (ml-dsa-poly-vector +ml-dsa-65-k+))
         (t0 (ml-dsa-poly-vector +ml-dsa-65-k+)))
    ;; Decode secret vectors
    (let ((offset 128))
      ;; Decode s1 (l * 128 bytes)
      (dotimes (i +ml-dsa-65-l+)
        (setf (aref s1 i) (decode-poly-eta (subseq sk offset (+ offset 128))
                                            +ml-dsa-65-eta+))
        (incf offset 128))
      ;; Decode s2 (k * 128 bytes)
      (dotimes (i +ml-dsa-65-k+)
        (setf (aref s2 i) (decode-poly-eta (subseq sk offset (+ offset 128))
                                            +ml-dsa-65-eta+))
        (incf offset 128))
      ;; Decode t0 (k * 416 bytes)
      (dotimes (i +ml-dsa-65-k+)
        (setf (aref t0 i) (decode-poly-t0 sk offset))
        (incf offset 416)))
    ;; Expand A and compute mu (no NTT needed - use schoolbook multiplication)
    (let* ((a (expand-a rho))
           (mu-xof (ironclad:make-digest :shake256 :output-length 64)))
      (ironclad:update-digest mu-xof tr)
      (ironclad:update-digest mu-xof message)
      (let* ((mu (ironclad:produce-digest mu-xof))
             (rnd (or randomizer (random-bytes 32))))
        ;; Rejection sampling loop
        (loop for kappa from 0 by +ml-dsa-65-l+ below 10000
              do (let ((result (ml-dsa-65-sign-attempt
                                a s1 s2 t0 mu k-seed rnd kappa)))
                   (when result
                     (return-from ml-dsa-65-sign result))))
        nil))))  ; Give up after too many iterations

(defun ml-dsa-65-sign-attempt (a s1 s2 t0 mu k-seed rnd kappa)
  "Single signing attempt. Returns signature or NIL if rejected.
   FIPS 204 Algorithm 2."
  ;; Sample mask y
  (let ((y (ml-dsa-poly-vector +ml-dsa-65-l+)))
    ;; Expand y from seed
    (dotimes (i +ml-dsa-65-l+)
      (let* ((seed (make-octet-vector 66)))
        (replace seed k-seed)
        (replace seed rnd :start1 32)
        (setf (aref seed 64) (mod kappa 256))
        (setf (aref seed 65) (mod (+ kappa i) 256))
        (let ((xof (ironclad:make-digest :shake256 :output-length 640)))
          (ironclad:update-digest xof seed)
          (setf (aref y i) (decode-poly-z (ironclad:produce-digest xof) 0)))))
    ;; Compute w = Ay using schoolbook multiplication
    (let ((w (ml-dsa-matrix-vector-mul a y)))
      ;; Decompose w into w1
      (let ((w1 (ml-dsa-poly-vector +ml-dsa-65-k+)))
        (dotimes (i +ml-dsa-65-k+)
          (dotimes (j 256)
            (setf (aref (aref w1 i) j) (highbits (aref (aref w i) j)))))
        ;; Compute c_tilde = H(mu || w1)
        (let ((c-xof (ironclad:make-digest :shake256
                                           :output-length +ml-dsa-65-ctilde-bytes+)))
          (ironclad:update-digest c-xof mu)
          (dotimes (i +ml-dsa-65-k+)
            (ironclad:update-digest c-xof (encode-poly-w1 (aref w1 i))))
          (let* ((c-tilde (ironclad:produce-digest c-xof))
                 (c (sample-in-ball c-tilde)))
            ;; Compute z = y + c*s1, cs2 = c*s2, ct0 = c*t0
            (let* ((cs1 (ml-dsa-scalar-poly-mul c s1))
                   (cs2 (ml-dsa-scalar-poly-mul c s2))
                   (ct0 (ml-dsa-scalar-poly-mul c t0))
                   (z (ml-dsa-vector-add y cs1)))
              ;; Compute r = w - cs2 + ct0 (this is what verification computes)
              (let ((r (ml-dsa-vector-add (ml-dsa-vector-sub w cs2) ct0))
                    (z-valid t)
                    (r0-valid t))
                ;; Check ||z||_inf < gamma1 - beta
                (block z-check
                  (dotimes (i +ml-dsa-65-l+)
                    (dotimes (j 256)
                      (when (>= (abs (ml-dsa-mod-q-signed (aref (aref z i) j)))
                                (- +ml-dsa-65-gamma1+ +ml-dsa-65-beta+))
                        (setf z-valid nil)
                        (return-from z-check)))))
                ;; Check ||lowbits(r)||_inf < gamma2 - beta (where r = w - cs2 + ct0)
                (when z-valid
                  (block r0-check
                    (dotimes (i +ml-dsa-65-k+)
                      (dotimes (j 256)
                        (when (>= (abs (lowbits (aref (aref r i) j)))
                                  (- +ml-dsa-65-gamma2+ +ml-dsa-65-beta+))
                          (setf r0-valid nil)
                          (return-from r0-check))))))
                (when (and z-valid r0-valid)
                  ;; Compute hints: h = MakeHint(-ct0, r) per FIPS 204
                  (let ((h (make-array +ml-dsa-65-k+))
                        (hint-count 0))
                    (dotimes (i +ml-dsa-65-k+)
                      (setf (aref h i) (make-array 256 :initial-element 0))
                      (dotimes (j 256)
                        (let ((hint-bit (make-hint (- (ml-dsa-mod-q-signed (aref (aref ct0 i) j)))
                                                   (aref (aref r i) j))))
                          (setf (aref (aref h i) j) hint-bit)
                          (incf hint-count hint-bit))))
                    ;; Check hint count <= omega
                    (when (<= hint-count +ml-dsa-65-omega+)
                      ;; Encode and return signature
                      (let ((sig (make-octet-vector +ml-dsa-65-sig-size+))
                            (offset +ml-dsa-65-ctilde-bytes+))
                        (replace sig c-tilde)
                        (dotimes (i +ml-dsa-65-l+)
                          (replace sig (encode-poly-z (aref z i)) :start1 offset)
                          (incf offset 640))
                        ;; Encode hints (omega + k = 61 bytes for ML-DSA-65)
                        (let ((hint-bytes (encode-hints h)))
                          (replace sig hint-bytes :start1 offset))
                        sig))))))))))))

;;;; =========================================================================
;;;; Utility Functions
;;;; =========================================================================

(defun decode-uint64 (data offset)
  "Decode 64-bit little-endian integer from DATA at OFFSET."
  (let ((result 0))
    (dotimes (i 8 result)
      (setf result (logior result (ash (aref data (+ offset i)) (* i 8)))))))
