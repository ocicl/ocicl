;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)

(defun egcd (a b)
  "Extended Euclidean algorithm, aka extended greatest common
denominator."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type integer a b))
  (assert (and (>= a 0)
               (>= b 0)))
  (do ((q 0)
       (c a (- d (* q c)))
       (d b c)
       (u_c 1 (- u_d (* q u_c)))
       (v_c 0 (- v_d (* q v_c)))
       (u_d 0 u_c)
       (v_d 1 v_c))
      ((= c 0)
       (values d u_d v_d))
   (setq q (floor d c))))


;;; Some number theory functions taken from Maxima

(defun jacobi (a b)
  (declare (integer a b) (optimize (speed 3)))
  (cond ((zerop b)
         (if (= 1 (abs a)) 1 0))
        ((and (evenp a) (evenp b))
         0)
        (t
         (let ((k 1)
               (tab2 (make-array 8 :element-type '(integer -1 1)
                                   :initial-contents #(0 1 0 -1 0 -1 0 1))))
           (declare (type (integer -1 1) k))
           (loop for v fixnum = 0 then (1+ v) ;remove 2's from b
                 while (evenp b) do (setf b (ash b -1))
                 finally (when (oddp v)
                           (setf k (aref tab2 (logand a 7))))) ; (-1)^(a^2-1)/8
           (when (minusp b)
             (setf b (- b))
             (when (minusp a)
               (setf k (- k))))
           (loop
              (when (zerop a)
                (return-from jacobi (if (> b 1) 0 k)))
              (loop for v fixnum = 0 then (1+ v)
                    while (evenp a) do (setf a (ash a -1))
                    finally (when (oddp v)
                              (when (minusp (aref tab2 (logand b 7))); (-1)^(b^2-1)/8
                                (setf k (- k)))))
              (when (plusp (logand a b 2)) ;compute (-1)^(a-1)(b-1)/4
                (setf k (- k)))
              (let ((r (abs a)))
                (setf a (rem b r))
                (setf b r)))))))

(defun power-mod-tab (b k m)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((l (ash 1 (1- k)))
         (tab (make-array l :element-type 'integer :initial-element 1))
         (bi b)
         (bb (mod (* b b) m)))
    (setf (svref tab 0) b)
    (do ((i 1 (1+ i)))
        ((= i l) tab)
      (setq bi (mod (* bi bb) m))
      (setf (svref tab i) bi))))

(defun power-mod (b e m)
  (declare (optimize (speed 3) (safety 0)))
  (cond
    ((zerop e)
      (mod 1 m))
    ((typep e 'fixnum)
      (do ((res 1)) (())
        (when (logbitp 0 e)
          (setq res (mod (* res b) m))
          (when (= 1 e) (return res)))
        (setq e (ash e -1)
              b (mod (* b b) m))))
    (t ;; sliding window variant:
      (let* ((l (integer-length e))
             (k (cond ((< l  65) 3)
                      ((< l 161) 4)
                      ((< l 385) 5)
                      ((< l 897) 6)
                      (t         7)))
             (tab (power-mod-tab b k m))
             (res 1) s u tmp)
        (do ((i (1- l)))
            ((< i 0) res)
          (cond
            ((logbitp i e)
              (setq s (max (1+ (- i k)) 0))
              (do () ((logbitp s e)) (incf s))
              (setq tmp (1+ (- i s)))
              (dotimes (h tmp) (setq res (mod (* res res) m)))
              (setq u (ldb (byte tmp s) e))
              (unless (= u 0) (setq res (mod (* res (svref tab (ash u -1))) m)))
              (setq i (1- s)))
            (t
              (setq res (mod (* res res) m))
              (decf i))))))))

(defun miller-rabin-decomposition (n) ;; assume n > 2 (n-1 is even)
  ;; return values q,k with n-1 = q*2^k
  (declare (optimize (speed 3) (safety 0)))
  (do ((k 1 (1+ k))
       (q (ash n -1) (ash q -1)))
      ((logbitp 0 q) (values q k))))

(defun miller-rabin-kernel (n q k &optional x)
  ;; now assume n-1 = q*2^k, k >= 1
  (declare (optimize (speed 3) (safety 0)))
  (unless x
    (setq x (+ (strong-random (- n 2)) 2)))
  (let ((y (power-mod x q n)) ;; j = 0
        (minus1 (1- n)))
    (if (or (= y 1) (= y minus1))
      t
      (do ((j 1 (1+ j)))
          ((= j k))
        (setq y (power-mod y 2 n))
        (when (= y minus1) (return t))
        (when (= y 1) (return)))))) ;; n prime => last y must have been 1 or -1

(defun lucas-sequence (k p n)
  (declare (optimize (speed 3) (safety 0)))
  (let ((uh 1) (vl 2) (vh p) (s 0) l)
    (do ()
	((logbitp 0 k))
      (setq k (ash k -1))
      (setq s (1+ s)))
    (setq l (integer-length k))
    (do ((j (1- l) (1- j)))
	((= 0 j))
      (if (logbitp j k)
	  (progn
	    (setq uh (mod (* uh vh) n))
	    (setq vl (mod (- (* vh vl) p) n))
	    (setq vh (mod (- (* vh vh) 2) n)))
	  (progn
	    (setq uh (mod (1- (* uh vl)) n))
	    (setq vh (mod (- (* vh vl) p) n))
	    (setq vl (mod (- (* vl vl) 2) n)))))
    (setq uh (mod (1- (* uh vl)) n))
    (setq vl (mod (- (* vh vl) p) n))
    (dotimes (j s)
      (setq uh (mod (* uh vl) n))
      (setq vl (mod (- (* vl vl) 2) n)))
    uh))

(defun primep-lucas (n)
  (declare (optimize (speed 3) (safety 0)))
  (let ((b 3))
    (loop until (= (jacobi (- (* b b) 4) n) -1) do
      (incf b))
    (zerop (lucas-sequence (1+ n) b n))))


;;; modular arithmetic utilities

(defun modular-inverse (n modulus)
  "Returns M such that N * M mod MODULUS = 1"
  (declare (type (integer 1 *) modulus))
  (declare (type (integer 0 *) n))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (when (or (zerop n) (and (evenp n) (evenp modulus)))
    (return-from modular-inverse 0))
  (loop
     with r1 of-type integer = n
     and r2 of-type integer = modulus
     and u1 of-type integer = 1
     and u2 of-type integer = 0
     and q of-type integer = 0
     and r of-type integer = 0
     until (zerop r2)
     do (progn
          (multiple-value-setq (q r) (floor r1 r2))
          (setf r1 r2
                r2 r)
          (decf u1 (* q u2))
          (rotatef u1 u2))
     finally (return (let ((inverse u1))
                       (when (minusp inverse)
                         (setf inverse (mod inverse modulus)))
                       (if (zerop (mod (* n inverse) modulus))
                           0
                           inverse)))))

(defun modular-inverse-with-blinding (n modulus)
  "As modular-inverse, but mask N with a blinding factor before
computing the modular inverse."
  (declare (type (integer 1 *) modulus))
  (declare (type (integer 0 *) n))
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let* ((b (loop for b = (+ 1 (strong-random (- modulus 1)))
                  until (= 1 (gcd b modulus))
                  finally (return b)))
         (x (mod (* n b) modulus))
         (y (modular-inverse x modulus)))
    (mod (* y b) modulus)))

(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient (Montgomery ladder)."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type integer n exponent modulus))
  (assert (<= 0 exponent modulus))
  (assert (> modulus 1))
  (do ((r0 1)
       (r1 n)
       (i (1- (integer-length modulus)) (1- i)))
      ((minusp i) r0)
    (declare (type fixnum i)
             (type integer r0 r1))
    (if (logbitp i exponent)
        (setf r0 (mod (* r0 r1) modulus)
              r1 (mod (* r1 r1) modulus))
        (setf r1 (mod (* r0 r1) modulus)
              r0 (mod (* r0 r0) modulus)))))

(defun expt-mod/unsafe (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient. This function is
faster than expt-mod, but it is not safe against side channel timing attacks."
  (power-mod n exponent modulus))


;;; prime numbers utilities

(defconst +small-primes+
  (make-array 269
              :element-type 'fixnum
              :initial-contents '(2 3 5 7 11 13 17 19 23 29 31 37 41 43
          47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131
          137 139 149 151 157 163 167 173 179 181 191 193 197 199 211
          223 227 229 233 239 241 251 257 263 269 271 277 281 283 293
          307 311 313 317 331 337 347 349 353 359 367 373 379 383 389
          397 401 409 419 421 431 433 439 443 449 457 461 463 467 479
          487 491 499 503 509 521 523 541 547 557 563 569 571 577 587
          593 599 601 607 613 617 619 631 641 643 647 653 659 661 673
          677 683 691 701 709 719 727 733 739 743 751 757 761 769 773
          787 797 809 811 821 823 827 829 839 853 857 859 863 877 881
          883 887 907 911 919 929 937 941 947 953 967 971 977 983 991
          997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063
          1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153
          1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237
          1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319
          1321 1327 1361 1367 1373 1381 1399 1409 1423 1427 1429 1433
          1439 1447 1451 1453 1459 1471 1481 1483 1487 1489 1493 1499
          1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597
          1601 1607 1609 1613 1619 1621 1627 1637 1657 1663 1667 1669
          1693 1697 1699 1709 1721 1723)))
(defconstant +last-small-prime+ 1723)

(defun generate-small-primes (n)
  "Generates a list of all primes up to N using the Sieve of
Eratosthenes.  Was used to generate the list above; included for
mathematical interest."
  (assert (<= 2 n (expt 2 20)))
  (loop for i from 2 to n
     with array = (make-array (1+ n) :initial-element 1)
     unless (zerop (aref array i))
     do (loop for j from 2 to (floor (/ n i))
           do (setf (aref array (* i j)) 0))
     finally (return (loop for j from 2 to n
                        unless (zerop (aref array j))
                        collect j))))

(defparameter *number-of-miller-rabin-tests* 64)

(defun prime-p (n &optional (prng *prng*))
  "True if N is a prime number (with very high probability; at most 1:2^128
chance of returning true for a composite number with the default
*NUMBER-OF-MILLER-RABIN-TESTS* of 64)."
  (declare (type integer n)
           (optimize (speed 3)))
  (if (<= n +last-small-prime+)
      (when (find n +small-primes+) t)
      (let ((*prng* prng))
        (and (loop for p across +small-primes+
                   never (zerop (mod n p)))
             (multiple-value-bind (q k) (miller-rabin-decomposition n)
               (loop repeat *number-of-miller-rabin-tests*
                     always (miller-rabin-kernel n q k)))
             (primep-lucas n)))))

(defun generate-prime-in-range (lower-limit upper-limit &optional (prng *prng*))
  (assert (< 0 lower-limit upper-limit))
  (loop for r = (strong-random (- upper-limit lower-limit -1) prng)
     for x = (+ r lower-limit)
     until (prime-p x prng)
     finally (return x)))

(defun generate-prime (num-bits &optional (prng *prng*))
  "Return a NUM-BITS-bit prime number with very high
probability (1:2^128 chance of returning a composite number)."
  (loop with big = (ash 1 (1- num-bits))
     for x = (logior (strong-random big prng) big 1)
     until (prime-p x prng)
     finally (return x)))

(defun generate-safe-prime (num-bits &optional (prng *prng*))
  "Generate a NUM-BITS-bit prime number p so that (p-1)/2 is prime too."
  (loop
     for q = (generate-prime (1- num-bits) prng)
     for p = (1+ (* 2 q))
     until (prime-p p prng)
     finally (return p)))

(defun find-generator (p &optional (prng *prng*))
  "Find a random generator of the multiplicative group (Z/pZ)*
where p is a safe prime number."
  (assert (> p 3))
  (loop
     with factors = (list 2 (/ (1- p) 2))
     for g = (strong-random p prng)
     until (loop
              for d in factors
              never (= 1 (expt-mod/unsafe g (/ (1- p) d) p)))
     finally (return g)))

(defun find-subgroup-generator (p q &optional (prng *prng*))
  "Find a random generator of a subgroup of order Q of the multiplicative
group (Z/pZ)* where p is a prime number."
  (let ((f (/ (1- p) q)))
    (assert (integerp f))
    (loop
       for h = (+ 2 (strong-random (- p 3) prng))
       for g = (expt-mod/unsafe h f p)
       while (= 1 g)
       finally (return g))))
