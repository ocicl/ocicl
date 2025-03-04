(defpackage #:com.inuoe.jzon/eisel-lemire
  (:use #:cl)
  (:export #:make-double))

(in-package #:com.inuoe.jzon/eisel-lemire)

(defmacro %uint64 (integer)
  `(ldb (byte 64 0) ,integer))

(defmacro %<<u64 (integer count)
  `(ldb (byte 64 0) (ash ,integer (logand ,count 63))))

(defmacro %>>u64 (integer count)
  `(ldb (byte 64 0) (ash ,integer (- (logand ,count 63)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +%pow10-min+ -348)
  (defconstant +%pow10-max+ +347))

;;
;; detailedPowersOfTen contains 128-bit mantissa approximations (rounded down)
;; to the powers of 10. For example:
;;
;;  - 1e43 ≈ (#xE596B7B0_C643C719                   * (2 ** 79))
;;  - 1e43 = (#xE596B7B0_C643C719_6D9CCD05_D0000000 * (2 ** 15))
;;
;; The mantissas are explicitly listed. The exponents are implied by a linear
;; expression with slope 217706.0;65536.0 ≈ log(10);log(2).
;;
;; Implementation adapted from
;;  https://github.com/google/wuffs/blob/ba3818cb6b473a2ed0b38ecfc07dbbd3a97e8ae7/script/print-mpb-powers-of-10.go
;;
;;
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun %gen-powers-of-ten-tables ()
    (let* ((min +%pow10-min+)
           (max +%pow10-max+)
           (count (+ 1 (- min) max ))
           ;; N is large enough so that (1<<N) is easily bigger than 1e310.
           (n 2048)
           ;; 1214 is 1023 + 191. 1023 is the bias for IEEE 754 double-precision floating
           ;;point. 191 is ((3 * 64) - 1) and we work with multiples-of-64-bit mantissas.
           (bias 1214)
           (min-table (make-array count :element-type '(unsigned-byte 64)))
           (max-table (make-array count :element-type '(unsigned-byte 64))))
      (loop :for e :from min :upto max
            :for idx :from 0
            :do (let ((z (cond
                           ((plusp e)
                             (* (ash 1 n) (expt 10 (+ e))))
                           (t
                             (truncate (ash 1 n) (expt 10 (- e))))))
                      (n (- n)))
                  (loop :while (>= z (ash 1 128))
                        :do (setf z (ash z -1))
                            (incf n))
                  (let* ((approx-n (ldb (byte 32 0) (+ (ash (* 217706 e) -16) 1087)))
                         (biased-n (ldb (byte 32 0) (+ bias n))))
                    (unless (= approx-n biased-n)
                      (error "biased-n approximation: have ~A, want ~A" approx-n biased-n))

                    (let ((hi (ldb (byte 64 64) z))
                          (lo (ldb (byte 64 0) z)))
                      (setf (aref min-table idx) lo)
                      (setf (aref max-table idx) hi)))))
      (values min-table max-table))))

(defvar *%detailed-powers-of-ten-min*)
(defvar *%detailed-powers-of-ten-max*)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (values *%detailed-powers-of-ten-min*
                *%detailed-powers-of-ten-max*)
        (%gen-powers-of-ten-tables)))

(declaim (type (simple-array (unsigned-byte 64) (#.(+ 1 (- +%pow10-min+) +%pow10-max+)))
               *%detailed-powers-of-ten-min*
               *%detailed-powers-of-ten-max*))

(declaim (inline %mul64))
(defun %mul64 (x y)
  (declare (type (unsigned-byte 64) x y))
  (let* ((x0 (ldb (byte 32 0) x))
         (x1 (ldb (byte 32 32) x))
         (y0 (ldb (byte 32 0) y))
         (y1 (ldb (byte 32 32) y))
         (w0 (ldb (byte 64 0) (* x0 y0)))
         (t0 (ldb (byte 64 0) (+ (* x1 y0) (ash w0 -32))))
         (w1 (ldb (byte 32 0) t0))
         (w2 (ldb (byte 32 32) t0))
         (w1 (ldb (byte 64 0) (+ w1 (* x0 y1))))
         (hi (ldb (byte 64 0) (+ (* x1 y1) w2 (ash w1 -32))))
         (lo (ldb (byte 64 0) (* x y))))
    (values hi lo)))

(defmacro %bits-double-float (x)
  #-ecl
  `(org.shirakumo.float-features:bits-double-float ,x)
  #+ecl
  (if (find-symbol (string '#:bits-double-float) '#:si)
    `(,(intern (string '#:bits-double-float) '#:si) ,x)
    (let ((tmp (gensym (string 'tmp))))
      `(ffi:with-foreign-object (,tmp :double)
        (setf (ffi:deref-pointer ,tmp :uint64-t) ,x)
        (ffi:deref-pointer ,tmp :double)))))

(defun make-double (mantissa exp10 neg)
  (when (and (typep mantissa '(unsigned-byte 64))
             (typep exp10 '(signed-byte 32)))
    (cond
      ((zerop mantissa)
        (if neg -0.0d0 0.0d0))
      ((or (< exp10 +%pow10-min+) (< +%pow10-max+ exp10))
        nil)
      (t
        (let* ((clz (- 64 (integer-length mantissa)))
               (mantissa (%<<u64 mantissa clz))
               (float64-exponent-bias 1023)
               (ret-exp-2 (%uint64 (- (%uint64 (+ (%>>u64 (* 217706 exp10) 16) 64 float64-exponent-bias)) clz)))
               (idx (- exp10 +%pow10-min+)))
          (multiple-value-bind (xhi xlo) (%mul64 mantissa (aref #.*%detailed-powers-of-ten-max* idx))
            (when (and (= (logand xhi #x1FF) #x1FF) (< (%uint64 (+ xlo mantissa)) mantissa))
              (multiple-value-bind (yhi ylo) (%mul64 mantissa (aref #.*%detailed-powers-of-ten-min* idx))
                (let* ((merged-lo (%uint64 (+ xlo yhi)))
                       (merged-hi (if (< merged-lo xlo) (%uint64 (1+ xhi)) xhi)))
                  (when (and (= (logand merged-hi #x1FF) #x1FF) (zerop (%uint64 (1+ merged-lo))) (< (%uint64 (+ ylo mantissa)) mantissa))
                    ;; todo can we clean this return-from?
                    (return-from make-double nil))
                  (setf xhi merged-hi)
                  (setf xlo merged-lo))))
            (let* ((msb (%>>u64 xhi 63))
                   (ret-mantissa (%>>u64 xhi (+ msb 9)))
                   (ret-exp-2 (%uint64 (- ret-exp-2 (logxor 1 msb)))))
              ;;
              ;; Half-way Ambiguity
              ;;
              (unless (and (zerop xlo) (zerop (logand xhi #x1FF)) (= (logand ret-mantissa 3) 1))
                (let* ((ret-mantissa (%uint64 (+ ret-mantissa (logand ret-mantissa 1))))
                       (ret-mantissa (%>>u64 ret-mantissa 1)))
                  (when (> (%>>u64 ret-mantissa 53) 0)
                    (setf ret-mantissa (%>>u64 ret-mantissa 1))
                    (setf ret-exp-2 (%uint64 (+ ret-exp-2 1))))

                  ;;
                  ;; ret-exp-2 is a uint64. Zero or underflow means that we're in subnormal
                  ;; float64 space. #x7FF or above means that we're in Inf/NaN float64 space.
                  ;;
                  (unless (>= (%uint64 (- ret-exp-2 1)) (- #x7FF 1))
                    (let ((ret-bits (logior (%<<u64 ret-exp-2 52) (logand ret-mantissa #x000FFFFFFFFFFFFF) (if neg #x8000000000000000 0))))
                      (%bits-double-float ret-bits))))))))))))
