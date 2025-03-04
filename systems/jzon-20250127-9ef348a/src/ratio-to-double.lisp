;;
;; This implementation was ported from the Clozure Common Lisp source
;;     CCL::%DOUBLE-FLOAT
;; alongside helper functions and values
;; This `ratio-to-double' function is used to
;; introduce portability across CL implementations using jzon
;; when dealing with certain number ranges, in particular, denormalized
;; numbers, where SBCL's rouding model causes us to lose data
;; in the course of (jzon:parse (jzon:stringify x))
;; due to the way that it rounds rationals close to 0
;; eg: 4.9d-324 yields 0.0 on SBCL
;;

(defpackage #:com.inuoe.jzon/ratio-to-double
  (:use #:cl)
  (:export
    #:ratio-to-double))

(in-package #:com.inuoe.jzon/ratio-to-double)

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

;;; make a float from hi - high 24 bits mantissa (ignore implied higher bit)
;;;                   lo -  low 28 bits mantissa
;;;                   exp  - take low 11 bits
;;;                   sign - sign(sign) => result
;;; hi result - 1 bit sign: 11 bits exp: 20 hi bits of hi arg
;;; lo result - 4 lo bits of hi arg: 28 lo bits of lo arg

(defun %make-float-from-fixnums (hi lo exp sign)
  (let ((bits (logior (ash (if (minusp sign) 1 0) 63)
                      (ash (ldb (byte 11 0) exp)  52)
                      (ash (ldb (byte 24 0) hi)   28)
                      (ash (ldb (byte 28 0) lo)   00))))
    (%bits-double-float bits)))

(defun ratio-to-double (number
                        &aux
                          (double-float-bias 1022)
                          (double-float-mantissa-width 52)
                          (double-float-digits (1+ double-float-mantissa-width))
                          (double-float-extended-precision 60)
                          (double-float-precision (1+ double-float-mantissa-width))
                          (double-float-normal-exponent-max 2046)
                          (double-float-max-exponent (1+ double-float-normal-exponent-max)))
  (let* ((num (numerator number))
         (den (denominator number))
         ; dont error if result is floatable when either top or bottom is not.
         ; maybe do usual first, catching error
         (numlen (integer-length num))
         (denlen (integer-length den))
         (exp (- numlen denlen))
         (minusp (minusp num)))
    (cond
      ((and (<= numlen double-float-bias)
            (<= denlen double-float-bias)
            (<= (abs exp) double-float-mantissa-width))
        (let ((fnum (coerce num 'double-float))
              (fden (coerce den 'double-float)))
          (/ fnum fden)))
      ((> exp double-float-mantissa-width)
        (coerce (round num den) 'double-float))
      ((>= exp 0)
        (let* ((shift (- double-float-digits exp))
               (num (abs num))
               (int (round (ash num shift) den)) ; gaak
               (intlen (integer-length int))
               (new-exp (+ intlen (- double-float-bias shift))))
          (when (> intlen double-float-digits)
            (setf shift (1- shift))
            (setf int (round (ash num shift) den))
            (setf intlen (integer-length int))
            (setf new-exp (+ intlen (- double-float-bias shift))))
          (when (> new-exp double-float-normal-exponent-max)
            (error 'floating-point-overflow
                   :operation 'double-float
                   :operands (list number)))
          (%make-float-from-fixnums
            (ldb (byte 25 (- intlen 25)) int)
            (ldb (byte 28 (max (- intlen 53) 0)) int)
            new-exp
            (if minusp -1 1))))
      (t ; den > num - exp negative
        (let* ((sign (if minusp -1 1))
               (integer (abs num))
               (integer-length (integer-length integer))
               ;; make sure we will have enough bits in the quotient
               ;; (and a couple extra for rounding)
               (shift-factor (+ (- (integer-length den) integer-length) double-float-extended-precision)))
          (if (plusp shift-factor)
            (setf integer (ash integer shift-factor))
            (setf den (ash den (- shift-factor))))  ; assume div > num
          (let* ((integer (multiple-value-bind (quotient remainder) (floor integer den)
                            (if (zerop remainder)
                              quotient
                              ; whats this - tells us there's junk below
                              (logior quotient 1))))
                 (power-of-2 (- shift-factor))
                 (length (integer-length integer))
                 (lowbits 0))
            (cond
              ((<= length double-float-precision)
                ;; float can be done exactly, so do it the easy way
                (scale-float (coerce (* sign integer) 'double-float)
                             power-of-2))
              (t
                (let* ((exponent (+ length power-of-2))
                       (biased-exponent (+ exponent double-float-bias))
                       (sticky-residue nil))
                  (when (<= biased-exponent 0)
                    ;; denormalize the number
                    (setf sticky-residue (not (zerop (ldb (byte (- 1 biased-exponent) 0) integer))))
                    (setf integer (ash integer (- biased-exponent 1)))
                    (setf biased-exponent 0))
                  (let ((lowest (min double-float-extended-precision length)))
                    (when (and (> length double-float-extended-precision)
                               (not (zerop (ldb (byte (- length double-float-extended-precision) 0) integer))))
                      (setf integer (logior integer (ash 1 (- length double-float-extended-precision)))))
                    ; somewhere between 1 and (- double-float-extended-precision double-float-precision) bits
                    (setf lowbits (ash (ldb (byte (- lowest double-float-precision) (- length lowest)) integer) (- double-float-extended-precision lowest))))
                  (let ((significand (ldb (byte (1- double-float-precision) (- length double-float-precision)) integer)))
                    (when (and (not (zerop (ldb (byte 1 (- length (1+ double-float-precision))) integer)))   ; round bit
                               (or sticky-residue (oddp significand)
                                   (not (zerop (ldb (byte (- double-float-extended-precision double-float-precision 1) 0) lowbits)))))
                      ;; round up
                      (setf significand (ldb (byte (1- double-float-precision) 0) (+ significand 1)))
                      (when (zerop significand)
                        (incf biased-exponent)))
                    (cond
                      ((and (zerop biased-exponent)
                            (zerop significand))
                        (error 'floating-point-underflow
                               :operation 'scale
                               :operands (list sign integer power-of-2)))
                      ((>= biased-exponent double-float-max-exponent)
                        (error 'floating-point-overflow
                               :operation 'scale
                               :operands (list sign integer power-of-2))))
                    (%make-float-from-fixnums
                      (ldb (byte 24 28) significand)
                      (ldb (byte 28 0) significand)
                      biased-exponent
                      sign)))))))))))
