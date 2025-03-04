(in-package #:cl-user)

(defpackage #:float-features-tests
  (:nicknames #:org.shirakumo.float-features-tests)
  (:use #:cl))

;;; to run (parachute:test :float-features-tests)
(in-package #:float-features-tests)

(parachute:define-test float-features-divide-by-fp-zero-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-infinity-p
      (flet ((foo () (read-from-string "0.0"))
             (bar () (read-from-string "23")))
        (float-features:with-float-traps-masked (:divide-by-zero)
          (/ (bar) (foo)))))))

(parachute:define-test float-features-divide-by-fp-zero-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (flet ((foo () (read-from-string "0.0"))
                (bar () (read-from-string "23")))
           (float-features:with-float-traps-masked ()
             (/ (bar) (foo))))
       (division-by-zero (error)
         (values t error)))))

(defun foo-float-features-1 (n)
  most-positive-long-float)

(defun bar-float-features-1 (n)
  most-positive-long-float)

(parachute:define-test float-features-overflow-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-infinity-p
      (float-features:with-float-traps-masked (:overflow :inexact)
        (let ((n (random 100)))
          (+ (foo-float-features-1 n) (bar-float-features-1 n)))))))

(parachute:define-test float-features-overflow-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (let ((n (random 100)))
           (float-features:with-float-traps-masked ()
             (+ (foo-float-features-1 n) (bar-float-features-1 n))))
       (floating-point-overflow (error)
         (values t error)))))

(defun foo-float-features-2 (n)
  (- most-positive-long-float (read-from-string "3")))

(defun bar-float-features-2 (n)
  (- most-positive-long-float (read-from-string "3")))

(parachute:define-test float-features-overflow-or-inexact-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-infinity-p
      (float-features:with-float-traps-masked (:overflow :inexact)
        (let ((n (random 100)))
          (+ (foo-float-features-2 n) (bar-float-features-2 n)))))))

(parachute:define-test float-features-overflow-or-inexact-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (let ((n (random 100)))
           (float-features:with-float-traps-masked ()
             (+ (foo-float-features-2 n) (bar-float-features-2 n))))
       (floating-point-inexact (error)
         (values t error))
       (floating-point-overflow (error)
         (values t error)))))

(defun foo-float-features-3 (n)
  (read-from-string "0.0"))

(defun bar-float-features-3 (n)
  (read-from-string "0.0"))

(parachute:define-test float-features-invalid-trapped
    :compile-at :compile-time
    (parachute:true
     (float-features:float-nan-p
      (float-features:with-float-traps-masked (:invalid)
        (let ((n (random 100)))
          (/ (foo-float-features-3 n) (bar-float-features-3 n)))))))

(parachute:define-test float-features-invalid-non-trapped
    :compile-at :compile-time
    (parachute:true
     (handler-case
         (float-features:with-float-traps-masked ()
           (let ((n (random 100)))
             (/ (foo-float-features-3 n) (bar-float-features-3 n))))
       (floating-point-inexact (error)
         (values t error))
       (division-by-zero (error)
         (values t error))
       (floating-point-invalid-operation (error)
         (values t error)))))

(parachute:define-test negative-bits-to-float
  :compile-at :compile-time
  (parachute:skip-on (ecl abcl) "not implemented"
    (parachute:is = -1f0 (float-features:bits-single-float #xBF800000))
    (parachute:is = -1d0 (float-features:bits-double-float #xBFF0000000000000))
    (parachute:is = #xBF800000 (float-features:single-float-bits -1f0))
    (parachute:is = #xBFF0000000000000 (float-features:double-float-bits -1d0))))


;; ecl is missingl bits-single-float/single-float-bits used by short versions
(parachute:define-test short-float-round-trip
  :compile-at :compile-time
  (parachute:skip-on (ecl abcl) "not implemented"
    (parachute:skip-on (allegro) "no idea, probably NaN stuff?"
      (parachute:true
       (float-features:with-float-traps-masked t
         (loop for i from 0 below 65536
               always (= i (float-features:short-float-bits
                            (float-features:bits-short-float i)))))))))

(defun short-bits-double (i)
  (cond
    ((zerop (ldb (byte 15 0) i))
     0d0)
    ((zerop (ldb (byte 5 10) i))
     (* (if (logbitp 15 i) -1 1)
        (expt 2 -14)
        (+ 0d0 (/ (ldb (byte 10 0) i) 1024))))
    (t (* (if (logbitp 15 i) -1 1)
          (expt 2 (- (ldb (byte 5 10) i) 15))
          (+ 1d0 (/ (ldb (byte 10 0) i) 1024))))))

(parachute:define-test short-float
  :compile-at :compile-time
  (parachute:skip-on (ecl abcl) "not implemented"
   ;; examples from wikipedia
    (parachute:is = 0.000000059604645s0 (float-features:bits-short-float 1))
    (parachute:is = 0.000060975552s0 (float-features:bits-short-float #x03ff))
    (parachute:is = 0.000061035156s0 (float-features:bits-short-float #x0400))
    (parachute:is = 65504s0 (float-features:bits-short-float #x7bff))
    (parachute:is = 0.99951172s0 (float-features:bits-short-float #x3bff))
    (parachute:is = 1s0 (float-features:bits-short-float #x3c00))
    (parachute:is = 1.00097656s0 (float-features:bits-short-float #x3c01))
    (parachute:is = 0.33325195s0 (float-features:bits-short-float #x3555))
    (parachute:is = -2s0 (float-features:bits-short-float #xc000))
    (parachute:is = 0s0 (float-features:bits-short-float #x0000))
    (parachute:is = -0s0 (float-features:bits-short-float #x8000))
    (parachute:is = float-features:short-float-positive-infinity
                  (float-features:bits-short-float #x7c00))
    (parachute:is = float-features:short-float-negative-infinity
                  (float-features:bits-short-float #xfc00))
    (parachute:true (float-features:float-nan-p
                     (float-features:bits-short-float #xffff)))
    (parachute:true (float-features:float-nan-p
                     (float-features:with-float-traps-masked t
                       (float-features:bits-short-float #xfd00))))
    ;; make sure all values are approximately right
    (parachute:true
     (loop for i below 65536
           for s = (float-features:with-float-traps-masked t
                     (float-features:bits-short-float i))
           for f = (short-bits-double i)
           always (or (float-features:float-nan-p s)
                      (float-features:float-infinity-p s)
                      (<= (abs (- f s))
                          (abs (* f single-float-epsilon))))))))

;; These are necessary to avoid the constant folding
(defun add (x y) (+ x y))

(parachute:define-test rounding-modes
  :compile-at :compile-time
  (parachute:skip-on (ecl abcl allegro clasp lispworks) "not implemented"
    (parachute:is = 4.500000000000001d0 (float-features:with-rounding-mode :positive (add 3.2D0 1.3D0)))
    (parachute:is = 4.5d0 (float-features:with-rounding-mode :negative (add 3.2D0 1.3D0)))
    (parachute:is = 4.5d0 (float-features:with-rounding-mode :nearest (add 3.2D0 1.3D0)))
    (parachute:is = 4.5d0 (float-features:with-rounding-mode :zero (add 3.2D0 1.3D0)))))
