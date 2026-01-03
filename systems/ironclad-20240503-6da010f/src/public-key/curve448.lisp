;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; curve448.lisp -- implementation of the curve448 Diffie-Hellman function

(in-package :crypto)


;;; class definitions

(defclass curve448-public-key ()
  ((y :initarg :y :reader curve448-key-y :type (simple-array (unsigned-byte 8) (*)))))

(defclass curve448-private-key ()
  ((x :initarg :x :reader curve448-key-x :type (simple-array (unsigned-byte 8) (*)))
   (y :initarg :y :reader curve448-key-y :type (simple-array (unsigned-byte 8) (*)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass curve448-point ()
    ;; Internally, we represent a point (x, y) using only the projective
    ;; coordinate (X, Z) for x, with x = X / Z.
    ((x :initarg :x :type integer)
     (z :initarg :z :type integer)))
  (defmethod make-load-form ((p curve448-point) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots p)))


;;; constants and function definitions

(defconstant +curve448-bits+ 448)
(defconstant +curve448-p+ 726838724295606890549323807888004534353641360687318060281490199180612328166730772686396383698676545930088884461843637361053498018365439)
(defconstant +curve448-a24+ 39081)

(defconst +curve448-g+
  (make-instance 'curve448-point :x 5 :z 1))


(defmethod ec-scalar-inv ((kind (eql :curve448)) n)
  (expt-mod n (- +curve448-p+ 2) +curve448-p+))

(defun curve448-double-and-add (x1 z1 x2 z2 x3)
  "Point doubling and addition on curve448 curve."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type integer x1 z1 x2 z2 x3))
  (let* ((t1 (mod (+ x1 z1) +curve448-p+))
         (t2 (mod (- x1 z1) +curve448-p+))
         (t3 (mod (- x2 z2) +curve448-p+))
         (t4 (mod (* t1 t3) +curve448-p+))
         (t5 (mod (+ x2 z2) +curve448-p+))
         (t6 (mod (* t2 t5) +curve448-p+))
         (t7 (mod (- t4 t6) +curve448-p+))
         (t8 (mod (* t7 t7) +curve448-p+))
         (z5 (mod (* x3 t8) +curve448-p+))
         (t9 (mod (+ t4 t6) +curve448-p+))
         (x5 (mod (* t9 t9) +curve448-p+))
         (t10 (mod (* t1 t1) +curve448-p+))
         (t11 (mod (* t2 t2) +curve448-p+))
         (x4 (mod (* t10 t11) +curve448-p+))
         (t12 (mod (- t10 t11) +curve448-p+))
         (t13 (mod (* t12 +curve448-a24+) +curve448-p+))
         (t14 (mod (+ t13 t10) +curve448-p+))
         (z4 (mod (* t14 t12) +curve448-p+)))
    (declare (type integer t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 x4 z4 x5 z5))
    (values x4 z4 x5 z5)))

(defmethod ec-scalar-mult ((p curve448-point) n)
  ;; Point multiplication on curve448 curve using the Montgomery ladder.
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type integer n))
  (with-slots (x z) p
    (declare (type integer x z))
    (assert (= 1 z))
    (do ((x1 1)
         (z1 0)
         (x2 x)
         (z2 1)
         (i 447 (1- i)))
        ((minusp i) (make-instance 'curve448-point :x x1 :z z1))
      (declare (type integer x1 z1 x2 z2)
               (type fixnum i))
      (if (logbitp i n)
          (multiple-value-setq (x2 z2 x1 z1)
            (curve448-double-and-add x2 z2 x1 z1 x))
          (multiple-value-setq (x1 z1 x2 z2)
            (curve448-double-and-add x1 z1 x2 z2 x))))))

(defmethod ec-make-point ((kind (eql :curve448)) &key x)
  (unless x
    (error 'missing-point-parameter
           :kind 'curve448
           :parameter 'x
           :description "coordinate"))
  (make-instance 'curve448-point :x x :z 1))

(defmethod ec-destructure-point ((p curve448-point))
  (with-slots (x z) p
    (declare (type integer x z))
    (let ((x (mod (* x (ec-scalar-inv :curve448 z)) +curve448-p+)))
      (list :x x))))

(defmethod ec-encode-scalar ((kind (eql :curve448)) n)
  (integer-to-octets n :n-bits +curve448-bits+ :big-endian nil))

(defmethod ec-decode-scalar ((kind (eql :curve448)) octets)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((x (ldb (byte +curve448-bits+ 0)
                (octets-to-integer octets :big-endian nil))))
    (setf (ldb (byte 2 0) x) 0)
    (setf (ldb (byte 1 (1- +curve448-bits+)) x) 1)
    x))

(defmethod ec-encode-point ((p curve448-point))
  (let* ((coordinates (ec-destructure-point p))
         (x (getf coordinates :x)))
    (ec-encode-scalar :curve448 x)))

(defmethod ec-decode-point ((kind (eql :curve448)) octets)
  (let ((x (ldb (byte +curve448-bits+ 0)
                (octets-to-integer octets :big-endian nil))))
    (ec-make-point :curve448 :x x)))

(defun curve448-public-key (sk)
  "Compute the public key associated to the private key SK."
  (declare (type (simple-array (unsigned-byte 8) (*)) sk)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let* ((s (ec-decode-scalar :curve448 sk))
         (p (ec-scalar-mult +curve448-g+ s)))
    (ec-encode-point p)))

(defmethod make-public-key ((kind (eql :curve448)) &key y &allow-other-keys)
  (unless y
    (error 'missing-key-parameter
           :kind 'curve448
           :parameter 'y
           :description "public key"))
  (make-instance 'curve448-public-key :y y))

(defmethod destructure-public-key ((public-key curve448-public-key))
  (list :y (curve448-key-y public-key)))

(defmethod make-private-key ((kind (eql :curve448)) &key x y &allow-other-keys)
  (unless x
    (error 'missing-key-parameter
           :kind 'curve448
           :parameter 'x
           :description "private key"))
  (make-instance 'curve448-private-key :x x :y (or y (curve448-public-key x))))

(defmethod destructure-private-key ((private-key curve448-private-key))
  (list :x (curve448-key-x private-key)
        :y (curve448-key-y private-key)))

(defmethod generate-key-pair ((kind (eql :curve448)) &key &allow-other-keys)
  (let ((sk (random-data (ceiling +curve448-bits+ 8))))
    (setf (ldb (byte 2 0) (elt sk 0)) 0)
    (setf (ldb (byte 1 7) (elt sk (- (ceiling +curve448-bits+ 8) 1))) 1)
    (let ((pk (curve448-public-key sk)))
      (values (make-private-key :curve448 :x sk :y pk)
              (make-public-key :curve448 :y pk)))))

(defmethod diffie-hellman ((private-key curve448-private-key) (public-key curve448-public-key))
  (let ((s (ec-decode-scalar :curve448 (curve448-key-x private-key)))
        (p (ec-decode-point :curve448 (curve448-key-y public-key))))
    (ec-encode-point (ec-scalar-mult p s))))
