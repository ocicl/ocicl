;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)


;;; scrypt from Colin Percival's
;;; "Stronger Key Derivation via Sequential Memory-Hard Functions"
;;; presented at BSDCan'09, May 2009.
;;; http://www.tarsnap.com/scrypt.html

(defclass scrypt-kdf ()
 ((n :initarg :n :reader scrypt-kdf-N)
  (r :initarg :r :reader scrypt-kdf-r)
  (p :initarg :p :reader scrypt-kdf-p)))

(defun scrypt-vector-salsa (b)
  (declare (type (simple-octet-vector 64) b))
  (let ((x (make-array 16 :element-type '(unsigned-byte 32))))
    (declare (type (simple-array (unsigned-byte 32) (16)) x))
    (declare (dynamic-extent x))
    (fill-block-ub8-le x b 0)
    (salsa20/8-core b x)))

(defun block-mix (b xy xy-start r)
  (declare (type (simple-array (unsigned-byte 8) (*)) b xy))
  ;; The derivation of the bound here is that (* I 64) in the first loop below
  ;; must be a legitimate array index.  That loop runs to (* 2 R), hence the
  ;; truncation by 128.  The subtraction of 64 comes from loops further down.
  (declare (type (integer 0 (#.(truncate (- array-dimension-limit 64) 128))) r))
  (let ((xs (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (64)) xs))
    (declare (dynamic-extent xs))
    (replace xs b :start2 (* 64 (1- (* 2 r))) :end1 64)
    (dotimes (i (* 2 r))
      (xor-block 64 xs 0 b (* i 64) xs 0)
      (scrypt-vector-salsa xs)
      (replace xy xs :start1 (+ xy-start (* i 64)) :end2 64))
    (dotimes (i r)
      (replace b xy :start1 (* i 64) :end1 (+ 64 (* i 64)) :start2 (+ xy-start (* 64 2 i))))
    (dotimes (i r)
      (replace b xy :start1 (* 64 (+ i r)) :end1 (+ (* 64 (+ i r)) 64) :start2 (+ xy-start (* 64 (1+ (* i 2))))))))

(defun smix (b b-start r N v xy)
  (declare (type (simple-array (unsigned-byte 8) (*)) b v xy))
  (declare (type (integer 0 (#.(truncate array-dimension-limit 128))) r))
  (let ((x xy)
        (xy-start (* 128 r))
        (smix-length (* 128 r)))
    (replace x b :end1 smix-length :start2 b-start)
    (dotimes (i N)
      (replace v x :start1 (* i smix-length) :end2 smix-length)
      (block-mix x xy xy-start r))
    (dotimes (i N)
      (let ((j (ldb (byte 32 0) (logand (ub64ref/le x (* (1- (* 2 r)) 64)) (1- N)))))
        (xor-block smix-length x 0 v (* j smix-length) x 0)
        (block-mix x xy xy-start r)))
    (replace b x :start1 b-start :end1 (+ b-start smix-length))))

(defmethod derive-key ((kdf scrypt-kdf) passphrase salt iteration-count key-length)
  (declare (ignore iteration-count))
  (let* ((pb-kdf (make-kdf 'pbkdf2 :digest 'sha256))
         (xy (make-array (* 256 (scrypt-kdf-r kdf)) :element-type '(unsigned-byte 8)))
         (v (make-array (* 128 (scrypt-kdf-r kdf) (scrypt-kdf-N kdf)) :element-type '(unsigned-byte 8)))
         (b (derive-key pb-kdf passphrase salt 1 (* (scrypt-kdf-p kdf) 128 (scrypt-kdf-r kdf)))))
    (dotimes (i (scrypt-kdf-p kdf))
      (smix b (* i 128 (scrypt-kdf-r kdf)) (scrypt-kdf-r kdf) (scrypt-kdf-N kdf) v xy))
    (reinitialize-instance pb-kdf :digest 'sha256)
    (derive-key pb-kdf passphrase b 1 key-length)))
