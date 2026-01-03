;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)

(defconstant +tea-n-rounds+ 32)
(defconstant +tea-delta+ #x9e3779b9)

(defclass tea (cipher 8-byte-block-mixin)
  ((key :accessor key)))

(define-block-encryptor tea 8
  (with-words ((y z) plaintext plaintext-start)
    (let ((key (key context))
          (sum 0))
      (declare (type (simple-array (unsigned-byte 32) (4)) key))
      (declare (type (unsigned-byte 32) sum))
      ;; could probably unroll this loop for reasonable performance gain
      (dotimes (i +tea-n-rounds+)
        (setf sum (mod32+ sum +tea-delta+))
        (setf y (mod32+ y (logxor (mod32+ (mod32ash z 4) (aref key 0))
                                  (mod32+ z sum)
                                  (mod32+ (mod32ash z -5) (aref key 1)))))
        (setf z (mod32+ z (logxor (mod32+ (mod32ash y 4) (aref key 2))
                                  (mod32+ y sum)
                                  (mod32+ (mod32ash y -5) (aref key 3))))))
      (store-words ciphertext ciphertext-start y z))))

(define-block-decryptor tea 8
  (with-words ((y z) ciphertext ciphertext-start)
    (let ((key (key context))
          (sum (mod32ash +tea-delta+ 5)))
      (declare (type (simple-array (unsigned-byte 32) (4)) key))
      (declare (type (unsigned-byte 32) sum))
      (dotimes (i +tea-n-rounds+)
        (setf z (mod32- z (logxor (mod32+ (mod32ash y 4) (aref key 2))
                                  (mod32+ y sum)
                                  (mod32+ (mod32ash y -5) (aref key 3)))))
        (setf y (mod32- y (logxor (mod32+ (mod32ash z 4) (aref key 0))
                                  (mod32+ z sum)
                                  (mod32+ (mod32ash z -5) (aref key 1)))))
        (setf sum (mod32- sum +tea-delta+)))
      (store-words plaintext plaintext-start y z))))

(defmethod schedule-key ((cipher tea) key)
  (let ((ub32key (make-array 4 :element-type '(unsigned-byte 32))))
    (with-words ((a b c d) key 0)
      (setf (aref ub32key 0) a
            (aref ub32key 1) b
            (aref ub32key 2) c
            (aref ub32key 3) d)
      (setf (key cipher) ub32key)
      cipher)))

(defcipher tea
  (:encrypt-function tea-encrypt-block)
  (:decrypt-function tea-decrypt-block)
  (:block-length 8)
  (:key-length (:fixed 16)))
