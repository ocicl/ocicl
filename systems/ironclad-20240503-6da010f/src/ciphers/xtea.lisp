;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)

(defconstant +xtea-n-rounds+ 32)
(defconstant +xtea-delta+ #x9e3779b9)

(defclass xtea (cipher 8-byte-block-mixin)
  ((key :accessor key)))

(define-block-encryptor xtea 8
  (with-words ((y z) plaintext plaintext-start)
    (let ((key (key context))
          (sum 0))
      (declare (type (simple-array (unsigned-byte 32) (4)) key))
      (declare (type (unsigned-byte 32) sum))
      ;; could probably unroll this loop for reasonable performance gain
      (dotimes (i +xtea-n-rounds+)
        (setf y (mod32+ y (logxor (mod32+ z (logxor (mod32ash z 4)
                                                    (mod32ash z -5)))
                                  (mod32+ sum
                                          (aref key (logand sum #x3))))))
        (setf sum (mod32+ sum +xtea-delta+))
        (setf z (mod32+ z (logxor (mod32+ y (logxor (mod32ash y 4)
                                                    (mod32ash y -5)))
                                  (mod32+ sum
                                          (aref key (logand (mod32ash sum -11)
                                                            #x3))))))
        )
      (store-words ciphertext ciphertext-start y z))))

(define-block-decryptor xtea 8
  (with-words ((y z) ciphertext ciphertext-start)
    (let ((key (key context))
          (sum (mod32ash +xtea-delta+ 5)))
      (declare (type (simple-array (unsigned-byte 32) (4)) key))
      (declare (type (unsigned-byte 32) sum))
      ;; could probably unroll this loop for reasonable performance gain
      (dotimes (i +xtea-n-rounds+)
        (setf z (mod32- z (logxor (mod32+ y (logxor (mod32ash y 4)
                                                    (mod32ash y -5)))
                                  (mod32+ sum
                                          (aref key (logand (mod32ash sum -11)
                                                            #x3))))))
        (setf sum (mod32- sum +xtea-delta+))
        (setf y (mod32- y (logxor (mod32+ z (logxor (mod32ash z 4)
                                                    (mod32ash z -5)))
                                  (mod32+ sum
                                          (aref key (logand sum #x3)))))))
      (store-words plaintext plaintext-start y z))))

(defmethod schedule-key ((cipher xtea) key)
  (let ((ub32key (make-array 4 :element-type '(unsigned-byte 32))))
    (with-words ((a b c d) key 0)
      (setf (aref ub32key 0) a
            (aref ub32key 1) b
            (aref ub32key 2) c
            (aref ub32key 3) d)
      (setf (key cipher) ub32key)
      cipher)))

(defcipher xtea
  (:encrypt-function xtea-encrypt-block)
  (:decrypt-function xtea-decrypt-block)
  (:block-length 8)
  (:key-length (:fixed 16)))
