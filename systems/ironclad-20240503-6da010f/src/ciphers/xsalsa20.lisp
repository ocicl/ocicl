;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; xsalsa20.lisp -- implementation of the XSalsa20 stream cipher

(in-package :crypto)

(defclass xsalsa20 (salsa20)
  ())

(defclass xsalsa20/12 (xsalsa20)
  ()
  (:default-initargs :core-function #'salsa20/12-core))

(defclass xsalsa20/8 (xsalsa20)
  ()
  (:default-initargs :core-function #'salsa20/8-core))

(defmethod shared-initialize :after ((cipher xsalsa20) slot-names
                                     &rest initargs
                                     &key (key nil key-p)
                                     (initialization-vector nil iv-p)
                                     &allow-other-keys)
  (declare (ignore initargs key key-p iv-p))
  (let ((state (salsa20-state cipher))
        (buffer (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (type salsa20-state state)
             (type salsa20-keystream-buffer buffer))
    (when initialization-vector
      (when (< (length initialization-vector) 24)
        (error 'invalid-initialization-vector
               :cipher (class-name (class-of cipher))
               :block-length 24))
      (setf (aref state 8) (ub32ref/le initialization-vector 8)
            (aref state 9) (ub32ref/le initialization-vector 12)))
    (funcall (salsa20-core-function cipher) buffer state)
    (setf (aref state 1) (mod32- (ub32ref/le buffer 0) (aref state 0))
          (aref state 2) (mod32- (ub32ref/le buffer 20) (aref state 5))
          (aref state 3) (mod32- (ub32ref/le buffer 40) (aref state 10))
          (aref state 4) (mod32- (ub32ref/le buffer 60) (aref state 15))
          (aref state 11) (mod32- (ub32ref/le buffer 24) (aref state 6))
          (aref state 12) (mod32- (ub32ref/le buffer 28) (aref state 7))
          (aref state 13) (mod32- (ub32ref/le buffer 32) (aref state 8))
          (aref state 14) (mod32- (ub32ref/le buffer 36) (aref state 9))
          (aref state 8) 0
          (aref state 9) 0)
    (if initialization-vector
        (setf (aref state 6) (ub32ref/le initialization-vector 16)
              (aref state 7) (ub32ref/le initialization-vector 20))
        (setf (aref state 6) 0
              (aref state 7) 0)))
  cipher)

(defcipher xsalsa20
  (:mode :stream)
  (:crypt-function salsa20-crypt)
  (:key-length (:fixed 16 32)))

(defcipher xsalsa20/12
  (:mode :stream)
  (:crypt-function salsa20-crypt)
  (:key-length (:fixed 16 32)))

(defcipher xsalsa20/8
  (:mode :stream)
  (:crypt-function salsa20-crypt)
  (:key-length (:fixed 16 32)))
