;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; xchacha.lisp -- implementation of the XChacha stream cipher

(in-package :crypto)

(defclass xchacha (chacha)
  ())

(defclass xchacha/12 (xchacha)
  ()
  (:default-initargs :core-function #'chacha/12-core))

(defclass xchacha/8 (xchacha)
  ()
  (:default-initargs :core-function #'chacha/8-core))

(defmethod shared-initialize :after ((cipher xchacha) slot-names
                                     &rest initargs
                                     &key (key nil key-p)
                                     (initialization-vector nil iv-p)
                                     &allow-other-keys)
  (declare (ignore initargs key key-p iv-p))
  (let ((state (chacha-state cipher))
        (buffer (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (type chacha-state state)
             (type chacha-keystream-buffer buffer))
    (when initialization-vector
      (when (< (length initialization-vector) 24)
        (error 'invalid-initialization-vector
               :cipher (class-name (class-of cipher))
               :block-length 24))
      (setf (aref state 12) (ub32ref/le initialization-vector 0)
            (aref state 13) (ub32ref/le initialization-vector 4)
            (aref state 14) (ub32ref/le initialization-vector 8)
            (aref state 15) (ub32ref/le initialization-vector 12)))
    (funcall (chacha-core-function cipher) buffer state)
    (setf (aref state 4) (mod32- (ub32ref/le buffer 0) (aref state 0))
          (aref state 5) (mod32- (ub32ref/le buffer 4) (aref state 1))
          (aref state 6) (mod32- (ub32ref/le buffer 8) (aref state 2))
          (aref state 7) (mod32- (ub32ref/le buffer 12) (aref state 3))
          (aref state 8) (mod32- (ub32ref/le buffer 48) (aref state 12))
          (aref state 9) (mod32- (ub32ref/le buffer 52) (aref state 13))
          (aref state 10) (mod32- (ub32ref/le buffer 56) (aref state 14))
          (aref state 11) (mod32- (ub32ref/le buffer 60) (aref state 15))
          (aref state 12) 0
          (aref state 13) 0)
    (if initialization-vector
        (setf (aref state 14) (ub32ref/le initialization-vector 16)
              (aref state 15) (ub32ref/le initialization-vector 20))
        (setf (aref state 14) 0
              (aref state 15) 0)))
  cipher)

(defcipher xchacha
  (:mode :stream)
  (:crypt-function chacha-crypt)
  (:key-length (:fixed 16 32)))

(defcipher xchacha/12
  (:mode :stream)
  (:crypt-function chacha-crypt)
  (:key-length (:fixed 16 32)))

(defcipher xchacha/8
  (:mode :stream)
  (:crypt-function chacha-crypt)
  (:key-length (:fixed 16 32)))
