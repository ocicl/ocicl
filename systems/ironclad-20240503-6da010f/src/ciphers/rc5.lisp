;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; rc5.lisp -- implementation of the RC5 encryption algorithm from RFC 2040

(in-package :ironclad)

;;; RC5 is technically a parameterized cipher admitting a variable
;;; number of rounds.  This implementation expose a method of selecting
;;; the number of rounds to be used (`n-rounds' &key parameter in
;;; CREATE-RC5-CONTEXT), but none of the upper-level machinery actually
;;; uses this parameter.  In a small overhaul of the MAKE-CIPHER
;;; functionality in Ironclad, it would be nice to change this state of
;;; affairs.  12 was the number of rounds suggested initially, but RC5
;;; with 12 rounds is susceptible to a differential plaintext attack.
;;; OpenSSL supports 12 or 16 as the number of rounds (with no error
;;; checking, natch).
;;;
;;; See also the TODO file.

(defconstant +rc5/32-p+ #xb7e15163)
(defconstant +rc5/32-q+ #x9e3779b9)

(defconstant +rc5-w+ 32)
(defconstant +rc5-ww+ 4)
(defconstant +rc5-b+ 64)
(defconstant +rc5-bb+ 8)

(deftype rc5-n-rounds () '(mod 256))
(deftype rc5-round-keys () '(simple-array (unsigned-byte 32) (*)))

(defclass rc5 (cipher 8-byte-block-mixin)
  ((n-rounds :reader n-rounds :initarg :n-rounds :type rc5-n-rounds)
   (round-keys :accessor round-keys
               :type (simple-array (unsigned-byte 32) (*))))
  (:default-initargs :n-rounds 12))

(define-block-encryptor rc5 8
  (let ((n-rounds (n-rounds context))
        (round-keys (round-keys context)))
    (declare (type rc5-n-rounds n-rounds))
    (declare (type rc5-round-keys round-keys))
    (with-words ((a b) plaintext plaintext-start :big-endian nil)
      (setf a (mod32+ a (aref round-keys 0))
            b (mod32+ b (aref round-keys 1)))
      (do ((i 1 (1+ i)))
          ((> i n-rounds)
           (store-words ciphertext ciphertext-start a b))
        (setf a (logxor a b))
        (setf a (mod32+ (rol32 a (mod b 32)) (aref round-keys (* i 2))))
        (setf b (logxor b a))
        (setf b (mod32+ (rol32 b (mod a 32)) (aref round-keys (1+ (* i 2)))))))))

(define-block-decryptor rc5 8
  (let ((n-rounds (n-rounds context))
        (round-keys (round-keys context)))
    (declare (type rc5-n-rounds n-rounds))
    (declare (type rc5-round-keys round-keys))
    (with-words ((a b) ciphertext ciphertext-start :big-endian nil)
      (do ((i n-rounds (1- i)))
          ((<= i 0)
           (setf b (mod32- b (aref round-keys 1))
                 a (mod32- a (aref round-keys 0)))
           (store-words plaintext plaintext-start a b))
        (setf b (rol32 (mod32- b (aref round-keys (1+ (* i 2))))
                       (mod (- 32 (mod a 32)) 32)))
        (setf b (logxor b a))
        (setf a (rol32 (mod32- a (aref round-keys (* i 2)))
                       (mod (- 32 (mod b 32)) 32)))
        (setf a (logxor a b))))))

(defun rc5-expand-key (key n-rounds)
  (declare (type (simple-array (unsigned-byte 8) (*)) key))
  (declare (type rc5-n-rounds n-rounds))
  (let* ((n-round-keys (* 2 (1+ n-rounds)))
         (round-keys (make-array n-round-keys :element-type '(unsigned-byte 32)))
         (expanded-key (make-array 256 :element-type '(unsigned-byte 8)
                                     :initial-element 0))
         (n-expanded-key-words (ceiling (length key) 4))
         (l (make-array 64 :element-type '(unsigned-byte 32))))
    (declare (dynamic-extent expanded-key l))
    (declare (type (simple-array (unsigned-byte 8) (256)) expanded-key))
    (declare (type (simple-array (unsigned-byte 32) (64)) l))
    (declare (type (simple-array (unsigned-byte 32) (*)) round-keys))
    ;; convert the key into a sequence of (unsigned-byte 32).  this way
    ;; is somewhat slow and consy, but it's easily shown to be correct.
    (replace expanded-key key)
    (loop for i from 0 below 64 do
          (setf (aref l i) (ub32ref/le expanded-key (* i 4))))
    ;; initialize the round keys
    (loop initially (setf (aref round-keys 0) +rc5/32-p+)
      for i from 1 below n-round-keys do
      (setf (aref round-keys i) (mod32+ (aref round-keys (1- i)) +rc5/32-q+)))
    ;; mix in the user's key
    (do ((k (* 3 (max n-expanded-key-words n-round-keys)) (1- k))
         (a 0)
         (b 0)
         (i 0 (mod (1+ i) n-round-keys))
         (j 0 (mod (1+ j) n-expanded-key-words)))
        ((<= k 0) round-keys)
      (declare (type (unsigned-byte 32) a b))
      (setf a (rol32 (mod32+ (aref round-keys i) (mod32+ a b)) 3))
      (setf (aref round-keys i) a)
      (setf b (let ((x (mod32+ a b)))
                (declare (type (unsigned-byte 32) x))
                (rol32 (mod32+ (aref l j) x) (mod x 32))))
      (setf (aref l j) b))))

(defmethod schedule-key ((cipher rc5) key)
  (let* ((n-rounds (n-rounds cipher))
         (round-keys (rc5-expand-key key n-rounds)))
    (setf (round-keys cipher) round-keys)
    cipher))

(defcipher rc5
  (:encrypt-function rc5-encrypt-block)
  (:decrypt-function rc5-decrypt-block)
  (:block-length 8)
  (:key-length (:variable 1 255 1)))
