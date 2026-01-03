;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; xor.lisp -- the do-nothing cipher

;;; It's not very secure, but it does come in handy to serve as a dummy
;;; cipher in security protocols before ciphers and keys have been
;;; established.

(cl:in-package :crypto)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +xor-block-length+ 8))

(defclass xor (cipher 8-byte-block-mixin)
  ((key :accessor xor-key :type simple-octet-vector)
   (key-index :accessor xor-key-index :initform 0 :type fixnum)))

(defun xor-crypt-block (context in in-start out out-start)
  (declare (type simple-octet-vector in out))
  (let* ((key (xor-key context))
         (key-index (xor-key-index context))
         (key-length (length key)))
    (declare (type simple-octet-vector key))
    (cond
      ((= key-length 1)
       (let ((byte (aref key 0)))
         ;; Ignore the case where we just crypt in place.
         (unless (and (zerop byte)
                      (eq in out)
                      (= in-start out-start))
           (dotimes (i +xor-block-length+)
             (setf (aref out (+ out-start i))
                   (logxor byte (aref in (+ in-start i))))))))
      (t
       (dotimes (i +xor-block-length+)
         (setf (aref out (+ out-start i))
               (logxor (aref key key-index) (aref in (+ in-start i))))
         (incf key-index)
         (when (>= key-index key-length)
           (setf key-index 0)))
       (setf (xor-key-index context) key-index)))))

(define-block-encryptor xor #.+xor-block-length+
  (xor-crypt-block context plaintext plaintext-start ciphertext ciphertext-start))

(define-block-decryptor xor #.+xor-block-length+
  (xor-crypt-block context ciphertext ciphertext-start plaintext plaintext-start))

(defmethod schedule-key ((cipher xor) key)
  ;; Optimize the probable common case of a key with bytes all the same.
  (let ((short-key (remove-duplicates key)))
    (if (= (length short-key) 1)
        (setf (xor-key cipher) short-key)
        (setf (xor-key cipher) key))
    cipher))

(defcipher xor
  (:encrypt-function xor-encrypt-block)
  (:decrypt-function xor-decrypt-block)
  (:block-length #.+xor-block-length+)
  (:key-length (:variable 1 256 1)))
