;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; public-key.lisp -- implementation of common public key components

(in-package :crypto)


(defun list-all-key-pair-kinds ()
  (copy-list '(:curve25519 :curve448 :dsa :ed25519 :ed448 :elgamal
               :rsa :secp256k1 :secp256r1 :secp384r1 :secp521r1)))


;;; class definitions

(defclass discrete-logarithm-group ()
  ((p :initarg :p :reader group-pval)
   (q :initarg :q :reader group-qval)
   (g :initarg :g :reader group-gval)))


;;; Special variable to force the signature nonce during tests instead of
;;; generating a random one.

(defparameter *signature-nonce-for-test* nil)


;;; converting from integers to octet vectors

(defun octets-to-integer (octet-vec &key (start 0) end (big-endian t) n-bits)
  (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec)
           (optimize (speed 3) (space 0) (safety 1) (debug 0)))
  (let ((end (or end (length octet-vec))))
    (multiple-value-bind (n-bits n-bytes)
        (let ((size (- end start)))
          (if n-bits
              (values n-bits (min (ceiling n-bits 8) size))
              (values (* 8 size) size)))
      (let ((sum (if big-endian
                     (loop with sum = 0
                           for i from (- end n-bytes) below end
                           do (setf sum (+ (ash sum 8) (aref octet-vec i)))
                           finally (return sum))
                     (loop for i from start below (+ start n-bytes)
                           for j from 0 by 8
                           sum (ash (aref octet-vec i) j)))))
        (ldb (byte n-bits 0) sum)))))

(defun integer-to-octets (bignum &key n-bits (big-endian t))
  (declare (optimize (speed 3) (space 0) (safety 1) (debug 0)))
  (let* ((n-bits (or n-bits (integer-length bignum)))
         (bignum (ldb (byte n-bits 0) bignum))
         (n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec))
    (if big-endian
        (loop for i from (1- n-bytes) downto 0
              for index from 0
              do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
              finally (return octet-vec))
        (loop for i from 0 below n-bytes
              for byte from 0 by 8
              do (setf (aref octet-vec i) (ldb (byte 8 byte) bignum))
              finally (return octet-vec)))))

(defun maybe-integerize (thing)
  (etypecase thing
    (integer thing)
    ((simple-array (unsigned-byte 8) (*)) (octets-to-integer thing))))
