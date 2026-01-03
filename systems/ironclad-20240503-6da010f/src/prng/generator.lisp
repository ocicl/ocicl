;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; generator.lisp -- Fortuna PRNG generator

(in-package :crypto)



(defvar +fortuna-cipher-block-size+ 16
  "Fortuna is only defined for 128-bit (16-byte) cyphers")

(defclass fortuna-generator ()
  ((key
    :initform (make-array 32
                          :element-type '(unsigned-byte 8)
                          :initial-element 0))
   (counter :initform 0)
   (digest :initform (make-digest :sha256))
   (cipher :initform :aes
           :initarg :cipher))
  (:documentation "Fortuna generator.  KEY is the key used to initialise
  CIPHER as an instance of CIPHER-NAME (which must be a valid NAME
  recognised by MAKE-CIPHER)."))

(defmethod initialize-instance :after ((generator fortuna-generator) &key)
  (with-slots (key cipher) generator
    (assert (= (block-length cipher) +fortuna-cipher-block-size+))
    (assert (find 32 (key-lengths cipher)))
    (setf cipher (make-cipher cipher :key key :mode :ecb))))

(defmethod prng-reseed (seed (generator fortuna-generator))
  (with-slots (key counter cipher digest) generator
    (reinitialize-instance digest)
    (update-digest digest key)
    (update-digest digest seed)
    (produce-digest digest :digest key)
    (reinitialize-instance digest)
    (digest-sequence digest key :digest key)
    (incf counter)
    (reinitialize-instance cipher :key key)))

(defun %generate-blocks (generator num-blocks)
  (with-slots (cipher key counter) generator
    (assert (and cipher (plusp counter)))
    (loop for i from 1 to num-blocks
       collect (let ((block (integer-to-octets counter
                                               :n-bits 128
                                               :big-endian nil)))
                      (encrypt-in-place cipher block)
                      block)
       into blocks
       do (incf counter)
       finally (return (apply #'concatenate 'simple-octet-vector blocks)))))

(defmethod prng-random-data (num-bytes (generator fortuna-generator))
  (assert (< 0 num-bytes (expt 2 20)))
  (let* ((output (subseq (%generate-blocks generator (ceiling num-bytes 16))
                         0
                         num-bytes))
         (key (%generate-blocks generator 2)))
    (setf (slot-value generator 'key) key)
    output))
