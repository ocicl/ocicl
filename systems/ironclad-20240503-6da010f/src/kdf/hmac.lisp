;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;; implementation of RFC 5869
(in-package :crypto)

(defclass hmac-kdf ()
  ((digest-name :initarg :digest :reader kdf-digest)
   (info :initarg :info :accessor hmac-kdf-info :type (simple-array (unsigned-byte 8) (*))
         :documentation "Optional context and application specific information")))

(defmethod shared-initialize ((kdf hmac-kdf) slot-names &rest initargs
                              &key digest info &allow-other-keys)
  (declare (ignore slot-names initargs))
  (setf (slot-value kdf 'digest-name) digest
        (hmac-kdf-info kdf) (or info (make-array 0 :element-type '(unsigned-byte 8))))
  kdf)

(defun hkdf-extract (digest salt ikm)
  (let ((hmac (make-hmac salt digest)))
    (update-hmac hmac ikm)
    (produce-mac hmac)))

(defun hkdf-expand (digest prk info key-length)
  (let ((digest-length (digest-length digest)))
    (assert (<= key-length (* 255 digest-length)))
    (subseq
     (apply #'concatenate '(vector (unsigned-byte 8))
            (loop with tmp = (make-array 0 :element-type '(unsigned-byte 8))
                  for i below (ceiling key-length digest-length)
                  collect
                  (setf tmp (hkdf-extract digest prk
                                          (concatenate '(vector (unsigned-byte 8)) tmp info (vector (1+ i)))))))
     0 key-length)))

(defun hmac-derive-key (digest passphrase salt info key-length)
  (let ((prk (hkdf-extract digest salt passphrase)))
    (hkdf-expand digest prk info key-length)))

(defmethod derive-key ((kdf hmac-kdf) passphrase salt iteration-count key-length)
  (hmac-derive-key (kdf-digest kdf) passphrase salt (hmac-kdf-info kdf) key-length))
