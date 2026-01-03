;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; aead.lisp -- authenticated encryption with associated data

(in-package :crypto)


(defclass aead-mode ()
  ((encryption-started :accessor encryption-started-p
                       :initform nil
                       :type boolean)
   (tag :accessor tag)))

(defmethod shared-initialize :after ((mode aead-mode) slot-names &rest initargs &key tag &allow-other-keys)
  (declare (ignore slot-names initargs))
  (setf (encryption-started-p mode) nil
        (tag mode) (copy-seq tag))
  mode)

(defun aeadp (name)
  (get name 'aead))

(defun list-all-authenticated-encryption-modes ()
  "Returns a list whose elements may be validly passed to
make-authenticated-encryption-mode."
  (loop for symbol being each external-symbol of (find-package :ironclad)
        if (aeadp symbol)
          collect (intern (symbol-name symbol) :keyword) into ciphers
        finally (return (sort ciphers #'string<))))

(defun authenticated-encryption-mode-supported-p (name)
  "Returns T if NAME would be in the list returned by
list-all-authenticated-encryption-modes NIL otherwise."
  (and (symbolp name) (aeadp (massage-symbol name))))

(defmacro defaead (name)
  `(setf (get ',name 'aead) t))

(defun make-authenticated-encryption-mode (name &rest args)
  "Return an authenticated encryption object suitable for use for both
encryption and decryption."
  (typecase name
    (symbol
     (let ((name (massage-symbol name)))
       (if (aeadp name)
           (apply #'make-instance name args)
           (error 'unsupported-authenticated-encryption-mode :name name))))
    (t
     (error 'type-error :datum name :expected-type 'symbol))))
