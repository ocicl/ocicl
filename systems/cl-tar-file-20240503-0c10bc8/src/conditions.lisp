;;;; conditions.lisp -- conditions signalled by the library
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(define-condition tar-file-error (error)
  ()
  (:documentation "All errors signaled are of this type."))

(define-condition simple-tar-file-error (tar-file-error simple-error)
  ())

(define-condition invalid-checksum-error (tar-file-error)
  ((provided :initarg :provided :reader provided)
   (computed :initarg :computed :reader computed))
  (:report (lambda (condition stream)
             (format stream "Invalid tar header checksum ~D (wanted ~D)"
                     (provided condition) (computed condition))))
  (:documentation "Signaled when the checksum in a tar header is invalid."))

(define-condition malformed-pax-attribute-entry (tar-file-error)
  ())
