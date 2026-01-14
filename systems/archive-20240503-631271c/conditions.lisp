;;;; conditions.lisp -- conditions signalled by the library

(in-package :archive)

(define-condition archive-error (simple-error)
  ()
  (:documentation "All errors signaled by ARCHIVE are of this type."))
