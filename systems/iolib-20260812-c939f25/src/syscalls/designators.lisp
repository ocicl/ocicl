;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- CFFI type designators.
;;;

(in-package :iolib/syscalls)

;;;-------------------------------------------------------------------------
;;; CFFI Type Designators
;;;-------------------------------------------------------------------------

(defmacro define-designator (name cffi-type &body type-clauses)
  (let ((type `(quote (or ,@(mapcar #'car type-clauses))))
        (ctype (format-symbol t "~A-~A" (string name) (string '#:designator))))
    `(progn
       (deftype ,name () ,type)
       (defun ,name (,name)
         (etypecase ,name
           ,@type-clauses))
       (define-foreign-type ,ctype ()
         ()
         (:simple-parser ,ctype)
         (:actual-type ,cffi-type))
       (defmethod expand-to-foreign (value (type ,ctype))
         `(convert-to-foreign
           (let ((,',name ,value))
             (etypecase ,',name ,@',type-clauses))
           ,',cffi-type)))))

(define-designator pointer-or-nil :pointer
  (null (null-pointer))
  (foreign-pointer pointer-or-nil))

(define-designator bool :int
  (null 0)
  (t    1))


;;;-------------------------------------------------------------------------
;;; Other Types
;;;-------------------------------------------------------------------------

;;; FIXME: with fd namespaces on Linux, someday this might be no
;;; longer correct
(deftype fd   () '(integer 0 65535))
