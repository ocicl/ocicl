;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Declaring forms as foldable(pure)
;;;

(in-package :iolib/base)

#+sbcl
(defmacro %deffoldable (func argument-types return-type)
  `(sb-c:defknown ,func ,argument-types ,return-type (sb-c:foldable)
     :overwrite-fndb-silently t))

#-(or sbcl)
(defmacro %deffoldable (&rest args)
  (declare (ignore args)))

(defun constantp (form &optional env)
  (cl:constantp (if (symbolp form)
                    (macroexpand form env)
                    form)
                env))

(defun constant-form-value (form &optional env)
  (declare (ignorable env))
  #+clozure
  (ccl::eval-constant form)
  #+sbcl
  (sb-int:constant-form-value form env)
  #-(or clozure sbcl)
  (eval form))

(defmacro deffoldable (func &optional
                       (argument-types (list t))
                       (return-type t))
  (alexandria:with-gensyms (form env args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%deffoldable ,func ,argument-types ,return-type)
       (define-compiler-macro ,func (&whole ,form &rest ,args
                                            &environment ,env)
         (declare (ignore ,args))
         (if (constantp ,form ,env)
             (constant-form-value ,form ,env)
             ,form)))))
