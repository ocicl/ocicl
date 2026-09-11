;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- RETURN* wrappers.
;;;

(in-package :iolib/base)

(cl:defmacro defun (name args &body body)
  `(,(find-right-symbol :defun :series)
     ,name ,args ,@(wrap-body-for-return-star body name)))

(cl:defmacro defmethod (name args &body body)
  (cond
    ;; no method qualifier, this is actually the lambda-list
    ((listp args)
     `(,(find-right-symbol :defmethod)
        ,name ,args ,@(wrap-body-for-return-star body name)))
    ;; args is the method qualifier
    (t
     `(,(find-right-symbol :defmethod) ,name
        ,args ,(car body) ,@(wrap-body-for-return-star (cdr body) name)))))

(cl:defmacro lambda* (args &body body)
  `(,(find-right-symbol :lambda)
     ,args ,@(wrap-body-for-return-star body)))

(cl:defmacro defmacro (name args &body body)
  `(,(find-right-symbol :defmacro)
     ,name ,args ,@(wrap-body-for-return-star body name)))

(cl:defmacro define-compiler-macro (name args &body body)
  `(,(find-right-symbol :define-compiler-macro)
     ,name ,args ,@(wrap-body-for-return-star body name)))

(cl:defun find-right-symbol (name &rest packages)
  (multiple-value-bind (symbol foundp)
      (if (eql (find-symbol (string name) *package*)
               (find-symbol (string name) :iolib/base))
          ;; NAME has been imported from IOLIB.UTILS, so we must
          ;; find a default somewhere else, defaulting to the CL package
          (find-symbol (string name) (find-right-package packages))
          ;; use the symbol named NAME from the *PACKAGE* or CL
          (find-symbol (string name) (find-right-package (package-name *package*))))
    (assert foundp (symbol) "Couldn't find any symbol as default for ~S" name)
    (values symbol)))

(cl:defun find-right-package (packages)
  (dolist (pkg (ensure-list packages) :common-lisp)
    (when (member pkg (package-use-list *package*)
                  :key #'package-name
                  :test #'string-equal)
      (return pkg))))

(cl:defun wrap-body-for-return-star (body &optional block-name)
  (flet ((block-name (block)
           (etypecase block
             (cons (destructuring-bind (kind block-name) block
                     (assert (eql 'setf kind))
                     (check-type block-name symbol)
                     block-name))
             (symbol block))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      (with-gensyms (value)
        (remove-if
         #'null
         `(,docstring
           ,@declarations
           ,(if block-name
                `(macrolet ((return* (,value) `(return-from ,',(block-name block-name) ,,value)))
                   ,@body)
                (with-gensyms (block)
                  `(block ,block
                     (macrolet ((return* (value) `(return-from ,',block ,value)))
                       ,@body))))))))))
