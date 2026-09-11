;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Creating aliases in CL namespaces
;;;

(in-package :iolib/base)

(defvar *namespaces* nil)

(defmacro defalias (alias original)
  (destructuring-bind (namespace new-name)
      alias
    (assert (member namespace *namespaces*) (namespace)
            "Namespace ~A does not exist" namespace)
    `(progn
       ,@(make-alias namespace original new-name)
       ',new-name)))

(defmacro defnamespace (namespace &optional docstring)
  (check-type namespace symbol)
  (check-type docstring (or null string))
  `(progn
     (pushnew ',namespace *namespaces*)
     ,@(when docstring
         `((handler-bind ((warning #'muffle-warning))
             (setf (documentation ',namespace 'namespace) ,docstring))))
     ',namespace))

(defgeneric make-alias (namespace original alias))

(defnamespace function
  "The namespace of ordinary and generic functions.")

(defmethod make-alias ((namespace (eql 'function))
                       original alias)
  `((setf (fdefinition ',alias)
          (fdefinition ',original))
    (setf (documentation ',alias 'function)
          (documentation ',original 'function))
    (defalias (compiler-macro ,alias) ,original)))

(defnamespace macro
  "The namespace of macros.")

(defmethod make-alias ((namespace (eql 'macro))
                       original alias)
  `((setf (macro-function ',alias)
          (macro-function ',original))
    (setf (documentation ',alias 'function)
          (documentation ',original 'function))))

(defnamespace compiler-macro
  "The namespace of compiler macros.")

(defmethod make-alias ((namespace (eql 'compiler-macro))
                       original alias)
  `((setf (compiler-macro-function ',alias)
          (compiler-macro-function ',original))
    (setf (documentation ',alias 'compiler-macro)
          (documentation ',original 'compiler-macro))))

(defnamespace special
  "The namespace of special variables.")

(defmethod make-alias ((namespace (eql 'special))
                       original alias)
  `((define-symbol-macro ,alias ,original)
    (setf (documentation ',alias 'variable)
          (documentation ',original 'variable))))

(defnamespace constant
  "The namespace of constant variables.")

(defmethod make-alias ((namespace (eql 'constant))
                       original alias)
  `((define-symbol-macro ,alias ,original)
    (setf (documentation ',alias 'variable)
          (documentation ',original 'variable))))

(defnamespace class
  "The namespace of classes.")

(defmethod make-alias ((namespace (eql 'class))
                       original alias)
  `((setf (find-class ,alias)
          (find-class ,original))
    (setf (documentation ',alias 'type)
          (documentation ',original 'type))))
