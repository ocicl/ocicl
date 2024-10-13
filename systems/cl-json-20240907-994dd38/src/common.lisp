;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10; Package: JSON -*-
;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defmacro let-gensyms ((&rest names) &body body)
  `(let ,(loop for name in names collect `(,name (gensym)))
     ,@body))

;;; Custom variables

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *custom-vars* nil)

(defmacro with-shadowed-custom-vars (&body body)
  `(let ,(loop for (var) in *custom-vars*
            collect `(,var (if (boundp ',var) ,var)))
     ,@body))

(defun custom-key-to-variable (key)
  (car (rassoc key *custom-vars*)))

(defmacro loop-on-custom ((key var &optional value) &rest clauses)
  (if value
      (destructuring-bind (key-args . clauses) clauses
        `(loop for (,key ,value) on ,key-args by #'cddr
            for ,var = (custom-key-to-variable ,key)
            if ,var ,@clauses))
      `(loop for (,var . ,key) in *custom-vars*
            ,@clauses)))

(defmacro set-custom-vars (&rest customizations)
  `(setq
    ,@(loop-on-custom (key var value) customizations
         append (list var value))))

(defmacro bind-custom-vars ((&rest customizations) &body body)
  `(let ,(loop-on-custom (key var value) customizations
            collect (list var value))
     ,@body))

)

(defmacro define-custom-var ((key name) &rest other-args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn (pushnew '(,name . ,key) *custom-vars* :test #'equal)
            (defvar ,name ,@other-args))))


;;; Characters

(defparameter +json-lisp-escaped-chars+
  '((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . #\)
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)
    (#\u . (4 . 16)))
  "Mapping between JSON String escape sequences and Lisp chars.")

(defvar *use-strict-json-rules* t
  "If non-nil, signal error on unrecognized escape sequences in JSON
Strings.  If nil, translate any such sequence to the char after
slash.")


;;; Symbols

(defparameter +json-lisp-symbol-tokens+
  '(("true" . t)
    ("null" . nil)
    ("false" . nil))
  "Mapping between JSON literal names and Lisp boolean values.")

(defvar *json-symbols-package* (find-package 'keyword)
  "The package where JSON Object keys etc. are interned.
Default KEYWORD, NIL = use current *PACKAGE*.")

(defun json-intern (string)
  "Intern STRING in the current *JSON-SYMBOLS-PACKAGE*."
  (intern string (or *json-symbols-package* *package*)))

(define-condition unknown-symbol-error (parse-error)
  ((datum :accessor unknown-symbol-error-datum :initarg :datum))
  (:documentation
   "Signalled by safe-json-intern when a symbol that is 
not already interned in *json-symbols-package* is found.")
  (:report
   (lambda (condition stream)
     (format stream
             "SAFE-JSON-INTERN only allows previously interned symbols, ~A is not interned in *json-symbols-package*"
             (unknown-symbol-error-datum condition)))))

(defun unknown-symbol-error (string)
  (error 'unknown-symbol-error :datum string))

(defun safe-json-intern (string)
  "The default json-intern is not safe. Interns of many 
unique symbols could potentially use a lot of memory.
An attack could exploit this by submitting something that is passed
through cl-json that has many very large, unique symbols. This version
is safe in that respect because it only allows symbols that already 
exists."
  (or (find-symbol string
                   (or *json-symbols-package* *package*))
      (unknown-symbol-error string)))

(defvar *json-identifier-name-to-lisp* 'camel-case-to-lisp
  "Designator for a function which maps string (a JSON Object key) to
string (name of a Lisp symbol).")

(defvar *lisp-identifier-name-to-json* 'lisp-to-camel-case
  "Designator for a function which maps string (name of a Lisp symbol)
to string (e. g. JSON Object key).")

(defvar *identifier-name-to-key* 'json-intern
  "Designator for a function which, during decoding, maps the *json-identifier-name-to-lisp*
-transformed key to the value it will have in the result object.")
