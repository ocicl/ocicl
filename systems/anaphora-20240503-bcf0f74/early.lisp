;;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-Lisp; Package: ANAPHORA -*-

;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author, 
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(in-package :anaphora)

(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
		     (destructuring-bind (var prefix)
			 (if (consp binding) binding (list binding binding))
		       `(,var (gensym ,(string prefix)))))
		 bindings)
     ,@body))

(defmacro ignore-first (first expr)
  (declare (ignore first))
  expr)
