;;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-Lisp; Package: ANAPHORA -*-

;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author, 
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(in-package :anaphora)

;;; This was the original implementation of SYMBOLIC -- and still good
;;; for getting the basic idea. Brian Masterbrooks solution to
;;; infinite recusion during macroexpansion, that nested forms of this
;;; are subject to, is in symbolic.lisp.
;;;
;;; (defmacro symbolic (op test &body body &environment env)
;;;   `(symbol-macrolet ((it ,test))
;;;        (,op it ,@body)))

(defmacro alet (form &body body)
  "Binds the FORM to IT (via LET) in the scope of the BODY."
  `(anaphoric ignore-first ,form (progn ,@body)))

(defmacro slet (form &body body)
  "Binds the FORM to IT (via SYMBOL-MACROLET) in the scope of the BODY. IT can
be set with SETF."
  `(symbolic ignore-first ,form (progn ,@body)))

(defmacro aand (first &rest rest)
  "Like AND, except binds the first argument to IT (via LET) for the
scope of the rest of the arguments."
  `(anaphoric and ,first ,@rest))

(defmacro sor (first &rest rest)
  "Like OR, except binds the first argument to IT (via SYMBOL-MACROLET) for
the scope of the rest of the arguments. IT can be set with SETF."
  `(symbolic or ,first ,@rest))

(defmacro aif (test then &optional else)
  "Like IF, except binds the result of the test to IT (via LET) for
the scope of the then and else expressions."  
  `(anaphoric if ,test ,then ,else))

(defmacro sif (test then &optional else)
  "Like IF, except binds the test form to IT (via SYMBOL-MACROLET) for
the scope of the then and else expressions. IT can be set with SETF"
  `(symbolic if ,test ,then ,else))

(defmacro asif (test then &optional else)
  "Like IF, except binds the result of the test to IT (via LET) for
the the scope of the then-expression, and the test form to IT (via
SYMBOL-MACROLET) for the scope of the else-expression. Within scope of
the else-expression, IT can be set with SETF."
    `(let ((it ,test))
       (if it
	   ,then
	   (symbolic ignore-first ,test ,else))))

(defmacro aprog1 (first &body rest)
  "Binds IT to the first form so that it can be used in the rest of the
forms. The whole thing returns IT."
  `(anaphoric prog1 ,first ,@rest))

(defmacro awhen (test &body body)
  "Like WHEN, except binds the result of the test to IT (via LET) for the scope
of the body."
  `(anaphoric when ,test ,@body))

(defmacro swhen (test &body body)
  "Like WHEN, except binds the test form to IT (via SYMBOL-MACROLET) for the
scope of the body. IT can be set with SETF."
  `(symbolic when ,test ,@body))

(defmacro sunless (test &body body)
  "Like UNLESS, except binds the test form to IT (via SYMBOL-MACROLET) for the
scope of the body. IT can be set with SETF."
  `(symbolic unless ,test ,@body))

(defmacro acase (keyform &body cases)
  "Like CASE, except binds the result of the keyform to IT (via LET) for the
scope of the cases."
  `(anaphoric case ,keyform ,@cases))

(defmacro scase (keyform &body cases)
  "Like CASE, except binds the keyform to IT (via SYMBOL-MACROLET) for the
scope of the body. IT can be set with SETF."
  `(symbolic case ,keyform ,@cases))

(defmacro aecase (keyform &body cases)
  "Like ECASE, except binds the result of the keyform to IT (via LET) for the
scope of the cases."
  `(anaphoric ecase ,keyform ,@cases))

(defmacro secase (keyform &body cases)
  "Like ECASE, except binds the keyform to IT (via SYMBOL-MACROLET) for the
scope of the cases. IT can be set with SETF."
  `(symbolic ecase ,keyform ,@cases))
  
(defmacro accase (keyform &body cases)
  "Like CCASE, except binds the result of the keyform to IT (via LET) for the
scope of the cases. Unlike CCASE, the keyform/place doesn't receive new values
possibly stored with STORE-VALUE restart; the new value is received by IT."
  `(anaphoric ccase ,keyform ,@cases))

(defmacro sccase (keyform &body cases)
  "Like CCASE, except binds the keyform to IT (via SYMBOL-MACROLET) for the
scope of the cases. IT can be set with SETF."
  `(symbolic ccase ,keyform ,@cases))

(defmacro atypecase (keyform &body cases)
  "Like TYPECASE, except binds the result of the keyform to IT (via LET) for
the scope of the cases."
  `(anaphoric typecase ,keyform ,@cases))

(defmacro stypecase (keyform &body cases)
  "Like TYPECASE, except binds the keyform to IT (via SYMBOL-MACROLET) for the
scope of the cases. IT can be set with SETF."
  `(symbolic typecase ,keyform ,@cases))

(defmacro aetypecase (keyform &body cases)
  "Like ETYPECASE, except binds the result of the keyform to IT (via LET) for
the scope of the cases."
  `(anaphoric etypecase ,keyform ,@cases))

(defmacro setypecase (keyform &body cases)
  "Like ETYPECASE, except binds the keyform to IT (via SYMBOL-MACROLET) for
the scope of the cases. IT can be set with SETF."
  `(symbolic etypecase ,keyform ,@cases))

(defmacro actypecase (keyform &body cases)
  "Like CTYPECASE, except binds the result of the keyform to IT (via LET) for
the scope of the cases. Unlike CTYPECASE, new values possible stored by the
STORE-VALUE restart are not received by the keyform/place, but by IT."
  `(anaphoric ctypecase ,keyform ,@cases))

(defmacro sctypecase (keyform &body cases)
  "Like CTYPECASE, except binds the keyform to IT (via SYMBOL-MACROLET) for
the scope of the cases. IT can be set with SETF."
  `(symbolic ctypecase ,keyform ,@cases))

(defmacro acond (&body clauses)
  "Like COND, except result of each test-form is bound to IT (via LET) for the
scope of the corresponding clause."
  (labels ((rec (clauses)
	     (if clauses
		 (destructuring-bind ((test &body body) . rest)  clauses
		   (if body
		       `(anaphoric if ,test (progn ,@body) ,(rec rest))
		       `(anaphoric if ,test it ,(rec rest))))
		 nil)))
    (rec clauses)))

(defmacro scond (&body clauses)
  "Like COND, except each test-form is bound to IT (via SYMBOL-MACROLET) for the
scope of the corresponsing clause. IT can be set with SETF."
  (labels ((rec (clauses)
	     (if clauses
		 (destructuring-bind ((test &body body) . rest) clauses
		   (if body
		       `(symbolic if ,test (progn ,@body) ,(rec rest))
		       `(symbolic if ,test it ,(rec rest))))
		 nil)))
    (rec clauses)))

(defmacro alambda (lambda-list &body body)
  "Like LAMBDA, except that SELF is bound to the resulting function (via LABELS)
within BODY."
  `(labels ((self ,lambda-list ,@body))
     #'self))
