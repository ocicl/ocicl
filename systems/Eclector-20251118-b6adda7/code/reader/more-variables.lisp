(cl:in-package #:eclector.reader)

(defvar *skip-reason*)

;;; Labels

(defvar *labels*)

;;; Quasiquote syntax

(defparameter *quasiquotation-state* '(nil . nil))

(defparameter *quasiquotation-depth* 0)

;;; List syntax

;;; If this variable is bound to a function, that function will be
;;; called in %READ-DELIMITED-LIST with input stream and the close
;;; character, replacing the usual behavior of %READ-DELIMITED-LIST.
(defvar *list-reader* nil)

(defvar *consing-dot* '#:|.|)

(defparameter *consing-dot-allowed-p* nil)

(define-condition end-of-list (condition)
  ((%character :initarg :character :reader %character)))

(#+sbcl sb-ext:defglobal #-sbcl defvar **end-of-list**
  (make-condition 'end-of-list :character #\)))

(declaim (inline signal-end-of-list))
(defun signal-end-of-list (character)
  (if (char= character #\))
      (signal **end-of-list**)
      (signal 'end-of-list :character character)))
