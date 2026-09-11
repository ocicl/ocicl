;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :iolib/base)

;;;-------------------------------------------------------------------------
;;; Subtype Errors
;;;-------------------------------------------------------------------------

(define-condition subtype-error (error)
  ((datum :initarg :type :reader subtype-error-datum)
   (expected-supertype :initarg :expected-supertype
                       :reader subtype-error-expected-supertype))
  (:report
   (lambda (condition stream)
     (format stream "~S is not a recognizable subtype of ~S"
             (subtype-error-datum condition)
             (subtype-error-expected-supertype condition)))))


;;;-------------------------------------------------------------------------
;;; Literal Syntax Errors
;;;-------------------------------------------------------------------------

(define-condition unknown-literal-syntax (reader-error)
  ((name :initarg :name :reader unknown-literal-syntax-name))
  (:report (lambda (condition stream)
             (format stream "Unknown literal read syntax: ~S"
                     (unknown-literal-syntax-name condition)))))


;;;-------------------------------------------------------------------------
;;; Bugs
;;;-------------------------------------------------------------------------

(define-condition iolib-bug (error)
  ((message :initarg :message :reader iolib-bug-message))
  (:report
   (lambda (condition stream)
     (format stream "~A.~%This seems to be a bug in IOlib. ~
                     Please report it to iolib-devel@common-lisp.net"
             (iolib-bug-message condition)))))

(defun bug (control &rest args)
  (error 'iolib-bug :message (format nil "~?" control args)))
