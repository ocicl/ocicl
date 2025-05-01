;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Resolver conditions.
;;;

(in-package :iolib/sockets)

(define-condition resolver-error (isys:iolib-error)
  ((datum :initarg :data :reader resolver-error-datum))
  (:documentation
   "Signaled when an error occurs while trying to resolve an address."))
(setf (documentation 'resolver-error-datum 'function)
      "Return the datum that caused the signalling of a RESOLVER-ERROR condition.")

(defmacro define-resolver-error (name format-string &optional documentation)
  `(define-condition ,name (resolver-error) ()
     (:report (lambda (condition stream)
                (format stream ,format-string (resolver-error-datum condition))))
     (:documentation ,documentation)))

(define-resolver-error resolver-again-error
  "Temporary failure occurred while resolving: ~S"
  "Condition signaled when a temporary failure occurred.")

(define-resolver-error resolver-fail-error
  "Non recoverable error occurred while resolving: ~S"
  "Condition signaled when a non-recoverable error occurred.")

(define-resolver-error resolver-no-name-error
  "Host or service not found: ~S"
  "Condition signaled when a host or service was not found.")

(define-resolver-error resolver-unknown-error
  "Unknown error while resolving: ~S"
  "Condition signaled when an unknown error is signaled while resolving
an address.")
