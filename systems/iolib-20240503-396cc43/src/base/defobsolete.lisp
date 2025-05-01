;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Declaring forms as obsolete.
;;;

(in-package :iolib/base)

(define-condition deprecation-warning (style-warning)
  ((function-name :initarg :function-name :reader deprecation-warning-function-name)
   (type :initarg :type :reader deprecation-warning-type)
   (reason :initarg :reason :reader deprecation-warning-reason))
  (:report (lambda (condition stream)
             (format stream "~A is an obsolete ~A~@[; ~A~]"
                     (deprecation-warning-function-name condition)
                     (deprecation-warning-type condition)
                     (deprecation-warning-reason condition))))
  (:documentation "Warning signaled at compile-time indicating that a certain function has been deprecated."))

(defun setf-function-name-p (function-name)
  ;; FIXME: This would be better written using pattern matching
  (and (consp function-name)
       (eql 'setf (first function-name))
       (symbolp (second function-name))
       (null (cddr function-name))))

(defun function-name-p (function-name)
  "Returns T if FUNCTION-NAME is a legal function name:
a symbol or a list (CL:SETF symbol)."
  (or (symbolp function-name)
      (setf-function-name-p function-name)))

(deftype function-name ()
  "A legal function name: a symbol or a list (CL:SETF symbol)."
  `(or symbol (and cons (satisfies setf-function-name-p))))

(defun signal-obsolete (function-name reason type action)
  (funcall (ecase action
             (:warn #'warn)
             (:error #'error))
           'deprecation-warning :function-name function-name
           :type type :reason reason))

(defmacro defobsolete (function-name reason &key (type "function") (action :warn))
  "Declare the function denoted by FUNCTION-NAME as obsolete. REASON must
either be a string or the name of a function to be used as alternative.
ACTION chooses the function used to signal the deprecation warning:
if :WARN then CL:WARN will be used, if :ERROR then CL:ERROR."
  (check-type function-name function-name "a legal function name")
  (check-type reason (or function-name string) "a legal function name or a string")
  (check-type type (or symbol string))
  (check-type action (member :warn :error))
  (when (function-name-p reason)
    (setf reason (format nil "use ~A instead." reason)))
  `(define-compiler-macro ,function-name (&whole whole &rest args)
     (declare (ignore args))
     (signal-obsolete ',function-name ,reason ',type ,action)
     whole))
