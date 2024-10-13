;;;; conditions
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(define-condition tar-condition ()
  ()
  (:documentation "The base condition."))

(define-condition tar-error (tar-condition error)
  ()
  (:documentation "The base error condition."))

(define-condition unsupported-property (tar-error)
  ((name
    :initarg :name
    :reader unsupported-property-name
    :documentation "The name of the unsupported property."))
  (:documentation "Signaled when a property is set or accessed that the
underlying tar file cannot represent."))

(defun ignore-unsupported-property (&optional value condition)
  "A restart to ignore an UNSUPPORTED-PROPERTY CONDITION. Either returns VALUE
from the accessor or silently ignores the attempt to set the value."
  (let ((use-value (find-restart 'use-value condition)))
    (if (null use-value)
        (invoke-restart (find-restart 'ignore-unsupported-property condition))
        (invoke-restart use-value value))))

(defun call-with-ignored-unsupported-properties (thunk &optional value)
  (handler-bind
      ((unsupported-property (lambda (c) (ignore-unsupported-property value c))))
    (funcall thunk)))

(defmacro with-ignored-unsupported-properties ((&optional value) &body body)
  "Execute BODY in a context where UNSUPPORTED-PROPERTY errors are ignored and
VALUE is returned from any attempt to access them."
  `(call-with-ignored-unsupported-properties (lambda () ,@body) ,value))

(define-condition unbound-property (tar-error)
  ((name
    :initarg :name
    :reader unbound-property-name
    :documentation "The name of the unbound property."))
  (:documentation "Signaled when a property is accessed that is unbound."))

(define-condition required-property-missing (tar-error)
  ((name
    :initarg :name
    :reader required-property-missing-name
    :documentation "The name of the missing property."))
  (:documentation
   "Signaled when trying to WRITE-ENTRY with a required property that is
missing."))

(define-condition unsupported-property-value (tar-error)
  ((name
    :initarg :name
    :reader unsupported-property-value-name
    :documentation "The name of the unsupported property.")
   (value
    :initarg :value
    :reader unsupported-property-value-value
    :documentation "The value of the unsupported property."))
  (:documentation
   "Signaled when trying to WRITE-ENTRY with a property that is unsupported by
the underlying archive type."))

(define-condition property-value-too-long (unsupported-property-value)
  ()
  (:documentation
   "Signaled when trying to WRITE-ENTRY with a property that is too long for
the underlying archive type."))

(defun truncate-value (&optional condition)
  "Truncate the value and write it to the archive."
  (invoke-restart (find-restart 'truncate-value condition)))

(defun call-with-truncated-unsupported-values (thunk properties)
  (handler-bind
      ((unsupported-property-value (lambda (c)
                                     (when (and (find-restart 'truncate-value c)
                                                (or (eql properties t)
                                                    (member (unsupported-property-value-name c)
                                                            properties)))
                                       (truncate-value c)))))
    (funcall thunk)))

(defmacro with-truncated-unsupported-values ((&key (properties t)) &body body)
  "Evaluate BODY in a context where truncatable values are automatically
truncated."
  `(call-with-truncated-unsupported-values (lambda () ,@body) ,properties))
