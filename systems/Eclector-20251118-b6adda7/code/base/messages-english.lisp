(cl:in-package #:eclector.base)

;;; Utilities

(defun describe-character-english (stream character &optional colon at-sign)
  (declare (ignore colon at-sign))
  (format stream "character ~:[named ~A~*~;~*~C~]"
          (and (not (char= character #\Space)) (graphic-char-p character))
          (char-name character) character))

;;; Condition reports

(macrolet
    ((define-reporter (((condition-var condition-specializer) stream-var)
                       &body body)
       `(defmethod acclimation:report-condition
            ((,condition-var ,condition-specializer)
             ,stream-var
             (language acclimation:english))
          ,@body)))

  (define-reporter ((condition end-of-file) stream)
    (format stream "~@<Unexpected end of input.~@:>")))
