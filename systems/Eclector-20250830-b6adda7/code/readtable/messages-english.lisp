(cl:in-package #:eclector.readtable)

(macrolet ((define-reporter (((condition-var condition-specializer) stream-var)
                             &body body)
             `(defmethod acclimation:report-condition
                  ((,condition-var ,condition-specializer)
                   ,stream-var
                   (language acclimation:english))
                ,@body)))

  (define-reporter ((condition unterminated-dispatch-macro) stream)
    (format stream "~@<While reading dispatching macro character ~:C, ~
                    expected a sub-character when input ended.~@:>"
            (disp-char condition)))

  (define-reporter ((condition sub-char-must-not-be-a-decimal-digit) stream)
    (format stream "~@<The ~/eclector.base::describe-character-english/ ~
                    cannot be defined as a dispatch macro ~
                    sub-character, as it is a decimal digit.~@:>"
            (sub-char condition)))

  (define-reporter ((condition char-must-be-a-dispatching-character) stream)
    (format stream "~@<The ~/eclector.base::describe-character-english/ ~
                    cannot have a dispatch macro set for it, as it has ~
                    not been defined as a dispatch macro~@
                    (as by ~A)~@:>"
            (disp-char condition) 'cl:make-dispatch-macro-character))

  (define-reporter ((condition unknown-macro-sub-character) stream)
    (format stream "~@<No dispatch function is defined for the ~
                    ~/eclector.base::describe-character-english/ as a ~
                    sub-character of the dispatch macro ~
                    ~/eclector.base::describe-character-english/.~@:>"
            (sub-char condition) (disp-char condition))))
