(cl:in-package #:eclector.readtable.simple)

(defmethod recovery-description-using-language
    ((strategy (eql 'ignore-partial-dispatch-macro))
     (language acclimation:english))
  "Ignore the incomplete dispatch macro.")

(defmethod recovery-description-using-language
    ((strategy (eql 'ignore-partial-macro))
     (language acclimation:english))
  "Ignore the entire reader macro.")
