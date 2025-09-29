(cl:in-package #:eclector.reader.test)

;;; Client for testing SHARPSIGN-S

(defclass sharpsign-s-client () ())

(defmethod eclector.reader:make-structure-instance
    ((client sharpsign-s-client) (name t) (initargs t))
  (list* name initargs))

;;; Client for testing invalid `*read-default-float-format*' value

(defclass invalid-float-format-client () ())

(defmethod eclector.reader:state-value
    ((client invalid-float-format-client)
     (aspect (eql '*read-default-float-format*)))
  ;; We use a valid type designator to avoid problems when the value
  ;; is passed to `typep'.
  'character)
