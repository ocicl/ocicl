(cl:in-package #:acclimation)

(defgeneric documentation (name type language))

(defgeneric (setf documentation) (documentation name type language))

(defmethod documentation (name type (language english))
  (cl:documentation name type))

(defmethod (setf documentation) (documentation name type (language english))
  (setf (cl:documentation name type) documentation))
