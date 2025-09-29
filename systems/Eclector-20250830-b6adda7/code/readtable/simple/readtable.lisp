(cl:in-package #:eclector.readtable.simple)

(defclass readtable ()
  ((%syntax-types
    :initform (make-hash-table)
    :reader syntax-types)
   (%macro-characters
    :initform (make-hash-table)
    :reader macro-characters)
   (%dispatch-macro-characters
    :initform (make-hash-table)
    :reader dispatch-macro-characters)
   (%readtable-case :initform :upcase
                    :accessor eclector.readtable:readtable-case)))
