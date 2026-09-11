(uiop:define-package #:40ants-doc/object-package
  (:use #:cl)
  (:export #:object-package))
(in-package #:40ants-doc/object-package)


(defgeneric object-package (object)
  (:documentation "Should return a package object where locative object was defined.

                   This package will be bound to *PACKAGE* when processing the documentation piece
                   using 40ANTS-DOC-FULL/COMMONDOC/BUILDER:TO-COMMONDOC generic-function.")
  
  (:method ((object t))
    (warn "Unable to figure out *package* for object ~S"
          object)
    nil))
