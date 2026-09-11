(uiop:define-package #:40ants-doc/locatives/asdf-system
  (:use #:cl)
  (:export #:asdf-system-documentation-title))
(in-package #:40ants-doc/locatives/asdf-system)


(defgeneric asdf-system-documentation-title (system)
  (:documentation "Returns a title for a section describing an ASDF system.

                   You might want to define a method using EQL specializer
                   to make a title shorter or to remove a system name from it.")
  
  (:method ((system asdf:system))
    (format nil "~A ASDF System Details"
            (string-upcase
             (asdf:primary-system-name system)))))
