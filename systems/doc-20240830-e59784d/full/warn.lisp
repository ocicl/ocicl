(uiop:define-package #:40ants-doc-full/warn
  (:use #:cl))
(in-package #:40ants-doc-full/warn)


(defvar *dont-warn-on* nil
  "This list will contain strings
   defined on certain locatives using DONT-WARN-ON macro.")


(defmacro with-ignored (names &body body)
  `(let ((*dont-warn-on* (append ,names
                                 *dont-warn-on*)))
     ,@body))


(defun ignore-p (name)
  (member (string-trim '(#\. #\, #\? #\!)
                       name)
          *dont-warn-on*
          :test #'string-equal))
