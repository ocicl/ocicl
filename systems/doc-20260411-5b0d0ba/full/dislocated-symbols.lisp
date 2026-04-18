(uiop:define-package #:40ants-doc-full/dislocated-symbols
  (:use #:cl)
  (:export
   #:dislocated-symbols
   #:supports-dislocated-symbols-p))
(in-package #:40ants-doc-full/dislocated-symbols)


(defgeneric supports-dislocated-symbols-p (obj)
  (:method ((obj t))
    nil))


(defgeneric dislocated-symbols (obj))

