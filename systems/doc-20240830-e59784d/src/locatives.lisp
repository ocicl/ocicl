(uiop:define-package #:40ants-doc/locatives
  (:use #:cl)
  (:documentation "This package holds all symbols denoting 40ANTS-DOC locatives.

                   It serves for a forward declaration of supported locatives.
                   To build documentation you'll need to load the 40ANTS-DOC-FULL system
                   which includes methods supporting these locatives.")
  (:import-from #:asdf
                #:system)
  (:export #:argument
           #:system
           #:class
           #:compiler-macro
           #:constant
           ;; TODO: probably remove?
           #:dislocated
           #:function
           #:generic-function
           #:glossary-term
           #:include
           #:stdout-of
           #:locative
           #:macro
           #:method
           #:package
           #:restart
           #:section
           #:accessor
           #:reader
           #:writer
           #:structure-accessor
	   #:symbol-macro
           #:type
           #:variable))
(in-package #:40ants-doc/locatives)

