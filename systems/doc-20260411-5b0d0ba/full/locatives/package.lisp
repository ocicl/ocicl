(uiop:define-package #:40ants-doc-full/locatives/package
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown))
(in-package #:40ants-doc-full/locatives/package)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(define-locative-type package ())

(defmethod locate-object (symbol (locative-type (eql 'package))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (or (find-package symbol) (locate-error)))

(defmethod canonical-reference ((package package))
  (40ants-doc/reference::make-reference (package-name package) 'package))


(defmethod 40ants-doc-full/commondoc/builder:to-commondoc ((package package))
  (let* ((reference (canonical-reference package))
         (symbol (package-name package))
         (docstring (40ants-doc/docstring:get-docstring package t))
         (children (when docstring
                     (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))
    (40ants-doc-full/commondoc/bullet:make-bullet reference
                                                  :children children
                                                  :ignore-words symbol)))
