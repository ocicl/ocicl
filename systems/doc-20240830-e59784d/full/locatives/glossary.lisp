(uiop:define-package #:40ants-doc-full/locatives/glossary
  (:use #:cl)
  ;; (:import-from #:40ants-doc/core)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/locatives
                #:glossary-term)
  (:import-from #:40ants-doc/glossary)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:to-commondoc)
  (:import-from #:40ants-doc-full/commondoc/bullet
                #:make-bullet)
  (:import-from #:40ants-doc/docstring
                #:strip-docstring-indentation)
  (:import-from #:40ants-doc-full/utils
                #:maybe-downcase))
(in-package #:40ants-doc-full/locatives/glossary)


(40ants-doc/locatives/base:define-locative-type glossary-term ()
  "Refers to a glossary term defined by 40ANTS-DOC/GLOSSARY:DEFINE-GLOSSARY-TERM.")

(defmethod 40ants-doc/locatives/base::locate-object (symbol (locative-type (eql 'glossary-term))
                                                     locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) '40ants-doc/glossary::glossary-term))
  (symbol-value symbol))


(defun glossary-term-title-or-name (glossary-term)
  (check-type glossary-term 40ants-doc/glossary::glossary-term)
  (or (40ants-doc/glossary::glossary-term-title glossary-term)
      (maybe-downcase
       (prin1-to-string (40ants-doc/glossary::glossary-term-name glossary-term)))))


(defmethod to-commondoc ((glossary-term 40ants-doc/glossary::glossary-term))
  (let* ((symbol (40ants-doc/glossary::glossary-term-name glossary-term))
         (reference
           (40ants-doc/reference::canonical-reference (40ants-doc/reference::make-reference
                                                       symbol '(glossary-term))))
         (docstring (let ((docstring (40ants-doc/glossary::glossary-term-docstring glossary-term)))
                      (when docstring
                        (strip-docstring-indentation docstring))))
         (children (when docstring
                     (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))

    (make-bullet reference
                 :name (glossary-term-title-or-name glossary-term)
                 :children children
                 :ignore-words symbol)))

(defmethod 40ants-doc/reference-api:canonical-reference ((glossary-term 40ants-doc/glossary::glossary-term))
  (40ants-doc/reference:make-reference (40ants-doc/glossary::glossary-term-name glossary-term) 'glossary-term))

(defmethod 40ants-doc/source-api:find-source ((glossary-term 40ants-doc/glossary::glossary-term))
  (40ants-doc/locatives/base:locate-and-find-source (40ants-doc/glossary::glossary-term-name glossary-term) 'variable ()))


