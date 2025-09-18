(uiop:define-package #:40ants-doc-full/locatives/macro
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc-full/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/locatives
                #:macro)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:import-from #:40ants-doc/source-api
                #:find-source))
(in-package #:40ants-doc-full/locatives/macro)


(define-locative-type macro ())


(defmethod locate-object (symbol (locative-type (eql 'macro)) locative-args)
  (unless (macro-function symbol)
    (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defmethod 40ants-doc-full/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'macro)) locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (arglist (swank-backend:arglist symbol))
         (docstring (40ants-doc/docstring:get-docstring symbol 'function))
         ;; TODO:  we should move text transformation out from get-docstring to after it will be parsed
         (children (40ants-doc-full/commondoc/markdown:parse-markdown docstring)))

    (40ants-doc-full/commondoc/bullet:make-bullet reference
                                                  :arglist arglist
                                                  :children children
                                                  :ignore-words (40ants-doc-full/args::macro-arg-names arglist))))


(defmethod 40ants-doc/locatives/base:locate-and-find-source (symbol (locative-type (eql 'macro))
                                                             locative-args)
  (declare (ignore locative-args))
  (find-source (macro-function symbol)))
