(uiop:define-package #:40ants-doc-full/locatives/compiler-macro
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:import-from #:40ants-doc-full/args
                #:macro-arg-names))
(in-package #:40ants-doc-full/locatives/compiler-macro)


(define-locative-type compiler-macro ())

(defmethod locate-object (symbol (locative-type (eql 'compiler-macro))
                          locative-args)
  (unless (compiler-macro-function symbol)
    (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defmethod 40ants-doc-full/commondoc/builder:reference-to-commondoc ((symbol symbol) (locative-type (eql 'compiler-macro)) locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (arglist (swank-backend:arglist symbol))
         (docstring (40ants-doc/docstring:get-docstring symbol 'compiler-macro))
         (children (when docstring
                     (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))

    (40ants-doc-full/commondoc/bullet::make-bullet reference
                                                   :arglist locative-args
                                                   :children children
                                                   :dislocated-symbols (macro-arg-names arglist))))


(defmethod locate-and-find-source (symbol (locative-type (eql 'compiler-macro))
                                   locative-args)
  (declare (ignore locative-args))
  #-allegro
  (40ants-doc/source-api:find-source (compiler-macro-function symbol))
  #+allegro
  (find-one-location (swank-backend:find-definitions symbol)
                     '("compiler-macro")))
