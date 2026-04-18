(uiop:define-package #:40ants-doc-full/locatives/constant
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
  (:import-from #:40ants-doc-full/locatives/utils)
  (:import-from #:40ants-doc/locatives
                #:constant)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown))
(in-package #:40ants-doc-full/locatives/constant)


(define-locative-type constant (&optional initform)
  "Refers to a DEFCONSTANT. INITFORM, or if not specified,
  the value of the constant is included in the documentation.")


(defmethod locate-object (symbol (locative-type (eql 'constant)) locative-args)
  (assert (<= (length locative-args) 1))
  (assert (constantp symbol))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defmethod 40ants-doc-full/commondoc/builder:reference-to-commondoc ((symbol symbol) (locative-type (eql 'constant)) locative-args)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (let* ((reference (canonical-reference
                       (40ants-doc/reference::make-reference
                        symbol (cons locative-type locative-args))))
           (docstring (40ants-doc/docstring:get-docstring symbol 'variable))
           (arglist (cond (initformp
                           (prin1-to-string initform))
                          ((boundp symbol)
                           (prin1-to-string
                            (symbol-value symbol)))
                          (t "-unbound-")))
           (children (when docstring
                       (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))

      (40ants-doc-full/commondoc/bullet::make-bullet reference
                                                     :arglist arglist
                                                     :children children
                                                     :dislocated-symbols symbol))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'constant))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc-full/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                      '("defconstant" "constant" "variable")))
