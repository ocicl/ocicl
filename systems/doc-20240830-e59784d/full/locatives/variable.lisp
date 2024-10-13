(uiop:define-package #:40ants-doc-full/locatives/variable
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc-full/utils)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc-full/locatives/utils)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc-full/commondoc/markdown
                #:parse-markdown))
(in-package #:40ants-doc-full/locatives/variable)


(define-locative-type variable (&optional initform)
  "Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (assert (<= (length locative-args) 1))
  (40ants-doc/reference:make-reference symbol (cons locative-type locative-args)))


(defmethod 40ants-doc-full/commondoc/builder:reference-to-commondoc ((symbol symbol) (locative-type (eql 'variable)) locative-args)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (let* ((reference (canonical-reference
                       (40ants-doc/reference:make-reference symbol
                                                            (cons locative-type
                                                                  locative-args))))
           (docstring (40ants-doc/docstring:get-docstring symbol 'variable))
           (arglist (multiple-value-bind (value unboundp) (40ants-doc-full/utils::symbol-global-value symbol)
                      (cond (initformp
                             (prin1-to-string initform))
                            (unboundp "-unbound-")
                            (t
                             (prin1-to-string value)))))
           (children (when docstring
                       (parse-markdown docstring))))

      (40ants-doc-full/commondoc/bullet:make-bullet reference
                                                    :arglist arglist
                                                    :children children
                                                    :dislocated-symbols symbol))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc-full/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                      '("variable" "defvar" "defparameter"
                                                        "special-declaration")))

(defvar end-of-variable-example)
