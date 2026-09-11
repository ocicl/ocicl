(uiop:define-package #:40ants-doc-full/locatives/section
  (:use #:cl)
  (:import-from #:40ants-doc/reference
                #:make-reference)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/locatives/base
                #:define-locative-type
                #:locate-object
                #:locate-and-find-source)
  (:import-from #:40ants-doc/locatives
                #:section)
  (:import-from #:40ants-doc/core
                ;; #:section
                )
  (:import-from #:40ants-doc/source-api
                #:find-source))
(in-package #:40ants-doc-full/locatives/section)


(define-locative-type section ()
  "Refers to a section defined by 40ANTS-DOC:DEFSECTION.")

(defmethod locate-object (symbol (locative-type (eql 'section))
                          locative-args)
  (declare (ignore locative-args))
  (unless (typep (symbol-value symbol)
                 '40ants-doc/core::section)
    (error "Section locative works only with objects defined by 40ANTS-DOC:DEFSECTION."))
  (symbol-value symbol))

(defmethod canonical-reference ((section 40ants-doc/core::section))
  (make-reference (40ants-doc/core::section-name section)
                  'section))

(defmethod find-source ((section 40ants-doc/core::section))
  (locate-and-find-source (40ants-doc/core::section-name section) 'variable ()))
