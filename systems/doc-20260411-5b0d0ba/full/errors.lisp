(uiop:define-package #:40ants-doc-full/errors
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of)
  (:export #:40ants-doc-warning
           #:object-is-not-documented
           #:object-reference
           #:breadcrumbs))
(in-package #:40ants-doc-full/errors)


(define-condition 40ants-doc-warning (warning)
  ())


(define-condition object-is-not-documented (40ants-doc-warning)
  ((reference :initarg :reference
              :reader object-reference
              :documentation "Object of class 40ANTS-DOC-FULL/COMMONDOC/XREF:XREF.")
   (breadcrumbs :initarg :breadcrumbs
                :type (soft-list-of string)
                :reader breadcrumbs))
  (:report (lambda (err stream)
             (format stream
                     "Object referenced as ~A in ~{~A~^ / ~} is not documented."
                     (object-reference err)
                     (breadcrumbs err)))))
