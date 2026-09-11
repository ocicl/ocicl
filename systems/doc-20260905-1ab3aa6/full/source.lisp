(uiop:define-package #:40ants-doc-full/source
  (:use #:cl)
  (:import-from #:40ants-doc/source-api
                #:find-source)
  (:import-from #:40ants-doc/reference)
  (:import-from #:swank)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader))
(in-package #:40ants-doc-full/source)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


;;; This is bound to an EQUAL hash table in MAKE-GITHUB-SOURCE-URI-FN
;;; to speed up FIND-SOURCE. It's still very slow though.
(defvar *find-source-cache* nil)

(defmethod find-source :around (object)
  (if *find-source-cache*
      (let ((key (if (typep object '40ants-doc/reference::reference)
                     (list (40ants-doc/reference::reference-object object)
                           (40ants-doc/reference::reference-locative object))
                     object)))
        (or (gethash key *find-source-cache*)
            (setf (gethash key *find-source-cache*)
                  (call-next-method))))
      (call-next-method)))


(defmethod find-source (object)
  ;; Probably this could be replaced with
  ;; https://quickdocs.org/definitions
  ;; or with
  ;; https://www.hexstreamsoft.com/libraries/definitions-systems/
  (swank:find-definition-for-thing object))
