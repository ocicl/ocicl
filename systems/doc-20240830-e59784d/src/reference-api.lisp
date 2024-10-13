(uiop:define-package #:40ants-doc/reference-api
  (:use #:cl)
  (:export
   #:canonical-reference
   #:*source-uri-fn*
   #:source-uri))
(in-package #:40ants-doc/reference-api)


(defgeneric canonical-reference (object)
  (:documentation "Return a 40ANTS-DOC/REFERENCE:REFERENCE that resolves to OBJECT."))


(defgeneric reference-name (obj name ref link))


(defvar *source-uri-fn* nil
  "Set this to a function of one argument.

   The argument of this function will be a 40ANTS-DOC/REFERENCE:REFERENCE
   object and the result should be a full URL leading to the web page where
   referenced object can be viewed. Usually this is a GitHub's page.

   When you are using 40ANTS-DOC-FULL/BUILDER:UPDATE-ASDF-SYSTEM-DOCS,
   this variable will be automatically bound to the result of
   40ANTS-DOC-FULL/GITHUB:MAKE-GITHUB-SOURCE-URI-FN function call if
   ASDF system has a :SOURCE-CONTROL slot.

   See 40ANTS-DOC-FULL/GITHUB:MAKE-GITHUB-SOURCE-URI-FN for details.")


(defun source-uri (reference)
  "Returns URI for the reference object
   if *SOURCE-URI-FN* is bound to a function."
  (let ((fn *source-uri-fn*))
    (when fn
      (funcall fn reference))))
