(uiop:define-package #:40ants-doc-full/commondoc/piece
  (:use #:cl)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/object-package))
(in-package #:40ants-doc-full/commondoc/piece)


(defclass documentation-piece ()
  ((doc-reference :initarg :doc-reference
                  :type 40ants-doc/reference::reference
                  :reader doc-reference))
  (:documentation "This class is a mixin to be added to any common doc node, which can be linked to a 40ANTS-DOC/REFERENCE::REFERENCE"))


(defmethod 40ants-doc/object-package::object-package ((obj documentation-piece))
  (let* ((reference (doc-reference obj))
         (ref-object (40ants-doc/reference:resolve reference))
         (package (40ants-doc/object-package::object-package ref-object)))
    package))
