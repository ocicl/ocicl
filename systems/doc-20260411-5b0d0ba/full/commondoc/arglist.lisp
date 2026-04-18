(uiop:define-package #:40ants-doc-full/commondoc/arglist
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-doc.format)
  (:import-from #:common-html.emitter)
  (:import-from #:commondoc-markdown)
  (:import-from #:spinneret
                #:with-html)
  (:import-from #:40ants-doc-full/utils
                #:maybe-downcase))
(in-package #:40ants-doc-full/commondoc/arglist)


(defclass arglist (common-doc:document-node)
  ((text :initarg :text
         :reader arglist-text)))


(defun make-arglist (text)
  (make-instance 'arglist :text text))


(defmethod maybe-downcase ((arglist arglist))
  (make-arglist (maybe-downcase (arglist-text arglist))))


(common-html.emitter::define-emitter (obj arglist)
  "Emit an arglist."
  (let* ((spinneret:*html* common-html.emitter::*output-stream*))
    (with-html
      (:span :class "locative-args"
             (arglist-text obj)))))


(defmethod common-doc.format:emit-document ((format commondoc-markdown:markdown)
                                            (node arglist)
                                            stream)
  (let ((text (arglist-text node)))
    (typecase text
      (string
       (format stream " ~A"
               text))
      (t
       (write-char #\Space stream)
       (write-char #\( stream)
       (format stream "~{~A~^ ~}"
               (uiop:ensure-list text))
       (write-char #\) stream)))))
