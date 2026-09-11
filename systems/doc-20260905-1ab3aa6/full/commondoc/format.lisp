(uiop:define-package #:40ants-doc-full/commondoc/format
  (:use #:cl)
  (:import-from #:common-html)
  (:import-from #:commondoc-markdown)
  (:export #:with-format
           #:current-files-extension
           #:files-extension
           #:*keyword-format-to-real-class*))
(in-package #:40ants-doc-full/commondoc/format)


(defvar *current-format*)

(setf (documentation '*current-format* 'variable)
      "Keeps track a commondoc format currently generated")


(defgeneric files-extension (format)
  (:method ((format (eql 'common-html:html)))
    "html")
  (:method ((format (eql 'commondoc-markdown:markdown)))
    "md"))


(defun current-files-extension ()
  (unless (boundp '*current-format*)
    (error "Please, use WITH-FORMAT macro"))
  
  (files-extension *current-format*))


(defun call-with-format (format func)
  (let ((*current-format* format))
    (funcall func)))


(defmacro with-format ((format) &body body)
  `(call-with-format (ensure-format-class-name ,format)
                     (lambda ()
                       ,@body)))


(defvar *keyword-format-to-real-class*
  (list :html 'common-html:html
        :markdown 'commondoc-markdown:markdown)
  "Keeps a mapping from keyword format names to class-names supported by CommonDoc.")


(defun ensure-format-class-name (name)
  (if (keywordp name)
      (or (getf *keyword-format-to-real-class* name)
          (error "Format ~S is not supported. It absent at ~A"
                 name
                 '*keyword-format-to-real-class*))
      name))
