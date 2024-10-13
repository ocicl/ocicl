(uiop:define-package #:40ants-doc-full/commondoc/utils
  (:use #:cl)
  (:import-from #:40ants-doc-full/swank)
  (:import-from #:40ants-doc-full/utils
                #:maybe-downcase)
  (:import-from #:40ants-doc/docstring
                #:*whitespace-chars*)
  (:import-from #:str)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes)
  (:export))
(in-package #:40ants-doc-full/commondoc/utils)


(defun left-word (node)
  (check-type node common-doc:text-node)
  (let* ((text (str:replace-all '(#\Newline) " "
                                (common-doc:text node)))
         (stripped (string-left-trim *whitespace-chars*
                                     text))
         (parts (str:split #\Space stripped :limit 2)))
    (first parts)))


(defun right-word (node)
  (check-type node common-doc:text-node)
  (let* ((text (str:replace-all '(#\Newline) " "
                                (common-doc:text node)))
         (stripped (string-right-trim *whitespace-chars*
                                      text))
         (parts (str:rsplit #\Space stripped :limit 2)))
    (car (last parts))))


(defun read-locative (text)
  (let* ((package (find-package "40ANTS-DOC/LOCATIVES"))
         (text (string-right-trim ".,!?" text ))
         (found (40ants-doc-full/swank::read-locative-from-string
                 text
                 :package package))
         (symbol
           ;; If FOUND is a locative name like a FUNCTION
           ;; or a list like (METHOD () (T T T))
           ;; then symbol will be bound to FUNCTION or METHOD.
           (typecase found
             (symbol found)
             (cons (typecase (car found)
                     (symbol (car found))
                     (t nil))))))
    (when symbol
      (multiple-value-bind (present-symbol status)
          (find-symbol (symbol-name symbol) package)
        (when (and (eq symbol present-symbol)
                   (eql status :external))
          found)))))


(defmethod maybe-downcase ((node common-doc:document-node))
  (flet ((replacer (node)
           (typecase node
             (common-doc:text-node
              (common-doc:make-text (maybe-downcase
                                     (common-doc:text node))))
             (t node))))
    (map-nodes node #'replacer)))
