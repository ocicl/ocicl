(uiop:define-package #:40ants-doc-full/commondoc/markdown
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:commondoc-markdown
                #:markdown-link)
  (:import-from #:common-doc)
  (:import-from #:common-doc.ops)
  (:import-from #:common-doc.format)
  (:import-from #:40ants-doc-full/commondoc/xref)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:node-supports-children)
  (:import-from #:40ants-doc-full/swank
                #:read-locative-from-string)
  (:export
   #:parse-markdown))
(in-package #:40ants-doc-full/commondoc/markdown)


;; TODO: Remove if common-doc.ops:collect-all-text will be OK:
;; 
;; (defgeneric node-to-text (node)
;;   (:method ((node common-doc:bold))
;;     (node-to-text
;;      (common-doc:children node)))
;;   (:method ((node common-doc:italic))
;;     (node-to-text
;;      (common-doc:children node)))
;;   (:method ((node common-doc:text-node))
;;     (common-doc:text node))
;;   (:method ((node list))
;;     (apply #'concatenate 'string
;;            (mapcar #'node-to-text
;;                    node))))


(defun replace-markdown-links (node)
  "Replaces unresolved markdown nodes with XREF objects."
  (flet ((replacer (node)
           (cond
             ((typep node 'markdown-link)
              (let* ((children (common-doc:children node)))
                
                (let* ((text (common-doc.ops:collect-all-text children))
                       (symbol (read-locative-from-string text))
                       (locative-name (commondoc-markdown:markdown-link-definition node))
                       (locative (when locative-name
                                   (read-locative-from-string locative-name
                                                              :package (find-package "40ANTS-DOC/LOCATIVES")))))
                  (40ants-doc-full/commondoc/xref:make-xref text
                                                            :symbol symbol
                                                            :locative locative))))
             (t
              node))))
    (40ants-doc-full/commondoc/mapper:map-nodes node #'replacer)))


(defun join-text-nodes (&rest nodes)
  "Concatenates all consequent text nodes."
  (loop with current-node = (car nodes)
        with results = nil
        for node in (cdr nodes)
        do
           (cond
             ((and (typep current-node
                          'common-doc:text-node)
                   (or (typep node
                              'common-doc:text-node)
                       (typep node
                              'string)))
              (setf (common-doc:text current-node)
                    (concatenate 'string
                                 (common-doc:text current-node)
                                 (etypecase node
                                   (common-doc:text-node
                                    (common-doc:text node))
                                   (string node)))))
             (t
              (push current-node
                    results)
              (setf current-node
                    node)))
        finally (push current-node
                      results)
                (return (nreverse results))))


(defun join-italic-var-names (node)
  (labels ((text-node-ends-with (node char)
             (str:ends-with-p char
                              (common-doc:text node)))
           (replacer (node)
             (when (node-supports-children node)
               (let* ((children (common-doc:children node)))
                 (setf (common-doc:children node)
                       (loop with skip-next = nil
                             with results = nil
                             for child in children
                             for idx upfrom 0
                             for next-node = (nth (1+ idx) children)
                             do
                                (if skip-next
                                    (setf skip-next nil)
                                    (cond
                                      ((and (typep child
                                                   'common-doc:text-node)
                                            (text-node-ends-with child ":")
                                            (typep next-node
                                                   'common-doc:italic))
                                       (setf results
                                             (append
                                              (nreverse
                                               (apply #'join-text-nodes
                                                      child
                                                      (append (list "*")
                                                              (common-doc:children
                                                               next-node)
                                                              (list "*"))))
                                              results))
                                       (setf skip-next t))
                                      (t
                                       (push child results))))
                             finally
                                (return (nreverse results))))))
             (values node)))
    (40ants-doc-full/commondoc/mapper:map-nodes node #'replacer)))


(defun transform-uppercase-italic-nodes-back-to-text (node)
  (labels ((replacer (node)
             (cond
               ((and (typep node
                            'common-doc:italic)
                     (loop with text = (common-doc.ops:collect-all-text node)
                           for char across text
                           always (or (char= char
                                             #\-)
                                      (upper-case-p char))))
                (common-doc:make-text
                 (concatenate 'string
                              "*"
                              (common-doc.ops:collect-all-text node)
                              "*")))
               (t
                node))))
    (40ants-doc-full/commondoc/mapper:map-nodes node #'replacer)))


(defun parse-markdown (text)
  "Parses markdown text and returns CommonDoc node.

   The diffence from usual common doc markdown parsing is
   that this function keeps uppercased variable names as text.
   Usual markdown parser parses *SOME-TEXT* \"italic\".

   Also, this function replaces markdown references with
   40ANTS-DOC/COMMONDOC:XREF nodes."
  (when text
    (replace-markdown-links
     (transform-uppercase-italic-nodes-back-to-text
      (join-italic-var-names
       (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                         text))))))
