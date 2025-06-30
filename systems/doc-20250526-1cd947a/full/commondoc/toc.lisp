(uiop:define-package #:40ants-doc-full/commondoc/toc
  (:use #:cl)
  (:import-from #:40ants-doc-full/utils
                #:make-relative-path)
  (:import-from #:40ants-doc-full/commondoc/page
                #:full-filename
                #:base-filename)
  (:import-from #:40ants-doc-full/page)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc-full/commondoc/format)
  (:import-from #:40ants-doc-full/rewrite)
  (:import-from #:40ants-doc-full/commondoc/changelog)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes)
  (:export
   #:make-toc))
(in-package #:40ants-doc-full/commondoc/toc)


(defvar *full-document*)


(defun make-toc (document page)
  "Collects toplevel objects of 40ANTS-DOC-FULL/COMMONDOC/SECTION:DOCUMENTATION-SECTION class
   and returns a table of content CommonDoc node."
  (let* ((current-sublist (list (common-doc:make-unordered-list (list))))
         (last-list-item nil)
         (current-page nil)
         (inside-changelog nil))
    (labels ((page-format (page)
               (or (40ants-doc-full/page:page-format page)
                   40ants-doc-full/commondoc/format::*current-format*))
             (collector (node)
               (when (and (typep node 'common-doc:section)
                          (not inside-changelog)
                          ;; We only want to include HTML documents into the TOC for HTML
                          ;; and Markdown documents to the TOC for Markdown.
                          (or (null current-page)
                              (eql (page-format page)
                                   (page-format current-page))))
                 (let* ((html-fragment (common-doc:reference node))
                        (page-uri
                          (when current-page
                            (full-filename current-page)))
                        (text (if page-uri
                                  (let* ((from-uri (full-filename page))
                                         (link-uri (make-relative-path from-uri
                                                                       page-uri))
                                         (rewritten-link-uri (40ants-doc-full/rewrite::rewrite-url link-uri))
                                         (result (common-doc:make-document-link rewritten-link-uri
                                                                                html-fragment
                                                                                (common-doc:title node))))
                                    result)
                                  (common-doc:title node)))
                        (p (common-doc:make-paragraph text))
                        (li (common-doc:make-list-item p)))
                   (setf (common-doc:children (car current-sublist))
                         (append (common-doc:children (car current-sublist))
                                 (list li)))
                   (setf last-list-item
                         li)))
               node)
             (on-down (node)
             
               (typecase node
                 (40ants-doc-full/commondoc/page:page
                  (setf current-page node))
                 
                 (40ants-doc-full/commondoc/changelog::changelog
                  (setf inside-changelog t))

                 (common-doc:section
                  (push (common-doc:make-unordered-list (list))
                        current-sublist)
                  (setf (common-doc:children last-list-item)
                        (append (common-doc:children last-list-item)
                                (list (car current-sublist)))))))
             (on-up (node)
               (typecase node
                 (40ants-doc-full/commondoc/changelog::changelog
                  (setf inside-changelog nil))
                 (common-doc:section
                  (pop current-sublist)))))
      (map-nodes document #'collector
                 :on-going-down #'on-down
                 :on-going-up #'on-up))
    
    (assert (= (length current-sublist) 1))
    (flet ((remove-empty-sublists (node)
             (when (typep node 'common-doc:list-item)
               (setf (common-doc:children node)
                     (loop for child in (common-doc:children node)
                           unless (null (common-doc:children child))
                             collect child)))
             node))
      (map-nodes (car current-sublist)
                 #'remove-empty-sublists))))


(defmethod 40ants-doc-full/commondoc/page:make-page-toc ((obj 40ants-doc-full/commondoc/page:page))
  (unless (boundp '*full-document*)
    (error "Please, set 40ANTS-DOC/COMMONDOC/TOC:*FULL-DOCUMENT* variable to generate TOC."))
  (make-toc *full-document* obj))
