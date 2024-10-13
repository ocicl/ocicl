(uiop:define-package #:40ants-doc-full/search
  (:use #:cl)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes)
  (:import-from #:40ants-doc-full/commondoc/section)
  (:import-from #:40ants-doc-full/commondoc/page
                #:full-filename)
  (:import-from #:40ants-doc-full/page)
  (:import-from #:40ants-doc-full/utils
                #:make-clean-uri)
  (:import-from #:40ants-doc/locatives)
  (:import-from #:common-html)
  (:import-from #:stem)
  (:import-from #:str)
  (:import-from #:jonathan)
  (:import-from #:40ants-doc-full/commondoc/bullet
                #:bullet)
  (:import-from #:40ants-doc-full/commondoc/format
                #:with-format)
  (:import-from #:40ants-doc-full/commondoc/piece
                #:doc-reference)
  (:import-from #:40ants-doc/reference
                #:reference-locative
                #:reference-object)
  (:import-from #:40ants-doc/locatives/base
                #:locative-type)
  (:import-from #:common-doc)
  (:import-from #:common-doc.ops
                #:collect-all-text)
  (:import-from #:40ants-doc-full/rewrite
                #:*clean-urls*))
(in-package #:40ants-doc-full/search)


(defun objects-to-alist (hash)
  (list* :obj
         (loop for prefix being the hash-key of hash
               using (hash-value subhash)
               collect (cons prefix
                             (list* :obj
                                    (loop for name being the hash-key of subhash
                                          using (hash-value values)
                                          collect (cons name values)))))))

(defparameter *locative-to-name*
  '(symbol "Symbol"
    40ants-doc/locatives:argument "Argument"
    40ants-doc/locatives:system "ASDF System"
    40ants-doc/locatives:class "Class"
    40ants-doc/locatives:compiler-macro "Compiler Macro"
    40ants-doc/locatives:constant "Constant"
    40ants-doc/locatives:function "Function"
    40ants-doc/locatives:generic-function "Generic Function"
    40ants-doc/locatives:glossary-term "Glossary Term"
    40ants-doc/locatives:include "Included Block"
    40ants-doc/locatives:stdout-of "Stdout of Code"
    40ants-doc/locatives:locative "Locative"
    40ants-doc/locatives:macro "Macro"
    40ants-doc/locatives:method "Method"
    40ants-doc/locatives:package "Package"
    40ants-doc/locatives:restart "Restart"
    40ants-doc/locatives:section "Section"
    40ants-doc/locatives:accessor "Accessor"
    40ants-doc/locatives:reader "Slot Reader"
    40ants-doc/locatives:writer "Slot Write"
    40ants-doc/locatives:structure-accessor "Structure Accessor"
    40ants-doc/locatives:type "Type"
    40ants-doc/locatives:variable "Variable"))


(defun generate-search-index (document page)
  (with-format (:html)
    (let ((docnames nil)
          (filenames nil)
          (objects (make-hash-table :test 'equal))
          (objnames (loop for (symbol name) on *locative-to-name* by #'cddr
                          collect (list "lisp"
                                        (string-downcase
                                         (symbol-name symbol))
                                        name))) ;; names of types from objtypes
          (objtypes (loop for (symbol name) on *locative-to-name* by #'cddr
                          collect (format nil "lisp:~A"
                                          (string-downcase
                                           (symbol-name symbol)))))
          (symbol-to-idx (loop with result = (make-hash-table)
                               for (symbol name) on *locative-to-name* by #'cddr
                               for idx upfrom 0
                               do (setf (gethash symbol result)
                                        idx)
                               finally (return result)))
          (terms (make-hash-table :test 'equal)) ;; map of terms to indices in filenames
          (titles nil)
          (titleterms nil)
          (current-page nil)
          (document-idx -1))
      (labels ((first-section (page)
                 (loop for child in (common-doc:children page)
                       when (typep child '40ants-doc-full/commondoc/section:documentation-section)
                       do (return child)))
               (process (node)
                 (typecase node
                   (common-doc:text-node
                    (when current-page
                      (loop for word in (str:split #\Space
                                                   (common-doc:text node)
                                                   :omit-nulls t)
                            do (pushnew document-idx
                                        (gethash (stem:stem word) terms)))))
                   (bullet
                    (when current-page
                      (let* ((reference (doc-reference node))
                             (object (reference-object reference))
                             (locative (reference-locative reference))
                             (locative-type (locative-type locative))
                             (package (typecase object
                                        (symbol (symbol-package object))
                                        (t nil)))
                             (prefix (if package
                                         (package-name
                                          package)
                                         ""))
                             
                             (name (typecase object
                                     (symbol (symbol-name object))
                                     (t object)))
                             (html-fragment (common-doc:reference node))
                             ;; This index will be used by sphinx code
                             ;; to lookup an object's type name:
                             (obj-index (gethash locative-type symbol-to-idx 0))
                             (object (list document-idx obj-index 2 html-fragment))
                             (prefixed (or (gethash prefix objects)
                                           (setf (gethash prefix objects)
                                                 (make-hash-table :test 'equal)))))
                        (setf (gethash name prefixed)
                              object)))))
                 node)
               (go-down (node)
                 (typecase node
                   (40ants-doc-full/commondoc/page:page
                    (when (member (40ants-doc-full/page:page-format node)
                                  (list 'common-html:html nil))
                      (setf current-page node)
                      (incf document-idx)
                      (push (collect-all-text
                             (common-doc:title
                              (first-section node)))
                            titles)
                      ;; Here we need to make a path relative to the search page
                      ;; because search page might be in /search/index.html and it might
                      ;; refer to documents like /index.html. We can't use paths
                      ;; starting from / because this will not work for static
                      ;; documentation on local computer.
                      (push (40ants-doc-full/utils:make-relative-path (40ants-doc-full/page:base-filename page)
                                                                      (40ants-doc-full/page:base-filename node))
                            docnames)
                      (let ((path (full-filename node
                                                 :from page)))
                        (push (if *clean-urls*
                                  (make-clean-uri path)
                                  path)
                              filenames)))))
                 node)
               (go-up (node)
                 (typecase node
                   (40ants-doc-full/commondoc/page:page
                    (when (member (40ants-doc-full/page:page-format node)
                                  (list 'common-html:html nil))
                      (setf current-page nil))))
                 node))
        (map-nodes document #'process
                   :on-going-down #'go-down
                   :on-going-up #'go-up))
      (let ((index (list :obj
                         (cons "docnames" (nreverse docnames))
                         (cons "filenames" (nreverse filenames))
                         (cons "objects" (objects-to-alist objects))
                         (cons "objnames" objnames)
                         (cons "objtypes" objtypes)
                         (cons "terms" terms)
                         (cons "titles" (nreverse titles))
                         (cons "titleterms" titleterms))))
        (with-output-to-string (s)
          (write-string "Search.setIndex(" s)
          (write-string (jonathan:to-json index :from :jsown) s)
          (write-string ")" s))))))
