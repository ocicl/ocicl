(uiop:define-package #:40ants-doc-full/external-index
  (:use #:cl)
  (:import-from #:40ants-doc-full/utils
                #:with-temp-package
                #:url-join)
  (:import-from #:40ants-doc/reference
                #:make-external-reference
                #:external-reference-p)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:40ants-doc-full/commondoc/page)
  (:import-from #:40ants-doc-full/rewrite)
  (:import-from #:str)
  (:import-from #:jonathan)
  (:import-from #:dexador
                #:http-request-not-found))
(in-package #:40ants-doc-full/external-index)


(defun load-data-from-url (url)
  (let ((json-url (url-join url "references.json"))
        (original-url url))
    (handler-case (dex:get json-url)
      (http-request-not-found ()
        (dex:get original-url)))))


(defun load-data-from-file (path)
  "PATH can be either a directory or a direct path to references.json file."
  (let ((dir (uiop:directory-exists-p path)))
    (alexandria:read-file-into-string
     (if dir
         (uiop:merge-pathnames* "references.json" dir)
         path))))


(defun load-data-from (filename-or-url)
  (cond
    ((and (typep filename-or-url 'string)
          (or (str:starts-with-p "https://" filename-or-url)
              (str:starts-with-p "http://" filename-or-url)))
     (load-data-from-url filename-or-url))
    (t
     (load-data-from-file filename-or-url))))


(defun read-references-index (filename-or-url)
  (check-type filename-or-url (or pathname string))
  (loop with data = (load-data-from filename-or-url)
        with items = (jonathan:parse data :as :alist)
        for item in items
        collect (make-external-reference (assoc-value item "object" :test #'string-equal)
                                         (assoc-value item "locative" :test #'string-equal)
                                         (assoc-value item "url" :test #'string-equal))))


(defun make-full-reference-url (reference page base-url-or-dir)
  (let* ((page-uri (40ants-doc-full/commondoc/page:full-filename page))
         (rewritten-link-uri (40ants-doc-full/rewrite::rewrite-url page-uri))
         (fragment (40ants-doc-full/utils::html-safe-name
                    (40ants-doc/reference::reference-to-anchor reference))))
    (concatenate 'string
                 (url-join 
                  (etypecase base-url-or-dir
                    (pathname (namestring base-url-or-dir))
                    (string base-url-or-dir))
                  rewritten-link-uri)
                 "#"
                 fragment)))


(defun write-references-index (filename references base-url-or-dir)
  (with-temp-package ()
    (loop 
      for (reference . page) in (remove-if #'external-reference-p references
                                           :key #'car)
      for url = (make-full-reference-url reference page base-url-or-dir)
      for object = (40ants-doc/reference:reference-object reference)
      for locative = (40ants-doc/reference:reference-locative reference)
      collect (list (cons :url url)
                    (cons :object
                          (etypecase object
                            (string object)
                            (symbol (prin1-to-string object))))
                    (cons :locative locative)) into items
      finally 
         (alexandria:write-string-into-file
          (jonathan:to-json items :from :alist)
          filename
          :if-exists :supersede))))
