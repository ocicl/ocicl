(uiop:define-package #:40ants-doc-full/commondoc/changelog
  (:use #:cl)
  (:import-from #:40ants-doc-full/builder/vars
                #:*base-url*)
  (:import-from #:40ants-doc-full/commondoc/section
                #:documentation-section
                #:make-documentation-section)
  (:import-from #:40ants-doc/changelog)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:common-html)
  (:import-from #:common-doc)
  (:import-from #:common-doc.ops)
  (:import-from #:common-doc.format)
  (:import-from #:local-time
                #:parse-timestring
                #:format-rfc1123-timestring)
  (:import-from #:xml-emitter
                #:rss-item
                #:with-rss2
                #:rss-channel-header)
  (:import-from #:40ants-doc-full/builder/vars
                #:*base-url*)
  (:import-from #:40ants-doc-full/commondoc/format)
  (:import-from #:40ants-doc-full/commondoc/html
                #:with-html)
  (:import-from #:str)
  (:import-from #:40ants-doc-full/commondoc/page))
(in-package #:40ants-doc-full/commondoc/changelog)


(defclass rss-feed (common-doc.format:document-format)
  ()
  (:documentation "RSS feed."))

(defmethod 40ants-doc-full/commondoc/format:files-extension ((format (eql 'rss-feed)))
  "xml")

(defclass changelog (documentation-section)
  ())

(defclass version (documentation-section)
  ((date :initform nil
         :initarg :date
         :type (or null local-time:date)
         :accessor version-date)))


(defmethod 40ants-doc-full/commondoc/builder:to-commondoc ((obj 40ants-doc/changelog::changelog))
  (make-documentation-section obj :class-name 'changelog))


(defmethod 40ants-doc-full/commondoc/builder:to-commondoc ((obj 40ants-doc/changelog::version))
  (let ((version (make-documentation-section obj :class-name 'version)))
    (when-let ((date (40ants-doc/changelog::version-date obj)))
      (setf (version-date version)
            (parse-timestring date))
      (setf (common-doc:title version)
            (append (common-doc:title version)
                    (list (common-doc:make-text (format nil " (~A)"
                                                        date))))))
    version))


(defun get-changelog-rss-url ()
  (when (and (boundp '*base-url*)
             *base-url*)
    (concatenate 'string
                 *base-url*
                 (unless (str:ends-with-p "/" *base-url*)
                   "/")
                 "changelog.xml")))


(defmethod 40ants-doc-full/commondoc/section::emit-html-after-title ((node changelog))
  (with-html
    (when-let ((url (get-changelog-rss-url)))
      (:a :class "rss-icon"
          :href url))))


(defmethod common-doc.format:emit-document ((format rss-feed)
                                            (node 40ants-doc-full/commondoc/page:page)
                                            stream)
  (loop for child in (common-doc:children node)
        do (common-doc.format:emit-document format child stream)))


(defmethod common-doc.format:emit-document ((format rss-feed)
                                            (node changelog)
                                            stream)
  (with-rss2 (stream)
    (rss-channel-header (if 40ants-doc-full/builder/vars::*current-asdf-system*
                            (format nil "~A ChangeLog"
                                    (asdf:primary-system-name
                                     40ants-doc-full/builder/vars::*current-asdf-system*))
                            "ChangeLog")
                        (if (boundp '*base-url*)
                            *base-url*
                            ""))
    (loop for child in (common-doc:children node)
          do (common-doc.format:emit-document format child stream))))


(defmethod common-doc.format:emit-document ((format rss-feed)
                                            (node version)
                                            stream)

  (let ((title (common-doc.ops:collect-all-text (common-doc:title node)))
        (description
          (with-output-to-string (s)
            (loop with format = (make-instance 'common-html:html)
                  for item in (common-doc:children node)
                  do (common-doc.format:emit-document format item s)))))

    (rss-item title
              :description description
              :pubdate (when-let ((date (version-date node)))
                         (format-rfc1123-timestring
                          nil date)))))
