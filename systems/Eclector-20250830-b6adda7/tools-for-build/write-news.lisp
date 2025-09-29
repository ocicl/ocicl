(cl:defpackage #:eclector.tools-for-build.write-news
  (:use
   #:cl)

  (:import-from #:eclector.tools-for-build.read-changes
   #:punctuationp
   #:split-into-lines)

  (:export
   #:write-news))

(cl:in-package #:eclector.tools-for-build.write-news)

;;; Write plaintext NEWS

(defun write-paragraph (paragraph stream format)
  (destructuring-bind (keyword &rest content) paragraph
    (assert (eq keyword :paragraph))
    (pprint-logical-block (stream content)
      (loop for (chunk next) on content
            do (etypecase chunk
                 ((cons (eql :when)))
                 ((cons (eql :symbol))
                  (if (eq format :markdown)
                      (format stream "`~A`" (second chunk))
                      (format stream "~:@(~A~)" (second chunk))))
                 ((cons (eql :tt))
                  (if (eq format :markdown)
                      (format stream "`~A`" (second chunk))
                      (write-string (second chunk) stream)))
                 (string
                  (write-string chunk stream)))
            when (and next
                      (not (or (eq (punctuationp next) t)
                               (typep next '(cons (eql :when)))))
                      (not (eq (punctuationp chunk) :open)))
            do (write-string " " stream)
               (pprint-newline :fill stream)))))

(defun write-code (code stream format)
  (destructuring-bind (keyword language content) code
    (assert (eq keyword :code))
    (when (eq format :markdown)
      (format stream "```~@[~A~]~@:_"
              (ecase language
                ((nil) nil)
                (:common-lisp "cl"))))
    (pprint-logical-block
        (stream code :per-line-prefix (if (eq format :plaintext)
                                          "  "
                                          ""))
      (let ((lines (split-into-lines content)))
        (loop for (line next) on lines
              do (write-string line stream)
              when next
                do (pprint-newline :mandatory stream))))
    (when (eq format :markdown)
      (format stream "~@:_```"))))

(defun write-item (item stream format)
  (destructuring-bind (keyword &rest children) item
    (assert (eq keyword :item))
    (format stream "* ")
    (pprint-logical-block (stream children)
      (loop for (child next) on children
            do (etypecase child
                 ((cons (eql :paragraph))
                  (write-paragraph child stream format))
                 ((cons (eql :code))
                  (write-code child stream format)))
               (when next
                 (format stream "~@:_~@:_"))))
    (format stream "~@:_~@:_")))

(defun write-release (release stream format)
  (destructuring-bind (keyword version date &rest items) release
    (assert (eq keyword :release))
    (when (eq format :markdown)
      (format stream "# "))
    (format stream "Release ~A (~:[not yet released~;~:*~A~])~@:_~
                    ~@:_"
            version date)
    (dolist (item items)
      (write-item item stream format))))

(defun write-news (changes filename format &key count)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede)
    (let ((*print-right-margin* (if (eq format :markdown)
                                    most-positive-fixnum
                                    nil)))
      (pprint-logical-block (stream changes)
        (destructuring-bind (keyword &rest releases) changes
          (assert (eq keyword :changes))
          (loop repeat (or count (length releases))
                for release in releases
                do (write-release release stream format)))))))
