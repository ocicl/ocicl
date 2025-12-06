#.(cl:progn
    (cl:load (cl:merge-pathnames "../tools-for-build/read-changes" cl:*load-pathname*))
    cl:nil)

(cl:defpackage #:eclector.documentation.write-changelog
  (:use
   #:cl)

  (:import-from #:eclector.tools-for-build.read-changes
   #:read-changes
   #:punctuationp)

  (:export
   #:write-changelog))

(cl:in-package #:eclector.documentation.write-changelog)

(defun write-escaped (string stream)
  (loop for character across string
        do (case character
             (#\{ (write-string "@{" stream))
             (#\} (write-string "@}" stream))
             (#\@ (write-string "@@" stream))
             (t   (write-char character stream)))))

(defun write-paragraph (paragraph stream)
  (destructuring-bind (keyword &rest content) paragraph
    (assert (eq keyword :paragraph))
    (pprint-logical-block (stream content)
      (labels ((write-content (content)
                 (loop for (chunk next) on content
                       do (etypecase chunk
                            ((cons (eql :when))
                             (when (equal (second chunk) "manual")
                               (write-content (nthcdr 2 chunk))))
                            ((cons (member :symbol :tt))
                             (format stream "@t{")
                             (write-escaped (second chunk) stream)
                             (format stream "}"))
                            ((cons (eql :ref))
                             (destructuring-bind (keyword namespace target) chunk
                               (declare (ignore keyword))
                               (ecase namespace
                                 (:figure (format stream "@ref{fig:~A}" target))
                                 (:section (format stream "@xref{~A}" target)))))
                            (string
                             (write-escaped chunk stream)))
                       when (and next
                                 (not (eq (punctuationp next) t))
                                 (not (eq (punctuationp chunk) :open)))
                         do (write-string " " stream)
                            (pprint-newline :fill stream))))
        (write-content content))))
  (format stream "~@:_~@:_"))

(defun write-code (code stream)
  (destructuring-bind (keyword language content) code
    (assert (eq keyword :code))
    (let ((command (ecase language
                     ((nil) "example")
                     (:common-lisp "lisp"))))
     (format stream "@~A~@:_" command)
     (write-string content)
     (format stream "~@:_@end ~A~@:_~@:_" command))))

(defun write-item (item stream)
  (destructuring-bind (keyword &rest children) item
    (assert (eq keyword :item))
    (format stream "@item~@:_")
    (dolist (child children)
      (etypecase child
        ((cons (eql :paragraph)) (write-paragraph child stream))
        ((cons (eql :code)) (write-code child stream))))))

(defun write-release (release stream)
  (destructuring-bind (keyword version date &rest items) release
    (assert (eq keyword :release))
    (format stream "@item Release ~A (~:[not yet released~;~:*~A~])~@:_~
                    ~@:_~
                    @itemize~@:_"
            version date)
    (dolist (item items)
      (write-item item stream))
    (format stream "@end itemize~@:_")))

(defun write-changelog (changes stream)
  (pprint-logical-block (stream changes)
    (destructuring-bind (keyword &rest releases) changes
      (assert (eq keyword :changes))
      (format stream "@node Changelog~@:_~
                      @unnumbered Changelog~@:_~@:_")
      (format stream "@table @asis~@:_")
      (dolist (release releases)
        (write-release release stream))
      (format stream "@end table~@:_"))))

;;; Entry point

(let ((changes (read-changes "../data/changes.sexp")))
  (write-changelog changes *standard-output*))
