(cl:defpackage #:eclector.tools-for-build.read-changes
  (:use
   #:cl)

  (:export
   #:read-changes
   #:punctuationp
   #:split-into-lines))

(cl:in-package #:eclector.tools-for-build.read-changes)

;;; Read changes

(defun read-changes (filename)
  (with-open-file (stream filename :direction :input)
    (with-standard-io-syntax
      (read stream))))

;;; Utilities

(defun punctuationp (string)
  (cond ((not (= (length string) 1))
         nil)
        ((member (aref string 0) '(#\. #\? #\! #\, #\; #\: #\)))
         t)
        ((member (aref string 0) '(#\())
         :open)))

(defun split-into-lines (string)
  (loop with length = (length string)
        for start = 0 then (1+ index)
        for index = (position #\Newline string :start start)
        while (and index (< index length))
        collect (subseq string start index) into lines
        finally (return (nconc lines (list (subseq string start))))))
