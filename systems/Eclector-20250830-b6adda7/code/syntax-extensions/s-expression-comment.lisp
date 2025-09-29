(cl:defpackage #:eclector.syntax-extensions.s-expression-comment
  (:use
   #:cl)

  (:export
   #:s-expression-comment))

(cl:in-package #:eclector.syntax-extensions.s-expression-comment)

(defun s-expression-comment (stream sub-char parameter)
  (declare (ignore sub-char))
  (let* ((client eclector.base:*client*)
         (suppress (eclector.reader:state-value client '*read-suppress*))
         (count (if (null parameter) 1 parameter)))
    (flet ((discard ()
             (loop repeat count do (eclector.reader:read stream t nil t))))
      (setf eclector.reader:*skip-reason* :s-expression-comment)
      (if suppress
          (discard)
          (eclector.reader:call-with-state-value
           client #'discard '*read-suppress* t))))
  (values))
