(cl:in-package #:eclector.concrete-syntax-tree)

(defun read (&optional (input-stream *standard-input*)
                       (eof-error-p t)
                       (eof-value nil))
  (eclector.parse-result:read
   (or eclector.reader:*client* *cst-client*)
   input-stream eof-error-p eof-value))

(defun read-preserving-whitespace (&optional (input-stream *standard-input*)
                                             (eof-error-p t)
                                             (eof-value nil))
  (eclector.parse-result:read-preserving-whitespace
   (or eclector.reader:*client* *cst-client*)
   input-stream eof-error-p eof-value))

(locally (declare #+sbcl (sb-ext:muffle-conditions eclector.base:&optional-and-&key-style-warning))
  (defun read-from-string (string &optional
                                  (eof-error-p t)
                                  (eof-value nil)
                                  &key
                                  (start 0)
                                  (end nil)
                                  (preserve-whitespace nil))
    (eclector.parse-result:read-from-string
     (or eclector.reader:*client* *cst-client*)
     string eof-error-p eof-value :start start :end end
     :preserve-whitespace preserve-whitespace)))
