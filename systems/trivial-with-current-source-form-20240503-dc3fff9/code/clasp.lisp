;;;; clasp.lisp --- Clasp implementation.
;;;;
;;;; Author: Bike <aeshtaer@gmail.com>

(cl:in-package #:trivial-with-current-source-form)

(defun expand (forms body)
  `(ext:with-current-source-form (,@forms) ,@body))
