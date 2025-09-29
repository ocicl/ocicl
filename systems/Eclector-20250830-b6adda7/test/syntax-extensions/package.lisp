(cl:defpackage #:eclector.syntax-extensions.test
  (:use
   #:cl
   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:eclector.syntax-extensions.test)

(def-suite :eclector.syntax-extensions)

(defun run-tests ()
  (run! :eclector.syntax-extensions))
