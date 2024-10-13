(in-package :cl-user)
(defpackage path-parse-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :path-parse-test)

(def-suite tests
  :description "path-parse tests.")
(in-suite tests)

(test path
  (finishes
   (path-parse:path))
  (is-true
   (listp (path-parse:path)))
  (is-true
   (every #'uiop:absolute-pathname-p (path-parse:path)))
  (is-true
   (every #'uiop:directory-pathname-p (path-parse:path))))

(defun run-tests ()
  (run! 'tests))
