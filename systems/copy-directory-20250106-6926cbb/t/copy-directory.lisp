(in-package :cl-user)
(defpackage copy-directory-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :copy-directory-test)

(def-suite tests
  :description "copy-directory tests.")
(in-suite tests)

(test copies
  (let ((source (asdf:system-relative-pathname :copy-directory #p"t/dir/"))
        (dest (asdf:system-relative-pathname :copy-directory #p"t/copy/")))
    (is-false (probe-file dest))
    (is
     (pathnamep (copy-directory:copy source dest)))
    (is-true (probe-file dest))
    (is-true
     (probe-file (merge-pathnames #p"a.txt" dest)))
    (uiop:delete-directory-tree dest :validate t))
  (let ((source (asdf:system-relative-pathname :copy-directory #p"t/dir/"))
        (dest (asdf:system-relative-pathname :copy-directory #p"t/deep/copy/")))
    (is-false (probe-file dest))
    (is
     (pathnamep (copy-directory:copy source dest)))
    (is-true (probe-file dest))
    (is-true
     (probe-file (merge-pathnames #p"a.txt" dest)))
    (uiop:delete-directory-tree dest :validate t)))

(defun run-tests ()
  (run! 'tests))
