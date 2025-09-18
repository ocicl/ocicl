(uiop:define-package #:40ants-doc-test/utils
  (:use #:cl)
  (:export #:get-diff
           #:get-files-diff))
(in-package 40ants-doc-test/utils)


(defun get-files-diff (path-before path-after)
  (uiop:run-program (format nil "diff -u \"~A\" \"~A\""
                            path-before
                            path-after)
                    :ignore-error-status t
                    :output :string
                    :error-output :output))


(defun get-diff (baseline-path new-content)
  (uiop:with-temporary-file (:stream new :pathname new-path :direction :output)
    (write-string new-content new)
    (finish-output new)
    (get-files-diff baseline-path
                    new-path)))
