(defpackage :trivial-file-size/tests
  (:use :cl :fiveam :trivial-file-size)
  (:export :run-file-size-tests))
(in-package :trivial-file-size/tests)

(def-suite trivial-file-size)
(in-suite trivial-file-size)

(defun run-file-size-tests ()
  (fiveam:run 'trivial-file-size))

(defun resolve-test-file (file)
  (asdf:system-relative-pathname
   "trivial-file-size/tests"
   (concatenate 'string "tests/"
                file)))

(test empty-file
  (let ((file (resolve-test-file "empty-file")))
    (is (zerop (file-size-in-octets file)))))

(test one-byte-file
  (let ((file (resolve-test-file "one-byte-file")))
    (is (= 1 (file-size-in-octets file)))))

(test two-byte-file
  (let ((file (resolve-test-file "two-byte-file")))
    (is (= 2 (file-size-in-octets file)))))

(test no-such-file
      (let ((file (resolve-test-file "no-such-file")))
        (let ((size (file-size-in-octets file)))
          (is #+abcl (= size 0)
              #-abcl (null size)))))
