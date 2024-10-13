(uiop:define-package #:40ants-doc-test/utils-test
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest)
  (:import-from #:40ants-doc-full/utils
                #:make-relative-path))
(in-package 40ants-doc-test/utils-test)


(deftest test-relative-paths
  (ok (string= (make-relative-path "foo"
                                   "foo")
               ""))
  (ok (string= (make-relative-path "foo"
                                   "bar")
               "bar"))
  (ok (string= (make-relative-path "blah/minor"
                                   "bar")
               "../bar"))
  (ok (string= (make-relative-path "foo/blah/minor"
                                   "bar")
               "../../bar"))
  (ok (string= (make-relative-path "bar"
                                   "foo/blah/minor")
               "foo/blah/minor"))

  (ok (string= (make-relative-path "foo/blah/minor"
                                   "foo/blah/bar")
               "bar"))
  (ok (string= (make-relative-path "foo/blah/minor"
                                   "foo/bar")
               "../bar"))
  (ok (string= (make-relative-path "foo/blah/minor"
                                   "some/bar")
               "../../some/bar")))
