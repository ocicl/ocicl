(uiop:define-package #:40ants-doc-test/locatives
  (:use #:cl)
  (:import-from #:40ants-doc-full/swank)
  (:import-from #:rove
                #:ok
                #:testing
                #:deftest))
(in-package 40ants-doc-test/locatives)


(deftest test-reading-locative
  (testing "Usual symbols should read without problems"
    (ok (eql (40ants-doc-full/swank::read-locative-from-string "FUNCTION")
             'function)))
  
  (testing "Locative with arguments can be parsed as well"
    (ok (equal (40ants-doc-full/swank::read-locative-from-string "(METHOD () (STRING))")
               '(method () (string))))))
