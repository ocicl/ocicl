(uiop:define-package #:40ants-doc-test/markdown
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:import-from #:rove
                #:ok
                #:deftest))
(in-package 40ants-doc-test/markdown)


(deftest test-fully-qualified-symbols-shouldnt-be-splitted-to-italic
  (rove:testing "The SOME-VAR:*BLAH* should become a single common-doc:text-node"
    (let* ((paragraph (40ants-doc-full/commondoc/markdown:parse-markdown "SOME-VAR:*BLAH*"))
           (children (common-doc:children paragraph)))
      (ok (= (length children)
             1))
      (ok (typep (first children)
                 'common-doc:text-node)))))


(deftest test-simple-markdown-text
  (rove:testing "Simple text should be parsed like a paragraph of a text"
    (let* ((paragraph (40ants-doc-full/commondoc/markdown:parse-markdown "Just a text"))
           (children (common-doc:children paragraph)))
      (ok (= (length children)
             1))
      (ok (typep (first children)
                 'common-doc:text-node)))))


(deftest test-variable-name-should-remain-text-instead-of-italic
  (rove:testing "Star symbols denote italic in markdown, but they are often used to surround global var names in CL to let them be tranformed into a XREFs we need to make them back TEXT-NODEs"
    (let* ((paragraph (40ants-doc-full/commondoc/markdown:parse-markdown "*SOME-VAR*"))
           (children (common-doc:children paragraph)))
      (ok (= (length children)
             1))
      (ok (typep (first children)
                 'common-doc:text-node))
      (ok (string= (common-doc:text (first children))
                   "*SOME-VAR*")))))
