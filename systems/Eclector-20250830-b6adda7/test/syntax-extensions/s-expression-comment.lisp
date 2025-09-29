(cl:defpackage #:eclector.syntax-extensions.s-expression-comment.test
  (:use
   #:cl
   #:fiveam))

(cl:in-package #:eclector.syntax-extensions.s-expression-comment.test)

(def-suite* :eclector.syntax-extensions.s-expression-comment
  :in :eclector.syntax-extensions)

(defvar *s-expression-comment-readtable*
  (let ((readtable (eclector.readtable:copy-readtable
                    eclector.reader:*readtable*)))
    (eclector.readtable:set-dispatch-macro-character
     readtable #\# #\; #'eclector.syntax-extensions.s-expression-comment:s-expression-comment)
    readtable))

(test smoke
  "Smoke test for the `s-expression-comment' reader macro."
  (let ((eclector.reader:*readtable* *s-expression-comment-readtable*))
    (is (equal 2
               (eclector.reader:read-from-string
                "#; 1 2")))
    (is (equal '()
               (eclector.reader:read-from-string
                "(#;1 #;(2 3 4))")))
    (is (equal '(1 4)
               (eclector.reader:read-from-string
                "(1 #; #; 2 3 4)")))
    (is (equalp '#(1 4 (5 7) 8)
                (eclector.reader:read-from-string
                 "#(1 #2; 2 3 4 (5 #; 6 7) 8)")))
    (is (equal '(1 5)
               (eclector.reader:read-from-string
                "(1 #; (2 #; 3 4) 5)")))
    (is (equal '(5 6 7)
               (eclector.reader:read-from-string
                "(#4; 1 2 3 4 5 6 7 #3; 8 9 10)")))))

(test parse-result
  "Test parse results involving s-expression comments."
  (is (equal '(lambda () (list))
             (cst:raw (let ((eclector.reader:*readtable*
                              *s-expression-comment-readtable*))
                        (eclector.concrete-syntax-tree:read-from-string
                         "(lambda () (list #;1 #;(2 3 4)))")))))

  (is (equal '2
             (cst:raw (let ((eclector.reader:*readtable*
                              *s-expression-comment-readtable*))
                        (eclector.concrete-syntax-tree:read-from-string "#; 1 2"))))))
