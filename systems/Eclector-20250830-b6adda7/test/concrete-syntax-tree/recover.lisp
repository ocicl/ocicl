(cl:in-package #:eclector.concrete-syntax-tree.test)

(def-suite* :eclector.concrete-syntax-tree.recover
  :in :eclector.concrete-syntax-tree)

(defun read-with-cst-client (stream)
  (let ((eclector.base:*client*
          (make-instance 'eclector.concrete-syntax-tree:cst-client)))
    (eclector.concrete-syntax-tree:read stream)))

(test recover/labeled-objects
  "Test recovering from syntax errors related to labeled objects."
  (mapc (lambda (input-and-expected-conditions)
          (destructuring-bind (input expected-conditions
                               &optional (expected-position (length input)))
              input-and-expected-conditions
            (multiple-value-bind (position cst orphans)
                (read-and-check-recover
                 input expected-conditions #'read-with-cst-client)
              (is (equalp expected-position position)
                  "~@<For input ~S, expected position ~S but got ~S~@:>"
                  input expected-position position)
              (is-consistent-with-raw cst)
              (is (equal '() orphans)))))
        '(("#1=#1#"
           (eclector.reader:sharpsign-equals-only-refers-to-self))
          ("(#1="
           (eclector.reader:end-of-input-after-sharpsign-equals
            eclector.reader:unterminated-list))
          ("#1=
;;2
;;3"
           (eclector.reader:end-of-input-after-sharpsign-equals)))))
