(cl:in-package #:eclector.concrete-syntax-tree.test)

(in-suite :eclector.concrete-syntax-tree)

(test read-code/smoke
  "Test reading the Eclector source code into CSTs."
  (flet ((test-case (client)
           (finishes
             (eclector.test:map-all-system-expressions
              (lambda (filename form-number parse-result)
                (declare (ignore filename form-number))
                ;; Do not use (is (typep ...)) so the number of checks does
                ;; not vary with source code changes.
                (unless (typep parse-result '#1=concrete-syntax-tree:cst)
                  (fail "~@<~S is not of type ~S.~@:>" parse-result '#1#))
                (unless (valid-cst-parse-result-p client parse-result)
                  (fail "~@<~S is not a valid CST parse result.~@:>"
                        client parse-result)))
              (lambda (&rest args)
                (let ((eclector.base:*client* client))
                  (apply #'eclector.concrete-syntax-tree:read args)))))))
    ;; Read code into CSTs.
    (test-case (make-instance 'eclector.concrete-syntax-tree:cst-client))
    ;; Read code into CSTs and represent labeled object definitions
    ;; and references as distinct CST nodes.
    (test-case (make-instance 'wrapper-cst-client))))
