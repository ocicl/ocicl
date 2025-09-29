(cl:in-package #:eclector.test)

(in-suite :eclector.reader)

(test read-code/host-equivalence
  "Compare Eclector code read with Eclector and read with the host reader."
  (flet ((collect-expressions (reader)
           (let ((result '()))
             (map-all-system-expressions
              (lambda (filename i expression)
                (push (list filename i expression) result))
              reader
              ;; The concrete-syntax-tree system may not be loaded
              ;; when this test runs.
              :filter (lambda (system-name)
                        (not (search "concrete-syntax-tree" system-name))))
             (nreverse result))))
    (loop for (filename1 form-number1 expression1) in (collect-expressions
                                                       'eclector.reader:read)
          for (nil       nil          expression2) in (collect-expressions
                                                       'read)
          ;; Do not use (is (code-equal ...)) so the number of checks
          ;; does not vary with source code changes.
          do (unless (code-equal expression2 expression1)
               (fail "~@<Mismatch in file ~S, expression ~:D:~@:_~
                      ~2@T~S~@:_is not equal to~@:_~
                      ~2@T~S~@:>"
                     filename1 form-number1 expression1 expression2)))))
