(cl:in-package #:eclector.parse-result.test)

(in-suite :eclector.parse-result)

(test read-code/smoke
  "Test reading Eclector source code into parse results."
  (finishes
    (let ((client (make-instance 'simple-result-client)))
      (eclector.test:map-all-system-expressions
       (lambda (filename form-number parse-result)
         (declare (ignore filename form-number))
         ;; Do not use (is (typep ...)) so the number of checks does
         ;; not vary with source code changes.
         (unless (typep parse-result '#1=parse-result)
           (fail "~@<~S is not of type ~S.~@:>" parse-result '#1#)))
       (alexandria:curry 'eclector.parse-result:read client)
       ;; The concrete-syntax-tree system may not be loaded when this
       ;; test runs.
       :filter (lambda (system-name)
                 (not (search "concrete-syntax-tree" system-name)))))))
