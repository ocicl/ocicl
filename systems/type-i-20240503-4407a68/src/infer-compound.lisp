(in-package :type-i)
(in-optimizer :trivial)



;;; null



(define-inference-rule not-tests (test)
  (match test
    ((list 'not test)
     (when-let ((type (test-type test)))
       `((typep ? '(not ,type)))))))


(define-inference-rule and-tests (test)
  (match test
    ((list* 'and tests)
     (let ((types (mapcar #'test-type tests)))
       (when (every #'identity types)
         `((typep ? '(and ,types))))))))

(define-inference-rule or-tests (test)
  (match test
    ((list* 'or tests)
     (let ((types (mapcar #'test-type tests)))
       (when (every #'identity types)
         `((typep ? '(or ,types))))))))
