(cl:in-package #:eclector.parse-result.test)

(def-suite* :eclector.parse-result.client
  :in :eclector.parse-result)

;;; Test annotating labeled object references

(defclass parse-result+annotation-client
    (eclector.reader.test::label-reference-annotation-mixin
     simple-result-client)
  ())

(test labeled-objects/annotation
  "Test annotating labeled object references in parse results."
  (let* ((client (make-instance 'parse-result+annotation-client))
         (result (eclector.parse-result:read-from-string client "#1=(1 #1#)"))
         (object (raw result)))
    (is (typep result 'cons-result))
    (is (eq 1 (raw (first-child result))))
    (let ((reference (raw (first-child (rest-child result)))))
      (is (eq :circular-reference (first reference)))
      (is (eq object (second reference))))))

;;; Combine custom labeled object representation with annotating
;;; labeled object references.

(defclass parse-result+custom-labeled-objects+annotation-client
    (eclector.reader.test::label-reference-annotation-mixin
     simple-result-client
     eclector.reader.test::custom-labeled-objects-client)
  ())

(test labeled-objects/custom+annotation
  "Test annotating references and custom labeled object in parse results."
  (let* ((client (make-instance 'parse-result+custom-labeled-objects+annotation-client))
         (result (eclector.parse-result:read-from-string client "#1=(1 #1#)"))
         (object (raw result)))
    (is (typep result 'cons-result))
    (is (eq 1 (raw (first-child result))))
    (let ((reference (raw (first-child (rest-child result)))))
      (is (eq :circular-reference (first reference)))
      (is (eq object (second reference))))))
