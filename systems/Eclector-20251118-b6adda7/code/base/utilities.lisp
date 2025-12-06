(cl:in-package #:eclector.base)

#+sbcl
(defun &optional-and-&key-style-warning-p (condition)
  (and (typep condition 'simple-condition)
       (eql 0 (search "&OPTIONAL and &KEY found in the same lambda list"
                      ;; We do this seemingly overcomplicated maneuver
                      ;; instead of checking the format control slot
                      ;; directly because the contents of that slot
                      ;; may not be a string.
                      (with-standard-io-syntax
                        (let ((*print-right-margin* most-positive-fixnum))
                          (apply #'format nil (simple-condition-format-control condition)
                                 (simple-condition-format-arguments condition))))))))

#+sbcl
(deftype &optional-and-&key-style-warning ()
  '(satisfies &optional-and-&key-style-warning-p))
