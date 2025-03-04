;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:org.mapcar.parse-number-tests
  (:use #:common-lisp #:org.mapcar.parse-number)
  (:export #:run-tests))

(in-package #:org.mapcar.parse-number-tests)

(defparameter *test-values*
  `("1" "-1" "1034" "3." "-3." "-364" "80/335" "1.3214" "3.5333" "5.2d50" "2.4E4" "6.8d3" "#xFF"
    "#b-1000" "#o-101/75" "13.09s3" "35.66l5" "21.4f2" "#C(1 2)"
    "#c ( #xF #o-1 ) " "#c(1d1 2s1)" "#16rFF" "#9r10" "#C(#9r44/61 4f4)"
    "2.56 " "+1" "+1." "+1.4" "+.14" "+0.14" " -4.312"
    ,(write-to-string least-positive-single-float) ,(write-to-string most-positive-single-float))
  "These are the values that are going to be tested.")

(defparameter *expected-failures* ()
  "These are the values that are expected to be parsed incorrectly.")

(defparameter *invalid-values*
  '("5 . 5" "--10" "/20" "d10" "5/5/5" "1.2.3" "1d0s0" "10/" "10d"
    "10.5/20" "15/20.5" "5/10d0" "5d05/10" "5d0.1" "#x5.0d0"
    "#x5l0" "10/-5" "#x5.0" "." "#x10/-5" " " "")
  "These are the values to be tested that when parsed are expected to
   signal an invalid-number error.")

(defun run-tests ()
  "Test all of the values and invalid-numbers."
  (format t "~&~16@A (~16@A) = ~16A ~16A~%~%"
          "String value" "READ value" "Parsed value" "*rdff*")
  (let ((expected-failures '())
        (unexpected-failures '())
        (unexpected-non-invalids '()))
    (dolist (value *test-values*)
      (dolist (*read-default-float-format* '(double-float single-float))
        (let ((left (read-from-string value))
              (right (parse-number value)))
          (format t "~&~18@S (~26@A) = ~26A ~20A~%"
                  value
                  left
                  right
                  *read-default-float-format*)
          (unless (eql left right)
            (if (find value *expected-failures* :test #'string=)
                (pushnew value expected-failures :test #'string=)
                (pushnew value unexpected-failures :test #'string=))))))
    (format t "~2&~16@A: ~26@A~%~%" "String Value" "Invalid")
    (dolist (value *invalid-values*)
      (dolist (*read-default-float-format* '(double-float single-float))
        (let ((invalid (handler-case (progn (parse-number value) nil)
                         (invalid-number () t))))
          (format t "~&~18@S ~26@A ~77T~A"
                  value invalid *read-default-float-format*)
          (unless invalid
            (pushnew value unexpected-non-invalids :test #'string=)))))
    (flet ((format-failures (label val)
             (when val
               (format t "~%~A: ~{~_~S~^, ~}." label val))))
      (let ((unexpected-successes
              (set-difference *expected-failures* expected-failures
                              :test #'string=)))
        (format-failures "Expected failures" expected-failures)
        (format-failures "Unexpected failures" unexpected-failures)
        (format-failures "Unexpected successes" unexpected-successes)
        (format-failures "Unexpected non-invalid numbers"
                         unexpected-non-invalids)
        (if (or unexpected-failures unexpected-successes unexpected-non-invalids)
            nil
            t)))))
