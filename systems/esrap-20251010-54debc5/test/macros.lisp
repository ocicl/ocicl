;;;; Copyright (c) 2019 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package #:esrap-tests)

(in-suite esrap)

(test expand-transforms.smoke
  "Smoke test for the EXPAND-TRANSFORMS function."

  (flet ((test-case (transforms expected-identityp expected-constantp expected-textp)
           (multiple-value-bind (code identityp constantp textp)
               (expand-transforms transforms)
             (declare (ignore code))
             (is (eq expected-identityp identityp))
             (is (eq expected-constantp constantp))
             (is (eq expected-textp     textp)))))
    (test-case '()                                                  t   nil nil)
    (test-case '((:identity t))                                     t   nil nil)
    ;; (test-case '((:text t) (:constant 5))                           nil t   nil) TODO should signal an error
    (test-case '((:constant 5) (:text t))                           nil t   t)
    (test-case '((:function string-upcase) (:text t) (:constant 5)) nil nil nil)
    (test-case '((:text t))                                         nil nil t)))
