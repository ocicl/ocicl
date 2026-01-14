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

(test long-input
  "Inputs of various lengths that stress the chunk cache."
  (loop :for length :from 0 :to 5000
        :do (let* ((input (make-string length :initial-element #\a))
                   (result (esrap:parse '(* #\a) input)))
              (is (eql length (length result))))))

(macrolet ((define-rules ()
             (let ((names '()))
              `(progn
                 ,@(loop :for i :from 0 :to (* 2 esrap::+packrat-hash-table-switch-point+)
                         :for string = (format nil "~R" i)
                         :for name = (intern string)
                         :do (push name names)
                         :collect `(defrule ,name ,string))
                 (defrule number
                     (or ,@names))))))
  (define-rules))

(test many-rules
  "Many alternative rules that stress the packrat cache."
  (is (equal "zero" (parse 'number "zero"))))
