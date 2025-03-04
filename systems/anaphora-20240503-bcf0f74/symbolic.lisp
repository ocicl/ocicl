;;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-Lisp; Package: ANAPHORA -*-

;;;; Copyright (c) 2003 Brian Mastenbrook

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :anaphora)

(defmacro internal-symbol-macrolet (&rest whatever)
  `(symbol-macrolet ,@whatever))

(define-setf-expander internal-symbol-macrolet (binding-forms place &environment env)
  (multiple-value-bind (dummies vals newvals setter getter)
      (get-setf-expansion place env)
    (values dummies
	    (substitute `(symbol-macrolet ,binding-forms it) 'it vals)
	    newvals
	    `(symbol-macrolet ,binding-forms ,setter)
	    `(symbol-macrolet ,binding-forms ,getter))))

(with-unique-names (s-indicator current-s-indicator)
  (defmacro symbolic (operation test &rest other-args)
    (with-unique-names (this-s)
      (let ((current-s (get s-indicator current-s-indicator)))
        (setf (get s-indicator current-s-indicator) this-s)
        `(symbol-macrolet
             ((,this-s (internal-symbol-macrolet ((it ,current-s)) ,test))
              (it ,this-s))
           (,operation it ,@other-args)))))

  (defmacro anaphoric (op test &body body)  
    (with-unique-names (this-s)
      (setf (get s-indicator current-s-indicator) this-s)
      `(let* ((it ,test)
              (,this-s it))
         (declare (ignorable ,this-s))
         (,op it ,@body)))))
