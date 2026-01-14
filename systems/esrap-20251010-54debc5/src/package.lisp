;;;; Copyright (c) 2007-2013 Nikodemus Siivola <nikodemus@random-state.net>
;;;; Copyright (c) 2012-2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
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

(cl:defpackage #:esrap
  (:use
   #:cl
   #:alexandria)

  (:import-from #:trivial-with-current-source-form
   #:with-current-source-form)

  #+sbcl (:lock t)

  ;; Conditions
  (:export
   #:invalid-expression-error
   #:invalid-expression-error-expression

   #:undefined-rule-symbol

   #:undefined-rule-error

   #:esrap-error
   #:esrap-error-position
   #:esrap-error-text

   #:esrap-parse-error
   #:esrap-parse-error-result
   #:esrap-parse-error-context

   #:left-recursion
   #:left-recursion-nonterminal
   #:left-recursion-path)

  ;; Parsing
  (:export
   #:*on-left-recursion*

   #:parse)

  ;; Expressions
  (:export
   #:! #:? #:+ #:* #:& #:~
   #:character-ranges)

  ;; Introspection
  (:export

   #:expression-start-terminals

   #:rule
   #:rule-dependencies
   #:rule-expression
   #:rule-symbol

   #:find-rule
   #:add-rule #:remove-rule
   #:change-rule

   #:trace-rule #:untrace-rule
   #:untrace-all-rules

   #:describe-grammar
   #:describe-terminal)

  ;; Macros
  (:export
   #:defrule

   #:&bounds

   #:call-transform
   #:text))
