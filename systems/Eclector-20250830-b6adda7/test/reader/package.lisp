(cl:defpackage #:eclector.reader.test
  (:use
   #:common-lisp
   #:fiveam)

  (:import-from #:eclector.reader
   #:convert-according-to-readtable-case
   #:skip-whitespace
   #:skip-whitespace*)

  (:import-from #:eclector.test
   #:equal*
   #:relaxed-equalp
   #:do-input-cases        #:expect
   #:do-stream-input-cases #:with-stream
   #:check-signals-error
   #:error-case
   #:do-recover-test-case

   #:make-car-spine-list
   #:make-tree
   #:read-long-list

   #:gen-labels-and-references)

  (:export
   #:run-tests))

(cl:in-package #:eclector.reader.test)

;;; Main test suite and test entry point

(def-suite :eclector.reader
    :in :eclector)

(defun run-tests ()
  (run! :eclector.reader))
