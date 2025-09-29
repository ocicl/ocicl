(cl:defpackage #:eclector.parse-result.test
  (:use
   #:common-lisp
   #:fiveam)

  (:import-from #:eclector.test
   #:equal* #:equalp*

   #:do-input-cases        #:expect
   #:do-stream-input-cases #:with-stream
   #:error-case)

  (:export
   #:run-tests))

(cl:in-package #:eclector.parse-result.test)

(def-suite :eclector.parse-result
  :in :eclector)

(defun run-tests ()
  (run! :eclector.parse-result))
