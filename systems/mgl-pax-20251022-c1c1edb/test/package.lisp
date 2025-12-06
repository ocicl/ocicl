(mgl-pax:define-package :mgl-pax-test
  (:use #:common-lisp #:mgl-pax #:dref #:dref-ext #:try)
  (:export #:test)
  (:import-from #:dref-test
                #:check-ref
                #:check-ref-sets
                #:check-source-location))
