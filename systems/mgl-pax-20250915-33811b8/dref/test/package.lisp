(mgl-pax:define-package :dref-test
  (:documentation "Test package for DRef.")
  (:use #:common-lisp #:dref #:dref-ext #:try)
  (:export #:test
           #:check-ref
           #:check-ref-sets
           #:check-source-location))
