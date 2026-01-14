(defpackage :jsown-tests
  (:use :common-lisp
        :jsown
        :fiveam)
  (:export :test-all
           :test-readers
           :test-writers
           :test-accessors))

(in-package :jsown-tests)

(def-suite test-all
    :description "All tests made for jsown")
