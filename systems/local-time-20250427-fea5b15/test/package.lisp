(cl:in-package :cl-user)

(fiasco:define-test-package #:local-time.test
  (:use #:common-lisp
        #:alexandria
        #:fiasco
        #:local-time))

(in-package #:local-time.test)

(local-time::reread-timezone-repository)
