;;;; trivial-file-size.asd

(defsystem "trivial-file-size"
  :description "Stat a file's size."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :in-order-to ((test-op (test-op "trivial-file-size/tests")))
  :depends-on ((:feature :sbcl    (:require :sb-posix))
               (:feature :allegro (:require "osi"))
               (:feature :cmucl   (:require :unix))
               (:feature :clisp   (:require "syscalls"))
               "uiop")
  :components ((:file "package")
               (:file "trivial-file-size")))

(defsystem "trivial-file-size/tests"
  :description "Tests for trivial-file-size."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :perform (test-op (o c)
                    (symbol-call :trivial-file-size/tests
                                 '#:run-file-size-tests))
  :depends-on ("fiveam" "trivial-file-size")
  :components ((:file "tests")))
