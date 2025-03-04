;; -*- Lisp; indent-tabs-mode: nil -*-

(defsystem :parse-number
  :author "Matthew Danish <mrd@debian.org>"
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"
  :description "Number parsing library"
  :license "BSD 3-Clause"
  :version (:read-file-form "version.sexp")
  :components ((:static-file "version.sexp")
               (:file "parse-number"))
  :in-order-to ((test-op (test-op :parse-number/tests))))

(defsystem :parse-number/tests
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Sharp Lispers <sharplispers@googlegroups.com>"
  :description "Parse-Number test suite"
  :license "BSD 3-Clause"
  :depends-on (:parse-number)
  :components ((:file "tests"))
  :perform (test-op (o c)
             (symbol-call :org.mapcar.parse-number-tests :run-tests)))
