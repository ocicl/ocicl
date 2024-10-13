;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

#-(or abcl allegro clasp ccl cmu ecl lispworks sbcl)
(error "static-vectors does not support this Common Lisp implementation!")

(defsystem :static-vectors
  :description "Create vectors allocated in static memory."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :depends-on (:alexandria :cffi)
  :pathname "src/"
  :serial t
  :components ((:file "pkgdcl")
               (:file "constantp")
               (:file "impl-abcl" :if-feature :abcl)
               (:file "impl-allegro" :if-feature :allegro)
               (:file "impl-clasp" :if-feature :clasp)
               (:file "impl-clozure" :if-feature :ccl)
               (:file "impl-cmucl" :if-feature :cmu)
               (:file "impl-ecl" :if-feature :ecl)
               (:file "impl-lispworks" :if-feature :lispworks)
               (:file "impl-sbcl" :if-feature :sbcl)
               (:file "constructor")
               (:file "cffi-type-translator"))
  :in-order-to ((test-op (test-op :static-vectors/test))))

(defsystem :static-vectors/test
  :description "Static-vectors test suite."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :depends-on (:static-vectors :fiveam)
  :pathname "tests/"
  :components ((:file "static-vectors-tests"))
  :perform (test-op (o c) (symbol-call :5am :run! :static-vectors)))
