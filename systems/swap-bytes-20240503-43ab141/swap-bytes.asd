;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :swap-bytes
  :author "Stas Boukarev <stassats@gmail.com>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :description "Optimized byte-swapping primitives."
  :version (:read-file-form "version.sexp")
  :licence "MIT"
  :defsystem-depends-on (:trivial-features)
  :depends-on (:trivial-features)
  :components ((:file "package")
               (:file "ccl"
                :if-feature (:and :ccl (:or :x86 :x86-64))
                :depends-on ("package"))
               (:file "sbcl-defknowns"
                :if-feature (:and :sbcl (:or :x86 :x86-64))
                :depends-on ("package"))
               (:file "sbcl-vops"
                :if-feature (:and :sbcl (:or :x86 :x86-64))
                :depends-on ("package" "sbcl-defknowns"))
               (:file "sbcl"
                :if-feature (:and :sbcl (:or :x86 :x86-64))
                :depends-on ("package" "sbcl-defknowns" "sbcl-vops"))
               (:file "portable"
                :if-feature (:not (:or (:and :ccl (:or :x86 :x86-64))
                                       (:and :sbcl (:or :x86 :x86-64))))
                :depends-on ("package" "ccl" "sbcl"))
               (:file "network" :depends-on ("package" "portable"))
               (:file "endianness" :depends-on ("package" "portable")))
  :in-order-to ((test-op (test-op :swap-bytes/test))))

(defsystem :swap-bytes/test
  :author "Stas Boukarev <stassats@gmail.com>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :description "Swap-bytes test suite."
  :version (:read-file-form "version.sexp")
  :depends-on (:swap-bytes :fiveam)
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :5am :run! :swap-bytes)))
