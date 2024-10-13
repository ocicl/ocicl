;;; -*- mode: lisp -*-

(asdf:defsystem #:tar-file
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :maintainer "Eric Timmons <eric@timmons.dev>"
  :description "A system for reading and writing physical entries from/to tar files."
  :license "BSD-style (http://opensource.org/licenses/BSD-3-Clause)"
  :depends-on ("alexandria" "babel" "trivial-gray-streams" "uiop"
               "40ants-doc" "salza2" "chipz" "flexi-streams")
  :pathname "src"
  :in-order-to ((test-op (load-op "tar-file/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-file-test)))
                      (error "Tests failed")))
  :components ((:file "blocked-stream" :depends-on ("package"))
               (:file "bounded-stream" :depends-on ("package"))
               (:file "buffer-ops" :depends-on ("package" "constants"))
               (:file "conditions" :depends-on ("package" "constants"))
               (:file "constants" :depends-on ("package"))
               (:file "docs" :depends-on ("package"))
               (:file "entry" :depends-on ("package" "constants" "generics"))
               (:file "external-macros" :depends-on ("generics" "utils"))
               (:file "generics" :depends-on ("package"))
               (:file "gnu" :depends-on ("package" "macros" "tar-file"))
               (:file "macros" :depends-on ("generics"))
               (:file "package")
               (:file "peeking-stream" :depends-on ("package"))
               (:file "tar-file"
                :depends-on ("generics" "bounded-stream" "macros"
                                        "external-macros" "entry" "constants"))
               (:file "utils" :depends-on ("package"))
               (:file "ustar" :depends-on ("package" "macros" "tar-file"))
               (:file "v7" :depends-on ("package" "macros" "tar-file"))))

(defsystem #:tar-file/test
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :maintainer "Eric Timmons <eric@timmons.dev>"
  :description "A test system for tar-file."
  :pathname "test"
  :components ((:file "blocked-stream" :depends-on ("package"))
               (:file "package")
               (:file "read" :depends-on ("package"))
               (:file "shared" :depends-on ("package"))
               (:file "write" :depends-on ("package")))
  :depends-on (#:tar-file #:parachute))
