;;; -*- mode: lisp -*-

(asdf:defsystem #:tar
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A high level interface for tar archives"
  :license "MIT"
  :depends-on ("alexandria" "babel" "local-time" "split-sequence"
                            (:version "tar-file" "0.2.0")
                            "uiop" "40ants-doc")
  :pathname "src"
  :in-order-to ((test-op (load-op "tar/test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-test)))
                      (error "Tests failed")))
  :components ((:file "archive" :depends-on ("package"))
               (:file "conditions" :depends-on ("package"))
               (:file "entry" :depends-on ("package" "utils"))
               (:file "gnu-archive" :depends-on ("package" "archive" "utils" "validation"))
               (:file "package")
               (:file "pax-archive" :depends-on ("package" "archive" "utils" "validation"))
               (:file "ustar-archive" :depends-on ("package" "archive" "utils" "validation"))
               (:file "utils" :depends-on ("package"))
               (:file "v7-archive" :depends-on ("package" "archive" "validation"))
               (:file "validation" :depends-on ("package" "archive" "entry"))))

(asdf:defsystem #:tar/common-extract
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Common utilities for tar-simple-extract and tar-extract."
  :license "MIT"
  :depends-on ("split-sequence" "tar" "uiop")
  :pathname "src/common-extract"
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "utils" :depends-on ("package"))
               (:file "pathname-computation" :depends-on ("package"))))

(asdf:defsystem #:tar/create
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to create tar archives from the filesystem."
  :license "MIT"
  :depends-on ("tar" "uiop" "local-time" "osicat")
  :pathname "src/create/"
  :in-order-to ((test-op (load-op "tar/create-test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-create-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "create" :depends-on ("package"))
               (:file "posix" :depends-on ("create") :if-feature (:not :windows))
               (:file "windows" :depends-on ("create") :if-feature :windows)))

(asdf:defsystem #:tar/docs
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Documentation system for tar."
  :license "MIT"
  :depends-on ("tar" "40ants-doc" "tar/simple-extract" "tar/extract" "tar/create")
  :pathname "src/"
  :components ((:file "docs")))

(asdf:defsystem #:tar/extract
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to extract tar archives to the filesystem."
  :license "MIT"
  :depends-on ("alexandria" "local-time" "osicat" "tar" "uiop" "tar/common-extract")
  :pathname "src/extract"
  :in-order-to ((test-op (load-op "tar/extract-test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-extract-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "features" :depends-on ("package"))
               (:file "utils" :depends-on ("package" "features"))
               (:file "fd-stream" :depends-on ("package" "utils" "features"))
               (:file "extract" :depends-on ("package" "conditions" "utils" "features"))))

(asdf:defsystem #:tar/simple-extract
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "A system to extract tar archives to the filesystem using only portable CL constructs."
  :license "MIT"
  :depends-on ("tar" "tar/common-extract" "uiop" "local-time")
  :pathname "src/simple-extract"
  :in-order-to ((test-op (load-op "tar/simple-extract-test")))
  :perform (test-op (o c)
                    (unless (eql :passed (uiop:symbol-call
                                          :parachute :status
                                          (uiop:symbol-call :parachute :test :tar-simple-extract-test)))
                      (error "Tests failed")))
  :components ((:file "package")
               (:file "extract" :depends-on ("package"))))

;;; Tests

(defsystem #:tar/test
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Tar test system"
  :license "MIT"
  :pathname "test"
  :components ((:file "package")
               (:file "read" :depends-on ("package" "shared"))
               (:file "shared" :depends-on ("package"))
               (:file "write" :depends-on ("package" "shared")))
  :depends-on ("tar" "parachute"))

(defsystem #:tar/create-test
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Tar create test system"
  :license "MIT"
  :pathname "test/create"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "v7" :depends-on ("package" "utils"))
               (:file "ustar" :depends-on ("package" "utils"))
               (:file "pax" :depends-on ("package" "utils"))
               ;;(:file "gnu" :depends-on ("package" "utils"))
               )
  :depends-on ("tar/create" "parachute" "osicat"))

(defsystem #:tar/extract-test
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Tar extract test system"
  :license "MIT"
  :pathname "test/extract"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "extract" :depends-on ("package" "utils"))
               (:file "stress" :depends-on ("package" "utils")))
  :depends-on ("tar/extract" "parachute"))

(defsystem #:tar/simple-extract-test
  :version (:read-file-form "version.lisp-expr")
  :author "Eric Timmons <eric@timmons.dev>"
  :description "Tar simple extract test system"
  :license "MIT"
  :pathname "test/simple-extract"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "v7" :depends-on ("package" "utils"))
               (:file "stress" :depends-on ("package" "utils")))
  :depends-on ("tar/simple-extract" "parachute" "osicat"))
