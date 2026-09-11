(defsystem "local-time"
  :version "1.0.6"
  :license "BSD"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "A library for manipulating dates and times, based on a paper by Erik Naggum"
  :depends-on (:uiop)
  :in-order-to ((test-op (test-op "local-time/test")))
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "local-time")))))

(defsystem "local-time/test"
  :version "1.0.6"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Testing code for the local-time library"
  :depends-on (:fiasco :local-time)
  :perform (test-op (o s) (uiop:symbol-call '#:fiasco
                                            '#:run-package-tests
                                            :package
                                            (find-package '#:local-time.test)))
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "simple")
                             (:file "comparison")
                             (:file "formatting")
                             (:file "parsing")
                             (:file "timezone")))))
