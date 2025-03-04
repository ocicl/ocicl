(defsystem defclass-std-test
  :name "defclass-std-test"
  :version "0.1.1"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/defclass-std"
  :bug-tracker "https://github.com/EuAndreh/defclass-std/issues"
  :source-control (:git "git@github.com:EuAndreh/defclass-std.git")
  :license "LLGPL"
  :description "Test system for defclass-std."
  :depends-on (defclass-std
                  prove)
  :components ((:module "t"
                        :components ((:test-file "defclass-std"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern "RUN-TEST-SYSTEM" :prove-asdf) c)
                    (asdf:clear-system c)))
