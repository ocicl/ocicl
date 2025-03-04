(defsystem defclass-std
  :name "defclass-std"
  :version "0.2.0"
  :author "André Miranda"
  :maintainer "vindarel"
  :mailto "vindarel@mailz.org"
  ;; :homepage "https://github.com/EuAndreh/defclass-std"
  :homepage "https://github.com/lisp-maintainers/defclass-std"
  :bug-tracker "https://github.com/lisp-maintainers/defclass-std/issues"
  :source-control (:git "git@github.com:lisp-maintainers/defclass-std.git")
  :license "LLGPL"
  :depends-on (alexandria
               closer-mop
               anaphora)
  :components ((:module "src"
                        :components ((:file "defclass-std")))
               (:static-file "README.md"))
  :description "Two shortcut macros to write DEFCLASS and PRINT-OBJECT forms quickly."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-truename* "README.md"))
  :in-order-to ((test-op (test-op defclass-std-test))))
