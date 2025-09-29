(defsystem "eclector"
  :description "A portable, extensible Common Lisp reader."
  :license     "BSD"
  :author      ("Robert Strandh"
                "Jan Moringen")
  :maintainer  "Jan Moringen"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("alexandria"
                "closer-mop"
                "acclimation")

  :components  ((:module "base"
                 :pathname "code/base"
                 :serial t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "variables")
                              (:file "generic-functions")
                              (:file "conditions")
                              (:file "read-char")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "readtable"
                 :pathname "code/readtable"
                 :depends-on ("base")
                 :serial t
                 :components ((:file "package")
                              (:file "variables")
                              (:file "conditions")
                              (:file "generic-functions")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "simple-readtable"
                 :pathname "code/readtable/simple"
                 :depends-on ("base"
                              "readtable")
                 :serial t
                 :components ((:file "package")
                              (:file "readtable")
                              (:file "methods")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "reader"
                 :pathname "code/reader"
                 :depends-on ("base"
                              "readtable")
                 :serial t
                 :components ((:file "package")
                              ;; The file variables defines standard
                              ;; variables with names in the
                              ;; COMMON-LISP package which control the
                              ;; reader. Loading the file can be
                              ;; useful when Eclector is used as the
                              ;; reader of a Common Lisp
                              ;; implementation.
                              (:file "variables"
                               :if-feature :eclector-define-cl-variables)
                              (:file "more-variables")
                              (:file "generic-functions")
                              (:file "additional-conditions")
                              (:file "utilities")
                              (:file "labeled-objects")
                              (:file "tokens")
                              (:file "read-common")
                              (:file "read")
                              (:file "macro-functions")
                              (:file "init")
                              (:file "quasiquote-macro")
                              (:file "deprecation")
                              ;; Translatable messages
                              (:file "messages-english")))

                (:module "parse-result"
                 :pathname "code/parse-result"
                 :depends-on ("reader")
                 :serial t
                 :components ((:file "package")
                              (:file "client")
                              (:file "generic-functions")
                              (:file "labeled-objects")
                              (:file "read")))

                (:static-file "README.md")
                (:static-file "LICENSE"))

  :in-order-to ((test-op (test-op "eclector/test"))))

(defsystem "eclector/test"
  :description "Tests for the eclector system"
  :license     "BSD"
  :author      "Jan Moringen"

  :depends-on  ("alexandria"
                "eclector"
                (:version "fiveam" "1.4"))

  :components  ((:module "test"
                 :serial t
                 :components ((:file "package")
                              (:file "test-utilities")
                              (:file "code-reading-utilities")
                              (:file "gen-labeled-objects")))

                (:module "readtable"
                 :pathname "test/readtable"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")
                              (:file "generic-functions")))

                (:module "simple-readtable"
                 :pathname "test/readtable/simple"
                 :depends-on ("readtable")
                 :serial t
                 :components ((:file "package")
                              (:file "methods")))

                (:module "reader"
                 :pathname "test/reader"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")

                              (:file "test-utilities")
                              (:file "gen-quasiquote")

                              (:file "utilities")
                              (:file "labeled-objects")
                              (:file "tokens")
                              (:file "read")
                              (:file "macro-functions")
                              (:file "quasiquote-macro")

                              (:file "readtable")

                              (:file "client")

                              (:file "recover")

                              (:file "read-code")))

                (:module "parse-result"
                 :pathname "test/parse-result"
                 :depends-on ("test")
                 :serial t
                 :components ((:file "package")
                              (:file "read")
                              (:file "client")
                              (:file "recover")
                              (:file "read-code"))))

  :perform     (test-op (operation component)
                 (let ((successp (uiop:symbol-call '#:eclector.test '#:run-tests)))
                   (when (and (boundp 'cl-user::*result*) (not successp))
                     (setf (symbol-value 'cl-user::*result*) nil)))))
