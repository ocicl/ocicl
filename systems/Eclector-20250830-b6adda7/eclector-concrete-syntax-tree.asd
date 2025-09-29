(defsystem "eclector-concrete-syntax-tree"
  :description "Reading into concrete syntax tree objects."
  :license     "BSD"
  :author      ("Robert Strandh"
                "Jan Moringen")
  :maintainer  "Jan Moringen"

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("alexandria"
                (:version "concrete-syntax-tree" "0.3")

                "eclector")

  :components  ((:module "concrete-syntax-tree"
                 :pathname "code/concrete-syntax-tree"
                 :serial t
                 :components ((:file "package")
                              (:file "client")
                              (:file "labeled-objects")
                              (:file "read"))))

  :in-order-to ((test-op (test-op "eclector-concrete-syntax-tree/test"))))

(defsystem "eclector-concrete-syntax-tree/test"
  :description "Test for the eclector-concrete-syntax-tree system"
  :license     "BSD"
  :author      "Jan Moringen"

  :depends-on  ("eclector-concrete-syntax-tree"
                "eclector/test"
                (:version "fiveam" "1.4"))

  :components  ((:module "concrete-syntax-tree"
                 :pathname "test/concrete-syntax-tree"
                 :serial t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "read")
                              (:file "client")
                              (:file "labeled-objects")
                              (:file "recover")
                              (:file "read-code"))))

  :perform     (test-op (operation component)
                 (let ((successp (uiop:symbol-call
                                  '#:eclector.concrete-syntax-tree.test
                                  '#:run-tests)))
                   (when (and (boundp 'cl-user::*result*) (not successp))
                     (setf (symbol-value 'cl-user::*result*) nil)))))
