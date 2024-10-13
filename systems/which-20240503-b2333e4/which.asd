(defsystem which
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/which"
  :bug-tracker "https://github.com/eudoxia0/which/issues"
  :source-control (:git "git@github.com:eudoxia0/which.git")
  :depends-on (:path-parse
               :cl-fad
               :uiop)
  :components ((:module "src"
                :serial t
                :components
                ((:file "which"))))
  :description "The which UNIX command in Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op which-test))))
