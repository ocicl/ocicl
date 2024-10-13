(defsystem copy-directory
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:uiop
               :cl-fad
               :which)
  :components ((:module "src"
                :serial t
                :components
                ((:file "copy-directory"))))
  :description "Copy a directory."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op copy-directory-test))))
