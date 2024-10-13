(defsystem copy-directory-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:copy-directory
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "copy-directory")))))
