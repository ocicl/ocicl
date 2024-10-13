(defsystem which-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:which
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "which")))))
