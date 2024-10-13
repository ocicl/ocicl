(defsystem path-parse-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:path-parse
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "path-parse")))))
