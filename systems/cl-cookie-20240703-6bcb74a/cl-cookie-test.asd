#|
  This file is a part of cl-cookie project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem cl-cookie-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:cl-cookie
               :rove)
  :components ((:module "t"
                :components
                ((:file "cl-cookie"))))
  :description "Test system for cl-cookie"
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
