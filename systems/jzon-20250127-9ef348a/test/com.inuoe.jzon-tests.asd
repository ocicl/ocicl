(defsystem #:com.inuoe.jzon-tests
  :version "0.0.0"
  :description "Tests for the jzon library"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "MIT"
  :components
  ((:file "jzon-tests"))
  :perform
  (test-op (o c) (symbol-call '#:com.inuoe.jzon-tests '#:run))
  :depends-on
  (#:alexandria
   #:fiveam
   #:flexi-streams
   (:feature (:not :ecl) #:float-features)
   #:com.inuoe.jzon
   #:uiop))
