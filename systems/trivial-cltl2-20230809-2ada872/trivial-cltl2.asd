(asdf:defsystem :trivial-cltl2
  :version "0.1.1"
  :author "Tomohiro Matsuyama"
  :description "Compatibility package exporting CLtL2 functionality"
  :license "LLGPL"
  :components ((:file "cltl2")
               (:file "allegro" :if-feature :allegro)))
