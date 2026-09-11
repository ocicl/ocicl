(asdf:defsystem :jsown
  :name "JSOWN"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Fast JSON parsing library.  Mainly geared torwards fetching only a few keys of many objects, but efficient for other types of content too"
  :serial t
  :components ((:file "packages")
               (:file "accessors")
               (:file "reader")
               (:file "writer")))
