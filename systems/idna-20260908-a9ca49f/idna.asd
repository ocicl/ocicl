;;;; cl-punycode.asd

(asdf:defsystem #:idna
  :description "IDNA (international domain names) string encoding and decoding routines"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:file "package")
               (:file "encode")
               (:file "decode")))

