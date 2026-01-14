;;;; trivial-with-current-source-form.asd --- System definition for the trivial-with-current-source-form system.
;;;;
;;;; Copyright (C) 2014-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "trivial-with-current-source-form"
  :description "Helps macro writers produce better errors for macro users"
  :license     "MIT" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "sbcl"        :if-feature :sbcl)
                              (:file       "clasp"       :if-feature :clasp)
                              (:file       "unsupported" :if-feature (:not (:or :clasp :sbcl)))
                              (:file       "macro")))))
