;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :ironclad-text
  :components
  ((:module :src
            :serial t
            :components
            ((:file "text")
             )))
  :depends-on (:ironclad :flexi-streams))
