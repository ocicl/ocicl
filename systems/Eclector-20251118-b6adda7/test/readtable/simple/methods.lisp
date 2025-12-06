(cl:in-package #:eclector.readtable.simple.test)

(in-suite :eclector.readtable.simple)

(test readtablep.smoke
  "Smoke test for the method on the READTABLEP protocol function."
  (is-false (eclector.readtable:readtablep 1))
  (is-true (eclector.readtable:readtablep
            (make-instance 'eclector.readtable.simple:readtable))))
