(cl:in-package #:eclector.readtable.test)

(in-suite :eclector.readtable)

(test set-syntax-from-char.smoke
  "Smoke test for the SET-SYNTAX-FROM-CHAR function."
  (let* ((from eclector.reader:*readtable*)
         (to   (eclector.readtable:copy-readtable from)))
    (is (eq t (eclector.readtable:set-syntax-from-char #\! #\; to from)))))
