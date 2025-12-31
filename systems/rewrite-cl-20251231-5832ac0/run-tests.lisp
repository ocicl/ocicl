;;;; run-tests.lisp - Run the full test suite

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system "rewrite-cl/test")
(funcall (intern "RUN-TESTS" "REWRITE-CL/TEST"))
