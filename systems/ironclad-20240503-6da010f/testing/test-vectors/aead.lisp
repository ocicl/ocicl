;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(in-package :crypto-tests)

#.(loop for mode in (crypto:list-all-authenticated-encryption-modes)
        collect `(rtest:deftest ,mode
                   (run-test-vector-file ',mode *authenticated-encryption-tests*)
                   t)
          into forms
        finally (return `(progn ,@forms)))

#.(loop for mode in (crypto:list-all-authenticated-encryption-modes)
        collect `(rtest:deftest ,(crypto::symbolicate mode '#:/incremental)
                   (run-test-vector-file ',mode *authenticated-encryption-incremental-tests*)
                   t)
          into forms
        finally (return `(progn ,@forms)))
