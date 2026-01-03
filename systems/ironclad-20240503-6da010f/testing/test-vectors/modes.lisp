;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; modes.lisp -- sanity checks for mode reversibility

(in-package :crypto-tests)

(rtest:deftest :modes.cbc
  (run-test-vector-file :cbc *mode-tests*)
  t)

(rtest:deftest :modes.cbc.padding
  (run-test-vector-file :cbc *mode-padding-tests*)
  t)

(rtest:deftest :modes.cfb
  (run-test-vector-file :cfb *mode-tests*)
  t)

(rtest:deftest :modes.cfb8
  (run-test-vector-file :cfb8 *mode-tests*)
  t)

(rtest:deftest :modes.ofb
  (run-test-vector-file :ofb *mode-tests*)
  t)

(rtest:deftest :modes.ctr
  (run-test-vector-file :ctr *mode-tests*)
  t)
