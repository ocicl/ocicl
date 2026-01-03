;;; test/runner.lisp --- Test runner functions for pure-tls
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:pure-tls/test)

(defun run-tests ()
  "Run all pure-tls test suites (excluding network-dependent tests).
   Returns T if all tests pass, NIL otherwise."
  (format t "~&=== Running pure-tls Test Suite ===~%~%")
  (format t "--- Crypto Tests ---~%")
  (let ((crypto-ok (run! 'crypto-tests)))
    (format t "~%--- Record Layer Tests ---~%")
    (let ((record-ok (run! 'record-tests)))
      (format t "~%--- Handshake Tests ---~%")
      (let ((handshake-ok (run! 'handshake-tests)))
        (format t "~%--- Certificate Tests ---~%")
        (let ((cert-ok (run! 'certificate-tests)))
          (format t "~%=== Summary ===~%")
          (format t "Note: Run (run-network-tests) separately for network tests.~%")
          (and crypto-ok record-ok handshake-ok cert-ok))))))
