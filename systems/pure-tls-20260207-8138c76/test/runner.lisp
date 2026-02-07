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
    (format t "~%--- ML-DSA Tests ---~%")
    (let ((mldsa-ok (run! 'ml-dsa-tests)))
      (format t "~%--- Record Layer Tests ---~%")
      (let ((record-ok (run! 'record-tests)))
        (format t "~%--- Handshake Tests ---~%")
        (let ((handshake-ok (run! 'handshake-tests)))
          (format t "~%--- Certificate Tests ---~%")
          (let ((cert-ok (run! 'certificate-tests)))
            (format t "~%--- Context Tests ---~%")
            (let ((context-ok (run! 'context-tests)))
              (format t "~%--- Context Integration Tests ---~%")
              (let ((context-integration-ok (run! 'context-integration-tests)))
                (format t "~%--- OpenSSL Tests ---~%")
              (let ((openssl-ok (run! 'openssl-tests)))
                (format t "~%--- BoringSSL Pattern Tests ---~%")
                (let ((boringssl-ok (run! 'boringssl-tests)))
                  (format t "~%--- X509test Validation Tests ---~%")
                  (let ((x509test-ok (run! 'x509test-tests)))
                    (format t "~%=== Summary ===~%")
                    (format t "Note: Run (run-network-tests) separately for network tests.~%")
                    (and crypto-ok mldsa-ok record-ok handshake-ok cert-ok context-ok context-integration-ok openssl-ok boringssl-ok x509test-ok))))))))))))

(defun run-openssl-tests ()
  "Run OpenSSL test suite adaptation tests.
   Returns T if all tests pass, NIL otherwise."
  (format t "~&=== Running OpenSSL Tests ===~%~%")
  (run! 'openssl-tests))

(defun run-ml-dsa-tests ()
  "Run ML-DSA post-quantum signature tests.
   Returns T if all tests pass, NIL otherwise."
  (format t "~&=== Running ML-DSA Tests ===~%~%")
  (run! 'ml-dsa-tests))

(defun run-network-tests ()
  "Run network-dependent tests (requires internet access).
   Returns T if all tests pass, NIL otherwise."
  (format t "~&Running TLS 1.3 network tests...~%~%")
  (run! 'network-tests))
