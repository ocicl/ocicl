;;; x509test-tests.lisp --- X.509 certificate validation tests from Google's x509test
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Tests X.509 certificate parsing against test cases from Google's x509test
;;; project (https://github.com/google/x509test). These tests verify compliance
;;; with RFC 5280 and X.690 DER encoding rules.

(in-package #:pure-tls/test)

;;;; Test Certificate Directory

(defvar *x509test-certs-dir*
  (merge-pathnames "x509test/"
                   (asdf:system-relative-pathname :pure-tls/test "test/certs/"))
  "Directory containing x509test certificates.")

(defun x509test-cert-path (filename)
  "Return path to an x509test certificate."
  (merge-pathnames filename *x509test-certs-dir*))

;;;; Test Suite Definition

(def-suite x509test-tests
  :description "X.509 certificate validation tests from Google's x509test project")

(in-suite x509test-tests)

;;;; Valid Certificate Tests (should parse successfully)

(test x509test-ok-ext-basic-constraints
  "Test valid certificate with Basic Constraints extension (RFC 5280 s4.2.1.9)"
  (let* ((cert-path (x509test-cert-path "ok-ext-basic-constraints.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse certificate with Basic Constraints")))

(test x509test-ok-v1
  "Test valid X.509 version 1 certificate"
  (let* ((cert-path (x509test-cert-path "ok-v1.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse X.509 v1 certificate")))

(test x509test-ok-v3
  "Test valid X.509 version 3 certificate"
  (let* ((cert-path (x509test-cert-path "ok-v3.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse X.509 v3 certificate")))

;;;; Invalid Certificate Tests (should be rejected)

(test x509test-xf-duplicate-extension
  "Test rejection of certificate with duplicate extensions (RFC 5280 s4.2)"
  (let ((cert-path (x509test-cert-path "xf-duplicate-extension.pem")))
    (signals pure-tls:tls-decode-error
      (pure-tls:parse-certificate-from-file cert-path)
      "Should reject certificate with duplicate extensions")))

(test x509test-xf-der-invalid-bitstring
  "Test rejection of certificate with invalid BIT STRING DER encoding (X.690)"
  (let ((cert-path (x509test-cert-path "xf-der-invalid-bitstring.pem")))
    (signals pure-tls:tls-decode-error
      (pure-tls:parse-certificate-from-file cert-path)
      "Should reject certificate with non-zero padding bits in BIT STRING")))

(test x509test-xf-der-invalid-nonminimal-int
  "Test rejection of certificate with non-minimal INTEGER encoding (X.690)"
  (let ((cert-path (x509test-cert-path "xf-der-invalid-nonminimal-int.pem")))
    (signals pure-tls:tls-decode-error
      (pure-tls:parse-certificate-from-file cert-path)
      "Should reject certificate with non-minimal integer encoding")))

(test x509test-xf-algo-mismatch
  "Test rejection of certificate with signature algorithm mismatch (RFC 5280 s4.1.1.2)"
  (let ((cert-path (x509test-cert-path "xf-algo-mismatch1.pem")))
    (signals pure-tls:tls-decode-error
      (pure-tls:parse-certificate-from-file cert-path)
      "Should reject certificate where inner/outer signature algorithms differ")))

(test x509test-xf-unknown-critical-ext
  "Test rejection of certificate with unknown critical extension (RFC 5280 s4.2)"
  (let ((cert-path (x509test-cert-path "xf-unknown-critical-ext.pem")))
    (signals pure-tls:tls-decode-error
      (pure-tls:parse-certificate-from-file cert-path)
      "Should reject certificate with unrecognized critical extension")))

(test x509test-xf-serial-negative
  "Test rejection of certificate with negative serial number (RFC 5280 s4.1.2.2)"
  (let ((cert-path (x509test-cert-path "xf-serial-negative.pem")))
    (signals pure-tls:tls-decode-error
      (pure-tls:parse-certificate-from-file cert-path)
      "Should reject certificate with negative serial number")))

;;;; Test Runner

(defun run-x509test-tests ()
  "Run all x509test certificate validation tests."
  (format t "~%--- X509test Validation Tests ---~%~%")
  (let ((results (run 'x509test-tests)))
    (explain! results)
    (results-status results)))
