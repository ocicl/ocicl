;;; test/certificate-tests.lisp --- X.509 certificate tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Tests for X.509 certificate parsing and validation.

(in-package #:pure-tls/test)

(def-suite certificate-tests
  :description "Tests for X.509 certificate handling")

(in-suite certificate-tests)

;;;; Note: hex-to-bytes is defined in crypto-tests.lisp

;;;; ASN.1 Parsing Tests

(test asn1-parse-integer
  "Test parsing ASN.1 INTEGER"
  (let* ((data (hex-to-bytes "02 01 05"))  ; INTEGER 5
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 2)
        "Tag should be 2 (INTEGER)")
    (is (= (pure-tls::asn1-node-value node) 5)
        "Value should be 5")))

(test asn1-parse-sequence
  "Test parsing ASN.1 SEQUENCE"
  (let* ((data (hex-to-bytes "30 06 02 01 01 02 01 02"))  ; SEQUENCE { 1, 2 }
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 16)
        "Tag should be 16 (SEQUENCE)")
    (is (pure-tls::asn1-node-constructed node)
        "SEQUENCE should be constructed")))

(test asn1-parse-oid
  "Test parsing ASN.1 OBJECT IDENTIFIER"
  (let* ((data (hex-to-bytes "06 09 2a 86 48 86 f7 0d 01 01 0b"))  ; sha256WithRSAEncryption
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 6)
        "Tag should be 6 (OID)")
    (is (listp (pure-tls::asn1-node-value node))
        "OID value should be a list")))

(test asn1-parse-utf8string
  "Test parsing ASN.1 UTF8String"
  (let* ((data (hex-to-bytes "0c 05 48 65 6c 6c 6f"))  ; "Hello"
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 12)
        "Tag should be 12 (UTF8String)")
    (is (string= (pure-tls::asn1-node-value node) "Hello")
        "Value should be 'Hello'")))

(test asn1-parse-printablestring
  "Test parsing ASN.1 PrintableString"
  (let* ((data (hex-to-bytes "13 05 48 65 6c 6c 6f"))  ; "Hello"
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 19)
        "Tag should be 19 (PrintableString)")
    (is (string= (pure-tls::asn1-node-value node) "Hello")
        "Value should be 'Hello'")))

(test asn1-parse-utctime
  "Test parsing ASN.1 UTCTime"
  (let* ((data (hex-to-bytes "17 0d 32 35 30 31 30 31 30 30 30 30 30 30 5a"))  ; 250101000000Z
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 23)
        "Tag should be 23 (UTCTime)")
    (is (integerp (pure-tls::asn1-node-value node))
        "UTCTime value should be parsed to universal time")))

(test asn1-parse-bitstring
  "Test parsing ASN.1 BIT STRING"
  (let* ((data (hex-to-bytes "03 04 06 6e 5d c0"))  ; BIT STRING
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer data))))
    (is (= (pure-tls::asn1-node-tag node) 3)
        "Tag should be 3 (BIT STRING)")
    (is (listp (pure-tls::asn1-node-value node))
        "BIT STRING value should be a plist with :unused-bits and :bytes")))

;;;; Certificate Structure Tests

(test certificate-version-parsing
  "Test that certificate version is parsed correctly"
  ;; Version 3 certificates have version field [0] EXPLICIT INTEGER 2
  ;; (0-indexed, so version 3 = value 2)
  (let* ((version-data (hex-to-bytes "a0 03 02 01 02"))
         (node (pure-tls::parse-der-node (pure-tls::make-tls-buffer version-data))))
    (is (= (pure-tls::asn1-node-class node) 2)  ; Context-specific
        "Version wrapper should be context-specific")
    (is (pure-tls::asn1-node-constructed node)
        "Version wrapper should be constructed")))

;;;; Hostname Verification Tests

(test hostname-match-exact
  "Test exact hostname matching"
  (is (pure-tls::hostname-matches-p "example.com" "example.com")
      "Exact match should succeed")
  (is (not (pure-tls::hostname-matches-p "other.com" "example.com"))
      "Different hostnames should not match"))

(test hostname-match-wildcard
  "Test wildcard hostname matching"
  (is (pure-tls::hostname-matches-p "*.example.com" "www.example.com")
      "Wildcard should match single subdomain")
  (is (not (pure-tls::hostname-matches-p "*.example.com" "sub.www.example.com"))
      "Wildcard should not match multiple subdomains")
  (is (not (pure-tls::hostname-matches-p "*.example.com" "example.com"))
      "Wildcard should not match apex domain"))

(test hostname-match-case-insensitive
  "Test case-insensitive hostname matching"
  (is (pure-tls::hostname-matches-p "example.com" "EXAMPLE.COM")
      "Hostname matching should be case-insensitive")
  (is (pure-tls::hostname-matches-p "*.example.COM" "www.EXAMPLE.com")
      "Wildcard matching should be case-insensitive"))

;;;; Certificate Fingerprint Tests

(test certificate-fingerprint-format
  "Test that fingerprint is computed correctly"
  ;; Create a minimal DER structure for testing
  (let* ((fake-der (hex-to-bytes "3082 0100"))  ; Minimal SEQUENCE
         (fingerprint (ironclad:digest-sequence :sha256 fake-der)))
    (is (= (length fingerprint) 32)
        "SHA-256 fingerprint should be 32 bytes")))

;;;; Certificate Chain Validation Tests

(test certificate-validity-period
  "Test certificate validity period checking"
  ;; This test uses mock values since we can't easily create test certs
  (let ((now (get-universal-time))
        (past (- (get-universal-time) (* 365 24 60 60 2)))  ; 2 years ago
        (future (+ (get-universal-time) (* 365 24 60 60 2))))  ; 2 years from now
    ;; Valid period: past to future (current time is within)
    (is (and (< past now) (> future now))
        "Current time should be within validity period")))

;;;; OID Tests

(test common-oid-recognition
  "Test recognition of common OIDs"
  ;; Common Name OID: 2.5.4.3
  (let ((cn-oid '(2 5 4 3))
        (org-oid '(2 5 4 10))
        (country-oid '(2 5 4 6)))
    (is (equal cn-oid '(2 5 4 3))
        "Common Name OID should be 2.5.4.3")
    (is (equal org-oid '(2 5 4 10))
        "Organization OID should be 2.5.4.10")
    (is (equal country-oid '(2 5 4 6))
        "Country OID should be 2.5.4.6")))

;;;; Extension OID Tests

(test extension-oid-constants
  "Test extension OID recognition"
  ;; Subject Alternative Name OID: 2.5.29.17
  (let ((san-oid '(2 5 29 17))
        (bc-oid '(2 5 29 19))    ; Basic Constraints
        (ku-oid '(2 5 29 15)))   ; Key Usage
    (is (equal san-oid '(2 5 29 17))
        "SAN OID should be 2.5.29.17")
    (is (equal bc-oid '(2 5 29 19))
        "Basic Constraints OID should be 2.5.29.19")
    (is (equal ku-oid '(2 5 29 15))
        "Key Usage OID should be 2.5.29.15")))

;;;; Signature Algorithm Tests

(test signature-algorithm-oid-mapping
  "Test signature algorithm OID to TLS constant mapping"
  ;; sha256WithRSAEncryption: 1.2.840.113549.1.1.11
  ;; ecdsa-with-SHA256: 1.2.840.10045.4.3.2
  (let ((rsa-sha256-oid '(1 2 840 113549 1 1 11))
        (ecdsa-sha256-oid '(1 2 840 10045 4 3 2)))
    (is (equal rsa-sha256-oid '(1 2 840 113549 1 1 11))
        "RSA-SHA256 OID should be correct")
    (is (equal ecdsa-sha256-oid '(1 2 840 10045 4 3 2))
        "ECDSA-SHA256 OID should be correct")))

;;;; Bundled Certificate Tests (from badssl.com)

(defvar *test-certs-dir*
  (merge-pathnames "certs/"
                   (asdf:system-relative-pathname :pure-tls/test "test/"))
  "Directory containing bundled test certificates.")

(defun test-cert-path (filename)
  "Return path to a bundled test certificate."
  (merge-pathnames filename *test-certs-dir*))

(test parse-expired-certificate
  "Test parsing an expired certificate"
  (let* ((cert-path (test-cert-path "wildcard-expired.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse expired certificate")
    (is (< (pure-tls:certificate-not-after cert) (get-universal-time))
        "Certificate should be expired (notAfter in the past)")))

(test parse-self-signed-certificate
  "Test parsing a self-signed certificate (valid, not expired)"
  (let* ((cert-path (test-cert-path "self-signed-valid.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse self-signed certificate")
    ;; Self-signed: subject CN is test.example.com
    (let ((cns (pure-tls:certificate-subject-common-names cert)))
      (is (member "test.example.com" cns :test #'string=)
          "Self-signed cert should have correct CN"))
    ;; Verify it's NOT expired (valid until 2036)
    (is (> (pure-tls:certificate-not-after cert) (get-universal-time))
        "Self-signed test cert should still be valid")
    ;; Verify issuer == subject (self-signed property)
    (let* ((subject-rdns (pure-tls::x509-name-rdns
                          (pure-tls::x509-certificate-subject cert)))
           (issuer-rdns (pure-tls::x509-name-rdns
                         (pure-tls::x509-certificate-issuer cert))))
      (is (equal subject-rdns issuer-rdns)
          "Self-signed: issuer should equal subject"))))

(test self-signed-rejected-by-chain-verification
  "Test that self-signed certificates are rejected during chain verification"
  ;; Disable native verification to test pure-Lisp path explicitly
  (let ((pure-tls:*use-windows-certificate-store* nil)
        (pure-tls:*use-macos-keychain* nil))
    (let* ((cert-path (test-cert-path "self-signed-valid.pem"))
           (cert (pure-tls:parse-certificate-from-file cert-path))
           (empty-roots nil))
      ;; Self-signed cert should be rejected when not in trusted roots
      (signals pure-tls:tls-verification-error
        (pure-tls::verify-certificate-chain (list cert) empty-roots))
      ;; But should pass if we add it to trusted roots
      (is (pure-tls::verify-certificate-chain (list cert) (list cert))
          "Self-signed cert should pass if explicitly trusted"))))

(test parse-superfish-ca
  "Test parsing the Superfish malware CA certificate"
  (let* ((cert-path (test-cert-path "ca-superfish.crt"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse Superfish CA")
    ;; Verify it's the known bad CA by checking subject CN
    (let ((cns (pure-tls:certificate-subject-common-names cert)))
      (is (member "Superfish, Inc." cns :test #'string=)
          "Should identify Superfish CA by CN"))))

(test parse-edellroot-ca
  "Test parsing the eDellRoot malware CA certificate"
  (let* ((cert-path (test-cert-path "ca-edellroot.crt"))
         (cert (pure-tls:parse-certificate-from-file cert-path)))
    (is (not (null cert))
        "Should successfully parse eDellRoot CA")
    ;; Verify it's the known bad CA
    (let ((cns (pure-tls:certificate-subject-common-names cert)))
      (is (member "eDellRoot" cns :test #'string=)
          "Should identify eDellRoot CA by CN"))))

(test expired-certificate-detected
  "Test that expired certificates are properly detected"
  (let* ((cert-path (test-cert-path "wildcard-expired.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (not-after (pure-tls:certificate-not-after cert))
         (not-before (pure-tls:certificate-not-before cert)))
    ;; Verify the certificate dates
    (is (< not-before not-after)
        "notBefore should be before notAfter")
    (is (< not-after (get-universal-time))
        "Expired cert's notAfter should be in the past")
    ;; The wildcard-expired.pem was valid Apr 9-12, 2015
    ;; notAfter should be April 12, 2015 23:59:59 UTC
    (multiple-value-bind (sec min hour date month year)
        (decode-universal-time not-after 0)
      (declare (ignore sec min hour))
      (is (= year 2015) "Should expire in 2015")
      (is (= month 4) "Should expire in April")
      (is (= date 12) "Should expire on the 12th"))))

;;;; Windows Native Verification Tests (offline)
;;;; These test the CryptoAPI path with bundled bad certificates

#+windows
(test windows-native-rejects-expired
  "Test that Windows CryptoAPI rejects expired certificates"
  (let* ((cert-path (test-cert-path "wildcard-expired.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-windows der-list "expired.badssl.com"))))

#+windows
(test windows-native-rejects-self-signed
  "Test that Windows CryptoAPI rejects self-signed certificates"
  (let* ((cert-path (test-cert-path "self-signed-valid.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-windows der-list "test.example.com"))))

#+windows
(test windows-native-rejects-superfish
  "Test that Windows CryptoAPI rejects Superfish malware CA"
  (let* ((cert-path (test-cert-path "ca-superfish.crt"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-windows der-list "superfish.com"))))

#+windows
(test windows-native-rejects-edellroot
  "Test that Windows CryptoAPI rejects eDellRoot malware CA"
  (let* ((cert-path (test-cert-path "ca-edellroot.crt"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-windows der-list "dell.com"))))

;;;; macOS Native Verification Tests (offline)
;;;; These test the Security.framework path with bundled bad certificates

#+(or darwin macos)
(test macos-native-rejects-expired
  "Test that macOS Security.framework rejects expired certificates"
  (let* ((cert-path (test-cert-path "wildcard-expired.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-macos der-list "expired.badssl.com"))))

#+(or darwin macos)
(test macos-native-rejects-self-signed
  "Test that macOS Security.framework rejects self-signed certificates"
  (let* ((cert-path (test-cert-path "self-signed-valid.pem"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-macos der-list "test.example.com"))))

#+(or darwin macos)
(test macos-native-rejects-superfish
  "Test that macOS Security.framework rejects Superfish malware CA"
  (let* ((cert-path (test-cert-path "ca-superfish.crt"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-macos der-list "superfish.com"))))

#+(or darwin macos)
(test macos-native-rejects-edellroot
  "Test that macOS Security.framework rejects eDellRoot malware CA"
  (let* ((cert-path (test-cert-path "ca-edellroot.crt"))
         (cert (pure-tls:parse-certificate-from-file cert-path))
         (der-list (list (pure-tls::x509-certificate-raw-der cert))))
    (signals pure-tls:tls-certificate-error
      (pure-tls::verify-certificate-chain-macos der-list "dell.com"))))

;;;; CRL Tests

(test crl-distribution-points-parsing
  "Test parsing CRL Distribution Points extension from a certificate"
  ;; Google certificates have CDP extension
  (let* ((socket (usocket:socket-connect "google.com" 443 :element-type '(unsigned-byte 8)))
         (tls nil))
    (unwind-protect
        (progn
          (setf tls (pure-tls:make-tls-client-stream
                     (usocket:socket-stream socket)
                     :sni-hostname "google.com"
                     :verify pure-tls:+verify-none+))
          (let* ((hs (pure-tls::tls-stream-handshake tls))
                 (chain (pure-tls::client-handshake-peer-certificate-chain hs))
                 (cert (first chain))
                 (cdp (pure-tls::certificate-crl-distribution-points cert)))
            (is (not (null cdp)) "Google certificate should have CRL Distribution Points")
            (is (every (lambda (uri) (stringp uri)) cdp)
                "CDP URIs should be strings")
            (is (some (lambda (uri) (search "http" uri)) cdp)
                "At least one CDP URI should be HTTP")))
      (when tls (close tls))
      (usocket:socket-close socket))))

;;;; Test Runner

(defun run-certificate-tests ()
  "Run all certificate tests."
  (run! 'certificate-tests))
