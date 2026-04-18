;;; test/record-tests.lisp --- TLS record layer tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Tests for the TLS 1.3 record layer including framing,
;;; encryption, and padding.

(in-package #:pure-tls/test)

(def-suite record-tests
  :description "Tests for TLS record layer")

(in-suite record-tests)

;;;; Note: hex-to-bytes and bytes-equal are defined in crypto-tests.lisp

;;;; Record Header Tests

(test record-header-format
  "Test TLS record header format"
  ;; TLS 1.3 record header: content_type (1) + legacy_version (2) + length (2)
  (let ((header (make-array 5 :element-type '(unsigned-byte 8)
                              :initial-contents '(23 3 3 0 5))))
    ;; Content type 23 = application_data
    (is (= (aref header 0) 23)
        "Content type should be application_data (23)")
    ;; Legacy version 0x0303 = TLS 1.2
    (is (= (aref header 1) 3) "Legacy version major should be 3")
    (is (= (aref header 2) 3) "Legacy version minor should be 3")
    ;; Length
    (is (= (logior (ash (aref header 3) 8) (aref header 4)) 5)
        "Length should be 5")))

;;;; Content Type Tests

(test content-type-constants
  "Verify content type constants"
  (is (= pure-tls::+content-type-change-cipher-spec+ 20))
  (is (= pure-tls::+content-type-alert+ 21))
  (is (= pure-tls::+content-type-handshake+ 22))
  (is (= pure-tls::+content-type-application-data+ 23)))

;;;; Record Size Limits

(test record-size-constants
  "Verify record size limit constants"
  (is (= pure-tls::+max-record-size+ 16384)
      "Max record size should be 2^14 (16384)")
  (is (= pure-tls::+max-record-size-with-padding+ 16640)
      "Max encrypted record should be 2^14 + 256 (16640) per RFC 8446 §5.4"))

;;;; AEAD Nonce Construction (RFC 8446 Section 5.3)

(test aead-nonce-construction
  "Test AEAD nonce XOR construction"
  ;; per_record_nonce = sequence_number XOR static_iv
  (let ((static-iv (hex-to-bytes "cf782b88dd83549aadf1e984"))
        (sequence-0 (hex-to-bytes "000000000000000000000000"))
        (sequence-1 (hex-to-bytes "000000000000000000000001")))
    ;; Sequence 0: nonce should equal static IV
    (let ((nonce-0 (map '(vector (unsigned-byte 8)) #'logxor static-iv sequence-0)))
      (is (bytes-equal nonce-0 static-iv)
          "Nonce for sequence 0 should equal static IV"))
    ;; Sequence 1: last byte should be XORed
    (let ((nonce-1 (map '(vector (unsigned-byte 8)) #'logxor static-iv sequence-1)))
      (is (= (aref nonce-1 11) (logxor (aref static-iv 11) 1))
          "Nonce for sequence 1 should have last byte XORed with 1"))))

;;;; TLS 1.3 Inner Plaintext Format

(test inner-plaintext-format
  "Test TLS 1.3 inner plaintext structure"
  ;; TLSInnerPlaintext = content + ContentType + zeros (padding)
  (let* ((content (flexi-streams:string-to-octets "Hello" :external-format :utf-8))
         (content-type 23)  ; application_data
         (padding-length 3)
         (inner-length (+ (length content) 1 padding-length))
         (inner (make-array inner-length :element-type '(unsigned-byte 8)
                                         :initial-element 0)))
    ;; Fill content
    (replace inner content)
    ;; Set content type byte
    (setf (aref inner (length content)) content-type)
    ;; Padding is zeros (already initialized)

    (is (= (length inner) 9)
        "Inner plaintext length should be content + type + padding")
    (is (= (aref inner 5) 23)
        "Content type byte should be at position after content")
    (is (zerop (aref inner 8))
        "Padding bytes should be zero")))

;;;; Record Padding Policy Tests

(test record-padding-policy-nil
  "Test no padding policy"
  (let ((pure-tls:*record-padding-policy* nil))
    ;; With nil policy, no extra padding should be added
    (is (null pure-tls:*record-padding-policy*)
        "Nil padding policy should be nil")))

(test record-padding-policy-block
  "Test block padding policy"
  (let ((pure-tls:*record-padding-policy* :block-256))
    (is (eql pure-tls:*record-padding-policy* :block-256)
        "Block-256 padding policy should be set")))

;;;; Alert Record Tests

(test alert-level-constants
  "Verify alert level constants"
  (is (= pure-tls::+alert-level-warning+ 1))
  (is (= pure-tls::+alert-level-fatal+ 2)))

(test alert-description-constants
  "Verify important alert description constants"
  (is (zerop pure-tls:+alert-close-notify+))
  (is (= pure-tls:+alert-unexpected-message+ 10))
  (is (= pure-tls:+alert-bad-record-mac+ 20))
  (is (= pure-tls:+alert-handshake-failure+ 40))
  (is (= pure-tls:+alert-bad-certificate+ 42))
  (is (= pure-tls:+alert-certificate-expired+ 45))
  (is (= pure-tls:+alert-unknown-ca+ 48))
  (is (= pure-tls:+alert-decode-error+ 50)))

;;;; Handshake Record Tests

(test handshake-type-constants
  "Verify handshake message type constants"
  (is (= pure-tls::+handshake-client-hello+ 1))
  (is (= pure-tls::+handshake-server-hello+ 2))
  (is (= pure-tls::+handshake-new-session-ticket+ 4))
  (is (= pure-tls::+handshake-encrypted-extensions+ 8))
  (is (= pure-tls::+handshake-certificate+ 11))
  (is (= pure-tls::+handshake-certificate-request+ 13))
  (is (= pure-tls::+handshake-certificate-verify+ 15))
  (is (= pure-tls::+handshake-finished+ 20))
  (is (= pure-tls::+handshake-key-update+ 24)))

;;;; Test Runner

(defun run-record-tests ()
  "Run all record layer tests."
  (run! 'record-tests))
