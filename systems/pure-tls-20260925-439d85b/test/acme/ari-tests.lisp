;;; test/acme/ari-tests.lisp --- ACME Renewal Information (RFC 9773) tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Exercises certificate identifier construction (against the RFC 9773
;;; Appendix A vector and an independently computed value for a repository
;;; test certificate), RFC 3339 timestamp parsing, the renewalInfo query
;;; over the stubbed transport, and the renewal decision rule.

(in-package #:pure-tls/acme/test)

(def-suite ari-tests
  :description "ACME Renewal Information (RFC 9773) support.")

(in-suite ari-tests)

;;;; ---------------------------------------------------------------------------
;;;; Certificate identifier
;;;; ---------------------------------------------------------------------------

(defun usb8 (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(test ari-cert-id-matches-rfc9773-appendix-a
  ;; AKI keyIdentifier 69:88:...:D4, serial 0x87654321 (high bit set, so the
  ;; DER content octets gain a leading zero) => the RFC's example identifier.
  (is (string= "aYhba4dGQEHhs3uEe6CuLN4ByNQ.AIdlQyE"
               (acme::ari-cert-id
                (usb8 #x69 #x88 #x5B #x6B #x87 #x46 #x40 #x41 #xE1 #xB3
                      #x7B #x84 #x7B #xA0 #xAE #x2C #xDE #x01 #xC8 #xD4)
                #x87654321))))

(test der-serial-content-octets
  ;; High bit clear: no sign padding.
  (is (equalp (usb8 #x7f) (acme::der-integer-content-octets #x7f)))
  ;; High bit set: leading zero octet.
  (is (equalp (usb8 #x00 #x80) (acme::der-integer-content-octets #x80)))
  ;; Zero encodes as a single zero octet.
  (is (equalp (usb8 #x00) (acme::der-integer-content-octets 0)))
  (signals acme:acme-error (acme::der-integer-content-octets -1)))

(test cert-id-from-parsed-certificate
  ;; Expected value computed independently (Python base64.urlsafe_b64encode)
  ;; from the certificate's AKI keyIdentifier and serial as printed by
  ;; openssl x509 -text.
  (let* ((path (asdf:system-relative-pathname
                "pure-tls" "test/certs/resumption-leaf.pem"))
         (leaf (first (pure-tls:load-certificate-chain (namestring path)))))
    (is (string= "eawKNEeQwkRcRcTgZoqIlr2gr1E.DUN5vCDK3YSefLKqy6dzRxK48B0"
                 (acme:certificate-ari-cert-id leaf)))))

(test cert-id-requires-aki
  ;; A certificate without an AKI extension has no ARI identity.
  (signals acme:acme-error
    (acme:certificate-ari-cert-id (pure-tls::make-x509-certificate))))

;;;; ---------------------------------------------------------------------------
;;;; RFC 3339 timestamps
;;;; ---------------------------------------------------------------------------

(test rfc3339-utc
  (is (= (encode-universal-time 0 0 4 13 5 2026 0)
         (acme:parse-rfc3339-time "2026-05-13T04:00:00Z"))))

(test rfc3339-numeric-offset
  ;; 04:00:00+02:00 is 02:00:00Z.
  (is (= (encode-universal-time 0 0 2 13 5 2026 0)
         (acme:parse-rfc3339-time "2026-05-13T04:00:00+02:00")))
  (is (= (encode-universal-time 0 30 9 13 5 2026 0)
         (acme:parse-rfc3339-time "2026-05-13T04:00:00-05:30"))))

(test rfc3339-fractional-seconds-truncate
  (is (= (acme:parse-rfc3339-time "2026-05-13T04:00:00Z")
         (acme:parse-rfc3339-time "2026-05-13T04:00:00.999Z"))))

(test rfc3339-lowercase-t-and-z
  (is (= (acme:parse-rfc3339-time "2026-05-13T04:00:00Z")
         (acme:parse-rfc3339-time "2026-05-13t04:00:00z"))))

(test rfc3339-rejects-malformed
  (dolist (bad '("" "not-a-time" "2026-05-13 04:00:00Z" "2026-05-13T04:00:00"
                 "2026-05-13T04:00:00." "2026-05-13T04:00:00+0200"
                 "2026-13-40T04:00:00Z"))
    (signals acme:acme-error (acme:parse-rfc3339-time bad))))

;;;; ---------------------------------------------------------------------------
;;;; renewalInfo query
;;;; ---------------------------------------------------------------------------

(defun make-ari-test-client ()
  (let ((client (make-test-client)))
    (setf (acme::acme-client-directory client)
          (append (acme::acme-client-directory client)
                  '((:renewal-info . "https://example.test/renewal-info"))))
    client))

(defparameter *window-json*
  "{\"suggestedWindow\":{\"start\":\"2026-05-13T04:00:00Z\",\"end\":\"2026-05-14T04:00:00Z\"}}")

(test renewal-info-returns-window
  (with-acme-stub ((list (list *window-json* 200 nil)))
    (multiple-value-bind (start end)
        (acme:client-renewal-info (make-ari-test-client) "aki.serial")
      (is (= start (encode-universal-time 0 0 4 13 5 2026 0)))
      (is (= end (encode-universal-time 0 0 4 14 5 2026 0)))
      ;; The certificate identifier lands as a path segment under the
      ;; directory's renewalInfo URL.
      (is (string= "https://example.test/renewal-info/aki.serial"
                   (getf (first *stub-requests*) :url))))))

(test renewal-info-nil-without-directory-entry
  (with-acme-stub (nil)
    (is (null (acme:client-renewal-info (make-test-client) "aki.serial")))
    ;; No endpoint means no request at all.
    (is (null *stub-requests*))))

(test renewal-info-nil-on-http-error
  (with-acme-stub ((list (list "{\"type\":\"urn:ietf:params:acme:error:malformed\"}"
                               404 nil)))
    (is (null (acme:client-renewal-info (make-ari-test-client) "aki.serial")))))

(test renewal-info-nil-on-malformed-body
  (with-acme-stub ((list (list "{\"suggestedWindow\":{\"start\":\"garbage\"}}" 200 nil)))
    (is (null (acme:client-renewal-info (make-ari-test-client) "aki.serial")))))

;;;; ---------------------------------------------------------------------------
;;;; Renewal decision
;;;; ---------------------------------------------------------------------------

(test renewal-due-follows-ari-window
  (let ((now (encode-universal-time 0 0 12 1 6 2026 0)))
    (is-false (acme:renewal-due-p 0 (+ now 100) now :window-start (+ now 60)))
    (is-true (acme:renewal-due-p (- now 100) (+ now (* 90 86400)) now
                                 :window-start now))))

(test renewal-due-at-a-third-of-lifetime-remaining
  (let* ((day 86400)
         (issued (encode-universal-time 0 0 0 1 6 2026 0)))
    (flet ((due-p (lifetime-days elapsed-days)
             (acme:renewal-due-p issued
                                 (+ issued (* lifetime-days day))
                                 (+ issued (* elapsed-days day)))))
      ;; 90-day certificate: due once 30 days remain -- the old fixed default.
      (is-false (due-p 90 59))
      (is-true (due-p 90 60))
      ;; 45-day certificate: due once 15 days remain.
      (is-false (due-p 45 29))
      (is-true (due-p 45 30))
      ;; 6-day short-lived certificate: due once 2 days remain, not daily.
      (is-false (due-p 6 3))
      (is-true (due-p 6 4)))))

;;;; ---------------------------------------------------------------------------
;;;; Runner
;;;; ---------------------------------------------------------------------------

(defun run-ari-tests ()
  "Run the ACME renewal information test suite. Returns T if all tests pass."
  (format t "~&=== Running ACME Renewal Information Tests ===~%~%")
  (run! 'ari-tests))
