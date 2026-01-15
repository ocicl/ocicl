;;; test/network-tests.lisp --- Live network validation tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Network tests for TLS 1.3 connections against major sites.

(in-package #:pure-tls/test)

(def-suite network-tests
  :description "Live TLS 1.3 connection tests")

(in-suite network-tests)

;;;; Connection Helper

(defun try-tls-connect (hostname &key (port 443) (verify pure-tls:+verify-required+) context)
  "Attempt TLS connection. Returns :success or an error keyword.
   On failure, prints error details to help with debugging."
  (let ((socket nil))
    (unwind-protect
        (handler-case
            (progn
              (setf socket (usocket:socket-connect hostname port
                                                   :element-type '(unsigned-byte 8)))
              (let ((tls (if context
                             (pure-tls:make-tls-client-stream
                              (usocket:socket-stream socket)
                              :hostname hostname :verify verify :context context)
                             (pure-tls:make-tls-client-stream
                              (usocket:socket-stream socket)
                              :hostname hostname :verify verify))))
                ;; TLS handshake succeeded - that's all we need to verify
                (close tls)
                :success))
          (pure-tls:tls-certificate-error (e)
            (format t "~&  [~A] cert-error: ~A~%" hostname e)
            :cert-error)
          (pure-tls:tls-verification-error (e)
            (format t "~&  [~A] verify-error: ~A~%" hostname e)
            :verify-error)
          (pure-tls:tls-handshake-error (e)
            (format t "~&  [~A] handshake-error: ~A~%" hostname e)
            :handshake-error)
          (pure-tls:tls-error (e)
            (format t "~&  [~A] tls-error: ~A~%" hostname e)
            :tls-error)
          (error (e)
            (format t "~&  [~A] other-error: ~A~%" hostname e)
            :other-error))
      (when socket (ignore-errors (usocket:socket-close socket))))))

;;;; TLS 1.3 Connection Tests (Major Sites)

(test connect-google
  "Connect to google.com"
  (is (eql (try-tls-connect "www.google.com") :success)))

(test connect-cloudflare
  "Connect to cloudflare.com"
  (is (eql (try-tls-connect "www.cloudflare.com") :success)))

(test connect-github
  "Connect to github.com"
  (is (eql (try-tls-connect "github.com") :success)))

(test connect-mozilla
  "Connect to mozilla.org"
  (is (eql (try-tls-connect "www.mozilla.org") :success)))

(test connect-amazon
  "Connect to amazon.com"
  (is (eql (try-tls-connect "www.amazon.com") :success)))

#+windows
(defun %make-empty-trust-context ()
  "Create a context that forces native Windows verification (no CA bundle)."
  (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-required+
                                        :auto-load-system-ca nil)))
    (setf (pure-tls::tls-context-trust-store ctx)
          (pure-tls::make-trust-store :certificates nil))
    ctx))

#+windows
(test connect-google-windows-native
  "Connect to google.com using Windows CryptoAPI verification"
  (let ((ctx (%make-empty-trust-context))
        (pure-tls:*use-windows-certificate-store* t))
    (is (eql (try-tls-connect "www.google.com" :context ctx) :success))))

;;;; CRL Tests (moved from certificate-tests - these require network access)

(test crl-parsing
  "Test parsing a CRL file"
  ;; Fetch and parse a real CRL from Google
  (let ((google-cdp-uri "http://c.pki.goog/wr2/oBFYYahzgVI.crl"))
    (let ((crl (pure-tls::fetch-crl google-cdp-uri :timeout 10)))
      (when crl  ; May fail if network unavailable
        (is (> (pure-tls::crl-version crl) 0) "CRL should have a version")
        (is (pure-tls::crl-issuer crl) "CRL should have an issuer")
        (is (pure-tls::crl-this-update crl) "CRL should have thisUpdate")
        (is (pure-tls::crl-valid-p crl) "CRL should be currently valid")
        (is (listp (pure-tls::crl-revoked-certificates crl))
            "Revoked certificates should be a list")))))

(test crl-cache
  "Test CRL caching functionality"
  (pure-tls::clear-crl-cache)
  ;; Cache a mock entry
  (let ((test-uri "http://test.example.com/test.crl"))
    ;; No entry initially
    (is (null (pure-tls::get-cached-crl test-uri))
        "Cache should be empty initially")
    ;; Test caching with real CRL fetch
    (let ((google-uri "http://c.pki.goog/wr2/oBFYYahzgVI.crl"))
      (pure-tls::clear-crl-cache)
      (let ((crl1 (pure-tls::fetch-crl google-uri :timeout 10)))
        (when crl1
          (let ((crl2 (pure-tls::fetch-crl google-uri :timeout 10)))
            (is (eq crl1 crl2) "Second fetch should return cached CRL")))))))

(test crl-revocation-check
  "Test certificate revocation checking"
  ;; Test with a real certificate - should be :valid or :unknown (not :revoked)
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
                 (issuer (second chain)))
            ;; Test with signature verification (requires issuer cert)
            (when issuer
              (let ((status (pure-tls::check-certificate-revocation
                             cert :issuer-cert issuer :timeout 10)))
                (is (member status '(:valid :unknown))
                    "Google certificate should not be revoked (with signature verification)")))
            ;; Test without signature verification (backward compatibility)
            (let ((status (pure-tls::check-certificate-revocation
                           cert :verify-signature nil :timeout 10)))
              (is (member status '(:valid :unknown))
                  "Google certificate should not be revoked (without signature verification)"))))
      (when tls (close tls))
      (usocket:socket-close socket))))

(test crl-signature-verification
  "Test CRL signature verification"
  ;; Fetch a CRL and verify its signature
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
                 (issuer (second chain)))
            (when (and issuer (pure-tls::certificate-crl-distribution-points cert))
              (let* ((cdp-uri (first (pure-tls::certificate-crl-distribution-points cert)))
                     (crl (pure-tls::fetch-crl cdp-uri :timeout 10)))
                (when crl
                  ;; Verify the CRL signature
                  (is (pure-tls::verify-crl-signature crl issuer)
                      "CRL signature should verify against issuer certificate")
                  ;; Verify CRL issuer matches
                  (is (pure-tls::crl-issuer-matches-p crl cert)
                      "CRL issuer should match certificate issuer"))))))
      (when tls (close tls))
      (usocket:socket-close socket))))

;;;; Test Runner

(defun run-network-tests ()
  "Run network validation tests (requires internet)."
  (format t "~&Running TLS 1.3 network tests...~%~%")
  (run! 'network-tests))
