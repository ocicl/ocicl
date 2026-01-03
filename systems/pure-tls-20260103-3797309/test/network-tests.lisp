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
  "Attempt TLS connection. Returns :success or an error keyword."
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
          (pure-tls:tls-certificate-error () :cert-error)
          (pure-tls:tls-verification-error () :verify-error)
          (pure-tls:tls-handshake-error () :handshake-error)
          (pure-tls:tls-error () :tls-error)
          (error () :other-error))
      (when socket (ignore-errors (usocket:socket-close socket))))))

;;;; TLS 1.3 Connection Tests (Major Sites)

(test connect-google
  "Connect to google.com"
  (is (eq (try-tls-connect "www.google.com") :success)))

(test connect-cloudflare
  "Connect to cloudflare.com"
  (is (eq (try-tls-connect "www.cloudflare.com") :success)))

(test connect-github
  "Connect to github.com"
  (is (eq (try-tls-connect "github.com") :success)))

(test connect-mozilla
  "Connect to mozilla.org"
  (is (eq (try-tls-connect "www.mozilla.org") :success)))

(test connect-amazon
  "Connect to amazon.com"
  (is (eq (try-tls-connect "www.amazon.com") :success)))

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
    (is (eq (try-tls-connect "www.google.com" :context ctx) :success))))

;;;; Test Runner

(defun run-network-tests ()
  "Run network validation tests (requires internet)."
  (format t "~&Running TLS 1.3 network tests...~%~%")
  (run! 'network-tests))
