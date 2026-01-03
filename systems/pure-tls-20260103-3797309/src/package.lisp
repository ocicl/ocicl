;;; package.lisp --- Package definitions for pure-tls
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cl-user)

(defpackage #:pure-tls
  (:use #:cl #:trivial-gray-streams)
  (:export
   ;; Stream creation
   #:make-tls-client-stream
   #:make-tls-server-stream
   #:with-tls-client-stream
   #:with-tls-server-stream

   ;; Context management
   #:make-tls-context
   #:tls-context-free
   #:with-tls-context
   #:*default-tls-context*

   ;; Stream class
   #:tls-stream
   #:tls-client-stream
   #:tls-server-stream

   ;; Stream accessors
   #:tls-peer-certificate
   #:tls-peer-certificate-chain
   #:tls-selected-alpn
   #:tls-cipher-suite
   #:tls-version
   #:tls-client-hostname
   #:tls-request-key-update

   ;; Certificate handling
   #:parse-certificate
   #:parse-certificate-from-file
   #:load-certificate-chain
   #:load-private-key
   #:certificate-subject-common-names
   #:certificate-fingerprint
   #:certificate-not-before
   #:certificate-not-after
   #:certificate-free
   #:verify-hostname

   ;; Crypto utilities
   #:random-bytes
   #:constant-time-equal
   #:zeroize
   #:with-zeroized-vector

   ;; Record padding (traffic analysis mitigation)
   #:*record-padding-policy*


   ;; Conditions
   #:tls-error
   #:tls-handshake-error
   #:tls-certificate-error
   #:tls-verification-error
   #:tls-alert-error
   #:tls-decode-error

   ;; Verification modes
   #:+verify-none+
   #:+verify-peer+
   #:+verify-required+

   ;; Alert codes
   #:+alert-close-notify+
   #:+alert-unexpected-message+
   #:+alert-bad-record-mac+
   #:+alert-record-overflow+
   #:+alert-handshake-failure+
   #:+alert-bad-certificate+
   #:+alert-certificate-revoked+
   #:+alert-certificate-expired+
   #:+alert-certificate-unknown+
   #:+alert-illegal-parameter+
   #:+alert-unknown-ca+
   #:+alert-decode-error+
   #:+alert-decrypt-error+
   #:+alert-protocol-version+
   #:+alert-insufficient-security+
   #:+alert-internal-error+
   #:+alert-user-canceled+
   #:+alert-missing-extension+
   #:+alert-unsupported-extension+
   #:+alert-unrecognized-name+

   ;; Cipher suites
   #:+tls-aes-128-gcm-sha256+
   #:+tls-aes-256-gcm-sha384+
   #:+tls-chacha20-poly1305-sha256+

   ;; Configuration
   #:*default-buffer-size*
   #:*default-verify-mode*

   ;; Session resumption
   #:*session-ticket-cache*
   #:*server-ticket-key*
   #:session-ticket-cache-clear

   ;; Platform-specific verification
   #:*use-windows-certificate-store*
   #+windows #:verify-certificate-chain-windows
   #:*use-macos-keychain*
   #+(or darwin macos) #:verify-certificate-chain-macos))
