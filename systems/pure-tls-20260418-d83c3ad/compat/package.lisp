;;; package.lisp --- cl+ssl compatibility package
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Provides a drop-in replacement for cl+ssl.

(in-package #:cl-user)

(defpackage #:cl+ssl
  (:use #:cl)
  (:export
   ;; Stream creation
   #:make-ssl-client-stream
   #:make-ssl-server-stream

   ;; Context management
   #:make-context
   #:ssl-ctx-free
   #:with-global-context
   #:call-with-global-context
   #:*ssl-global-context*

   ;; Stream accessors
   #:ssl-stream-x509-certificate
   #:get-selected-alpn-protocol
   #:stream-fd

   ;; Certificate handling
   #:decode-certificate
   #:decode-certificate-from-file
   #:x509-free
   #:certificate-not-after-time
   #:certificate-not-before-time
   #:certificate-subject-common-names
   #:certificate-fingerprint
   #:verify-hostname

   ;; Utilities
   #:reload
   #:ensure-initialized
   #:random-bytes
   #:use-certificate-chain-file
   #:ssl-load-global-verify-locations
   #:ssl-set-global-default-verify-paths

   ;; PEM password
   #:with-pem-password

   ;; Conditions
   #:ssl-error
   #:ssl-error-verify
   #:ssl-error-initialize
   #:ssl-error-stream
   #:ssl-error-code

   ;; Deprecated
   #:ssl-check-verify-p

   ;; Constants
   #:+ssl-verify-none+
   #:+ssl-verify-peer+
   #:+ssl-verify-fail-if-no-peer-cert+
   #:+ssl-verify-client-once+

   #:+ssl-op-no-sslv2+
   #:+ssl-op-no-sslv3+
   #:+ssl-op-no-tlsv1+
   #:+ssl-op-no-tlsv1-1+
   #:+ssl-op-no-tlsv1-2+

   #:+ssl-sess-cache-off+
   #:+ssl-sess-cache-client+
   #:+ssl-sess-cache-server+
   #:+ssl-sess-cache-both+
   #:+ssl-sess-cache-no-auto-clear+
   #:+ssl-sess-cache-no-internal-lookup+
   #:+ssl-sess-cache-no-internal-store+
   #:+ssl-sess-cache-no-internal+

   ;; Defaults
   #:*default-cipher-list*
   #:*default-buffer-size*
   #:*make-ssl-client-stream-verify-default*
   #:*default-unwrap-stream-p*))
