;;; package.lisp --- Package definition for pure-tls/acme
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; ACME client for automatic certificate management (Let's Encrypt).

(in-package #:cl-user)

(defpackage #:pure-tls/acme
  (:use #:cl)
  (:nicknames #:acme)
  (:export
   ;; Configuration
   #:*staging-url*
   #:*production-url*
   #:*acme-debug*

   ;; Certificate store
   #:cert-store
   #:make-cert-store
   #:cert-store-base-path
   #:store-domain-cert-path
   #:store-domain-key-path
   #:store-has-certificate-p
   #:store-certificate-expires-soon-p

   ;; ACME client
   #:acme-client
   #:make-acme-client
   #:acme-client-store
   #:client-init
   #:client-register-account
   #:client-new-order
   #:client-get-authorization
   #:client-respond-challenge
   #:client-poll-status
   #:client-finalize-order
   #:client-download-certificate
   #:client-compute-key-authorization

   ;; Hunchentoot integration
   #:acme-acceptor
   #:make-acme-acceptor

   ;; CSR generation (for advanced use)
   #:generate-csr
   #:generate-domain-key

   ;; Conditions
   #:acme-error
   #:acme-challenge-error
   #:acme-order-error
   #:acme-certificate-error))
