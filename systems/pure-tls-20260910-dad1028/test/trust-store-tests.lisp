;;; test/trust-store-tests.lisp --- Trust store auto-load regression tests
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Regression tests for system trust-store auto-loading in MAKE-TLS-CONTEXT.
;;;
;;; Bug: MAKE-TLS-CONTEXT auto-loaded the system CA store only for
;;; +VERIFY-REQUIRED+, leaving a +VERIFY-PEER+ context with an empty trust
;;; store.  +VERIFY-PEER+ still verifies a presented certificate (servers
;;; always present one), so such a context failed every chain with UNKNOWN-CA.
;;; This bit the cl+ssl compatibility layer, whose default context is
;;; +VERIFY-PEER+ -- e.g. drakma-based HTTPS (ocicl's self-update path) failed
;;; on Linux with "No trusted root certificates available for verification".
;;;
;;; These tests pin the trust source with SSL_CERT_FILE (a fixture bundle) so
;;; they do not depend on the host's real system trust store.

(in-package #:pure-tls/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(def-suite trust-store-tests
  :description "System trust-store auto-loading in make-tls-context")

(in-suite trust-store-tests)

(defmacro with-ssl-cert-file ((path) &body body)
  "Run BODY with the SSL_CERT_FILE environment variable bound to PATH,
restoring the previous value afterwards."
  (let ((old (gensym)) (p (gensym)))
    `(let ((,old (uiop:getenv "SSL_CERT_FILE"))
           (,p (namestring ,path)))
       (unwind-protect
            (progn (sb-posix:setenv "SSL_CERT_FILE" ,p 1) ,@body)
         (if ,old
             (sb-posix:setenv "SSL_CERT_FILE" ,old 1)
             (sb-posix:unsetenv "SSL_CERT_FILE"))))))

(defun context-root-count (ctx)
  "Number of trusted roots loaded into CTX (0 when the trust store is NIL)."
  (let ((ts (pure-tls::tls-context-trust-store ctx)))
    (if ts (length (pure-tls::trust-store-certificates ts)) 0)))

(test verify-peer-auto-loads-system-roots
  "+verify-peer+ must auto-load the system trust store (UNKNOWN-CA regression)."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-peer+)))
      (is (plusp (context-root-count ctx))
          "+verify-peer+ context has an empty trust store; a presented ~
           certificate would fail with UNKNOWN-CA"))))

(test verify-required-auto-loads-system-roots
  "+verify-required+ still auto-loads the system trust store."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-required+)))
      (is (plusp (context-root-count ctx))))))

(test verify-none-does-not-auto-load
  "+verify-none+ must NOT auto-load a trust store."
  (with-ssl-cert-file ((test-cert-path "self-signed-valid.pem"))
    (let ((ctx (pure-tls:make-tls-context :verify-mode pure-tls:+verify-none+)))
      (is (zerop (context-root-count ctx))))))

(test explicit-ca-file-overrides-auto-load
  "An explicit :ca-file is honored regardless of verify mode."
  (let ((ctx (pure-tls:make-tls-context
              :verify-mode pure-tls:+verify-peer+
              :ca-file (namestring (test-cert-path "self-signed-valid.pem")))))
    (is (plusp (context-root-count ctx)))))
