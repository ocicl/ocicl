;;; context-tests.lisp --- Tests for request context (timeout/cancellation)
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package :pure-tls/test)

(def-suite context-tests
    :description "Request context timeout and cancellation tests")

(in-suite context-tests)

(test nil-context-handling
  "Test that NIL context works (backwards compatibility)"
  (finishes
    (pure-tls::check-tls-context nil)))

(test timeout-basic
  "Test basic timeout functionality with cl-cancel"
  (signals cl-cancel:deadline-exceeded
    (cl-cancel:with-timeout-context (ctx 0.1)
      (sleep 0.2)
      (cl-cancel:check-cancellation ctx))))

(test cancellation-basic
  "Test basic cancellation functionality"
  (multiple-value-bind (ctx cancel-fn)
      (cl-cancel:with-cancel (cl-cancel:background))
    (funcall cancel-fn)
    (signals cl-cancel:cancelled
      (cl-cancel:check-cancellation ctx))))

(test check-context-tls-error
  "Test that our check-context raises TLS-specific errors"
  (signals pure-tls:tls-deadline-exceeded
    (cl-cancel:with-timeout-context (ctx 0.1)
      (sleep 0.2)
      (pure-tls::check-tls-context ctx))))

(test check-context-cancellation
  "Test that cancellation raises tls-context-cancelled"
  (multiple-value-bind (ctx cancel-fn)
      (cl-cancel:with-cancel (cl-cancel:background))
    (funcall cancel-fn)
    (signals pure-tls:tls-context-cancelled
      (pure-tls::check-tls-context ctx))))

(test context-remaining-time
  "Test context-remaining-time helper"
  (cl-cancel:with-timeout-context (ctx 10)
    (let ((remaining (pure-tls::context-remaining-time ctx)))
      (is (and remaining (>= remaining 9) (<= remaining 10))))))

;; Note: Full integration tests (TLS handshake with timeout) require network access
;; and are in network-tests.lisp
