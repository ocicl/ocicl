;;; context-support.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Request context support for timeouts and cancellation using cl-cancel.

(in-package :pure-tls)

;;; Conditions

(define-condition tls-context-cancelled (tls-error)
  ((context :initarg :context :reader context-cancelled-context))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "TLS operation cancelled via request context")))
  (:documentation "Signaled when a TLS operation is cancelled via its request context."))

(define-condition tls-deadline-exceeded (tls-error)
  ((context :initarg :context :reader deadline-exceeded-context)
   (deadline :initarg :deadline :reader deadline-exceeded-deadline))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "TLS operation exceeded deadline")))
  (:documentation "Signaled when a TLS operation exceeds its deadline."))

;;; Helper functions

(defun check-tls-context (&optional (ctx cl-cancel:*current-cancel-context*))
  "Check if context CTX is cancelled or past its deadline.
   Signals appropriate TLS error if so. Returns NIL if context is still valid or is NIL.
   Uses *current-cancel-context* by default for automatic propagation."
  (when ctx
    (handler-case
        (cl-cancel:check-cancellation ctx)
      (cl-cancel:deadline-exceeded ()
        (error 'tls-deadline-exceeded
               :context ctx
               :deadline (cl-cancel:deadline ctx)))
      (cl-cancel:cancelled ()
        (error 'tls-context-cancelled
               :context ctx))))
  nil)

(defun context-remaining-time (ctx)
  "Return the remaining time in seconds for context CTX, or NIL if no deadline.
   Returns 0 if deadline is already exceeded."
  (when ctx
    (let ((deadline (cl-cancel:deadline ctx)))
      (when deadline
        (max 0 (- deadline (cl-cancel:get-current-time)))))))

(defun effective-timeout (&optional (default 10))
  "Return effective timeout in seconds based on current context.
   Uses context remaining time if available, otherwise DEFAULT.
   Caps at 30 seconds to avoid excessive waits."
  (let* ((ctx cl-cancel:*current-cancel-context*)
         (remaining (when ctx (context-remaining-time ctx))))
    (cond
      ((and remaining (plusp remaining)) (min remaining 30))
      (remaining 0)  ; Deadline already exceeded
      (t default))))

(defmacro with-optional-timeout ((var timeout-seconds) &body body)
  "Execute BODY with an optional timeout context bound to VAR.
   If TIMEOUT-SECONDS is NIL, VAR is bound to NIL (no timeout).
   Otherwise, creates a context with the specified deadline."
  (let ((timeout-sym (gensym "TIMEOUT-")))
    `(let ((,timeout-sym ,timeout-seconds))
       (if ,timeout-sym
           (cl-cancel:with-timeout-context (,var ,timeout-sym)
             ,@body)
           (let ((,var nil))
             ,@body)))))

;;; Close-on-cancel monitoring for immediate cancellation

(defun setup-close-on-cancel (context socket)
  "Set up automatic closure of SOCKET when CONTEXT is cancelled or deadline exceeded.
   Returns a cleanup function that must be called when the socket operation completes.
   This enables immediate interruption of blocking I/O operations."
  (if context
      (cl-cancel:close-stream-on-cancel socket context)
      (lambda () nil)))
