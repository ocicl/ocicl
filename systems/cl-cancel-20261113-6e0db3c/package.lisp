;;;; package.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(defpackage #:cl-cancel
  (:documentation "Cancellation propagation library for Common Lisp with deadlines and timeouts")
  (:use #:cl)
  (:export
   ;; Core types
   #:cancellable
   #:cancellable-context
   #:background-context
   #:deadline-context

   ;; Core protocol
   #:done-p
   #:cancelled-p
   #:deadline
   #:err

   ;; Cancellable creation
   #:background
   #:with-cancel
   #:with-timeout
   #:with-deadline

   ;; Cancellation
   #:cancel

   ;; Conditions
   #:cancelled
   #:deadline-exceeded
   #:cancellation-error

   ;; Utilities
   #:with-cancellable
   #:with-cancel-context
   #:with-timeout-context
   #:with-deadline-context
   #:check-cancellation
   #:wait-until-done
   #:call-with-cancellable
   #:ensure-cancellable
   #:*current-cancel-context*
   #:get-current-time
   #:sleep-fractional

   ;; Stream utilities
   #:close-stream-on-cancel

   ;; Timer management (for advanced use)
   #:shutdown-timer-thread))
