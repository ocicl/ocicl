;;;; streams.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Stream cancellation monitoring for cl-cancel
;;;;
;;;; Provides utilities for automatically closing streams when cancellables
;;;; are cancelled, enabling Go-style immediate cancellation for blocking I/O.

(in-package #:cl-cancel)

(defun close-stream-on-cancel (stream &optional (cancellable *current-cancel-context*))
  "Spawns a thread that closes STREAM when CANCELLABLE is cancelled.
   Returns a cleanup function that MUST be called when the I/O operation completes.

   This provides Go-style immediate cancellation by forcibly closing the socket,
   interrupting any blocked I/O operations. Uses semaphores for efficient waiting
   without polling.

   STREAM: The stream to close when cancellable is cancelled
   CANCELLABLE: The cancellable to monitor (defaults to *current-cancel-context*)

   Returns: A cleanup function (lambda of no arguments) that must be called to:
            - Stop the monitor thread
            - Prevent resource leaks

   Only monitors cancellable contexts - non-cancellable contexts (including NIL
   and background) are skipped since they can never be done.

   Usage:
     (let ((cancel-monitor (close-stream-on-cancel stream)))
       (unwind-protect
            (do-io-operation stream)
         (funcall cancel-monitor)))

   Examples:

     ;; HTTP client
     (with-timeout-context (ctx 5)
       (let* ((socket (connect-to-server))
              (cancel-monitor (close-stream-on-cancel socket)))
         (unwind-protect
              (read-http-response socket)
           (funcall cancel-monitor))))

     ;; Database connection
     (with-deadline-context (ctx (+ (get-current-time) 30))
       (let* ((conn (connect-to-db))
              (cancel-monitor (close-stream-on-cancel conn)))
         (unwind-protect
              (execute-query conn query)
           (funcall cancel-monitor))))

     ;; File I/O with timeout
     (with-timeout-context (ctx 10)
       (with-open-file (stream path)
         (let ((cancel-monitor (close-stream-on-cancel stream)))
           (unwind-protect
                (process-large-file stream)
             (funcall cancel-monitor)))))"
  ;; Only monitor if we have a cancellable context
  (if (typep cancellable 'cancellable-context)
      (let* ((done nil)
             (done-lock (bordeaux-threads:make-lock))
             (monitor-thread
               (bordeaux-threads:make-thread
                (lambda ()
                  ;; Wait until cancellable is done (no polling!)
                  (let ((err (wait-until-done cancellable)))
                    ;; Only close stream if cancellable actually became done (err is non-NIL)
                    (when err
                      (bordeaux-threads:with-lock-held (done-lock)
                        (unless done
                          ;; Cancellable was cancelled - abort the stream to interrupt blocked I/O
                          (ignore-errors (close stream :abort t)))))))
                :name "cancel-stream-monitor")))
        ;; Return cleanup function
        (lambda ()
          (bordeaux-threads:with-lock-held (done-lock)
            (setf done t))
          (bordeaux-threads:join-thread monitor-thread :timeout 0.1)))
      ;; No monitor needed for non-cancellable contexts
      (lambda () nil)))
