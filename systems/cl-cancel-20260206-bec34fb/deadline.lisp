;;;; deadline.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-cancel)

;;; Precise time utilities

(defun get-current-time ()
  "Get current time as fractional seconds since epoch.
   Returns a rational number with nanosecond precision."
  (multiple-value-bind (universal-time nanoseconds)
      (org.shirakumo.precise-time:get-precise-time)
    (+ universal-time (/ nanoseconds 1000000000))))

(defun sleep-fractional (seconds)
  "Sleep for SECONDS, which can be fractional.
   Handles both integer and fractional seconds precisely."
  (if (integerp seconds)
      (sleep seconds)
      (let ((full-seconds (floor seconds))
            (remaining (- seconds (floor seconds))))
        (when (plusp full-seconds)
          (sleep full-seconds))
        (when (plusp remaining)
          ;; Sleep remaining fraction (converted to nanoseconds for precision)
          (sleep (coerce remaining 'double-float))))))

;;; Centralized timer system
;;;
;;; Instead of spawning a thread per deadline, we use a single timer thread
;;; that manages all deadlines via a min-heap. This scales to thousands of
;;; concurrent cancellables with minimal overhead.

(defstruct deadline-entry
  "Entry in the deadline heap"
  (deadline 0 :type rational)
  (cancellable nil)
  (cancel-func nil))

;;; Min-heap implementation for deadline management

(defstruct heap
  "Simple min-heap based on deadline time"
  (data (make-array 16 :adjustable t :fill-pointer 0)))

(defun heap-parent (i)
  "Return parent index for heap node at index I"
  (floor (1- i) 2))

(defun heap-left (i)
  "Return left child index for heap node at index I"
  (1+ (* 2 i)))

(defun heap-right (i)
  "Return right child index for heap node at index I"
  (+ 2 (* 2 i)))

(defun heap-swap (heap i j)
  "Swap heap elements at indices I and J"
  (let ((data (heap-data heap)))
    (rotatef (aref data i) (aref data j))))

(defun heap-bubble-up (heap i)
  "Move heap element at index I upward to maintain min-heap property"
  (loop while (and (> i 0)
                   (< (deadline-entry-deadline (aref (heap-data heap) i))
                      (deadline-entry-deadline (aref (heap-data heap) (heap-parent i)))))
        do (heap-swap heap i (heap-parent i))
           (setf i (heap-parent i))))

(defun heap-bubble-down (heap i)
  "Move heap element at index I downward to maintain min-heap property"
  (let ((data (heap-data heap))
        (size (length (heap-data heap))))
    (loop
      (let* ((left (heap-left i))
             (right (heap-right i))
             (smallest i))
        (when (and (< left size)
                   (< (deadline-entry-deadline (aref data left))
                      (deadline-entry-deadline (aref data smallest))))
          (setf smallest left))
        (when (and (< right size)
                   (< (deadline-entry-deadline (aref data right))
                      (deadline-entry-deadline (aref data smallest))))
          (setf smallest right))
        (if (= smallest i)
            (return)
            (progn
              (heap-swap heap i smallest)
              (setf i smallest)))))))

(defun heap-insert (heap entry)
  "Insert deadline entry into heap"
  (vector-push-extend entry (heap-data heap))
  (heap-bubble-up heap (1- (length (heap-data heap)))))

(defun heap-peek (heap)
  "Return earliest deadline entry without removing it"
  (when (plusp (length (heap-data heap)))
    (aref (heap-data heap) 0)))

(defun heap-pop (heap)
  "Remove and return earliest deadline entry"
  (let ((data (heap-data heap)))
    (when (plusp (length data))
      (let ((result (aref data 0)))
        (setf (aref data 0) (aref data (1- (length data))))
        (decf (fill-pointer data))
        (when (plusp (length data))
          (heap-bubble-down heap 0))
        result))))

(defun heap-empty-p (heap)
  "Return T if heap is empty"
  (zerop (length (heap-data heap))))

;;; Global timer infrastructure
;;; Lazily initialized to avoid thread subsystem initialization during fasl load

(defvar *timer-lock* nil
  "Lock protecting timer data structures (lazily initialized)")

(defvar *timer-condition* nil
  "Condition variable for waking timer thread (lazily initialized)")

(defvar *deadline-heap* nil
  "Min-heap of pending deadlines (lazily initialized)")

(defvar *timer-thread* nil
  "Background thread processing deadlines")

(defvar *timer-shutdown* nil
  "Flag to shutdown timer thread")

(defvar *timer-initialized* nil
  "Atomic integer for timer infrastructure initialization state.
   Lazily created on first use via atomics:cas (portable across all implementations).
   Values: 0 = uninitialized, 1 = initializing, 2 = initialized")

(defun ensure-atomic-initialized ()
  "Ensure *timer-initialized* atomic-integer is created (portable via atomics library)"
  ;; Try to atomically claim creation if not yet initialized
  (when (and (null *timer-initialized*)
             (atomics:cas *timer-initialized* nil :creating))
    ;; We won - create the atomic-integer
    (let ((aint (bt2:make-atomic-integer :value 0)))
      (setf *timer-initialized* aint)))
  ;; Wait if someone else is creating
  (loop while (eql *timer-initialized* :creating)
        do (sleep 0.001)))

(defun ensure-timer-initialized ()
  "Lazily initialize timer infrastructure on first use.
   Avoids creating locks/condition-variables/atomic-integers at top-level,
   which can trigger thread subsystem initialization and cause fasl load hangs.
   Uses atomics:cas and bt2:atomic-integer for portable lock-free initialization."
  ;; First ensure atomic-integer is created (race-safe)
  (ensure-atomic-initialized)

  ;; Now proceed with timer initialization using the atomic-integer
  (unless (= (bt2:atomic-integer-value *timer-initialized*) 2)
    ;; Try to atomically transition from 0 (uninitialized) to 1 (initializing)
    (when (bt2:atomic-integer-compare-and-swap *timer-initialized* 0 1)
      (unwind-protect
           (progn
             (setf *timer-lock* (bt2:make-lock :name "deadline-timer-lock"))
             (setf *timer-condition* (bt2:make-condition-variable :name "deadline-timer-condition"))
             (setf *deadline-heap* (make-heap))
             ;; Atomically transition to 2 (initialized)
             (bt2:atomic-integer-compare-and-swap *timer-initialized* 1 2))
        ;; If initialization fails, reset to 0 so another thread can retry
        (unless (= (bt2:atomic-integer-value *timer-initialized*) 2)
          (bt2:atomic-integer-compare-and-swap *timer-initialized* 1 0))))
    ;; Wait for initialization to complete if another thread is doing it
    (loop while (= (bt2:atomic-integer-value *timer-initialized*) 1)
          do (sleep 0.001))))

(defun timer-thread-loop ()
  "Main loop for centralized timer thread"
  (loop
    ;; Check shutdown under lock and break if needed
    (bt2:with-lock-held (*timer-lock*)
      (when *timer-shutdown*
        (return)))

    ;; Wait for next deadline or wake-up signal
    (let ((expired-entries nil))
      (bt2:with-lock-held (*timer-lock*)
        (let ((next-entry (heap-peek *deadline-heap*)))
          (if next-entry
              ;; Sleep until next deadline OR until woken by new shorter deadline
              (let ((sleep-time (- (deadline-entry-deadline next-entry) (get-current-time))))
                (when (plusp sleep-time)
                  ;; Sleep with timeout - can be woken early by condition-notify or shutdown
                  (bt2:condition-wait *timer-condition* *timer-lock*
                                     :timeout (float sleep-time 1.0d0))))
              ;; No deadlines - wait with timeout so we check shutdown periodically
              (bt2:condition-wait *timer-condition* *timer-lock* :timeout 1.0)))

        ;; Collect all expired deadlines while holding lock
        ;; Don't call any cancellable methods under lock to avoid lock-order inversion
        (loop with now = (get-current-time)
              for entry = (heap-peek *deadline-heap*)
              while (and entry (<= (deadline-entry-deadline entry) now))
              do (push (heap-pop *deadline-heap*) expired-entries)))

      ;; Process expired deadlines OUTSIDE the lock to avoid deadlock
      (dolist (entry expired-entries)
        (let ((cancel-func (deadline-entry-cancel-func entry))
              (ctx (deadline-entry-cancellable entry)))
          ;; Check cancelled-p outside lock
          (when (and cancel-func (not (cancelled-p ctx)))
            ;; Spawn thread to call cancel function (prevents blocking timer)
            (bt2:make-thread
             (lambda ()
               (funcall cancel-func
                        (make-condition 'deadline-exceeded :cancellable ctx)))
             :name "cancellable-deadline-cancel")))))))

(defun ensure-timer-thread ()
  "Start timer thread if not already running"
  (ensure-timer-initialized)
  (bt2:with-lock-held (*timer-lock*)
    (unless (and *timer-thread* (bt2:thread-alive-p *timer-thread*))
      (setf *timer-shutdown* nil)
      (setf *timer-thread*
            ;; Use :initial-bindings to make it a background thread that won't block exit
            (bt2:make-thread #'timer-thread-loop
                           :name "cl-cancel-deadline-timer"
                           :initial-bindings `((*standard-output* . ,*standard-output*)
                                               (*error-output* . ,*error-output*)))))))

(defun shutdown-timer-thread ()
  "Shutdown the timer thread gracefully"
  (when *timer-lock*  ; Only shutdown if initialized
    (bt2:with-lock-held (*timer-lock*)
      (setf *timer-shutdown* t)
      ;; Wake up timer thread so it can see shutdown flag
      (bt2:condition-notify *timer-condition*))
    ;; Wait a bit for thread to exit (poll with timeout)
    (loop repeat 20  ; 2 seconds worth of 100ms sleeps
          while (and *timer-thread* (bt2:thread-alive-p *timer-thread*))
          do (sleep 0.1))))

(defun add-deadline-to-timer (cancellable deadline cancel-func)
  "Add a deadline to the centralized timer system"
  (ensure-timer-initialized)
  (ensure-timer-thread)
  (bt2:with-lock-held (*timer-lock*)
    (let ((was-soonest (or (heap-empty-p *deadline-heap*)
                           (< deadline (deadline-entry-deadline (heap-peek *deadline-heap*))))))
      (heap-insert *deadline-heap*
                   (make-deadline-entry :deadline deadline
                                       :cancellable cancellable
                                       :cancel-func cancel-func))
      ;; Wake timer thread if this is now the soonest deadline
      (when was-soonest
        (bt2:condition-notify *timer-condition*)))))

;;; Deadline cancellable

(defclass deadline-context (cancellable-context)
  ((deadline :initarg :deadline
             :reader cancellable-deadline
             :documentation
             "Fractional seconds since epoch when this cancellable expires
             (rational number with nanosecond precision)"))
  (:documentation "A cancellable with a deadline"))

(defmethod deadline ((cancellable deadline-context))
  (cancellable-deadline cancellable))

(defmethod done-p ((cancellable deadline-context))
  (or (call-next-method)
      (>= (get-current-time) (cancellable-deadline cancellable))))

(defmethod err ((cancellable deadline-context))
  "Return deadline-exceeded error eagerly if deadline has passed, maintaining
the invariant that done-p true implies err is non-nil.

Note: This method creates a fresh condition object on each call after the
deadline passes but before cancel is called. Condition identity is not
guaranteed - use TYPEP/TYPECASE to check error types, not EQ for identity."
  ;; Check if we have a stored error from cancel
  (let ((stored-error (call-next-method)))
    (cond
      ;; If we have a stored error, use it
      (stored-error stored-error)
      ;; If deadline has passed but no stored error yet, create deadline-exceeded
      ((>= (get-current-time) (cancellable-deadline cancellable))
       (make-condition 'deadline-exceeded :cancellable cancellable))
      ;; Otherwise, no error yet
      (t nil))))

(defun with-deadline (parent deadline-time)
  "Returns a copy of parent with the deadline adjusted to be no later than
DEADLINE-TIME (fractional seconds since epoch). If the parent's deadline is already earlier,
with-deadline has no effect.

Returns (values cancellable cancel-function)
The cancel function is a lambda accepting an optional error parameter."
  (let* ((parent-deadline (deadline parent))
         (actual-deadline (if parent-deadline
                              (min deadline-time parent-deadline)
                              deadline-time)))
    (multiple-value-bind (ctx cancel-func)
        (with-cancel parent)
      (change-class ctx 'deadline-context :deadline actual-deadline)

      ;; Register with centralized timer if cancellable is not already cancelled and deadline hasn't passed
      (let ((seconds-until-deadline (- actual-deadline (get-current-time))))
        (cond
          ;; Deadline already passed - cancel immediately
          ((<= seconds-until-deadline 0)
           (unless (cancelled-p ctx)
             (cancel ctx (make-condition 'deadline-exceeded :cancellable ctx))))

          ;; Cancellable already cancelled (parent was done) - don't register timer
          ((cancelled-p ctx)
           ;; Already cancelled by with-cancel, no timer needed
           nil)

          ;; Register deadline with centralized timer system
          (t
           (add-deadline-to-timer ctx actual-deadline cancel-func))))

      (values ctx cancel-func))))

(defun with-timeout (parent timeout-seconds)
  "Returns with-deadline(parent, (+ (get-current-time) timeout-seconds)).
TIMEOUT-SECONDS can be fractional (e.g., 0.5 for 500ms) for sub-second precision.

Canceling this cancellable releases resources associated with it, so code should
call cancel as soon as the operations running in this cancellable complete.

Returns (values cancellable cancel-function)
The cancel function is a lambda accepting an optional error parameter."
  (with-deadline parent (+ (get-current-time) timeout-seconds)))

(defmacro with-deadline-context ((var deadline-time &optional (parent '*current-cancel-context*)) &body body)
  "Convenience macro to execute BODY with a deadline context.
Automatically cancels the context when done.
DEADLINE-TIME is fractional seconds since epoch (supports sub-second precision).

Example:
  (with-deadline-context (ctx (+ (get-current-time) 60) (background))
    (process-request))  ; ctx automatically cancelled on exit"
  (let ((cancel (gensym "CANCEL")))
    `(multiple-value-bind (,var ,cancel)
         (with-deadline (ensure-cancellable ,parent) ,deadline-time)
       (unwind-protect
            (with-cancellable (,var ,var)
              ,@body)
         (funcall ,cancel)))))

(defmacro with-timeout-context ((var timeout-seconds &optional (parent '*current-cancel-context*)) &body body)
  "Convenience macro to execute BODY with a timeout context.
Automatically cancels the context when done.
TIMEOUT-SECONDS can be fractional (e.g., 0.5 for 500ms) for sub-second precision.

Example:
  (with-timeout-context (ctx 30)
    (fetch-data))  ; ctx automatically cancelled on exit
  (with-timeout-context (ctx 0.5)  ; 500ms timeout
    (quick-operation))"
  (let ((cancel (gensym "CANCEL")))
    `(multiple-value-bind (,var ,cancel)
         (with-timeout (ensure-cancellable ,parent) ,timeout-seconds)
       (unwind-protect
            (with-cancellable (,var ,var)
              ,@body)
         (funcall ,cancel)))))
