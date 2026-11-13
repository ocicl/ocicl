;;;; tests/cancellable-tests.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-cancel/tests)

(in-suite :cl-cancel)

;;; Basic cancellable tests

(test background-never-done
  "Background cancellables are never done"
  (let ((ctx (background)))
    (is (not (done-p ctx)))
    (is (not (cancelled-p ctx)))
    (is (null (deadline ctx)))
    (is (null (err ctx)))))

(test with-cancel-basic
  "Basic cancellation works"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (is (not (done-p ctx)))
    (is (not (cancelled-p ctx)))
    (funcall cancel)
    (is (done-p ctx))
    (is (cancelled-p ctx))
    (is (typep (err ctx) 'cancelled))))

(test with-cancel-custom-error
  "Custom error can be provided to cancel"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (let ((custom-error (make-condition 'cancelled :cancellable ctx)))
      (funcall cancel custom-error)
      (is (done-p ctx))
      (is (eq (err ctx) custom-error)))))

(test cancel-propagates-to-children
  "Cancelling parent cancels all children"
  (multiple-value-bind (parent parent-cancel)
      (with-cancel (background))
    (multiple-value-bind (child1 child1-cancel)
        (with-cancel parent)
      (declare (ignore child1-cancel))
      (multiple-value-bind (child2 child2-cancel)
          (with-cancel parent)
        (declare (ignore child2-cancel))

        ;; None cancelled yet
        (is (not (done-p parent)))
        (is (not (done-p child1)))
        (is (not (done-p child2)))

        ;; Cancel parent
        (funcall parent-cancel)

        ;; All should be cancelled
        (is (done-p parent))
        (is (done-p child1))
        (is (done-p child2))

        ;; All should have error
        (is (typep (err parent) 'cancelled))
        (is (typep (err child1) 'cancelled))
        (is (typep (err child2) 'cancelled))))))

(test cancel-child-doesnt-affect-parent
  "Cancelling child doesn't affect parent or siblings"
  (multiple-value-bind (parent parent-cancel)
      (with-cancel (background))
    (declare (ignore parent-cancel))
    (multiple-value-bind (child1 child1-cancel)
        (with-cancel parent)
      (multiple-value-bind (child2 child2-cancel)
          (with-cancel parent)
        (declare (ignore child2-cancel))

        ;; Cancel child1
        (funcall child1-cancel)

        ;; Only child1 should be cancelled
        (is (not (done-p parent)))
        (is (done-p child1))
        (is (not (done-p child2)))))))

(test check-cancellation-signals-when-done
  "check-cancellation signals error when cancellable is done"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (funcall cancel)
    (signals cancelled
      (check-cancellation ctx))))

(test check-cancellation-no-signal-when-active
  "check-cancellation doesn't signal when cancellable is active"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (declare (ignore cancel))
    (finishes
      (check-cancellation ctx))))

;;; Timeout tests

(test with-timeout-basic
  "Timeout cancels after specified duration"
  (multiple-value-bind (ctx cancel)
      (with-timeout (background) 0.1)
    (declare (ignore cancel))
    (is (not (done-p ctx)))
    (sleep 0.2)
    (is (done-p ctx))
    (is (typep (err ctx) 'deadline-exceeded))))

(test with-timeout-manual-cancel-before-timeout
  "Manual cancel works before timeout expires"
  (multiple-value-bind (ctx cancel)
      (with-timeout (background) 1.0)
    (is (not (done-p ctx)))
    (funcall cancel)
    (is (done-p ctx))
    (is (typep (err ctx) 'cancelled))))

(test with-timeout-immediate
  "Zero or negative timeout cancels immediately"
  (multiple-value-bind (ctx cancel)
      (with-timeout (background) 0)
    (declare (ignore cancel))
    (is (done-p ctx))
    (is (typep (err ctx) 'deadline-exceeded))))

(test with-timeout-float-timeout-is-exact
  "A float timeout gives a rational deadline exactly that far away.  Added to
the current time as it was, a single-float timeout made a single-float
deadline, accurate only to 256 seconds at that magnitude, and a double-float
one could not be put in the timer's heap, whose entries hold rationals."
  (dolist (timeout (list 0.1f0 0.1d0))
    (let ((before (get-current-time)))
      (multiple-value-bind (ctx cancel)
          (with-timeout (background) timeout)
        (unwind-protect
             (let ((deadline (deadline ctx)))
               (is (typep deadline 'rational))
               (is (<= (+ before (rational timeout))
                       deadline
                       (+ (get-current-time) (rational timeout)))))
          (funcall cancel))))))

;;; Deadline tests

(test with-deadline-accepts-a-float
  "A float deadline is kept as the rational of equal value."
  (let ((deadline-time (float (+ (get-current-time) 10) 1d0)))
    (multiple-value-bind (ctx cancel)
        (with-deadline (background) deadline-time)
      (unwind-protect
           (progn
             (is (= (rational deadline-time) (deadline ctx)))
             (is (typep (deadline ctx) 'rational))
             (is (not (done-p ctx))))
        (funcall cancel)))))

(test with-deadline-basic
  "Deadline cancels at specified time"
  (let ((deadline-time (+ (get-current-time) 1/10)))
    (multiple-value-bind (ctx cancel)
        (with-deadline (background) deadline-time)
      (declare (ignore cancel))
      (is (not (done-p ctx)))
      (is (= (deadline ctx) deadline-time))
      (sleep 0.2)
      (is (done-p ctx))
      (is (typep (err ctx) 'deadline-exceeded)))))

(test with-deadline-inherits-shorter-parent
  "Child inherits shorter deadline from parent"
  (let ((parent-deadline (+ (get-current-time) 1/10))
        (child-deadline (+ (get-current-time) 10)))
    (multiple-value-bind (parent parent-cancel)
        (with-deadline (background) parent-deadline)
      (declare (ignore parent-cancel))
      (multiple-value-bind (child child-cancel)
          (with-deadline parent child-deadline)
        (declare (ignore child-cancel))
        ;; Child should inherit parent's shorter deadline
        (is (= (deadline child) parent-deadline))
        (sleep 0.2)
        (is (done-p parent))
        (is (done-p child))))))

(test with-deadline-uses-shorter-child
  "Child uses its own shorter deadline"
  (let ((parent-deadline (+ (get-current-time) 10))
        (child-deadline (+ (get-current-time) 1/10)))
    (multiple-value-bind (parent parent-cancel)
        (with-deadline (background) parent-deadline)
      (declare (ignore parent-cancel))
      (multiple-value-bind (child child-cancel)
          (with-deadline parent child-deadline)
        (declare (ignore child-cancel))
        ;; Child should use its own shorter deadline
        (is (= (deadline child) child-deadline))
        (sleep 0.2)
        (is (not (done-p parent)))
        (is (done-p child))))))

;;; Utility tests

(test wait-until-done-already-done
  "wait-until-done returns immediately if already done"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (funcall cancel)
    (let ((start (get-current-time))
          (err (wait-until-done ctx)))
      (let ((elapsed (- (get-current-time) start)))
        (is (< elapsed 0.1))  ; Should be nearly instant
        (is (typep err 'cancelled))))))

(test wait-until-done-waits-for-cancel
  "wait-until-done blocks until cancelled"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    ;; Spawn thread to cancel after delay
    (bt:make-thread
     (lambda ()
       (sleep 0.1)
       (funcall cancel))
     :name "test-canceller")

    (let ((start (get-current-time))
          (err (wait-until-done ctx)))
      (let ((elapsed (- (get-current-time) start)))
        (is (>= elapsed 0.1))  ; Should wait at least 0.1s
        (is (< elapsed 0.3))   ; But not much longer
        (is (typep err 'cancelled))))))

(test wait-until-done-timeout
  "wait-until-done respects timeout"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (declare (ignore cancel))
    (let ((start (get-current-time))
          (err (wait-until-done ctx 0.1)))
      (let ((elapsed (- (get-current-time) start)))
        (is (>= elapsed 0.1))
        (is (< elapsed 0.3))
        (is (null err))))))  ; Timeout returns NIL

;;; Macro tests

(test with-cancel-context-macro
  "with-cancel-context macro works and auto-cancels"
  (let ((ctx nil))
    (with-cancel-context (c (background))
      (setf ctx c)
      (is (not (done-p c))))
    ;; Should be cancelled after exiting
    (is (done-p ctx))))

(test with-timeout-context-macro
  "with-timeout-context macro works"
  ;; Cancellation is cooperative: the body has to check.
  (signals deadline-exceeded
    (with-timeout-context (ctx 0.1)
      (sleep 0.2)
      (check-cancellation))))

(test with-deadline-context-macro
  "with-deadline-context macro works"
  (signals deadline-exceeded
    (let ((deadline-time (+ (get-current-time) 1/10)))
      (with-deadline-context (ctx deadline-time)
        (sleep 0.2)
        (check-cancellation)))))

;;; Stress tests

(test many-children
  "Can handle many children efficiently"
  (multiple-value-bind (parent parent-cancel)
      (with-cancel (background))
    (let ((children '()))
      ;; Create 100 children
      (dotimes (i 100)
        (multiple-value-bind (child child-cancel)
            (with-cancel parent)
          (declare (ignore child-cancel))
          (push child children)))

      ;; Cancel parent
      (funcall parent-cancel)

      ;; All children should be cancelled
      (dolist (child children)
        (is (done-p child))))))

(test many-deadlines
  "Timer system handles many concurrent deadlines"
  (let ((contexts '()))
    ;; Create 50 contexts with staggered deadlines
    (dotimes (i 50)
      (multiple-value-bind (ctx cancel)
          (with-timeout (background) (+ 0.5 (* i 0.01)))
        (declare (ignore cancel))
        (push ctx contexts)))

    ;; Wait for all to expire
    (sleep 1.5)

    ;; All should be done
    (dolist (ctx contexts)
      (is (done-p ctx)))))

;;; Thread safety tests

(test concurrent-cancellation
  "Concurrent cancellation is thread-safe"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    ;; Spawn multiple threads trying to cancel simultaneously
    (let ((threads '()))
      (dotimes (i 10)
        (push (bt:make-thread
               (lambda ()
                 (funcall cancel))
               :name (format nil "canceller-~D" i))
              threads))

      ;; Wait for all threads
      (dolist (thread threads)
        (bt:join-thread thread))

      ;; Context should be cancelled exactly once
      (is (done-p ctx))
      (is (typep (err ctx) 'cancelled)))))

(test concurrent-child-creation
  "Concurrent child creation is thread-safe"
  (multiple-value-bind (parent parent-cancel)
      (with-cancel (background))
    (let ((children '())
          (children-lock (bt:make-lock)))
      ;; Spawn threads creating children
      (let ((threads '()))
        (dotimes (i 10)
          (push (bt:make-thread
                 (lambda ()
                   (dotimes (j 10)
                     (multiple-value-bind (child child-cancel)
                         (with-cancel parent)
                       (declare (ignore child-cancel))
                       (bt:with-lock-held (children-lock)
                         (push child children)))))
                 :name (format nil "creator-~D" i))
                threads))

        ;; Wait for all threads
        (dolist (thread threads)
          (bt:join-thread thread)))

      ;; Should have 100 children
      (is (= (length children) 100))

      ;; Cancel parent
      (funcall parent-cancel)

      ;; All children should be cancelled
      (dolist (child children)
        (is (done-p child))))))

;;; Dynamic variable tests

(test with-cancellable-binds-special
  "with-cancellable binds *current-cancel-context*"
  (let ((ctx (background)))
    (is (null *current-cancel-context*))
    (with-cancellable (c ctx)
      (is (eq c ctx))
      (is (eq *current-cancel-context* ctx)))
    (is (null *current-cancel-context*))))

(test ensure-cancellable-fallback
  "ensure-cancellable provides sensible fallback"
  (let ((*current-cancel-context* nil))
    (let ((ctx1 (ensure-cancellable)))
      (is (not (null ctx1)))

      (let ((ctx2 (background)))
        (is (eq (ensure-cancellable ctx2) ctx2))

        (let ((*current-cancel-context* ctx2))
          (is (eq (ensure-cancellable) ctx2))
          (is (eq (ensure-cancellable nil) ctx2)))))))


(test timeout-duration-retains-precision-at-epoch-scale
  "Float durations are normalized before epoch addition, not after rounding."
  (dolist (duration '(0.1 1.0 0.1d0 1/10 1))
    (let ((before (get-current-time)))
      (multiple-value-bind (ctx cancel) (with-timeout (background) duration)
        (unwind-protect
             (let ((after (get-current-time)))
               (is (rationalp (deadline ctx)))
               (is (<= (+ before (rational duration)) (deadline ctx)
                       (+ after (rational duration)))))
          (funcall cancel))))))

(test absolute-double-float-deadlines-normalize-without-changing-the-value
  (let ((requested (coerce (+ (get-current-time) 60) 'double-float)))
    (multiple-value-bind (ctx cancel) (with-deadline (background) requested)
      (unwind-protect
           (progn (is (rationalp (deadline ctx))) (is (= requested (deadline ctx))))
        (funcall cancel)))))

(test deadline-scopes-preserve-values-and-clean-up-nonlocal-exits
  (is (equal '(1 2) (multiple-value-list
                     (with-timeout-context (ctx 60) (values 1 2)))))
  (is (equal '(3 4) (multiple-value-list
                     (with-deadline-context (ctx (+ (get-current-time) 60)) (values 3 4)))))
  (let ((captured nil))
    (is (eq :escaped
            (catch 'scope-exit
              (with-timeout-context (ctx 60)
                (setf captured ctx)
                (throw 'scope-exit :escaped)))))
    (is (cancelled-p captured))
    (is (null (cl-cancel::cancellable-timer-entry captured)))))

(test deadline-scopes-preserve-body-errors
  (let ((original (make-condition 'simple-error :format-control "body failed")))
    (is (eq original
            (handler-case
                (with-timeout-context (ctx 60)
                  (cancel ctx (make-condition 'deadline-exceeded :cancellable ctx))
                  (error original))
              (error (condition) condition))))))

(test expired-deadline-scope-does-not-enter-body
  (let ((entered nil))
    (signals deadline-exceeded
      (with-deadline-context (ctx (1- (get-current-time)))
        (setf entered t)))
    (is (null entered))))

(test deadline-children-are-initialized-before-parent-publication
  (multiple-value-bind (parent cancel-parent) (with-cancel (background))
    (let ((creator nil) (valid t) (seen 0))
      (labels ((observe ()
                 (bt2:with-lock-held ((cl-cancel::cancellable-lock parent))
                   (let ((children (cl-cancel::cancellable-children parent)))
                     (setf seen (max seen (length children))
                           valid (and valid
                                      (every (lambda (child)
                                               (and (typep child 'deadline-context)
                                                    (slot-boundp child 'cl-cancel::deadline)
                                                    (slot-boundp child 'cl-cancel::timer-entry)))
                                             children)))))))
        (unwind-protect
             (progn
               (setf creator
                     (bt2:make-thread
                      (lambda () (dotimes (i 1000) (with-timeout parent 60)))))
               (loop repeat 1000 while (bt2:thread-alive-p creator)
                     do (observe) (sleep 0))
               (bt2:join-thread creator)
               (observe)
               (is (eq t valid))
               (is (= 1000 seen)))
          (when creator (bt2:join-thread creator))
          (funcall cancel-parent))))))

;;; close-stream-on-cancel tests (regression for cl-cancel-g6p)

(defun live-stream-monitors ()
  "Count the currently-alive cancel-stream-monitor threads."
  (count "cancel-stream-monitor" (bt2:all-threads)
         :key #'bt2:thread-name :test #'equal))

(defun wait-for (predicate &optional (seconds 3.0))
  "Poll PREDICATE until it returns true or SECONDS elapse; return its last value."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop for val = (funcall predicate)
          when val return val
          when (> (get-internal-real-time) deadline) return val
          do (sleep 0.005))))

(test close-stream-on-cancel-reaps-monitor
  "Normal I/O completion wakes and joins the monitor thread - no hang, no leak"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (let* ((baseline (live-stream-monitors))
           (stream (make-string-output-stream))
           (cleanup (close-stream-on-cancel stream ctx)))
      ;; The monitor thread is running while the I/O is in flight.
      (is (wait-for (lambda () (> (live-stream-monitors) baseline))))
      ;; Run cleanup in its own thread so a regression (the monitor parks on a
      ;; semaphore that normal completion never signals) surfaces as a clean
      ;; test failure instead of hanging the whole suite.
      (let ((runner (bt2:make-thread (lambda () (funcall cleanup))
                                     :name "cleanup-runner")))
        (is (wait-for (lambda () (not (bt2:thread-alive-p runner)))))
        (when (not (bt2:thread-alive-p runner))
          (bt2:join-thread runner)))
      ;; Monitor must be reaped, not leaked ...
      (is (= baseline (live-stream-monitors)))
      ;; ... and a never-cancelled stream must be left open.
      (is (open-stream-p stream))
      (funcall cancel))))

(test close-stream-on-cancel-closes-on-cancel
  "Cancelling the context forcibly closes the monitored stream"
  (multiple-value-bind (ctx cancel)
      (with-cancel (background))
    (let* ((stream (make-string-output-stream))
           (cleanup (close-stream-on-cancel stream ctx)))
      (unwind-protect
           (progn
             (is (open-stream-p stream))
             (funcall cancel)
             (is (wait-for (lambda () (not (open-stream-p stream))))))
        (funcall cleanup)))))

(test close-stream-on-cancel-skips-non-cancellable
  "A background (non-cancellable) context spawns no monitor thread"
  (let* ((baseline (live-stream-monitors))
         (stream (make-string-output-stream))
         (cleanup (close-stream-on-cancel stream (background))))
    (is (= baseline (live-stream-monitors)))
    (finishes (funcall cleanup))
    (is (open-stream-p stream))))
