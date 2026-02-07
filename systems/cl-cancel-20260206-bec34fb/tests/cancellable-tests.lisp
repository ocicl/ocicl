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

;;; Deadline tests

(test with-deadline-basic
  "Deadline cancels at specified time"
  (let ((deadline-time (+ (get-current-time) 0.1)))
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
  (let ((parent-deadline (+ (get-current-time) 0.1))
        (child-deadline (+ (get-current-time) 10.0)))
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
  (let ((parent-deadline (+ (get-current-time) 10.0))
        (child-deadline (+ (get-current-time) 0.1)))
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
  (signals deadline-exceeded
    (with-timeout-context (ctx 0.1)
      (sleep 0.2))))

(test with-deadline-context-macro
  "with-deadline-context macro works"
  (signals deadline-exceeded
    (let ((deadline-time (+ (get-current-time) 0.1)))
      (with-deadline-context (ctx deadline-time)
        (sleep 0.2)))))

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
