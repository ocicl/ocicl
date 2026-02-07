;;;; cancellable.lisp
;;;;
;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-cancel)

;;; Conditions

(define-condition cancellation-error (error)
  ((cancellable :initarg :cancellable :reader cancellation-error-cancellable))
  (:documentation "Base class for cancellation-related errors"))

(define-condition cancelled (cancellation-error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Cancelled")))
  (:documentation "Signaled when a cancellable is cancelled"))

(define-condition deadline-exceeded (cancellation-error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "Deadline exceeded")))
  (:documentation "Signaled when a cancellable deadline is exceeded"))

;;; Base cancellable class

(defclass cancellable ()
  ((parent :initarg :parent
           :initform nil
           :reader cancellable-parent
           :documentation "Parent cancellable in the hierarchy")
   (lock :initform (bt2:make-lock :name "cancellable-lock")
         :reader cancellable-lock
         :documentation "Lock for thread-safe operations"))
  (:documentation "Base cancellable class"))

(defclass cancellable-context (cancellable)
  ((cancelled :initform nil
              :accessor cancellable-cancelled-slot
              :documentation "Whether this cancellable has been cancelled")
   (cancel-error :initform nil
                 :accessor cancellable-cancel-error
                 :documentation "Error that caused cancellation")
   (children :initform '()
             :accessor cancellable-children
             :documentation "Child cancellables that should be cancelled with this one")
   (done-semaphore :initform (bt2:make-semaphore :name "cancellable-done" :count 0)
                   :reader cancellable-done-semaphore
                   :documentation "Semaphore signaled when cancellable becomes done"))
  (:documentation "A cancellable that can be cancelled"))

;;; Dynamic variable for implicit cancellable passing

(defvar *current-cancel-context* nil
  "The current cancel context. Can be bound dynamically to pass cancellable implicitly.")

;;; Protocol implementation

(defgeneric done-p (cancellable)
  (:documentation "Returns T if the cancellable is done (cancelled or deadline exceeded)"))

(defgeneric cancelled-p (cancellable)
  (:documentation "Returns T if the cancellable has been cancelled"))

(defgeneric deadline (cancellable)
  (:documentation "Returns the deadline of the cancellable, or NIL if none"))

(defgeneric err (cancellable)
  (:documentation "Returns the error that caused cancellation, or NIL"))

(defgeneric cancel (cancellable &optional error)
  (:documentation "Cancels the cancellable and all its children"))

(defgeneric remove-child (parent child)
  (:documentation "Removes a child from parent's children list"))

;;; Base implementations

(defmethod done-p ((cancellable cancellable))
  "Base cancellables are never done"
  nil)

(defmethod cancelled-p ((cancellable cancellable))
  "Base cancellables cannot be cancelled"
  nil)

(defmethod deadline ((cancellable cancellable))
  "Base cancellables have no deadline"
  nil)

(defmethod err ((cancellable cancellable))
  "Base cancellables have no error"
  nil)

(defmethod remove-child ((parent cancellable) child)
  "Base cancellables don't have children"
  (declare (ignore child))
  (values))

;;; Cancellable context implementations

(defmethod deadline ((cancellable cancellable-context))
  "Cancellable contexts inherit deadline from parent"
  (let ((parent (cancellable-parent cancellable)))
    (when parent
      (deadline parent))))

(defmethod done-p ((cancellable cancellable-context))
  "Check if cancellable is done.
   Uses lock to avoid data race, but releases before checking parent."
  ;; Read local state under lock, release, then check parent.
  ;; Never call parent methods while holding lock to prevent deadlock.
  (let ((local-cancelled nil)
        (parent nil))
    (bt2:with-lock-held ((cancellable-lock cancellable))
      (setf local-cancelled (cancellable-cancelled-slot cancellable)
            parent (cancellable-parent cancellable)))
    ;; Check local state first, then parent (without holding lock)
    (or local-cancelled
        (and parent (done-p parent)))))

(defmethod cancelled-p ((cancellable cancellable-context))
  "Check if cancellable is cancelled.
   Uses lock to avoid data race, but releases before checking parent."
  ;; Read local state under lock, release, then check parent.
  ;; Never call parent methods while holding lock to prevent deadlock.
  (let ((local-cancelled nil)
        (parent nil))
    (bt2:with-lock-held ((cancellable-lock cancellable))
      (setf local-cancelled (cancellable-cancelled-slot cancellable)
            parent (cancellable-parent cancellable)))
    ;; Check local state first, then parent (without holding lock)
    (or local-cancelled
        (and parent (cancelled-p parent)))))

(defmethod err ((cancellable cancellable-context))
  ;; Read local state under lock, release, then check parent.
  ;; Never call parent methods while holding lock to prevent deadlock.
  (let ((local-error nil)
        (parent nil))
    (bt2:with-lock-held ((cancellable-lock cancellable))
      (setf local-error (cancellable-cancel-error cancellable)
            parent (cancellable-parent cancellable)))
    ;; Check local error first, then parent (without holding lock)
    (or local-error
        (and parent (err parent)))))

(defmethod cancel ((cancellable cancellable-context) &optional (error (make-condition 'cancelled :cancellable cancellable)))
  "Cancel this cancellable and all children"
  (let ((children-to-cancel nil)
        (parent nil)
        (should-notify nil))
    (bt2:with-lock-held ((cancellable-lock cancellable))
      (unless (cancellable-cancelled-slot cancellable)
        (setf (cancellable-cancelled-slot cancellable) t
              (cancellable-cancel-error cancellable) error
              should-notify t)
        ;; Snapshot children list and clear it
        (setf children-to-cancel (cancellable-children cancellable)
              (cancellable-children cancellable) nil)
        ;; Get parent reference for removal
        (setf parent (cancellable-parent cancellable))))

    ;; Notify waiters that cancellable is done (do outside lock)
    (when should-notify
      (bt2:signal-semaphore (cancellable-done-semaphore cancellable)))

    ;; Cancel children without holding lock to avoid deadlock
    (dolist (child children-to-cancel)
      (cancel child error))

    ;; Remove ourselves from parent's children list
    (when parent
      (remove-child parent cancellable)))
  (values))

(defmethod remove-child ((parent cancellable-context) child)
  "Remove a child from parent's children list"
  (bt2:with-lock-held ((cancellable-lock parent))
    (setf (cancellable-children parent)
          (delete child (cancellable-children parent) :test #'eq)))
  (values))

;;; Background cancellable

(defclass background-context (cancellable)
  ()
  (:documentation "A cancellable that is never cancelled and has no deadline"))

(defun background ()
  "Returns a non-nil, empty cancellable. It is never cancelled and has no deadline.
It is typically used by the main function, initialization, and tests, and as the
top-level cancellable for incoming requests."
  (make-instance 'background-context))

;;; Cancellable creation functions

(defun with-cancel (parent)
  "Returns a copy of parent with a new Done channel. The returned cancellable's
Done channel is closed when the returned cancel function is called or when
the parent cancellable's Done channel is closed, whichever happens first.

Returns (values cancellable cancel-function)
The cancel function is a lambda accepting an optional error parameter."
  (let* ((ctx (make-instance 'cancellable-context :parent parent))
         (cancel-func (lambda (&optional (error (make-condition 'cancelled :cancellable ctx)))
                        (cancel ctx error))))
    ;; Register with parent
    (let ((cancel-error nil))
      (when (typep parent 'cancellable-context)
        (bt2:with-lock-held ((cancellable-lock parent))
          (if (cancellable-cancelled-slot parent)
              ;; Parent already cancelled - capture error, cancel after releasing lock
              (setf cancel-error (or (cancellable-cancel-error parent)
                                    (make-condition 'cancelled :cancellable parent)))
              ;; Parent still active - register child
              (push ctx (cancellable-children parent)))))

      ;; Cancel outside the lock if parent was already cancelled
      (when cancel-error
        (cancel ctx cancel-error)))

    ;; If parent is done but not cancellable (e.g., via deadline),
    ;; check one more time outside the lock
    (when (and (done-p parent) (not (cancellable-cancelled-slot ctx)))
      (cancel ctx (or (err parent)
                     (make-condition 'cancelled :cancellable parent))))

    (values ctx cancel-func)))

;;; Convenience macros

(defmacro with-cancel-context ((var &optional (parent '*current-cancel-context*)) &body body)
  "Convenience macro to execute BODY with a cancellable context.
Automatically cancels the context when done.

Example:
  (with-cancel-context (ctx (background))
    (process-request))  ; ctx automatically cancelled on exit"
  (let ((cancel (gensym "CANCEL")))
    `(multiple-value-bind (,var ,cancel)
         (with-cancel (ensure-cancellable ,parent))
       (unwind-protect
            (with-cancellable (,var ,var)
              ,@body)
         (funcall ,cancel)))))

;;; Utility functions and macros

(defun check-cancellation (&optional (cancellable *current-cancel-context*))
  "Checks if the cancellable is done and signals an error if so"
  (when (and cancellable (done-p cancellable))
    (error (or (err cancellable)
               (make-condition 'cancelled :cancellable cancellable)))))

(defun wait-until-done (cancellable &optional timeout)
  "Blocks until CANCELLABLE becomes done (cancelled or deadline exceeded).
   Returns immediately if cancellable is already done.
   Returns the error condition that caused cancellation, or NIL on timeout.

   TIMEOUT is in seconds (can be fractional). NIL means wait indefinitely.

   This provides efficient waiting without polling - the thread sleeps until
   the cancellable is cancelled or the timeout expires."
  (cond
    ((not (typep cancellable 'cancellable-context))
     ;; Non-cancellable contexts are never done
     (when timeout
       (sleep timeout))
     nil)
    ((done-p cancellable)
     ;; Already done - return immediately
     (err cancellable))
    (t
     ;; Wait for semaphore signal
     (when (bt2:wait-on-semaphore (cancellable-done-semaphore cancellable)
                                  :timeout timeout)
       ;; Semaphore was signaled - cancellable is done
       (err cancellable)))))

(defmacro with-cancellable ((var cancellable) &body body)
  "Binds the cancellable to both VAR and *CURRENT-CANCELLABLE* for the duration of BODY"
  `(let ((,var ,cancellable)
         (*current-cancel-context* ,cancellable))
     ,@body))

(defun call-with-cancellable (cancellable function)
  "Calls FUNCTION with CANCELLABLE bound to *CURRENT-CANCELLABLE*"
  (let ((*current-cancel-context* cancellable))
    (funcall function)))

(defun ensure-cancellable (&optional cancellable)
  "Returns CANCELLABLE if non-nil, otherwise *CURRENT-CANCELLABLE*, otherwise a background context"
  (or cancellable *current-cancel-context* (background)))

;;; Feature registration

(pushnew :cl-cancel *features*)
