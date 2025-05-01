;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Main event loop.
;;;

(in-package :iolib/multiplex)


;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass event-base ()
  ((mux :reader mux-of)
   (fds :initform (make-hash-table :test 'eql)
        :reader fds-of)
   (timers :initform (make-priority-queue :key #'%timer-expire-time)
           :reader timers-of)
   (fd-timers :initform (make-priority-queue :key #'%timer-expire-time)
              :reader fd-timers-of)
   (expired-events :initform nil
                   :accessor expired-events-of)
   (write-interval-threshold :initarg :write-interval-threshold
                             :accessor write-interval-threshold-of)
   (exit :initform nil
         :accessor exit-p)
   (exit-when-empty :initarg :exit-when-empty
                    :accessor exit-when-empty-p))
  (:default-initargs :mux *default-multiplexer*
   	             :write-interval-threshold 0.0d0
                     :exit-when-empty nil))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((base event-base) stream)
  (print-unreadable-object (base stream :type nil :identity t)
    (if (fds-of base)
        (format stream "event base, ~A FDs monitored, using: ~A"
                (hash-table-count (fds-of base)) (mux-of base))
        (format stream "event base, closed"))))


;;;-------------------------------------------------------------------------
;;; Generic functions
;;;-------------------------------------------------------------------------

(defgeneric set-io-handler (event-base fd event-type function &key timeout one-shot))

(defgeneric set-error-handler (event-base fd function))

(defgeneric add-timer (event-base function timeout &key one-shot))

(defgeneric remove-fd-handlers (event-base fd &key read write error)
  (:documentation "Removes FD handlers for the given event types.
If READ, WRITE and ERROR are all NIL (the default), then all are removed.
Returns T if some handlers were removed, NIL otherwise."))

(defgeneric remove-timer (event-base timer))

(defgeneric event-dispatch (event-base &key one-shot timeout min-step max-step))

(defgeneric exit-event-loop (event-base &key delay))

(defgeneric event-base-empty-p (event-base))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod initialize-instance :after
    ((base event-base) &key mux write-interval-threshold)
  (check-type write-interval-threshold non-negative-real)
  (setf (write-interval-threshold-of base)
        (float write-interval-threshold 1.0d0))
  (setf (slot-value base 'mux) (make-instance mux)))


;;;-------------------------------------------------------------------------
;;; CLOSE
;;;-------------------------------------------------------------------------

;;; KLUDGE: CLOSE is for streams. --luis
;;;
;;; Also, we might want to close FDs here.  Or have a version/argument
;;; that handles that.
(defmethod close ((event-base event-base) &key abort)
  (declare (ignore abort))
  (close-multiplexer (mux-of event-base))
  (dolist (slot '(mux fds timers fd-timers expired-events))
    (setf (slot-value event-base slot) nil))
  (values event-base))


;;;-------------------------------------------------------------------------
;;; Helper macros
;;;-------------------------------------------------------------------------

(defmacro with-event-base ((var &rest initargs) &body body)
  "Binds VAR to a new EVENT-BASE, instantiated with INITARGS,
within the extent of BODY.  Closes VAR."
  `(let ((,var (make-instance 'event-base ,@initargs)))
     (unwind-protect
          (locally ,@body)
       (when ,var (close ,var)))))


;;;-------------------------------------------------------------------------
;;; Utilities
;;;-------------------------------------------------------------------------

(defun fd-entry-of (event-base fd)
  (gethash fd (fds-of event-base)))

(defun (setf fd-entry-of) (fd-entry event-base fd)
  (setf (gethash fd (fds-of event-base)) fd-entry))

(defmethod exit-event-loop ((event-base event-base) &key (delay 0))
  (add-timer event-base
             (lambda () (setf (exit-p event-base) t))
             delay :one-shot t))

(defmethod event-base-empty-p ((event-base event-base))
  (and (zerop (hash-table-count (fds-of event-base)))
       (priority-queue-empty-p (timers-of event-base))))


;;;-------------------------------------------------------------------------
;;; SET-IO-HANDLER
;;;-------------------------------------------------------------------------

(defmethod set-io-handler :before
    ((event-base event-base) fd event-type function &key timeout one-shot)
  (declare (ignore timeout))
  (check-type fd unsigned-byte)
  (check-type event-type fd-event-type)
  (check-type function function-designator)
  ;; FIXME: check the type of the timeout
  (check-type one-shot boolean)
  (when (fd-monitored-p event-base fd event-type)
    (error "FD ~A is already monitored for event ~A" fd event-type)))

(defun fd-monitored-p (event-base fd event-type)
  "Generalised predicate returning the event handler if the given FD
is monitored for EVENT-TYPE."
  (let ((entry (fd-entry-of event-base fd)))
    (and entry (fd-entry-handler entry event-type))))

(defmethod set-io-handler
    ((event-base event-base) fd event-type function &key timeout one-shot)
  (let ((current-fd-entry (fd-entry-of event-base fd))
        (event (make-fd-handler fd event-type function one-shot)))
    (cond
      (current-fd-entry
       (%set-io-handler event-base fd event current-fd-entry timeout)
       (update-fd (mux-of event-base) current-fd-entry event-type :add))
      (t
       (let ((new-fd-entry (make-fd-entry fd)))
         (%set-io-handler event-base fd event new-fd-entry timeout)
         (monitor-fd (mux-of event-base) new-fd-entry))))
    (values event)))

(defun %set-io-handler (event-base fd event fd-entry timeout)
  (when timeout
    (%set-io-handler-timer event-base event timeout))
  (setf (fd-entry-handler fd-entry (fd-handler-type event)) event)
  (setf (fd-entry-of event-base fd) fd-entry)
  (values event))

(defun %set-io-handler-timer (event-base event timeout)
  (let ((timer (make-timer (lambda () (expire-event event-base event))
                           timeout)))
    (setf (fd-handler-timer event) timer)
    (schedule-timer (fd-timers-of event-base) timer)))

(defun expire-event (event-base event)
  (push event (expired-events-of event-base)))


;;;-------------------------------------------------------------------------
;;; SET-ERROR-HANDLER
;;;-------------------------------------------------------------------------

(defmethod set-error-handler :before
    ((event-base event-base) fd function)
  (check-type fd unsigned-byte)
  (check-type function function-designator)
  (unless (fd-entry-of event-base fd)
    (error "FD ~A is not being monitored" fd))
  (when (fd-has-error-handler-p event-base fd)
    (error "FD ~A already has an error handler" fd)))

(defun fd-has-error-handler-p (event-base fd)
  (let ((entry (fd-entry-of event-base fd)))
    (and entry (fd-entry-error-callback entry))))

(defmethod set-error-handler
    ((event-base event-base) fd function)
  (let ((fd-entry (fd-entry-of event-base fd)))
    (setf (fd-entry-error-callback fd-entry) function)))


;;;-------------------------------------------------------------------------
;;; ADD-TIMER
;;;-------------------------------------------------------------------------

(defmethod add-timer :before
    ((event-base event-base) function timeout &key one-shot)
  (declare (ignore timeout))
  (check-type function function-designator)
  ;; FIXME: check the type of the timeout
  (check-type one-shot boolean))

(defmethod add-timer
    ((event-base event-base) function timeout &key one-shot)
  (schedule-timer (timers-of event-base)
                  (make-timer function timeout :one-shot one-shot)))


;;;-------------------------------------------------------------------------
;;; REMOVE-FD-HANDLERS and REMOVE-TIMER
;;;-------------------------------------------------------------------------

(defmethod remove-fd-handlers
    ((event-base event-base) fd &key read write error)
  (unless (or read write error)
    (setf read t write t error t))
  (let ((entry (fd-entry-of event-base fd)))
    (cond
      (entry
       (prog1
           (%remove-fd-handlers event-base fd entry read write error)
         (when (and read write)
           (assert (null (fd-entry-of event-base fd))))))
      (t nil))))

(defun %remove-fd-handlers (event-base fd entry read write error)
  (let ((rev (fd-entry-read-handler entry))
        (wev (fd-entry-write-handler entry))
        (eev (fd-entry-error-callback entry))
        (removed nil))
    (when (and rev read)
      (%remove-io-handler event-base fd entry rev)
      (setf removed t))
    (when (and wev write)
      (%remove-io-handler event-base fd entry wev)
      (setf removed t))
    (when (and eev error)
      (setf (fd-entry-error-callback entry) nil)
      (setf removed t))
    removed))

(defun %remove-io-handler (event-base fd fd-entry event)
  (let ((event-type (fd-handler-type event)))
    (setf (fd-entry-handler fd-entry event-type) nil)
    (when-let (timer (fd-handler-timer event))
      (unschedule-timer (fd-timers-of event-base) timer))
    (cond
      ((fd-entry-empty-p fd-entry)
       (%remove-fd-entry event-base fd)
       (unmonitor-fd (mux-of event-base) fd-entry))
      (t
       (update-fd (mux-of event-base) fd-entry event-type :del)))))

(defun %remove-fd-entry (event-base fd)
  (remhash fd (fds-of event-base)))

(defmethod remove-timer :before
    ((event-base event-base) timer)
  (check-type timer timer))

(defmethod remove-timer ((event-base event-base) timer)
  (unschedule-timer (timers-of event-base) timer)
  (values event-base))


;;;-------------------------------------------------------------------------
;;; EVENT-DISPATCH
;;;-------------------------------------------------------------------------

(defvar *minimum-event-loop-step* 0.0d0)
(defvar *maximum-event-loop-step* nil)

(defmethod event-dispatch :around
    ((event-base event-base) &key timeout one-shot min-step max-step)
  (declare (ignore one-shot min-step max-step))
  (setf (exit-p event-base) nil)
  (let ((timer (when timeout
                 (exit-event-loop event-base :delay timeout))))
    (unwind-protect
         (call-next-method)
      (when timer
        (remove-timer event-base timer)))))

(defmethod event-dispatch ((event-base event-base) &key one-shot timeout
                           (min-step *minimum-event-loop-step*)
                           (max-step *maximum-event-loop-step*))
  (declare (ignore timeout))
  (coercef min-step 'double-float)
  (when max-step (coercef max-step 'double-float))
  (with-accessors ((mux mux-of) (fds fds-of) (exit-p exit-p)
                   (exit-when-empty exit-when-empty-p)
                   (timers timers-of) (fd-timers fd-timers-of)
                   (expired-events expired-events-of))
      event-base
    (labels ((poll-timeout (now)
               (let* ((deadline1 (time-to-next-timer timers))
                      (deadline2 (time-to-next-timer fd-timers))
                      (deadline (if (and deadline1 deadline2)
                                    (min deadline1 deadline2)
                                    (or deadline1 deadline2))))
                 (if deadline
                     (clamp-timeout (- deadline now) min-step max-step)
                     max-step)))
             (must-exit-loop-p ()
               (or exit-p
                   (and exit-when-empty
                        (event-base-empty-p event-base)))))
      (loop :with deletion-list := ()
            :with eventsp := nil
            :for now := (isys:get-monotonic-time)
            :for poll-timeout := (poll-timeout now)
            :until (must-exit-loop-p) :do
        (setf expired-events nil)
        (setf (values eventsp deletion-list)
              (dispatch-fd-events-once event-base poll-timeout now))
        (%remove-handlers event-base (delete nil deletion-list))
        (when (expire-pending-timers fd-timers now) (setf eventsp t))
        (dispatch-fd-timeouts expired-events)
        (when (expire-pending-timers timers now) (setf eventsp t))
        (when (and eventsp one-shot) (setf exit-p t))))))

(defun %remove-handlers (event-base event-list)
  (loop :for ev :in event-list
        :for fd := (fd-handler-fd ev)
        :for fd-entry := (fd-entry-of event-base fd)
     :do (%remove-io-handler event-base fd fd-entry ev)))

;;; Waits for events and dispatches them.  Returns T if some events
;;; have been received, NIL otherwise.
(defun dispatch-fd-events-once (event-base timeout now)
  (let ((wthreshold (write-interval-threshold-of event-base)))
    (loop
      :with fd-events := (harvest-events (mux-of event-base) timeout)
      :for ev :in fd-events
      :for dlist :=    (%handle-one-fd event-base ev now nil wthreshold)
                 :then (%handle-one-fd event-base ev now dlist wthreshold)
      :finally
         (priority-queue-reorder (fd-timers-of event-base))
       (return (values (consp fd-events) dlist)))))

(defun %handle-one-fd (event-base event now deletion-list wthreshold)
  (destructuring-bind (fd ev-types) event
    (let* ((readp nil) (writep nil)
           (fd-entry (fd-entry-of event-base fd))
           (errorp (and fd-entry (member :error ev-types))))
      (when fd-entry
        (when (member :read ev-types)
          (setf readp (%dispatch-event fd-entry :read
                                       (if errorp :error nil) now)))
        (when (member :write ev-types)
          (when (<= wthreshold (- now (fd-entry-write-ts fd-entry)))
            (unwind-protect
                 (setf writep (%dispatch-event fd-entry :write
                                               (if errorp :error nil) now))
              (setf (fd-entry-write-ts fd-entry) now))))
        (when errorp
          (when-let ((callback (fd-entry-error-callback fd-entry)))
            (funcall callback (fd-entry-fd fd-entry) :error))
          (setf readp t writep t))
        (when readp (push (fd-entry-read-handler fd-entry) deletion-list))
        (when writep (push (fd-entry-write-handler fd-entry) deletion-list)))
      (values deletion-list))))

(defun %dispatch-event (fd-entry event-type errorp now)
  (let ((ev (fd-entry-handler fd-entry event-type)))
    (when ev
      (funcall (fd-handler-callback ev)
               (fd-entry-fd fd-entry)
               event-type
               (if errorp :error nil))
      (when-let (timer (fd-handler-timer ev))
        (reschedule-timer-relative-to-now timer now))
      (fd-handler-one-shot-p ev))))

(defun dispatch-fd-timeouts (events)
  (dolist (ev events)
    (funcall (fd-handler-callback ev)
             (fd-handler-fd ev)
             (fd-handler-type ev)
             :timeout)))
