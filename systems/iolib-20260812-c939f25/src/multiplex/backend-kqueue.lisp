;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- kqueue(2) multiplexer implementation.
;;;

(in-package :iolib/multiplex)

(defconstant +kqueue-priority+ 1)

(define-multiplexer kqueue-multiplexer +kqueue-priority+ (multiplexer)
  ())

(defmethod print-object ((mux kqueue-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "kqueue(2) multiplexer")))

(defvar *kqueue-max-events* 200)

(defmethod initialize-instance :after ((mux kqueue-multiplexer) &key)
  (setf (slot-value mux 'fd) (isys:kqueue)))

(defun do-kqueue-event-request (kqueue-fd fd-entry filter request-type)
  (let ((fd (fd-entry-fd fd-entry)))
    (with-foreign-object (kev 'isys:kevent)
      (isys:bzero kev (isys:sizeof 'isys:kevent))
      (isys:ev-set kev fd filter request-type 0 0 (null-pointer))
      (isys:kevent kqueue-fd
                        kev 1
                        (null-pointer) 0
                        (null-pointer)))))

(defun calc-kqueue-monitor-filter (fd-entry)
  (if (null (fd-entry-read-handler fd-entry))
      isys:evfilt-write
      isys:evfilt-read))

(defmethod monitor-fd ((mux kqueue-multiplexer) fd-entry)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-monitor-filter fd-entry)
                               isys:ev-add)
    (isys:ebadf ()
      (warn "FD ~A is invalid, cannot monitor it." (fd-entry-fd fd-entry)))))

(defun calc-kqueue-update-filter-and-flags (event-type edge-change)
  (case event-type
    (:read
     (case edge-change
       (:add (values isys:evfilt-read isys:ev-add))
       (:del (values isys:evfilt-read isys:ev-delete))))
    (:write
     (case edge-change
       (:add (values isys:evfilt-write isys:ev-add))
       (:del (values isys:evfilt-write isys:ev-delete))))))

(defmethod update-fd ((mux kqueue-multiplexer) fd-entry event-type edge-change)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (handler-case
      (multiple-value-call #'do-kqueue-event-request (fd-of mux) fd-entry
                           (calc-kqueue-update-filter-and-flags event-type edge-change))
    (isys:ebadf ()
      (warn "FD ~A is invalid, cannot update its status."
            (fd-entry-fd fd-entry)))
    (isys:enoent ()
      (warn "FD ~A was not monitored, cannot update its status."
            (fd-entry-fd fd-entry)))))

(defun calc-kqueue-unmonitor-filter (fd-entry)
  (if (null (fd-entry-read-handler fd-entry))
      isys:evfilt-read
      isys:evfilt-write))

(defmethod unmonitor-fd ((mux kqueue-multiplexer) fd-entry)
  (handler-case
      (do-kqueue-event-request (fd-of mux) fd-entry
                               (calc-kqueue-unmonitor-filter fd-entry)
                               isys:ev-delete)
    (isys:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (isys:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux kqueue-multiplexer) timeout)
  (with-foreign-objects ((events 'isys:kevent *kqueue-max-events*)
                         (ts 'isys:timespec))
    (isys:bzero events (* *kqueue-max-events* (isys:sizeof 'isys:kevent)))
    (let (ready-fds)
      (isys:repeat-upon-condition-decreasing-timeout
          ((isys:eintr) tmp-timeout timeout)
        (when tmp-timeout
          (timeout->timespec tmp-timeout ts))
        (setf ready-fds
              (isys:kevent (fd-of mux) (null-pointer) 0
                                events *kqueue-max-events*
                                (if tmp-timeout ts (null-pointer)))))
      (macrolet ((kevent-slot (slot-name)
                   `(foreign-slot-value (mem-aref events 'isys:kevent i)
                                        'isys:kevent ',slot-name)))
        (loop for i below ready-fds
              for fd = (kevent-slot isys:ident)
              for flags = (kevent-slot isys:flags)
              for filter = (kevent-slot isys:filter)
              for data = (kevent-slot isys:data)
              for kqueue-event = (make-kqueue-event fd flags filter data)
              when kqueue-event collect kqueue-event)))))

;;; TODO: do something with DATA
(defun make-kqueue-event (fd flags filter data)
  (declare (ignore data))
  (let ((event ()))
    (switch (filter :test #'=)
      (isys:evfilt-write (push :write event))
      (isys:evfilt-read  (push :read event)))
    (flags-case flags
      ;; TODO: check what exactly EV_EOF means
      ;; (ev-eof   (pushnew :read event))
      (isys:ev-error (push :error event)))
    (when event
      (list fd event))))
