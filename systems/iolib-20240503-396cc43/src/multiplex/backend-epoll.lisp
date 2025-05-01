;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- epoll(2) multiplexer implementation.
;;;

(in-package :iolib/multiplex)

(defconstant +epoll-priority+ 1)

(define-multiplexer epoll-multiplexer +epoll-priority+ (multiplexer)
  ((events :reader event-set-of)))

(defmethod print-object ((mux epoll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "epoll(4) multiplexer")))

(defmethod initialize-instance :after ((mux epoll-multiplexer) &key (size 25))
  (setf (slot-value mux 'fd) (isys:epoll-create size))
  (setf (slot-value mux 'events)
        (foreign-alloc '(:struct isys:epoll-event)
                       :count (fd-limit-of mux))))

(defmethod close :after ((mux epoll-multiplexer) &key abort)
  (declare (ignore abort))
  (with-slots (events) mux
    (when events
      (foreign-free events)
      (setf events nil))))

(defun calc-epoll-flags (fd-entry)
  (logior (if (fd-entry-read-handler fd-entry)
              isys:epollin
              0)
          (if (fd-entry-write-handler fd-entry)
              isys:epollout
              0)
          isys:epollpri))

(defmethod monitor-fd ((mux epoll-multiplexer) fd-entry)
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev '(:struct isys:epoll-event))
      (isys:bzero ev (isys:sizeof '(:struct isys:epoll-event)))
      (setf (foreign-slot-value ev '(:struct isys:epoll-event) 'isys:events)
            flags)
      (setf (foreign-slot-value
             (foreign-slot-value ev '(:struct isys:epoll-event) 'isys:data)
             '(:union isys:epoll-data) 'isys:fd)
            fd)
      (handler-case
          (isys:epoll-ctl (fd-of mux) isys:epoll-ctl-add fd ev)
        (isys:ebadf ()
          (warn "FD ~A is invalid, cannot monitor it." fd))
        (isys:eexist ()
          (warn "FD ~A is already monitored." fd))))))

(defmethod update-fd ((mux epoll-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (assert fd-entry (fd-entry) "Must supply an FD-ENTRY!")
  (let ((flags (calc-epoll-flags fd-entry))
        (fd (fd-entry-fd fd-entry)))
    (with-foreign-object (ev '(:struct isys:epoll-event))
      (isys:bzero ev (isys:sizeof '(:struct isys:epoll-event)))
      (setf (foreign-slot-value ev '(:struct isys:epoll-event) 'isys:events)
            flags)
      (setf (foreign-slot-value
             (foreign-slot-value ev '(:struct isys:epoll-event) 'isys:data)
             '(:union isys:epoll-data) 'isys:fd)
            fd)
      (handler-case
          (isys:epoll-ctl (fd-of mux) isys:epoll-ctl-mod fd ev)
        (isys:ebadf ()
          (warn "FD ~A is invalid, cannot update its status." fd))
        (isys:enoent ()
          (warn "FD ~A was not monitored, cannot update its status." fd))))
    (values fd-entry)))

(defmethod unmonitor-fd ((mux epoll-multiplexer) fd-entry)
  (handler-case
      (isys:epoll-ctl (fd-of mux)
                           isys:epoll-ctl-del
                           (fd-entry-fd fd-entry)
                           (null-pointer))
    (isys:ebadf ()
      (warn "FD ~A is invalid, cannot unmonitor it." (fd-entry-fd fd-entry)))
    (isys:enoent ()
      (warn "FD ~A was not monitored, cannot unmonitor it."
            (fd-entry-fd fd-entry)))))

(defmethod harvest-events ((mux epoll-multiplexer) timeout)
  (with-accessors ((events event-set-of)
                   (fd-limit fd-limit-of))
      mux
    (isys:bzero events (* fd-limit (isys:sizeof '(:struct isys:epoll-event))))
    (let (ready-fds)
      (isys:repeat-upon-condition-decreasing-timeout
          ((isys:eintr) tmp-timeout timeout)
        (setf ready-fds (isys:epoll-wait (fd-of mux) events fd-limit
                                         (timeout->milliseconds tmp-timeout))))
      (macrolet ((epoll-slot (slot-name)
                   `(foreign-slot-value
                     ;; FIXME: tests fail when wrapping this bare reference
                     ;; in a :STRUCT.
                     (mem-aref events 'isys:epoll-event i)
                     '(:struct isys:epoll-event) ',slot-name)))
        (return*
         (loop :for i :below ready-fds
               :for fd := (foreign-slot-value (epoll-slot isys:data)
                                              '(:union isys:epoll-data) 'isys:fd)
               :for event-mask := (epoll-slot isys:events)
               :for epoll-event := (make-epoll-event fd event-mask)
               :when epoll-event :collect epoll-event))))))

(defun make-epoll-event (fd mask)
  (let ((event ()))
    (flags-case mask
      ((isys:epollout isys:epollhup)
       (push :write event))
      ((isys:epollin isys:epollpri isys:epollhup)
       (push :read event))
      (isys:epollerr
       (push :error event)))
    (when event
      (list fd event))))
