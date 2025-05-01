;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- poll(2) multiplexer implementation.
;;;

(in-package :iolib/multiplex)

(defconstant +poll-priority+ 2)

(define-multiplexer poll-multiplexer +poll-priority+ (multiplexer)
  ((fd-set :initform (allocate-pollfd-set) :accessor fd-set-of)
   (fd-set-size :initform 5 :accessor fd-set-size-of)
   (fd-count :initform 0 :accessor fd-count-of)))

(defun allocate-pollfd-set (&optional (count 5))
  (let ((fds (foreign-alloc 'nix::pollfd :count count)))
    (nix:bzero fds (* (isys:sizeof 'isys:pollfd) count))
    (values fds)))

(defmethod print-object ((mux poll-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "poll(2) multiplexer")))

(defmethod close-multiplexer progn ((mux poll-multiplexer))
  (foreign-free (fd-set-of mux))
  (setf (fd-set-of mux) nil))

(defvar *pollfd-table* (make-hash-table :test #'eql))

(defun calc-pollfd-flags (readp writep)
  (let ((flags 0))
    (when readp (setf flags (logior nix:pollin nix:pollrdhup nix:pollpri)))
    (when writep (setf flags (logior flags nix:pollout nix:pollhup)))
    (values flags)))

(defun set-pollfd-entry (fd-set index fd readp writep)
  (with-foreign-slots ((nix::fd nix::events nix::revents)
                       (mem-aref fd-set 'nix::pollfd index)
                       nix::pollfd)
    (setf nix::fd fd
          nix::revents 0
          nix::events (calc-pollfd-flags readp writep))))

(defun extend-pollfd-set (fd-set size)
  (let* ((new-size (+ size 5))
         (new-fd-set (foreign-alloc 'nix::pollfd :count new-size)))
    (nix:memcpy new-fd-set fd-set (* size (isys:sizeof 'isys:pollfd)))
    (foreign-free fd-set)
    (values new-fd-set new-size)))

(defmethod monitor-fd ((mux poll-multiplexer) fd-entry)
  (let ((fd (fd-entry-fd fd-entry))
        (readp (fd-entry-read-handler fd-entry))
        (writep (fd-entry-write-handler fd-entry)))
    (with-accessors ((fd-set fd-set-of) (size fd-set-size-of)
                     (count fd-count-of)) mux
      (when (= count size)
        (setf (values fd-set size) (extend-pollfd-set fd-set size)))
      (set-pollfd-entry fd-set count fd readp writep)
      (setf (gethash fd *pollfd-table*) count)
      (incf count))))

(defmethod update-fd ((mux poll-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (let* ((fd (fd-entry-fd fd-entry))
         (pos (gethash fd *pollfd-table*))
         (readp (fd-entry-read-handler fd-entry))
         (writep (fd-entry-write-handler fd-entry)))
    (with-accessors ((fd-set fd-set-of)) mux
      (set-pollfd-entry fd-set pos fd readp writep))))

(defun shrink-pollfd-set (fd-set count size pos)
  (let* ((new-size (if (> 5 (- size count)) (- size 5) size))
         (new-fd-set (foreign-alloc 'nix::pollfd :count new-size)))
    (when (plusp pos)
      (nix:memcpy new-fd-set fd-set (* pos (isys:sizeof 'isys:pollfd))))
    (when (< pos count)
      (nix:memcpy new-fd-set fd-set (* (- count pos) (isys:sizeof 'isys:pollfd))))
    (foreign-free fd-set)
    (values new-fd-set new-size)))

(defmethod unmonitor-fd ((mux poll-multiplexer) fd-entry)
  (let* ((fd (fd-entry-fd fd-entry))
         (pos (gethash fd *pollfd-table*)))
    (with-accessors ((fd-set fd-set-of) (size fd-set-size-of)
                     (count fd-count-of)) mux
      (setf (values fd-set size) (shrink-pollfd-set fd-set (1- count) size pos))
      (remhash fd *pollfd-table*)
      (decf count))))

(defmethod harvest-events ((mux poll-multiplexer) timeout)
  (with-accessors ((fd-set fd-set-of) (size fd-set-size-of)
                   (count fd-count-of)) mux
    ;; if there are no fds set and timeout is NULL
    ;; poll() blocks forever
    (when (and (zerop count)
               (null timeout))
      (warn "Non fds to monitor and no timeout set !")
      (return* nil))
    ;; FIXME: when does poll() return EBADF ?
    (nix:repeat-upon-condition-decreasing-timeout
        ((nix:eintr) tmp-timeout timeout)
      (nix:poll fd-set count (timeout->milliseconds tmp-timeout)))
    (harvest-pollfd-events fd-set count)))

(defun harvest-pollfd-events (fd-set count)
  (macrolet ((pollfd-slot (name index)
               `(foreign-slot-value (mem-aref fd-set 'nix::pollfd ,index)
                                    'nix::pollfd ,name)))
    (loop :for i :below count
          :for event := ()
          :for fd := (pollfd-slot 'nix::fd i)
          :for revents := (pollfd-slot 'nix::revents i)
       :do (flags-case revents
             ((nix:pollout nix:pollhup)              (push :write event))
             ((nix:pollin nix:pollrdhup nix:pollpri) (push :read event))
             ((nix:pollerr nix:pollnval)             (push :error event)))
       :when event :collect (list fd event))))
