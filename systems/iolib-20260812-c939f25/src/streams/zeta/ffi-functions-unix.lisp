;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *NIX-specific routines.
;;;

(in-package :iolib/zstreams)

;;;-------------------------------------------------------------------------
;;; FD polling
;;;-------------------------------------------------------------------------

(defun compute-poll-flags (type)
  (ecase type
    (:input  (logior isys:pollin isys:pollrdhup isys:pollpri))
    (:output (logior isys:pollout))
    (:io     (logior isys:pollin isys:pollrdhup isys:pollpri isys:pollout))))

(defun process-poll-revents (fd event-type revents)
  (flet ((poll-error ()
           (error 'isys:poll-error :code isys:ebadf :identifier :ebadf
                  :os-handle fd :event-type event-type
                  :message "invalid OS handle")))
    (let ((readp  nil) (rhupp nil)
          (writep nil) (whupp nil))
      (flags-case revents
        ((isys:pollin isys:pollpri)   (setf readp t))
        ((isys:pollrdhup)             (setf rhupp t))
        ((isys:pollout)               (setf writep t))
        ((isys:pollhup)               (setf whupp t))
        ((isys:pollerr isys:pollnval) (poll-error)))
      (values readp rhupp writep whupp))))

(defun timeout->milisec (timeout)
  (multiple-value-bind (sec usec)
      (decode-timeout timeout)
    (+ (* sec 1000) (truncate usec 1000))))

(defun %poll (fds timeout)
  (isys:repeat-upon-condition-decreasing-timeout
      ((isys:eintr) remaining-time timeout)
    (isys:poll fds 1 (timeout->milisec remaining-time))))

(defun poll-fd (file-descriptor event-type timeout)
  "Poll file descriptor `FD' for I/O readiness. `EVENT-TYPE' must be either
:INPUT, :OUTPUT or :IO. `TIMEOUT' must be a non-negative real measured
in seconds. If a timeout occurs `POLL-TIMEOUT' is signaled.
Returns two boolean values indicating readability and writeability of `FD'."
  (flet ((poll-error (posix-err)
           (error 'poll-error
                  :code (posix-file-error-code posix-err)
                  :identifier (posix-file-error-identifier posix-err)
                  :event-type event-type
                  :os-handle file-descriptor
                  :message (format nil "OS error ~A"
                                   (posix-file-error-identifier posix-err)))))
    (with-foreign-object (pollfd 'isys:pollfd)
      (isys:bzero pollfd (isys:sizeof 'isys:pollfd))
      (with-foreign-slots ((isys:fd isys:events isys:revents)
                           pollfd isys:pollfd)
        (setf isys:fd file-descriptor
              isys:events (compute-poll-flags event-type))
        (handler-case
            (cond
              ((plusp (%poll pollfd timeout))
               (process-poll-revents isys:fd event-type isys:revents))
              (t
               (error 'isys:poll-timeout
                      :os-handle file-descriptor
                      :event-type event-type)))
          (isys:syscall-error (err) (poll-error err)))))))


;;;-------------------------------------------------------------------------
;;; Set FD nonblocking
;;;-------------------------------------------------------------------------

(defun %set-fd-nonblock (fd)
  (declare (special *device*))
  (handler-case
      (with-foreign-object (arg :int)
        (setf (mem-aref arg :int) 1)
        (isys:ioctl fd isys:fionbio arg))
    (isys:syscall-error (err)
      (posix-file-error err *device* "issuing FIONBIO IOCTL on")))
  (values))


;;;-------------------------------------------------------------------------
;;; Get number of bytes availabe on FD
;;;-------------------------------------------------------------------------

(defun %get-fd-nbytes (fd)
  (declare (special *device*))
  (handler-case
      (with-foreign-object (arg :int)
        (isys:ioctl fd isys:fionread arg)
        (mem-aref arg :int))
    (isys:syscall-error (err)
      (posix-file-error err *device* "issuing FIONREAD IOCTL on"))))


;;;-------------------------------------------------------------------------
;;; File Descriptor reading
;;;-------------------------------------------------------------------------

(defun %read-octets/non-blocking (fd vector start end)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (isys:read fd (inc-pointer buf start) (- end start))
      (isys:ewouldblock () 0)
      (isys:syscall-error (err)
        (posix-file-error err *device* "reading data from"))
      (:no-error (nbytes)
        (if (zerop nbytes) :eof nbytes)))))

(defun %read-octets/timeout (fd vector start end timeout)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (isys:repeat-decreasing-timeout
        (remaining (clamp-timeout timeout) :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (poll-fd fd :input remaining)
                   (return-from :rloop 0))))
        (handler-case
            (isys:read fd (inc-pointer buf start) (- end start))
          (isys:ewouldblock () (check-timeout))
          (isys:syscall-error (err)
            (posix-file-error err *device* "reading data from"))
          (:no-error (nbytes)
            (return-from :rloop
              (if (zerop nbytes) :eof nbytes))))))))


;;;-------------------------------------------------------------------------
;;; File Descriptor writing
;;;-------------------------------------------------------------------------

(defun %write-octets/non-blocking (fd vector start end)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (handler-case
        (isys:write fd (inc-pointer buf start) (- end start))
      (isys:ewouldblock () 0)
      (isys:epipe () :hangup)
      (isys:syscall-error (err)
        (posix-file-error err *device* "writing data to"))
      (:no-error (nbytes)
        (if (zerop nbytes) :hangup nbytes)))))

(defun %write-octets/timeout (fd vector start end timeout)
  (declare (type ub8-simple-vector vector)
           (special *device*))
  (with-pointer-to-vector-data (buf vector)
    (isys:repeat-decreasing-timeout
        (remaining (clamp-timeout timeout) :rloop)
      (flet ((check-timeout ()
               (if (plusp remaining)
                   (poll-fd fd :output remaining)
                   (return-from :rloop 0))))
        (handler-case
            (isys:write fd (inc-pointer buf start) (- end start))
          (isys:ewouldblock () (check-timeout))
          (isys:epipe () (return-from :rloop :hangup))
          (isys:syscall-error (err)
            (posix-file-error err *device* "writing data to"))
          (:no-error (nbytes)
            (return-from :rloop
              (if (zerop nbytes) :hangup nbytes))))))))
