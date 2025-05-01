;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- FD mixin definitions
;;;

(in-package :iolib/streams)

(defmethod shared-initialize :around ((stream dual-channel-fd-mixin) slot-names &key)
  (declare (ignore slot-names))
  (call-next-method)
  (setf (isys:fd-nonblock-p (fd-of stream)) t))

;;;; CLOSE

(defmethod close :before ((fd-mixin dual-channel-fd-mixin) &key abort)
  (declare (ignore abort))
  (when (fd-of fd-mixin)
    (isys:close (fd-of fd-mixin))
    (setf (fd-of fd-mixin) nil)))

;;;; Get and Set O_NONBLOCK

(defmethod fd-non-blocking ((fd-mixin dual-channel-fd-mixin))
  (isys:fd-nonblock-p (fd-of fd-mixin)))
(defobsolete fd-non-blocking "stream FDs are now always non-blocking.")

(defmethod (setf fd-non-blocking) (mode (fd-mixin dual-channel-fd-mixin))
  (check-type mode boolean "a boolean value")
  (setf (isys:fd-nonblock-p (fd-of fd-mixin)) mode))
(defobsolete (setf fd-non-blocking) "stream FDs are now always non-blocking.")
