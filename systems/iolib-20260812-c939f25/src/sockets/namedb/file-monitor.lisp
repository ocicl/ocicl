;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; file-monitor.lisp --- Monitor files on disk.
;;;

(in-package :iolib/sockets)

(defclass file-monitor ()
  ((file :initform (error "Must supply a file name")
         :initarg :file :accessor file-of)
   (timestamp :initarg :timestamp :accessor timestamp-of)
   (update-fn :initarg :update-fn :accessor update-fn-of)
   (lock      :initarg :lock      :accessor lock-of))
  (:default-initargs :timestamp 0))

(defmethod initialize-instance :after ((monitor file-monitor) &key file)
  (unless (slot-boundp monitor 'lock)
    (setf (lock-of monitor)
          (bt:make-lock (format nil "Lock for monitor of ~S" file)))))

(defmethod print-object ((monitor file-monitor) stream)
  (print-unreadable-object (monitor stream :type nil :identity nil)
    (format stream "File monitor for ~S" (file-of monitor))))

(defun monitor-oldp (monitor)
  (declare (type file-monitor monitor))
  (let ((mtime (file-write-date (file-of monitor))))
    (values (< (timestamp-of monitor) mtime)
            mtime)))

(defgeneric update-monitor (monitor)
  (:method ((monitor file-monitor))
    (bt:with-lock-held ((lock-of monitor))
      (multiple-value-bind (oldp mtime) (monitor-oldp monitor)
        (when oldp
          (funcall (update-fn-of monitor) (file-of monitor))
          (multiple-value-prog1
              (values (timestamp-of monitor) mtime)
            (setf (timestamp-of monitor) mtime)))))))
