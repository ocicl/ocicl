;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :iolib/zstreams)

(define-condition posix-file-error (file-error)
  ((action :initarg :action :reader posix-file-error-action)
   (code :initarg :code :reader posix-file-error-code)
   (identifier :initarg :identifier :reader posix-file-error-identifier))
  (:report (lambda (condition stream)
             (format stream "Error while ~A ~S: ~A"
                     (posix-file-error-action condition)
                     (file-error-pathname condition)
                     (isys:strerror (posix-file-error-code condition))))))

(defun posix-file-error (syscall-error filename action)
  (error 'posix-file-error
         :code (posix-file-error-code syscall-error)
         :identifier (posix-file-error-identifier syscall-error)
         :pathname filename :action action))

(define-condition hangup (stream-error) ()
  (:report (lambda (c s)
             (format s "Stream ~S hang up."
                     (stream-error-stream c))))
  (:documentation "Condition signaled when the underlying device of a stream
is closed by the remote end while writing to it."))
