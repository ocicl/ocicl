;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; fd-entry.lisp --- FD event structure.
;;;

(in-package :iolib/multiplex)

;;;; EVENT

(deftype fd-event-type ()
  '(member :read :write))

(defstruct (fd-handler
             (:constructor make-fd-handler
                           (fd type callback one-shot-p &optional timer))
             (:copier nil))
  (fd nil :type unsigned-byte)
  (type nil :type fd-event-type)
  (callback nil :type function-designator)
  (timer nil :type (or null timer))
  ;; one-shot events are removed after being triggered
  (one-shot-p nil :type boolean))

;;;; FD-ENTRY

(defstruct (fd-entry
             (:constructor make-fd-entry (fd))
             (:copier nil))
  (fd 0 :type unsigned-byte)
  (read-handler  nil :type (or null fd-handler))
  (write-handler nil :type (or null fd-handler))
  (write-ts 0.0d0 :type double-float)
  (error-callback nil :type (or null function-designator)))

(defun fd-entry-handler (fd-entry event-type)
  (case event-type
    (:read  (fd-entry-read-handler  fd-entry))
    (:write (fd-entry-write-handler fd-entry))))

(defun (setf fd-entry-handler) (event fd-entry event-type)
  (case event-type
    (:read  (setf (fd-entry-read-handler  fd-entry) event))
    (:write (setf (fd-entry-write-handler fd-entry) event))))

(defun fd-entry-empty-p (fd-entry)
  (and (null (fd-entry-read-handler  fd-entry))
       (null (fd-entry-write-handler fd-entry))))
