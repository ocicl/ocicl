;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; protocols.lisp --- Protocol lookup.
;;;

(in-package :iolib/sockets)

(defvar *protocols-file* "/etc/protocols")

(defclass protocol ()
  ((name :initarg :name :reader protocol-name
         :documentation "The protocol's primary name.")
   (aliases :initarg :aliases :reader protocol-aliases
            :documentation "A list of aliases for this protocol.")
   (number :initarg :number :reader protocol-number
           :documentation "The protocol number."))
  (:documentation "Class representing a protocol."))

(defun make-protocol (name number &optional aliases)
  "Constructor for PROTOCOL objects."
  (let ((number (cond ((numberp number) number)
                      ((string  number) (parse-integer number)))))
    (make-instance 'protocol :name name :number number :aliases aliases)))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t :identity nil)
    (with-slots (name aliases number) protocol
      (format stream "Name: ~S Number: ~A Aliases: ~:[None~;~:*~{~S~^, ~}~]"
              name number aliases))))

(defun find-protocol-in-parsed-lines (tokens predicate)
  (when (< (length tokens) 2) (error 'parse-error))
  (destructuring-bind (name value &rest aliases) tokens
    (let ((value (parse-integer value)))
      (when (funcall predicate name value aliases)
        (make-protocol name value aliases)))))

(defun lookup-protocol-on-disk-by-name (file protocol)
  (flet ((good-proto-p (name value aliases)
           (declare (ignore value))
           (or (string= protocol name)
               (member protocol aliases :test #'string=))))
    (map-etc-file (lambda (tokens)
                    (ignore-errors
                      (let ((proto (find-protocol-in-parsed-lines tokens #'good-proto-p)))
                        (when proto (return* proto)))))
                  file)))

(defun lookup-protocol-on-disk-by-number (file protocol)
  (flet ((good-proto-p (name value aliases)
           (declare (ignore name aliases))
           (= protocol value)))
    (map-etc-file (lambda (tokens)
                    (ignore-errors
                      (let ((proto (find-protocol-in-parsed-lines tokens #'good-proto-p)))
                        (when proto (return* proto)))))
                  file)))

(define-condition unknown-protocol ()
  ((datum :initarg :datum :initform nil :reader unknown-protocol-datum))
  (:report (lambda (condition stream)
             (format stream "Unknown protocol: ~S" (unknown-protocol-datum condition))))
  (:documentation "Condition raised when a network protocol is not found."))
(setf (documentation 'unknown-protocol-datum 'function)
      "Return the datum that caused the signalling of an UNKNOWN-PROTOCOL condition.")

(defvar *protocol-cache-by-name*   (make-hash-table :test #'equal))
(defvar *protocol-cache-by-number* (make-hash-table :test #'eql))
(defvar *protocol-cache-lock* (bt:make-lock "/etc/protocols cache lock"))

(defun find-protocol (thing cache-fn disk-fn)
  (or (funcall cache-fn thing)
      (let ((protocol (funcall disk-fn *protocols-file* thing)))
        (when protocol
          (setf (gethash (protocol-name protocol) *protocol-cache-by-name*) protocol)
          (dolist (alias (protocol-aliases protocol))
            (setf (gethash alias *protocol-cache-by-name*) protocol))
          (setf (gethash (protocol-number protocol) *protocol-cache-by-number*) protocol)
          (values protocol)))))

(defun lookup-protocol-by-name (proto)
  (bt:with-lock-held (*protocol-cache-lock*)
    (find-protocol proto
                   (lambda (p) (gethash p *protocol-cache-by-name*))
                   #'lookup-protocol-on-disk-by-name)))

(defun lookup-protocol-by-number (proto)
  (bt:with-lock-held (*protocol-cache-lock*)
    (find-protocol proto
                   (lambda (p) (gethash p *protocol-cache-by-number*))
                   #'lookup-protocol-on-disk-by-number)))

(defun purge-protocol-cache (&optional file)
  (declare (ignore file))
  (clrhash *protocol-cache-by-name*)
  (clrhash *protocol-cache-by-number*))

(defvar *protocols-monitor*
  (make-instance 'file-monitor
                 :file *protocols-file*
                 :update-fn 'purge-protocol-cache
                 :lock *protocol-cache-lock*))

(deftype inet-protocol ()
  '(unsigned-byte 16))

(defun lookup-protocol (protocol)
  "Lookup a protocol by name or number.  Signals an
UNKNOWN-PROTOCOL error if no protocol is found."
  (check-type protocol (or unsigned-byte string symbol) "non-negative integer, a string or a symbol")
  (update-monitor *protocols-monitor*)
  (let* ((parsed (ensure-string-or-unsigned-byte protocol :type 'inet-protocol :errorp t))
         (proto (typecase parsed
                  (inet-protocol (lookup-protocol-by-number parsed))
                  (string        (lookup-protocol-by-name parsed)))))
    (if proto
        (values (protocol-number proto)
                (protocol-name proto)
                (protocol-aliases proto))
        (error 'unknown-protocol :datum protocol))))
