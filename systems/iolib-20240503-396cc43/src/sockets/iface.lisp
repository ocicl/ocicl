;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Network interface lookup.
;;;

(in-package :iolib/sockets)

(defun make-interface (name index)
  "Constructor for INTERFACE objects."
  (cons name index))

(define-condition unknown-interface (isys:enxio)
  ((datum :initarg :datum :initform nil :reader unknown-interface-datum))
  (:report (lambda (condition stream)
             (format stream "Unknown interface: ~A"
                     (unknown-interface-datum condition))))
  (:documentation "Condition raised when a network interface is not found."))
(setf (documentation 'unknown-interface-datum 'function)
      "Return the datum that caused the signalling of an UNKNOWN-INTERFACE condition.")

(defun signal-unknown-interface-error (syscall datum)
  (error 'unknown-interface :syscall syscall :datum datum))

(defun list-network-interfaces ()
  "Returns a list of network interfaces currently available."
  (let ((ifptr (null-pointer)))
    (unwind-protect
         (progn
           (setf ifptr (%if-nameindex))
           (loop :for p := ifptr :then (inc-pointer p (isys:sizeof '(:struct if-nameindex)))
                 :for name := (foreign-slot-value p '(:struct if-nameindex) 'name)
                 :for index := (foreign-slot-value p '(:struct if-nameindex) 'index)
               :while (plusp index) :collect (make-interface name index)))
      (unless (null-pointer-p ifptr) (%if-freenameindex ifptr)))))

(defun get-interface-by-index (index)
  (with-foreign-object (buffer :uint8 ifnamesize)
    (handler-case
        (%if-indextoname index buffer)
      (isys:enxio ()
        (signal-unknown-interface-error "if_indextoname" index))
      (:no-error (name)
        (make-interface name index)))))

(defun get-interface-by-name (name)
  (handler-case
      (%if-nametoindex name)
    (isys:enxio ()
      (signal-unknown-interface-error "if_nametoindex" name))
    (:no-error (index)
      (make-interface (copy-seq name) index))))

(defun interface-name (interface)
  "Return the name of an network interface."
  (car interface))

(defun interface-index (interface)
  "Return the OS index of a network interface."
  (cdr interface))

(defun lookup-interface (interface)
  "Lookup an interface by name or index. UNKNOWN-INTERFACE is
signalled if an interface is not found."
  (check-type interface (or unsigned-byte string symbol) "non-negative integer, a string or a symbol")
  (let ((parsed (ensure-string-or-unsigned-byte interface :errorp t)))
    (typecase parsed
      (unsigned-byte (get-interface-by-index parsed))
      (string        (get-interface-by-name  parsed)))))
