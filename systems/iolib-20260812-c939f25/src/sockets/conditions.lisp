;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Socket conditions.
;;;

(in-package :iolib/sockets)

(defgeneric error-code (err)
  (:method ((err isys:syscall-error))
    (isys:code-of err)))

(defgeneric error-identifier (err)
  (:method ((err isys:syscall-error))
    (isys:identifier-of err)))

(defgeneric error-message (err)
  (:method ((err isys:syscall-error))
    (isys:message-of err)))

;;;; Socket Errors

(define-condition socket-error (isys:syscall-error) ())

(defmethod print-object ((socket-error socket-error) stream)
  (print-unreadable-object (socket-error stream :type t :identity nil)
    (let ((code (iolib/syscalls:code-of socket-error)))
      (format stream "~S ~S ~S~@[, FD: ~S~]"
              (or code "[Unknown code]")
              (error-identifier socket-error)
              (if code (isys:strerror code) "[Can't get error string.]")
              (isys:handle-of socket-error)))))

(defparameter *socket-error-map* (make-hash-table :test 'eql))

(defmacro define-socket-error (name identifier &optional documentation)
  ;; FIXME: find a better way to conditionally define syscall errors
  (when (find identifier (cffi:foreign-enum-keyword-list 'isys:errno-values))
    (let ((errno (cffi:foreign-enum-value 'isys:errno-values identifier)))
      `(progn
         (setf (gethash ,errno *socket-error-map*) ',name)
         (define-condition ,name (,(isys:get-syscall-error-condition errno)
                                   socket-error) ()
           (:default-initargs :code ,(foreign-enum-value 'socket-error-values
                                                         identifier)
             :identifier ,identifier)
           (:documentation ,(or documentation (isys:strerror identifier))))))))

(defun lookup-socket-error (errno)
  (gethash errno *socket-error-map*))

(define-condition unknown-socket-error (socket-error) ()
  (:documentation "Error signalled upon finding an unknown socket error."))

(define-socket-error socket-address-in-use-error          :eaddrinuse)
(define-socket-error socket-address-family-not-supported-error :eafnosupport)
(define-socket-error socket-address-not-available-error   :eaddrnotavail)
(define-socket-error socket-network-down-error            :enetdown)
(define-socket-error socket-network-reset-error           :enetreset)
(define-socket-error socket-network-unreachable-error     :enetunreach)
(define-socket-error socket-no-network-error              :enonet)
(define-socket-error socket-connection-aborted-error      :econnaborted)
(define-socket-error socket-connection-reset-error        :econnreset)
(define-socket-error socket-connection-refused-error      :econnrefused)
(define-socket-error socket-connection-timeout-error      :etimedout)
(define-socket-error socket-connection-in-progress-error  :einprogress)
(define-socket-error socket-endpoint-shutdown-error       :eshutdown)
(define-socket-error socket-no-buffer-space-error         :enobufs)
(define-socket-error socket-host-down-error               :ehostdown)
(define-socket-error socket-host-unreachable-error        :ehostunreach)
(define-socket-error socket-already-connected-error       :eisconn)
(define-socket-error socket-not-connected-error           :enotconn)
(define-socket-error socket-option-not-supported-error    :enoprotoopt)
(define-socket-error socket-operation-not-supported-error :eopnotsupp)

(declaim (inline %signal-socket-error))
(defun %signal-socket-error (errno syscall fd fd2)
  (when-let (err (lookup-socket-error errno))
    (error err :syscall syscall :handle fd :handle2 fd2)))

;;; Used in the ERRNO-WRAPPER foreign type.
(declaim (inline signal-socket-error))
(defun signal-socket-error (errno &optional syscall fd fd2)
  (cond
    ((= errno isys:eintr)
     (error 'isys:eintr :syscall syscall :handle fd :handle2 fd2))
    ((= errno isys:ewouldblock)
     (error 'isys:ewouldblock :syscall syscall :handle fd :handle2 fd2))
    (t
     (or (%signal-socket-error errno syscall fd fd2)
         (error (isys:make-syscall-error errno syscall fd fd2))))))
