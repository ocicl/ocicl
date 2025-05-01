;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Syscall error conditions.
;;;

(in-package :iolib/syscalls)

;;;-------------------------------------------------------------------------
;;; System Errors
;;;-------------------------------------------------------------------------

(define-condition iolib-condition ()
  ())

(define-condition iolib-error (error iolib-condition)
  ())

(define-condition syscall-error (iolib-error)
  ((syscall :initarg :syscall :reader syscall-of
            :documentation "The name of the C syscall.")
   (code :initarg :code :reader code-of
         :documentation "Numeric error code, or NIL.")
   (identifier :initarg :identifier :reader identifier-of
               :documentation "Keyword identifier, or NIL.")
   (message :initarg :message :reader message-of
            :documentation "Error description.")
   (handle :initform nil :initarg :handle :reader handle-of
           :documentation "The OS handle involved in the error situation.")
   (handle2 :initform nil :initarg :handle2 :reader handle2-of
            :documentation "An optional second OS handler."))
  (:default-initargs :code nil :identifier :unknown :message nil)
  (:documentation "Base class for syscall errors."))

(defun syscall-error-p (thing)
  (typep thing 'syscall-error))

(defun syscall-error (control-string &rest args)
  (error 'syscall-error :message (format nil "~?" control-string args)))


;;;-------------------------------------------------------------------------
;;; I/O Poll Errors
;;;-------------------------------------------------------------------------

(define-condition poll-error (syscall-error)
  ((event-type :initarg :event-type :reader event-type-of))
  (:report (lambda (c s)
             (format s "Poll error(event ~S, handle ~A)"
                     (event-type-of c) (handle-of c))
             (when (message-of c)
               (format s ": ~A" (message-of c)))))
  (:documentation
   "Signaled when an error occurs while polling for I/O readiness
of a file descriptor."))

(define-condition poll-timeout (poll-error)
  ()
  (:report (lambda (c s)
             (format s "Poll timeout(event ~S, handle ~A)"
                     (event-type-of c) (handle-of c))
             (when (message-of c)
               (format s ": ~A" (message-of c)))))
  (:documentation
   "Signaled when a timeout occurs while polling for I/O readiness
of a file descriptor."))


;;;-------------------------------------------------------------------------
;;; Repeat upon conditions
;;;-------------------------------------------------------------------------

(defmacro repeat-upon-condition ((&rest conditions) &body body)
  (with-gensyms (block-name)
    `(loop :named ,block-name :do
       (ignore-some-conditions ,conditions
         (return-from ,block-name (progn ,@body))))))

(defmacro repeat-upon-eintr (&body body)
  `(repeat-upon-condition (eintr) ,@body))

(defmacro repeat-decreasing-timeout
    ((timeout-var timeout &optional (block-name nil blockp)) &body body)
  (unless (find timeout-var (flatten body))
    (warn "You probably want to use ~S inside the body ~A" timeout-var body))
  (unless blockp (setf block-name (gensym "BLOCK")))
  (with-gensyms (deadline temp-timeout)
    `(let* ((,timeout-var ,timeout)
            (,deadline (when ,timeout-var
                         (+ ,timeout-var (get-monotonic-time)))))
       (loop :named ,block-name :do
         ,@body
           (when ,deadline
             (let ((,temp-timeout (- ,deadline (get-monotonic-time))))
               (setf ,timeout-var
                     (if (plusp ,temp-timeout)
                         ,temp-timeout
                         0))))))))

(defmacro repeat-upon-condition-decreasing-timeout
    (((&rest conditions) timeout-var timeout &optional (block-name nil blockp)) &body body)
  (unless blockp (setf block-name (gensym "BLOCK")))
  `(repeat-decreasing-timeout (,timeout-var ,timeout ,block-name)
     (ignore-some-conditions ,conditions
       (return-from ,block-name (progn ,@body)))))
