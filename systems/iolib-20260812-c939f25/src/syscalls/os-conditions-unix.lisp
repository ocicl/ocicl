;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *NIX syscall error conditions.
;;;

(in-package :iolib/syscalls)

;;;-------------------------------------------------------------------------
;;; POSIX Syscall Errors
;;;-------------------------------------------------------------------------

;;; HASH TABLE mapping error codes to symbols denoting
;;; subtypes of SYSCALL-ERROR.
(defparameter *syscall-error-map* (make-hash-table :test 'eql))

(declaim (inline get-syscall-error-condition))
(defun get-syscall-error-condition (errno)
  (gethash errno *syscall-error-map*))

;;; Define an error condition for each ERRNO value defined in the
;;; ERRNO-VALUES enum type and populate *SYSCALL-ERROR-MAP*.
(macrolet
    ((define-syscall-errors (keywords)
       `(progn
          ,@(loop :for kw :in keywords :collect
               (let ((cond-name (intern (symbol-name kw)))
                     (code (foreign-enum-value 'errno-values kw)))
                 `(progn
                    (define-condition ,cond-name (syscall-error) ()
                      (:default-initargs :code ,code :identifier ,kw))
                    (setf (gethash ,code *syscall-error-map*) ',cond-name)))))))
  (define-syscall-errors
    #.(foreign-enum-keyword-list 'errno-values)))

;;; Instantiates a subclass of SYSCALL-ERROR matching ERR
;;; ERR must be either an integer denoting an ERRNO value.
(defun make-syscall-error (errno syscall fd fd2)
  (debug-only* (assert (integerp errno)))
  (let ((error-keyword (foreign-enum-keyword 'errno-values errno :errorp nil)))
    (unless error-keyword
      (bug "A non-existent ~A syscall error has been signaled: ~A, ~A"
           'errno-values (or error-keyword :unknown) errno))
    (make-condition (get-syscall-error-condition errno)
                    :syscall syscall :handle fd :handle2 fd2)))

(declaim (inline signal-syscall-error))
(defun signal-syscall-error (&optional (errno (errno)) syscall fd fd2)
  (cond
    ((= errno eintr)
     (error 'eintr :syscall syscall :handle fd :handle2 fd2))
    (t
     (error (make-syscall-error errno syscall fd fd2)))))

(defun signal-syscall-error-kw (error-keyword &optional syscall fd fd2)
  (let ((errno (foreign-enum-value 'errno-values error-keyword :errorp nil)))
    (unless error-keyword
      (bug "A non-existent ~A syscall error has been signaled: ~A, ~A"
           'errno-values error-keyword errno))
    (signal-syscall-error errno syscall fd fd2)))
