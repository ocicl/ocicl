;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Early FFI definitions.
;;;

(in-package :iolib/syscalls)

;;;-------------------------------------------------------------------------
;;; Syscall return wrapper
;;;-------------------------------------------------------------------------

;;; Error predicate that always returns NIL.  Not actually used
;;; because the RETURN-WRAPPER optimizes this call away.
(defun never-fails (errcode syscall)
  (declare (ignore errcode syscall))
  nil)

;;; NOTE: This is a pretty neat type that probably deserves to be
;;; included in CFFI. --luis
;;;
;;; This type is used by DEFSYSCALL to automatically check for errors
;;; using the ERROR-PREDICATE function which is passed the foreign
;;; function's return value (after going through RETURN-FILTER).  If
;;; ERROR-PREDICATE returns true, ERROR-GENERATOR is invoked.  See the
;;; RETURN-WRAPPER parse method and type translation.
(define-foreign-type syscall-wrapper ()
  ((syscall :initarg :syscall :reader syscall-of)
   (error-predicate :initarg :error-predicate :reader error-predicate-of)
   (error-location :initarg :error-location :reader error-location-of)
   (return-filter :initarg :return-filter :reader return-filter-of)
   (error-generator :initarg :error-generator :reader error-generator-of)
   (restart :initarg :restart :reader syscall-restart-p)
   (handle :initarg :handle :reader handle-of)
   (handle2 :initarg :handle2 :reader handle2-of)
   (base-type :initarg :base-type :reader base-type-of)))

(defun default-error-predicate (base-type)
  (case base-type
    (sstring
     '(lambda (s) (null s)))
    (:string
     '(lambda (s) (not (stringp s))))
    (t
     (case (cffi::canonicalize-foreign-type base-type)
       (:pointer
        'null-pointer-p)
       ((:char :short :int :long :long-long)
        'minusp)
       ;; FIXME: go here if the canonical type is unsigned.
       ((:unsigned-char :unsigned-short :unsigned-int
                        :unsigned-long :unsigned-long-long :void)
        'never-fails)
       (t
        (error "Could not choose an error-predicate function."))))))

(define-parse-method syscall-wrapper
    (base-type &key syscall handle handle2 restart
     (error-predicate 'never-fails error-predicate-p)
     (error-location :errno)
     (return-filter 'identity)
     (error-generator 'signal-syscall-error))
  ;; pick a default error-predicate
  (unless error-predicate-p
    (setf error-predicate (default-error-predicate base-type)))
  (when restart
    (setf error-generator 'signal-syscall-error/restart))
  (unless (or (eql 'never-fails error-predicate) error-generator)
    (error "Function can fail but no error-generator suplied."))
  (make-instance 'syscall-wrapper
                 :syscall syscall
                 :actual-type base-type
                 :base-type base-type
                 :handle handle
                 :handle2 handle2
                 :restart restart
                 :error-predicate error-predicate
                 :error-location error-location
                 :return-filter return-filter
                 :error-generator error-generator))

;;; This type translator sets up the appropriate calls to
;;; RETURN-FILTER, ERROR-PREDICATE and ERROR-GENERATOR around the
;;; foreign function call.
(defmethod expand-from-foreign (value (type syscall-wrapper))
  (if (and (eql 'identity (return-filter-of type))
           (eql 'never-fails (error-predicate-of type)))
      value
      (with-gensyms (retval errno block)
        (let ((foreign-call
                `(let* ,`((,retval (convert-from-foreign ,value ',(base-type-of type)))
                          ,@(case (error-location-of type)
                              (:errno `((,errno (errno))))
                              (:return `((,errno ,retval)))
                              (:negative-return `((,errno (- ,retval))))))
                   ,(let* ((return-val-exp
                             (if (eql 'identity (return-filter-of type))
                                 retval
                                 `(,(return-filter-of type) ,retval)))
                           (return-exp
                             (if (eql 'never-fails (error-predicate-of type))
                                 `return-val-exp
                                 `(if (,(error-predicate-of type) ,retval)
                                      (,(error-generator-of type) ,errno ,(syscall-of type)
                                       ,(handle-of type) ,(handle2-of type))
                                      ,return-val-exp))))
                      (if (syscall-restart-p type)
                          `(return-from ,block ,return-exp)
                          return-exp)))))
          (if (syscall-restart-p type)
              `(block ,block
                 (tagbody :restart
                    ,foreign-call))
              foreign-call)))))

(defmacro signal-syscall-error/restart (errno &optional syscall fd fd2)
  `(if (= eintr ,errno)
       (go :restart)
       (signal-syscall-error ,errno ,syscall ,fd ,fd2)))


;;;-------------------------------------------------------------------------
;;; Syscall definers
;;;-------------------------------------------------------------------------

(defmacro defentrypoint (name (&rest args) &body body)
  "Like DEFUN, in addition it DECLAIMs the function INLINE."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

(defmacro defcfun* (name-and-opts return-type &body args)
  "Like CFFI:DEFCFUN, in addition it DECLAIMs the function INLINE."
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name-and-opts)
    `(progn
       (declaim (inline ,lisp-name))
       (defcfun (,c-name ,lisp-name ,@options) ,return-type
         ,@args))))

(defmacro defsyscall (name-and-opts return-type &body args)
  "Like CFFI:DEFCFUN, in addition it DECLAIMs the function INLINE and
wraps the return value using SYSCALL-WRAPPER."
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name-and-opts)
    `(progn
       (declaim (inline ,lisp-name))
       (defcfun (,c-name ,lisp-name ,@options)
           (syscall-wrapper ,@(append (ensure-list return-type)
                                      (list :syscall c-name)))
         ,@args))))

(defmacro defkernel (name-and-opts return-type &body body)
  "Like DEFSYSCALL, but obtains the error number from the return value."
  (destructuring-bind (type &rest keyargs)
      (ensure-list return-type)
    (assert (eql :int type))
    (assert (null (getf keyargs 'error-location)))
    `(defsyscall ,name-and-opts
         (:int :error-location :negative-return ,@keyargs)
       ,@body)))

;;;-------------------------------------------------------------------------
;;; CFFI additions
;;;-------------------------------------------------------------------------

(defalias (function sizeof) cffi:foreign-type-size)

(deffoldable sizeof (t) t)
