(in-package #:org.shirakumo.precise-time)

(define-condition query-failed (error)
  ((function :initarg :function :initform NIL)
   (message :initarg :message :initform NIL))
  (:report (lambda (c s) (format s "The time query~@[ for ~a~] failed~@[:~%~%  ~a~]"
                                 (slot-value c 'function) (slot-value c 'message)))))

(defmacro define-protocol-constant (name type &body default)
  (let ((fun (intern (format NIL "%~a" (symbol-name name)))))
    `(progn
       (declaim (ftype (function () (values ,type &optional)) ,fun))
       (declaim (inline ,fun))
       (setf (fdefinition ',fun) (lambda () ,@default))
       (define-symbol-macro ,name (,fun)))))

(defmacro define-constant (name value)
  (let ((fun (intern (format NIL "%~a" (symbol-name name)))))
    `(defun ,fun ()
       (load-time-value ,value))))

(defmacro define-protocol-fun (name args vals &body default)
  `(progn
     (declaim (ftype (function ,(mapcar #'second args) (values ,@vals &optional)) ,name))
     (declaim (inline ,name))
     (setf (fdefinition ',name)
           (lambda ,(mapcar #'first args)
             ,@default))))

(defmacro define-implementation (fun args &body body)
  `(defun ,fun ,args
     (flet ((fail (&optional message)
              (error 'query-failed :function ',fun :message message)))
       (declare (ignorable #'fail))
       ,@body)))

(define-protocol-constant PRECISE-TIME-UNITS-PER-SECOND (unsigned-byte 64)
  INTERNAL-TIME-UNITS-PER-SECOND)

(define-protocol-constant MONOTONIC-TIME-UNITS-PER-SECOND (unsigned-byte 64)
  INTERNAL-TIME-UNITS-PER-SECOND)

(define-protocol-fun get-precise-time () ((unsigned-byte 64) (unsigned-byte 64))
  (values (get-universal-time)
          (mod (get-internal-real-time) INTERNAL-TIME-UNITS-PER-SECOND)))

(define-protocol-fun get-monotonic-time () ((unsigned-byte 64) (unsigned-byte 64))
  (let ((time (get-internal-real-time)))
    (values (truncate time INTERNAL-TIME-UNITS-PER-SECOND)
            (mod time INTERNAL-TIME-UNITS-PER-SECOND))))

(define-protocol-fun get-precise-time/double () (double-float)
  (multiple-value-bind (secs subsecs) (get-precise-time)
    (+ secs (/ (float subsecs 0d0) PRECISE-TIME-UNITS-PER-SECOND))))

(define-protocol-fun get-monotonic-time/double () (double-float)
  (multiple-value-bind (secs subsecs) (get-monotonic-time)
    (+ secs (/ (float subsecs 0d0) MONOTONIC-TIME-UNITS-PER-SECOND))))
