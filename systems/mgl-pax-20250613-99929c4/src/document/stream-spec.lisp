(in-package :mgl-pax)

;;;; Stream specs: multi-write, appending streams with a unified
;;;; interface for strings, files, etc.

(defgeneric make-stream-spec (object &rest args))

(defgeneric unmake-stream-spec (stream-spec))

;;; Call FN with a stream that supports reads (if :DIRECTION is
;;; :INPUT) or writes (if :DIRECTION is :OUTPUT). After the use of a
;;; stream-spec, subsequent writes always append.
(defgeneric call-with-open-stream-spec (stream-spec direction fn))

;;; Release the backing storage: delete the file or forget the string.
(defgeneric delete-stream-spec (stream-spec))

(defmacro with-open-stream-spec ((stream stream-spec &key (direction :input))
                                 &body body)
  `(call-with-open-stream-spec ,stream-spec ,direction
                               (lambda (,stream) ,@body)))


;;;; STRING-STREAM-SPEC

(defclass string-stream-spec ()
  ((string :initform "" :initarg :string :accessor string-stream-spec-string)))

(defmethod make-stream-spec ((object null) &rest args)
  (assert (endp args))
  (make-instance 'string-stream-spec))

(defmethod unmake-stream-spec ((spec string-stream-spec))
  (string-stream-spec-string spec))

(defmethod call-with-open-stream-spec ((spec string-stream-spec)
                                       (direction (eql :input)) fn)
  (funcall fn (make-string-input-stream (string-stream-spec-string spec))))

(defmethod call-with-open-stream-spec ((spec string-stream-spec)
                                       (direction (eql :output)) fn)
  (let ((output-stream (make-string-output-stream)))
    (unwind-protect
         (funcall fn output-stream)
      (setf (string-stream-spec-string spec)
            (concatenate 'string (string-stream-spec-string spec)
                         (get-output-stream-string output-stream))))))

(defmethod delete-stream-spec ((spec string-stream-spec))
  (setf (string-stream-spec-string spec) ""))


;;;; FILE-STREAM-SPEC

(defclass file-stream-spec ()
  ((pathname :initarg :pathname
             :reader file-stream-spec-pathname)
   (open-args :initform () :initarg :open-args
              :reader file-stream-spec-open-args)))

(defmethod print-object ((spec file-stream-spec) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream "~S" (file-stream-spec-pathname spec))))

(defmethod make-stream-spec ((object string) &rest args)
  (make-instance 'file-stream-spec :pathname object
                 ;; Copy ARGS, because we'll call REMF on it.
                 :open-args (copy-list args)))

(defmethod make-stream-spec ((object pathname) &rest args)
  (make-instance 'file-stream-spec :pathname object
                 ;; Copy ARGS, because we'll call REMF on it.
                 :open-args (copy-list args)))

(defmethod unmake-stream-spec ((spec file-stream-spec))
  (file-stream-spec-pathname spec))

(defmethod call-with-open-stream-spec ((spec file-stream-spec) direction fn)
  (let ((open-args (file-stream-spec-open-args spec))
        (pathname (file-stream-spec-pathname spec)))
    (when (getf open-args :ensure-directories-exist)
      (ensure-directories-exist pathname))
    (remf open-args :ensure-directories-exist)
    (unwind-protect
         (with-open-stream (stream (apply #'open pathname :direction direction
                                          open-args))
           (funcall fn stream))
      ;; Subsequent opens must append.
      (loop while (remf (slot-value spec 'open-args) :if-exists))
      (setf (slot-value spec 'open-args)
            (append (list :if-exists :append) (slot-value spec 'open-args))))))

(defmethod delete-stream-spec ((spec file-stream-spec))
  (delete-file (file-stream-spec-pathname spec)))


;;;; STREAM-STREAM-SPEC

(defmethod make-stream-spec ((stream stream) &rest args)
  (assert (endp args))
  stream)

(defmethod unmake-stream-spec ((stream stream))
  stream)

(defmethod call-with-open-stream-spec ((stream stream) direction fn)
  (ecase direction
    ((:input) (assert (input-stream-p stream)))
    ((:output) (assert (output-stream-p stream))))
  (funcall fn stream))


;;;; T

(defmethod make-stream-spec ((spec (eql t)) &rest args)
  (assert (endp args))
  *standard-output*)
