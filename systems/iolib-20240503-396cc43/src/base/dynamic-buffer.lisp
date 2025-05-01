;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Read/write adjustable buffer.
;;;

(in-package :iolib/base)

(defclass dynamic-buffer ()
  ((sequence     :initform nil :accessor sequence-of)
   (read-cursor  :initform 0   :accessor read-cursor-of)
   (write-cursor :initform 0   :accessor write-cursor-of)
   (growth-size  :initarg :growth-size :accessor growth-size-of))
  (:default-initargs :growth-size 3/2))

(defmethod initialize-instance :after ((buffer dynamic-buffer)
                                       &key (size 256) sequence (start 0) end)
  (etypecase sequence
    (null
     (setf (sequence-of buffer) (make-array size :element-type 'ub8)))
    (ub8-vector
     (check-bounds sequence start end)
     (let* ((sequence-size (- end start))
            (newseq (make-array sequence-size :element-type 'ub8)))
       (replace newseq sequence :start2 start :end2 end)
       (setf (sequence-of buffer)     newseq
             (write-cursor-of buffer) sequence-size)))))

(defmethod print-object ((buffer dynamic-buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (let ((*print-length* 40))
      (format stream "Size: ~A RC: ~A WC: ~A Contents: ~S"
              (size-of buffer)
              (read-cursor-of buffer)
              (write-cursor-of buffer)
              (sequence-of buffer)))))

(defgeneric size-of (buffer)
  (:method ((buffer dynamic-buffer))
    (length (sequence-of buffer))))

(declaim (inline ub16-to-vector))
(defun ub16-to-vector (value)
  (vector (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(declaim (inline ub32-to-vector))
(defun ub32-to-vector (value)
  (vector (ldb (byte 8 32) value)
          (ldb (byte 8 16) value)
          (ldb (byte 8 8) value)
          (ldb (byte 8 0) value)))

(defun maybe-grow-buffer (buffer request-size)
  (with-accessors ((seq sequence-of)
                   (size size-of)
                   (wcursor write-cursor-of)
                   (growth-size growth-size-of))
      buffer
    (when (< size (+ wcursor request-size))
      (let ((newsize (* growth-size (+ size request-size))))
        (setf seq (adjust-array seq newsize)))))
  (values buffer))

(defun write-vector (buffer vector &optional (start 0) end)
  (check-bounds vector start end)
  (let ((request-size (- end start)))
    (maybe-grow-buffer buffer request-size)
    (with-accessors ((seq sequence-of)
                     (wcursor write-cursor-of))
        buffer
      (replace seq vector :start1 wcursor :start2 start :end2 end)
      (incf wcursor request-size)))
  (values buffer))

(declaim (inline write-ub8))
(defun write-ub8 (buffer value)
  (write-vector buffer (vector value)))

(declaim (inline write-ub16))
(defun write-ub16 (buffer value)
  (write-vector buffer (ub16-to-vector value)))

(declaim (inline write-ub32))
(defun write-ub32 (buffer value)
  (write-vector buffer (ub32-to-vector value)))

(define-condition dynamic-buffer-input-error (error)
  ((buffer :initform (error "Must supply buffer")
           :initarg :buffer :reader buffer-of)))

(define-condition dynamic-buffer-eof (dynamic-buffer-input-error)
  ((octets-requested :initarg :requested :reader octets-requested-of)
   (octets-remaining :initarg :remaining :reader octets-remaining-of))
  (:report (lambda (condition stream)
             (format stream "You requested ~A octets but only ~A are left in the buffer"
                     (octets-requested-of condition)
                     (octets-remaining-of condition))))
  (:documentation
   "Signals that an INPUT-BUFFER contains less unread bytes than requested."))

(define-condition dynamic-buffer-index-out-of-bounds (dynamic-buffer-input-error)
  ((index :initarg :index :reader index-of))
  (:report (lambda (condition stream)
             (format stream "Trying to access ~A at invalid index ~A"
                     (buffer-of condition)
                     (index-of condition))))
  (:documentation
   "Signals that SEEK-READ-CURSOR on an INPUT-BUFFER was passed an invalid index."))

(declaim (inline seek-read-cursor))
(defun seek-read-cursor (buffer index)
  (check-type index unsigned-byte "an unsigned-byte")
  (if (>= index (size-of buffer))
      (error 'dynamic-buffer-index-out-of-bounds :buffer buffer :index index)
      (setf (read-cursor-of buffer) index)))

(declaim (inline unread-bytes))
(defun unread-bytes (buffer)
  (- (write-cursor-of buffer) (read-cursor-of buffer)))

(defun read-vector (buffer length)
  (with-accessors ((seq sequence-of)
                   (rcursor read-cursor-of))
      buffer
    (let* ((bytes-to-read (min (unread-bytes buffer) length))
           (newvector (make-array bytes-to-read :element-type 'ub8)))
      (replace newvector seq :start2 rcursor)
      (incf rcursor bytes-to-read)
      (values newvector))))

(defmacro read-ub-be (vector position &optional (length 1))
  `(+ ,@(loop :for i :below length
              :collect `(ash (aref ,vector (+ ,position ,i))
                             ,(* (- length i 1) 8)))))

(declaim (inline read-ub16-from-vector))
(defun read-ub16-from-vector (vector position)
  (read-ub-be vector position 2))

(declaim (inline read-ub32-from-vector))
(defun read-ub32-from-vector (vector position)
  (read-ub-be vector position 4))

(declaim (inline check-if-enough-bytes))
(defun check-if-enough-bytes (buffer length)
  (let ((remaining-bytes (unread-bytes buffer)))
    (when (< remaining-bytes length)
      (error 'dynamic-buffer-eof
             :buffer buffer
             :requested length
             :remaining remaining-bytes))))

(declaim (inline read-ub8))
(defun read-ub8 (buffer)
  (check-if-enough-bytes buffer 1)
  (prog1
      (aref (sequence-of buffer) (read-cursor-of buffer))
    (incf (read-cursor-of buffer))))

(declaim (inline read-ub16))
(defun read-ub16 (buffer)
  (check-if-enough-bytes buffer 2)
  (prog1
      (read-ub16-from-vector (sequence-of buffer) (read-cursor-of buffer))
    (incf (read-cursor-of buffer) 2)))

(declaim (inline read-ub32))
(defun read-ub32 (buffer)
  (check-if-enough-bytes buffer 4)
  (prog1
      (read-ub32-from-vector (sequence-of buffer) (read-cursor-of buffer))
    (incf (read-cursor-of buffer) 4)))
