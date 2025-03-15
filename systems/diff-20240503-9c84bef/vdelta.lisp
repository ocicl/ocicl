;;;; vdelta.lisp - computing copy/insert deltas with the vdelta algorithm

(in-package :diff)

(defconstant +buffer-size+ 1024)

(deftype op-kind () '(member :copy-target :copy-source :new-data))
(deftype bytebuf () '(simple-array (unsigned-byte 8) (*)))

;;; a convenience class for writing byte buffers
(defclass byte-buffer-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((buffer :accessor buffer :initarg :buffer :type bytebuf)
   (index :accessor index :initform 0 :type fixnum)))

(defmethod trivial-gray-streams:stream-write-byte ((stream byte-buffer-stream) byte)
  (with-slots (buffer index) stream
    (when (>= index (length buffer))
      (error "Cannot write any more data to stream!"))
    (setf (aref buffer index) byte)
    (incf index)
    byte))

(defmacro with-binary-file ((stream-name pathname direction) &body body)
  "A wrapper for WITH-OPEN-FILE that opens the stream with an element-type
of (UNSIGNED-BYTE 8).  DIRECTION is passed as the argument to :DIRECTION."
  `(with-open-file (,stream-name ,pathname
                    :direction ,direction
                    :element-type '(unsigned-byte 8))
    ,@body))

(defstruct svndiff-window
  (source-offset 0 :type (unsigned-byte 32))
  (source-len 0 :type (unsigned-byte 32))
  (target-len 0 :type (unsigned-byte 32))
  (ops (error "required argument") :type bytebuf)
  (new-data (error "required argument") :type bytebuf))

(defstruct (svndiff-op
             (:constructor make-svndiff-op (kind offset bytes)))
  (kind :new-data :type op-kind)
  (offset 0 :type (unsigned-byte 32))
  (bytes 0 :type (unsigned-byte 32)))

(defun match-length (seq1 start1 end1 seq2 start2 end2)
  (declare (type bytebuf seq1 seq2))
  (declare (type fixnum start1 end1 start2 end2))
  (do ((index1 start1 (1+ index1))
       (index2 start2 (1+ index2)))
      ((or (= index1 end1) (= index2 end2))
       (- index1 start1))
    (declare (type fixnum index1 index2))
    (when (/= (aref seq1 index1) (aref seq2 index2))
      (return (- index1 start1)))))

(defun string-to-byteseq (string)
  "Convert STRING to an array of (UNSIGNED-BYTE 8).  Assumes that the
Common Lisp implementation is underpinned with 8-bit characters."
  (let ((buf (make-array (length string)
                         :element-type '(unsigned-byte 8))))
    (map-into buf #'char-code string)))


;;; vdelta calculation

(declaim (inline combine-bytes))
(defun combine-bytes (byteseq start)
  (declare (type bytebuf byteseq))
  (let ((int 0))
    (dotimes (i 4)
      (setf int (logior int
                        (ash (aref byteseq (+ start i))
                             (* 8 i)))))
    int))

(defclass vdelta-context ()
  ((buffer :initform (make-array (* 2 +buffer-size+) :element-type '(unsigned-byte 8))
           :reader buffer)
   (source-start :initform 0
                 :accessor source-start)
   (source-length :initform 0
                  :accessor source-length)
   (target-start :initform 0
                 :accessor target-start)
   (target-length :initform 0
                  :accessor target-length)
   (table :initform (make-hash-table :test #'eql)
          :accessor table)))

(defun key-to-chars (key)
  (with-output-to-string (stream)
    (flet ((char-at (pos)
             (code-char (ldb (byte 8 pos) key))))
      (format stream "~A~A~A~A"
              (char-at 0) (char-at 8) (char-at 16) (char-at 24)))))

(defun print-match-table (table)
  (with-hash-table-iterator (fn table)
    (loop
      (multiple-value-bind (more-p key value) (fn)
        (unless more-p (return))
        (format t "~A: ~A~%" (key-to-chars key) value)))))

(defun initialize-match-table (context)
  (let ((buffer (buffer context))
        (start (source-start context))
        (length (source-length context))
        (table (table context)))
    (declare (type bytebuf buffer))
    (do ((i start (1+ i)))
        ((>= i (- length 3)) (print-match-table table))
      (let ((key (combine-bytes buffer i)))
        (multiple-value-bind (index presentp) (gethash key table)
          (if (not presentp)
              (setf (gethash key table) i)
              (let ((match-length (match-length buffer i length
                                                buffer index length)))
                (dotimes (j 3)
                  (let ((insert-index (+ start (- match-length 1 j))))
                    (setf (gethash (combine-bytes buffer insert-index)
                                   table)
                          insert-index)))
                (assert (> match-length 0))
                (incf i match-length))))))))
     
(defun calculate-svndiff-ops (context)
  (let ((buffer (buffer context))
        (source-start (source-start context))
        (source-length (source-length context))
        (target-start (target-start context))
        (target-length (target-length context))
        (target-end (+ (target-start context) (target-length context)))
        (table (table context))
        (add-start -1)
        (add-length 0)
        (instructions nil))
    (declare (type bytebuf buffer)
             (type fixnum source-length target-length add-start add-length))
    (flet ((push-new-data (index)
             (when (= -1 add-start)
               (setf add-start index))
             (incf add-length))
           (add-data-insn ()
             (when (not (= -1 add-start))
               (push (make-svndiff-op :new-data add-start add-length)
                     instructions)
               (setf add-start -1 add-length 0)))
           (add-copy-insn (index len)
             (let* ((targetp (>= index source-length))
                    (op-kind (if targetp :copy-target :copy-source))
                    (offset index))
               (push (make-svndiff-op op-kind offset len)
                     instructions))))
      (do ((i target-start (1+ i)))
          ((>= i target-end))
        ;; pick up any stragglers at the end of the string
        (cond
          ((<= (- target-end 3) i) (push-new-data i))
          (t
           (let ((key (combine-bytes buffer i)))
             (format t "Key: ~A~%" (key-to-chars key))
             (multiple-value-bind (index presentp) (gethash key table)
               (if (not presentp)
                   (progn
                     (push-new-data i)
                     ;; record a new position index
                     (setf (gethash key table) i))
                   (flet ((find-diff-loc ()
                            (if (>= index source-length)
                                ;; a index in version2
                                (values (match-length buffer i target-end
                                                      buffer index target-end)
                                        t)
                                (values (match-length buffer index source-length
                                                      buffer i target-end)
                                        nil))))
                     (format t "i: ~A, index: ~A~%" i index)
                     ;; clear any pending additions
                     (add-data-insn)
                     (multiple-value-bind (match-length targetp) (find-diff-loc)
                       (format t "match-length: ~A, ~A~%" match-length targetp)
                       (assert (> match-length 0))
                       (when targetp
                         (dotimes (j 3)
                           (let ((insert-index (+ target-start
                                                  (- match-length j))))
                             (assert (>= insert-index target-start))
                             (setf (gethash (combine-bytes buffer insert-index)
                                            table)
                                   insert-index))))
                       (add-copy-insn index match-length)
                       (incf i (1- match-length))))))))))
      (add-data-insn)
      (nreverse instructions))))