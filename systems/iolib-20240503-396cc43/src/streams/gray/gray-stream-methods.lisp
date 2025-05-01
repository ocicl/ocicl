;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Implementation using Gray streams.
;;;

(in-package :iolib/streams)

;;;-------------------------------------------------------------------------
;;; Instance Initialization
;;;-------------------------------------------------------------------------

(defun free-stream-buffers (ibuf obuf)
  (when ibuf (free-iobuf ibuf))
  (when obuf (free-iobuf obuf)))

;;; TODO: use the buffer pool
;;; TODO: handle instance reinitialization
(defmethod shared-initialize :after ((stream dual-channel-gray-stream) slot-names
                                     &key external-format input-buffer-size output-buffer-size)
  (declare (ignore slot-names))
  (let ((external-format (or external-format :default))
        (input-buffer-size (or input-buffer-size +bytes-per-iobuf+))
        (output-buffer-size (or output-buffer-size +bytes-per-iobuf+)))
   (check-type input-buffer-size buffer-index)
   (check-type output-buffer-size buffer-index)
   (with-accessors ((ibuf input-buffer-of)
                    (obuf output-buffer-of)
                    (ef external-format-of))
       stream
     (setf ibuf (allocate-iobuf input-buffer-size)
           obuf (allocate-iobuf output-buffer-size)
           ef external-format))))


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defmethod print-object ((o dual-channel-gray-stream) s)
  (with-slots (fd (ef external-format) (ib input-buffer) (ob output-buffer))
      o
    (print-unreadable-object (o s :type nil :identity t)
      (if fd
          (format s "~A ~S ~S ~S ~S/~S ~S ~S/~S ~S (~S ~S ~S)"
                  (type-of o) :fd fd
                  :ibuf (iobuf-length ib) (iobuf-size ib)
                  :obuf (iobuf-length ob) (iobuf-size ob)
                  :ef (babel-encodings:enc-name (babel:external-format-encoding ef))
                  :eol-style (babel:external-format-eol-style ef))
          (format s "~A ~A ~S (~S ~S ~S)"
                  (type-of o) :closed
                  :ef (babel-encodings:enc-name (babel:external-format-encoding ef))
                  :eol-style (babel:external-format-eol-style ef))))))


;;;-------------------------------------------------------------------------
;;; Common Methods
;;;-------------------------------------------------------------------------

(defmethod stream-element-type ((stream dual-channel-gray-stream))
  '(unsigned-byte 8))

;; TODO: use the buffer pool
(defmethod close :before ((stream dual-channel-gray-stream) &key abort)
  (with-accessors ((ibuf input-buffer-of)
                   (obuf output-buffer-of))
      stream
    (unless (or abort (null obuf))
      (finish-output stream))
    (free-stream-buffers ibuf obuf)
    (setf ibuf nil obuf nil)))

(defmethod (setf external-format-of)
    (external-format (stream dual-channel-gray-stream))
  (let ((canonical-ef (babel:ensure-external-format external-format)))
    (setf (slot-value stream 'external-format) canonical-ef)
    (setf (slot-value stream 'eol-writer)
          (case (babel:external-format-eol-style canonical-ef)
            (:lf   #'stream-write-lf)
            (:crlf #'stream-write-crlf)
            (:cr   #'stream-write-cr)))
    (setf (values (slot-value stream 'eol-finder)
                  (slot-value stream 'eol-finder/no-hang))
          (case (babel:external-format-eol-style canonical-ef)
            (:lf   (values #'stream-find-lf   #'stream-find-lf/no-hang))
            (:crlf (values #'stream-find-crlf #'stream-find-crlf/no-hang))
            (:cr   (values #'stream-find-cr   #'stream-find-cr/no-hang))))))


;;;-------------------------------------------------------------------------
;;; Input Methods
;;;-------------------------------------------------------------------------

(defmethod stream-clear-input ((stream dual-channel-gray-stream))
  (iobuf-reset (input-buffer-of stream)))

(declaim (inline %read-sequence))
(defun %read-sequence (stream seq start end)
  (check-bounds seq start end)
  (if (= start end)
      start
      (etypecase seq
        (ub8-sarray (%read-into-simple-array-ub8 stream seq start end))
        (string     (%read-into-string stream seq start end))
        (ub8-vector (%read-into-vector stream seq start end)))))

(declaim (inline read-sequence*))
(defun read-sequence* (stream sequence &key (start 0) end)
  (%read-sequence stream sequence start end))

(defmethod stream-read-sequence
    ((stream dual-channel-gray-stream) sequence start end &key)
  (%read-sequence stream sequence start end))

(defmethod drain-input-buffer
    ((stream dual-channel-gray-stream) sequence &key (start 0) end)
  (check-bounds sequence start end)
  (with-accessors ((ib input-buffer-of))
      stream
    (let ((nbytes (min (- end start)
                       (iobuf-length ib))))
      (when (plusp nbytes)
        (iobuf-copy-into-lisp-array ib (iobuf-start ib)
                                    sequence start
                                    nbytes)
        (incf (iobuf-start ib) nbytes)
        (let ((len (iobuf-length ib)))
          (values (+ start nbytes)
                  (and (plusp len) len)))))))


;;;-------------------------------------------------------------------------
;;; Output Methods
;;;-------------------------------------------------------------------------

(defmethod stream-clear-output ((stream dual-channel-gray-stream))
  (iobuf-reset (output-buffer-of stream))
  (setf (dirtyp stream) nil))

(defmethod stream-finish-output ((stream dual-channel-gray-stream))
  (with-accessors ((fd fd-of)
                   (write-fn write-fn-of)
                   (ob output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (with-hangup-guard stream
      (%write-octets-from-iobuf write-fn fd ob))
    (setf dirtyp nil)))

(defmethod stream-force-output ((stream dual-channel-gray-stream))
  (with-accessors ((fd fd-of)
                   (write-fn write-fn-of)
                   (ob output-buffer-of)
                   (dirtyp dirtyp))
      stream
    (with-hangup-guard stream
      (%write-octets-from-iobuf write-fn fd ob t))
    (unless (iobuf-empty-p ob)
      (setf dirtyp t))))

(declaim (inline %write-sequence))
(defun %write-sequence (stream seq start end)
  (check-bounds seq start end)
  (if (= start end)
      seq
      (etypecase seq
        (ub8-sarray (%write-simple-array-ub8 stream seq start end))
        (string     (stream-write-string stream seq start end))
        (ub8-vector (%write-vector-ub8 stream seq start end))
        (vector     (%write-vector stream seq start end)))))

(declaim (inline write-sequence*))
(defun write-sequence* (stream sequence &key (start 0) end)
  (%write-sequence stream sequence start end))

(defmethod stream-write-sequence ((stream dual-channel-gray-stream)
                                  sequence start end &key)
  (%write-sequence stream sequence start end))


;;;-------------------------------------------------------------------------
;;; Character Input
;;;-------------------------------------------------------------------------

(defun %stream-rewind-iobuf (stream iobuf encoding)
  (maybe-rewind-iobuf iobuf encoding)
  (setf (unread-index-of stream) (iobuf-start iobuf)))

(defmethod stream-read-char ((stream dual-channel-gray-stream))
  (with-accessors ((fd fd-of)
                   (ib input-buffer-of)
                   (read-fn read-fn-of)
                   (unread-index unread-index-of)
                   (ef external-format-of))
      stream
    (let ((encoding (babel:external-format-encoding ef)))
      (%stream-rewind-iobuf stream ib encoding)
      (cond
        ((and (iobuf-empty-p ib)
              (eql :eof (%fill-ibuf ib fd read-fn)))
         :eof)
        (t
         ;; At this point, there's at least one octet in the buffer
         (debug-only (assert (not (iobuf-empty-p ib))))
         (let ((line-end (funcall (eol-finder-of stream) ib fd read-fn)))
           (if (eql #\Newline line-end)
               #\Newline
               (decode-one-char fd read-fn ib encoding))))))))

(defmethod stream-read-char-no-hang ((stream dual-channel-gray-stream))
  (with-accessors ((fd fd-of)
                   (read-fn read-fn-of)
                   (ib input-buffer-of)
                   (ef external-format-of))
      stream
    (let ((encoding (babel:external-format-encoding ef)))
      (%stream-rewind-iobuf stream ib encoding)
      (when (iobuf-empty-p ib)
        (let ((nbytes (%fill-ibuf/no-hang ib fd read-fn)))
          (cond
            ((eql :eof nbytes) (return* :eof))
            ((zerop nbytes)    (return* nil)))))
      ;; At this point, there's at least one octet in the buffer
      (debug-only (assert (not (iobuf-empty-p ib))))
      (let ((line-end (funcall (eol-finder/no-hang-of stream) ib fd read-fn)))
        (case line-end
          ((nil) (decode-one-char/no-hang ib encoding))
          (#\Newline #\Newline)
          ;; There's a CR but it's not EOF so we could still receive a LF
          (:incomplete nil))))))

(defun %stream-unread-char (stream)
  (declare (type dual-channel-gray-stream stream))
  (with-accessors ((ib input-buffer-of)
                   (unread-index unread-index-of))
      stream
    (symbol-macrolet ((start (iobuf-start ib)))
      (cond
        ((> start unread-index)
         (setf start unread-index))
        ((= start unread-index)
         (error 'no-characters-to-unread :stream stream))
        (t (bug "On stream ~S the buffer start(~A) is less than the unread index(~A)."
                stream start unread-index)))))
  nil)

(defmethod stream-unread-char ((stream dual-channel-gray-stream) character)
  (declare (ignore character))
  (%stream-unread-char stream))

(defmethod stream-peek-char ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char stream)))
    (cond ((eql :eof char)
           :eof)
          (t
           (%stream-unread-char stream)
           char))))

;; (defmethod stream-read-line ((stream dual-channel-gray-stream))
;;   )

(defmethod stream-listen ((stream dual-channel-gray-stream))
  (let ((char (stream-read-char-no-hang stream)))
    (cond ((characterp char)
           (stream-unread-char stream char)
           t)
          (t nil))))


;;;-------------------------------------------------------------------------
;;; Character Output
;;;-------------------------------------------------------------------------

(defmethod stream-write-char ((stream dual-channel-gray-stream)
                              (character character))
  (%flush-obuf-if-needed stream)
  (if (char= character #\Newline)
      (funcall (eol-writer-of stream) stream)
      (let ((string (make-string 1 :initial-element character)))
        (declare (dynamic-extent string))
        (stream-write-string stream string))))

(defmethod stream-line-column ((stream dual-channel-gray-stream))
  0)

(defmethod stream-start-line-p ((stream dual-channel-gray-stream))
  nil)

(defmethod stream-terpri ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) nil)

(defmethod stream-fresh-line ((stream dual-channel-gray-stream))
  (write-char #\Newline stream) t)

(defmethod stream-write-string ((stream dual-channel-gray-stream)
                                (string string) &optional (start 0) end)
  (check-bounds string start end)
  (do* ((ef (external-format-of stream))
        (encoding (babel:external-format-encoding ef)))
      ((= start end))
    (case (char string start)
      (#\Newline
       (funcall (eol-writer-of stream) stream)
       (incf start))
      (t
       (setf start (%write-string-chunk stream string start end encoding)))))
  string)


;;;-------------------------------------------------------------------------
;;; Binary Input
;;;-------------------------------------------------------------------------

(defmethod stream-read-byte ((stream dual-channel-gray-stream))
  (with-accessors ((fd fd-of)
                   (read-fn read-fn-of)
                   (ib input-buffer-of))
      stream
    (flet ((fill-buf-or-eof ()
             (iobuf-reset ib)
             (when (eql :eof (%fill-ibuf ib fd read-fn))
               (return* :eof))))
      (when (zerop (iobuf-length ib))
        (fill-buf-or-eof))
      (iobuf-pop-octet ib))))


;;;-------------------------------------------------------------------------
;;; Binary Output
;;;-------------------------------------------------------------------------

(defmethod stream-write-byte ((stream dual-channel-gray-stream) integer)
  (check-type integer ub8 "an unsigned 8-bit value")
  (with-accessors ((ob output-buffer-of))
      stream
    (with-hangup-guard stream
      (%flush-obuf-if-needed stream))
    (iobuf-push-octet ob integer)))


;;;-------------------------------------------------------------------------
;;; Buffer-related functions
;;;-------------------------------------------------------------------------

(defmethod input-buffer-size ((stream dual-channel-gray-stream))
  (iobuf-length (input-buffer-of stream)))

(defmethod input-buffer-empty-p ((stream dual-channel-gray-stream))
  (iobuf-empty-p (input-buffer-of stream)))

(defmethod output-buffer-size ((stream dual-channel-gray-stream))
  (iobuf-length (output-buffer-of stream)))

(defmethod output-buffer-empty-p ((stream dual-channel-gray-stream))
  (iobuf-empty-p (output-buffer-of stream)))
