;;;; Gray stream that reads and writes in fixed block sizes.
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(define-condition blocked-stream-error (stream-error)
  ())

(define-condition simple-blocked-stream-error (blocked-stream-error simple-error)
  ())

(defclass blocked-stream (trivial-gray-streams:trivial-gray-stream-mixin
                          trivial-gray-streams:fundamental-binary-stream)
  ((wrapped-stream
    :initarg :stream
    :reader wrapped-stream
    :documentation
    "The underlying stream this wraps.")
   (block-size
    :initarg :block-size
    :initform 512
    :reader block-size
    :documentation
    "The size of the buffer used when reading and/or writing.")
   (start-file-position
    :accessor start-file-position
    :documentation
    "The FILE-POSITION of the WRAPPED-STREAM when this BLOCKED-STREAM is
    instantiated.")
   (offset
    :initform 0
    :accessor stream-offset
    :documentation
    "The number of bytes between the start of the buffer and
START-FILE-POSITION.")
   (index
    :initform 0
    :accessor index
    :documentation
    "The index of the next element to operate on.")
   (buffer-valid-p
    :accessor buffer-valid-p
    :initform nil
    :documentation
    "T iff the BUFFER has been read at the current OFFSET.")
   (buffer
    :accessor buffer
    :documentation
    "The buffer."))
  (:documentation
   "Wraps a binary stream and ensures that all reads from and writes to the
underlying stream occur in blocks of size BLOCK-SIZE. All blocks are aligned
with the position of the wrapped stream when this BLOCKED-STREAM is
instantiated. All FILE-POSITIONs of this stream a relative to the FILE-POSITION
of the wrapped stream when instantiated."))

(defclass blocked-input-stream (blocked-stream
                                trivial-gray-streams:fundamental-binary-input-stream)
  ((eof-index
    :initform nil
    :accessor eof-index
    :documentation
    "The index of EOF or NIL."))
  (:documentation
   "A BLOCKED-STREAM used for input."))

(defclass blocked-output-stream (blocked-stream
                                 trivial-gray-streams:fundamental-binary-output-stream)
  ((dirty-p
    :accessor dirty-p
    :initform nil
    :documentation
    "If non-NIL, the buffer has been modified."))
  (:documentation
   "A BLOCKED-STREAM used for output."))

(defclass blocked-io-stream (blocked-input-stream blocked-output-stream)
  ()
  (:documentation
   "A BLOCKED-STREAM used for both input and output."))

(defmethod initialize-instance :after ((blocked-stream blocked-stream)
                                       &key
                                         stream)
  ;; Create the buffer.
  (setf (buffer blocked-stream) (make-array (block-size blocked-stream)
                                            :element-type '(unsigned-byte 8)
                                            :initial-element 0)
        ;; Record the START-FILE-POSITION
        (start-file-position blocked-stream) (ignore-errors (file-position stream))))

(defgeneric discard-buffer (stream)
  (:documentation
   "Must be called to clean up the current buffer. Resets internal state and
potentially flushes the data to the wrapped stream."))

(defmethod discard-buffer ((stream blocked-stream))
  "Invalidate the buffer."
  (setf (buffer-valid-p stream) nil))

(defmethod discard-buffer ((stream blocked-output-stream))
  "Writes the entire buffer to the WRAPPED-STREAM. Assumes the FILE-POSITION of
the wrapped stream is in the correct place."
  (when (dirty-p stream)
    (write-sequence (buffer stream) (wrapped-stream stream))
    (setf (dirty-p stream) nil))
  (call-next-method))

(defmethod discard-buffer :before ((stream blocked-io-stream))
  "Ensures the FILE-POSITION of the WRAPPED-STREAM is in the correct place for
the buffer to be written."
  (when (dirty-p stream)
    (let ((current-position (file-position (wrapped-stream stream)))
          (desired-position (+ (stream-offset stream) (start-file-position stream))))
      (unless (= current-position desired-position)
        (unless (file-position (wrapped-stream stream) desired-position)
          (error 'simple-blocked-stream-error
                 :stream stream
                 :format-control "Unable to set FILE-POSITION."))))))

(defmethod fill-buffer ((stream blocked-input-stream))
  (let ((real-pos (read-sequence (buffer stream) (wrapped-stream stream)))
        (eof-index nil))
    (unless (= real-pos (block-size stream))
      ;; We've read a partial block before getting an EOF. Fill the remainder
      ;; of the buffer with zeroes.
      (fill (buffer stream) 0 :start real-pos)
      (setf eof-index real-pos))
    (setf (buffer-valid-p stream) t
          (eof-index stream) eof-index)))

(defmethod fill-buffer ((stream blocked-output-stream))
  (setf (buffer-valid-p stream) t)
  (fill (buffer stream) 0))

(defgeneric ensure-buffer-valid (stream)
  (:documentation
   "Ensure STREAM's buffer is valid, given the INDEX of the next operation."))

(defmethod ensure-buffer-valid ((stream blocked-stream))
  (cond
    ;; We haven't read from the current offset, so just fill it.
    ((not (buffer-valid-p stream))
     (fill-buffer stream))
    ;; We're at the end of the current buffer, discard it, reset the pointer to
    ;; the start, increase the offset, and fill it.
    ((= (index stream) (block-size stream))
     (discard-buffer stream)
     (setf (index stream) 0)
     (incf (stream-offset stream) (block-size stream))
     (fill-buffer stream))
    ;; We've moved past the edge of the buffer. Discard it,
    ((> (index stream) (block-size stream))
     (discard-buffer stream)
     (loop :while (> (index stream) (block-size stream))
           :do
              (decf (index stream) (block-size stream))
              (incf (stream-offset stream) (block-size stream)))
     (fill-buffer stream))))

(defmethod trivial-gray-streams:stream-read-byte ((stream blocked-input-stream))
  (ensure-buffer-valid stream)
  (with-accessors ((eof-index eof-index)
                   (index index)
                   (block-size block-size)
                   (buffer buffer))
      stream
    (if (and (not (null eof-index))
             (>= index eof-index))
        :eof
        (prog1 (aref buffer index)
          (incf index)))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream blocked-input-stream)
                                                      sequence start end
                                                      &key &allow-other-keys)
  (ensure-buffer-valid stream)
  (let ((num-bytes (- (or end (length sequence)) start))
        (num-bytes-remaining (- (or (eof-index stream) (block-size stream))
                                (index stream))))
    (replace sequence (buffer stream)
             :start1 start :end1 end
             :start2 (index stream) :end2 (eof-index stream))
    (incf (index stream) (min num-bytes num-bytes-remaining))
    (if (<= num-bytes num-bytes-remaining)
        (+ num-bytes start)
        (if (null (eof-index stream))
            (trivial-gray-streams:stream-read-sequence stream sequence
                                                       (+ start num-bytes-remaining) end)
            (+ num-bytes-remaining start)))))

(defmethod trivial-gray-streams:stream-write-byte ((stream blocked-output-stream) byte)
  (ensure-buffer-valid stream)
  (setf (dirty-p stream) t)
  (with-accessors ((index index)
                   (buffer buffer))
      stream
    (setf (aref buffer index) byte)
    (incf index)
    byte))

(defmethod trivial-gray-streams:stream-write-sequence ((stream blocked-output-stream)
                                                       sequence start end
                                                       &key &allow-other-keys)
  (ensure-buffer-valid stream)
  (setf (dirty-p stream) t)
  (let ((num-bytes (- (or end (length sequence)) start))
        (num-bytes-remaining (- (block-size stream) (index stream))))
    (replace (buffer stream) sequence
             :start1 (index stream)
             :start2 start :end2 end)
    (incf (index stream) (min num-bytes num-bytes-remaining))
    (if (<= num-bytes num-bytes-remaining)
        sequence
        (trivial-gray-streams:stream-write-sequence stream sequence
                                                    (+ start num-bytes-remaining) end))))

(defmethod trivial-gray-streams:stream-file-position ((stream blocked-stream))
  (+ (index stream) (stream-offset stream)))

(defmethod stream-element-type ((stream blocked-stream))
  (stream-element-type (wrapped-stream stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (newval (stream blocked-stream))
  (multiple-value-bind (chunk-number new-index)
      (floor newval (block-size stream))
    (let ((start-of-chunk-position (* chunk-number (block-size stream))))
      (cond
        ((and (= start-of-chunk-position (stream-offset stream))
              (or (null (eof-index stream))
                  (< new-index (eof-index stream))))
         ;; We've been asked to seek to a position already within our buffer
         ;; *and* is not beyond the EOF.
         (setf (index stream) new-index)
         t)
        ((= start-of-chunk-position (stream-offset stream))
         ;; We've been asked to seek beyond the EOF.
         (error 'simple-blocked-stream-error
                :stream stream
                :format-control "Attempted to move beyond the end of the stream."))
        ((and (null (start-file-position stream))
              (> start-of-chunk-position (stream-offset stream)))
         ;; We can't use FILE-POSITION directly to seek because we couldn't
         ;; determine the starting FILE-POSITION of the wrapped stream. However,
         ;; we want to seek forward, so we can just read blocks until we get
         ;; there.
         ;; (discard-buffer stream)
         (setf (index stream) (- newval (stream-offset stream)))
         (ensure-buffer-valid stream)
         t)
        ((null (start-file-position stream))
         ;; We weren't able to figure out the start position of the wrapped
         ;; stream and we're seeking backward. Nothing we can do.
         nil)
        (t
         (discard-buffer stream)
         (if (file-position (wrapped-stream stream) (+ (start-file-position stream)
                                                       start-of-chunk-position))
             (progn
               (setf (index stream) new-index
                     (stream-offset stream) start-of-chunk-position)
               t)
             (when (> start-of-chunk-position (stream-offset stream))
               (setf (index stream) (+ start-of-chunk-position new-index))
               (ensure-buffer-valid stream)
               t)))))))

(defmethod close ((stream blocked-stream) &key abort)
  (unless abort
    (discard-buffer stream))
  (call-next-method))
