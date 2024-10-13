;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defclass peeking-input-stream (trivial-gray-streams:trivial-gray-stream-mixin
                                trivial-gray-streams:fundamental-binary-input-stream)
  ;; It would be nice to just make wrapped stream a concatenating stream, but
  ;; I'm not confident it would preserve FILE-POSITION semantics on all
  ;; implementations.
  ((wrapped-stream
    :initarg :stream
    :reader wrapped-stream)
   (start-position
    :reader start-position)
   (num-bytes
    :initarg :num-bytes
    :reader num-bytes-peeked)
   (bytes
    :reader peeked-bytes)
   (unread-bytes
    :initarg :num-bytes
    :accessor unread-peeked-bytes))
  (:documentation
   "A stream that makes the first N elements available both via normal read
functions and via PEEKED-BYTES."))

(defmethod initialize-instance :after ((self peeking-input-stream)
                                       &key stream num-bytes
                                         (start-position (ignore-errors (file-position stream))))
  (setf (slot-value self 'start-position) start-position)
  (let ((buffer (make-array num-bytes :element-type '(unsigned-byte 8))))
    (read-sequence buffer stream)
    (setf (slot-value self 'bytes) buffer)))

;; (defmethod trivial-gray-streams:stream-element-type ((stream peeking-input-stream))
;;   '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-file-position ((stream peeking-input-stream))
  (when (start-position stream)
    (if (zerop (unread-peeked-bytes stream))
        (file-position (wrapped-stream stream))
        (+ (- (num-bytes-peeked stream) (unread-peeked-bytes stream))
           (start-position stream)))))

(defmethod (setf trivial-gray-streams:stream-file-position) (val (stream peeking-input-stream))
  (when (start-position stream)
    (if (zerop (unread-peeked-bytes stream))
        (file-position (wrapped-stream stream) val)
        nil)))

(defmethod trivial-gray-streams:stream-read-byte ((stream peeking-input-stream))
  (if (zerop (unread-peeked-bytes stream))
      (read-byte (wrapped-stream stream))
      (prog1 (aref (peeked-bytes stream) (- (length (peeked-bytes stream))
                                            (unread-peeked-bytes stream)))
        (decf (unread-peeked-bytes stream)))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream peeking-input-stream)
                                                      sequence start end
                                                      &key &allow-other-keys)
  (if (zerop (unread-peeked-bytes stream))
      (read-sequence sequence (wrapped-stream stream) :start start :end end)
      (let* ((end (or end (length sequence)))
             (buffer-size (- end start))
             (num-unread-peeked-bytes-remaining (unread-peeked-bytes stream)))
        (setf (subseq sequence start end) (peeked-bytes stream))
        (decf (unread-peeked-bytes stream) buffer-size)
        (if (minusp (unread-peeked-bytes stream))
            (prog1 (read-sequence sequence (wrapped-stream stream)
                                  :start num-unread-peeked-bytes-remaining :end end)
              (setf (unread-peeked-bytes stream) 0))
            (+ start num-unread-peeked-bytes-remaining)))))
