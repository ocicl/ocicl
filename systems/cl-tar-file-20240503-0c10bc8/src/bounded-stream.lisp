;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defclass bounded-input-stream (trivial-gray-streams:trivial-gray-stream-mixin
                                trivial-gray-streams:fundamental-binary-input-stream)
  ((%position :accessor %position :initarg :position)
   (wrapped-stream :reader wrapped-stream :initarg :stream)
   (n-bytes-remaining :accessor n-bytes-remaining :initarg :bytes))
  (:default-initargs :bytes 0))

(defun make-bounded-stream (stream size &optional start-position)
  (make-instance 'bounded-input-stream :stream stream :bytes size
                                       :position (or start-position (file-position stream))))

#+nil
(defmethod trivial-gray-streams:stream-element-type ((stream bounded-input stream))
  '(unsigned-byte 8))

(defmethod ensure-file-position ((stream bounded-input-stream))
  (let ((new-position (file-position (wrapped-stream stream) (%position stream))))
    (unless new-position
      (error "Unable to set FILE-POSITION."))))

(defmethod trivial-gray-streams:stream-read-byte ((stream bounded-input-stream))
  (if (zerop (n-bytes-remaining stream))
      :eof
      (progn
        (ensure-file-position stream)
        (prog1 (read-byte (wrapped-stream stream))
          (incf (%position stream))
          (decf (n-bytes-remaining stream))))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream bounded-input-stream)
                                                      sequence start end
                                                      &key &allow-other-keys)
  (if (zerop (n-bytes-remaining stream))
      start
      (progn
        (ensure-file-position stream)
        (let ((new-end (read-sequence sequence (wrapped-stream stream)
                                      :start start :end (min (or end (length sequence))
                                                             (+ start
                                                                (n-bytes-remaining stream))))))
          (incf (%position stream) (- new-end start))
          (decf (n-bytes-remaining stream) (- new-end start))
          new-end))))
