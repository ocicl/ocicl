(in-package :archive)

(defclass bounded-input-stream (trivial-gray-streams:trivial-gray-stream-mixin
                                trivial-gray-streams:fundamental-binary-input-stream)
  ((wrapped-stream :reader wrapped-stream :initarg :stream)
   (n-bytes-remaining :accessor n-bytes-remaining :initarg :bytes))
  (:default-initargs :bytes 0))

(defun make-bounded-stream (stream size)
  (make-instance 'bounded-input-stream :stream stream :bytes size))

#+nil
(defmethod trivial-gray-streams:stream-element-type ((stream bounded-input stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte ((stream bounded-input-stream))
  (if (zerop (n-bytes-remaining stream))
      :eof
      (prog1 (read-byte (wrapped-stream stream))
        (decf (n-bytes-remaining stream)))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream bounded-input-stream)
                                                      sequence start end
                                                      &key &allow-other-keys)
  (let ((new-end (read-sequence sequence (wrapped-stream stream)
                                :start start :end (min (or end (length sequence))
                                                       (+ start
                                                          (n-bytes-remaining stream))))))
    (decf (n-bytes-remaining stream) (- new-end start))
    new-end))
      
