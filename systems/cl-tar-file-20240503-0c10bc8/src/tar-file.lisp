;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defvar *type-detectors* nil
  "A list of functions, that when called with a header buffer must return a
  symbol naming the type of tar-file that the header belongs to, or NIL.")

(defparameter *default-type* 'v7-tar-file
  "The default tar-file type if no detectors register a hit.")

(defun register-type-detector (f)
  (pushnew f *type-detectors*))

(defun detect-type (buffer)
  (or (some (lambda (f) (funcall f buffer)) *type-detectors*)
      *default-type*))

(defclass tar-file ()
  ((direction
    :initarg :direction
    :reader %tar-file-direction
    :type (member :input :output))
   (open-tar-file-p
    :initform t
    :accessor open-tar-file-p)
   (stream
    :initarg :stream
    :reader tar-file-stream
    :type stream)
   (other-streams-to-close
    :initarg :other-streams-to-close
    :reader tar-file-other-streams-to-close
    :type list)
   (next-entry-start
    :accessor next-entry-start
    :type integer
    :initform 0)
   (header-encoding
    :initform *default-header-encoding*
    :initarg :header-encoding
    :accessor header-encoding))
  (:documentation
   "Base class of a tar file."))

(defgeneric header-type (tar-file)
  (:documentation
   "Given a tar-file, return a symbol naming the header class."))

(defgeneric entry-type (tar-file header)
  (:documentation
   "Return a symbol naming the class to use to represent the entry for HEADER in TAR-FILE."))

(defun make-compression-stream (stream direction compression)
  (ecase compression
    (:gzip
     (ecase direction
       (:input (chipz:make-decompressing-stream 'chipz:gzip stream))
       (:output (salza2:make-compressing-stream 'salza2:gzip-compressor stream))))
    (:auto
     (ecase direction
       (:output
        (let ((file-name (ignore-errors (pathname stream))))
          (if (null file-name)
              stream
              (let ((type (pathname-type file-name)))
                (if (or (null type) (not (uiop:string-suffix-p type "gz")))
                    stream
                    (make-compression-stream stream direction :gzip))))))
       (:input
        (let ((peeking-stream (make-instance 'peeking-input-stream :stream stream
                                                                   :num-bytes 3)))
          (if (and (= (aref (peeked-bytes peeking-stream) 0) #x1f)
                   (= (aref (peeked-bytes peeking-stream) 1) #x8b)
                   (= (aref (peeked-bytes peeking-stream) 2) #x08))
              (values (chipz:make-decompressing-stream 'chipz:gzip peeking-stream) (list peeking-stream))
              peeking-stream)))))
    ((nil)
     stream)))

(defun open-tar-file (stream &key (direction :input)
                               (type :auto)
                               (blocking-factor 20)
                               (header-encoding *default-header-encoding*)
                               (compression :auto))
  "Create a TAR-FILE object backed by STREAM. The STREAM should not be read
from or written to any more.

DIRECTION is either :INPUT or :OUTPUT.

BLOCKING-FACTOR is an integer that specifies how many 512-byte blocks should be
read from or written to STREAM at any one time.

TYPE is either AUTO or a class designator for a subclass of TAR-FILE. If :AUTO,
the appropriate class will be determined by looking at the first tar header.

HEADER-ENCODING is an encoding specifier recognized by Babel.

COMPRESSION determines what compression scheme is used, if any. It can be
either :AUTO (the default), NIL (no compression), or :GZIP. If :AUTO, the
compression type is determined using the PATHNAME of the stream (for :OUTPUT)
or by peeking at the stream for magic numbers (for :INPUT)."
  (declare (type (member :input :output) direction))
  (check-type compression (member :gzip :auto nil))
  (multiple-value-bind
        (compression-stream other-streams-to-close)
      (make-compression-stream stream direction compression)
    (let ((blocked-stream (make-instance (ecase direction
                                           (:input 'blocked-input-stream)
                                           (:output 'blocked-output-stream))
                                         :stream compression-stream
                                         :block-size (* +tar-n-block-bytes+ blocking-factor))))
      (flet ((read-buffer ()
               (let ((buffer (make-array +tar-n-block-bytes+ :initial-element 0
                                                             :element-type '(unsigned-byte 8))))
                 (assert (= +tar-n-block-bytes+ (read-sequence buffer blocked-stream)))
                 buffer)))
        (make-instance (if (eql type :auto)
                           (detect-type (read-buffer))
                           type)
                       :stream blocked-stream
                       :other-streams-to-close (append (unless (eql compression-stream stream)
                                                         (list compression-stream))
                                                       other-streams-to-close)
                       :direction direction
                       :header-encoding header-encoding)))))

(defmethod close-tar-file (tar-file)
  (when (open-tar-file-p tar-file)
    (close (tar-file-stream tar-file))
    (mapc #'close (tar-file-other-streams-to-close tar-file))
    (setf (open-tar-file-p tar-file) nil))
  t)

(defmethod read-entry :before ((tar-file tar-file))
  (unless (eq (%tar-file-direction tar-file) :input)
    (error "Attempting to read from a non-input tar-file"))
  (unless (open-tar-file-p tar-file)
    (error "Attempting to read from a closed tar-file")))

(defmethod write-entry :before ((tar-file tar-file) entry
                                &key stream)
  (declare (ignore stream))
  (unless (eq (%tar-file-direction tar-file) :output)
    (error "Attempting to write to a non-output tar-file"))
  (unless (open-tar-file-p tar-file)
    (error "Attempting to write to a closed tar-file")))

(defmethod write-entry-data ((tar-file tar-file) entry stream)
  (cond
    ((typep stream 'stream)
     (if (and (subtypep (stream-element-type stream) '(unsigned-byte 8))
	          (subtypep '(unsigned-byte 8) (stream-element-type stream)))
         (transfer-stream-to-tar-file tar-file stream)
         (error "Stream has invalid STREAM-ELEMENT-TYPE ~A"
                (stream-element-type stream))))
    ((typep stream 'pathname)
     (with-open-file (stream stream :element-type '(unsigned-byte 8))
       (transfer-stream-to-tar-file tar-file stream)))
    ((typep stream 'string)
     (transfer-octets-to-tar-file tar-file (string-to-bytevec stream :utf-8)))
    ((typep stream 'vector)
     (transfer-octets-to-tar-file tar-file stream))
    ((eq nil stream)
     ;; do nothing
     )
    (t
     (error "Invalid argument for :STREAM: ~A" stream))))

(defmethod write-entry ((tar-file tar-file) entry
                        &key stream)
  (with-slots ((tar-file-stream stream)) tar-file
    (let ((buffer (make-array +tar-n-block-bytes+ :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent buffer))
      ;; write the entry
      (write-header-to-buffer entry buffer (header-encoding tar-file) 0)
      (write-sequence buffer tar-file-stream))
    ;; write any associated data
    (write-entry-data tar-file entry stream)
    (values)))


;;; providing streamy access for an entry
(defun make-stream-for-entry (tar-file entry)
  (make-bounded-stream (tar-file-stream tar-file) (size entry)))

(defmethod read-entry :before ((tar-file tar-file))
  (unless (file-position (tar-file-stream tar-file) (next-entry-start tar-file))
    (error 'simple-tar-file-error :format-control "Unable to set FILE-POSITION.")))

(defmethod read-entry ((tar-file tar-file))
  (let ((start-position (file-position (tar-file-stream tar-file)))
        (buffer (make-array +tar-n-block-bytes+ :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (with-slots (stream) tar-file
      (let ((nbytes (read-sequence buffer stream)))
        (unless (= nbytes +tar-n-block-bytes+)
          (error "Corrupt tar-file"))))
    (if (null-block-p buffer 0)
        nil
        (let ((header (read-header-from-buffer (header-type tar-file) buffer
                                               (header-encoding tar-file)
                                               :start 0)))
          (make-instance (entry-type tar-file header)
                         :tar-file tar-file
                         :header header
                         :start-position start-position)))))

(defmethod read-entry :around ((tar-file tar-file))
  (let ((entry (call-next-method)))
    (unless (null entry)
      (setf (next-entry-start tar-file)
            (+ (start-position entry)
               +tar-n-block-bytes+
               (if (entry-has-data-p entry)
                   (round-up-to-tar-block (size entry))
                   0))))
    entry))

(defun transfer-stream-to-tar-file (tar-file stream)
  (let* ((bytes-copied (alexandria:copy-stream stream (tar-file-stream tar-file)))
         (rounded-bytes (round-up-to-tar-block bytes-copied))
         (bytes-remaining (- rounded-bytes bytes-copied)))
    (write-sequence (make-array bytes-remaining :element-type '(unsigned-byte 8)
                                                :initial-element 0)
                    (tar-file-stream tar-file))))

(defun transfer-octets-to-tar-file (tar-file octets)
  (let* ((rounded-bytes (round-up-to-tar-block (length octets)))
         (bytes-remaining (- rounded-bytes (length octets))))
    (write-sequence octets (tar-file-stream tar-file))
    (write-sequence (make-array bytes-remaining :element-type '(unsigned-byte 8)
                                                :initial-element 0)
                    (tar-file-stream tar-file))))

(defmethod finalize-tar-file ((tar-file tar-file))
  (let ((null-block (make-array +tar-n-block-bytes+
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (declare (dynamic-extent null-block))
    (dotimes (i 2)
      (write-sequence null-block (tar-file-stream tar-file)))
    (values)))
