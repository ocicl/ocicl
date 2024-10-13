;;;; entry.lisp -- abstract entry classes
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defclass entry ()
  ((tar-file
    :initarg :tar-file
    :reader entry-tar-file)
   (start-position
    :initarg :start-position
    :reader start-position
    :documentation
    "The FILE-POSITION of the start of the entry.")
   (header
    :initarg :header
    :reader header))
  (:documentation
   "Base class for all entries in a tar file."))

(defclass has-data-mixin ()
  ())

(defclass file-entry (entry has-data-mixin)
  ()
  (:documentation
   "A regular file."))

(defmethod write-file-entry (tar-file name &rest args &key uname gname mode mtime uid gid size data
                                                        prefix)
  (declare (ignore uname gname mode mtime uid gid prefix))
  ;; Compute the size when necessary.
  (let ((computed-size
          (etypecase data
            (string
             (setf data (string-to-bytevec data :utf-8))
             (push data args)
             (push :data args)
             (length data))
            (vector
             (length data))
            (pathname
             (with-open-file (s data :element-type '(unsigned-byte 8))
               (file-length s)))
            (null
             0)
            (stream
             (file-length data)))))
    (when (not (null computed-size))
      (cond
        ((null size)
         (setf size computed-size))
        ((/= size computed-size)
         (error 'simple-tar-file-error
                :format-control "Computed (~A) and specified (~A) sizes mismatch"
                :format-args (list computed-size size))))))
  (when (null size)
    (error 'simple-tar-file-error
           :format-control "Size not provided and unable to compute it."))
  (push size args)
  (push :size args)

  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag (if (typep tar-file 'v7-tar-file)
                                     +tar-regular-alternate-file+
                                     +tar-regular-file+)
                       (uiop:remove-plist-key :data args)))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header :stream data)
    (make-instance 'file-entry :header header :tar-file tar-file :start-position start-position)))

(defclass hard-link-entry (entry)
  ()
  (:documentation
   "A hard link."))

(defmethod write-hard-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (declare (ignore uname gname mode mtime uid gid linkname prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-hard-link+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'hard-link-entry :header header :tar-file tar-file :start-position start-position)))

(defclass symbolic-link-entry (entry)
  ()
  (:documentation
   "A symbolic link."))

(defmethod write-symbolic-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (declare (ignore uname gname mode mtime uid gid linkname prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-symbolic-link+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'symbolic-link-entry :header header :tar-file tar-file :start-position start-position)))

(defclass character-device-entry (entry)
  ()
  (:documentation
   "A character device."))

(defmethod write-character-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                    devmajor devminor
                                                                    prefix)
  (declare (ignore uname gname mode mtime uid gid devmajor devminor prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-character-device+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'character-device-entry :header header :tar-file tar-file :start-position start-position)))

(defclass block-device-entry (entry)
  ()
  (:documentation
   "A block device."))

(defmethod write-block-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                devmajor devminor
                                                                prefix)
  (declare (ignore uname gname mode mtime uid gid devmajor devminor prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-block-device+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'block-device-entry :header header :tar-file tar-file :start-position start-position)))

(defclass directory-entry (entry)
  ()
  (:documentation
   "A directory."))

(defmethod write-directory-entry (tar-file name &rest args &key uname gname mode mtime uid gid size
                                                             prefix)
  (declare (ignore uname gname mode mtime uid gid size prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-directory-file+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'directory-entry :header header :tar-file tar-file :start-position start-position)))

(defclass fifo-entry (entry)
  ()
  (:documentation
   "A FIFO."))

(defmethod write-fifo-entry (tar-file name &rest args &key uname gname mode mtime uid gid prefix)
  (declare (ignore uname gname mode mtime uid gid prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-fifo-device+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'fifo-entry :header header :tar-file tar-file :start-position start-position)))

(defclass pax-attributes-entry (entry has-data-mixin)
  ((attributes
    :accessor attributes
    :documentation
    "A hash table mapping attribute names (strings) to values (strings).")))

(defgeneric attribute (entry name &optional default)
  (:documentation
   "Get the NAME attribute from ENTRY."))

(defmethod attribute ((entry pax-attributes-entry) name &optional default)
  (gethash name (attributes entry) default))

(defgeneric attribute-names (entry)
  (:documentation
   "Return a list of attribute names contained within ENTRY."))

(defmethod attribute-names ((entry pax-attributes-entry))
  (alexandria:hash-table-values (attributes entry)))

(defmacro do-attributes ((name value entry &optional result) &body body)
  "Given a PAX ENTRY with attributes, execute BODY for every attribute, with
NAME bound to the attribute name and VALUE bound to the attribute value."
  `(block nil
     (maphash
      (lambda (,name ,value)
        ,@body)
      (attributes ,entry))
     ,result))

(defun read-attribute-length (stream)
  "Pop bytes out of the buffer until a space is read, then try turning that
  into a number."
  (let* ((bytes-read 0)
         (bytes (loop
                  :for byte := (read-byte stream nil :eof)
                  :when (eql byte :eof)
                    :do (return :eof)
                  :end
                  :do (incf bytes-read)
                  :until (eql byte +ascii-space+)
                  :collect byte)))
    (if (eql bytes :eof)
        :eof
        (values (parse-integer (bytevec-to-string (coerce bytes '(vector (unsigned-byte 8)))
                                                  :utf-8))
                bytes-read))))

(defun read-attribute (stream)
  (multiple-value-bind (num-bytes bytes-read)
      (read-attribute-length stream)
    (let (buffer
          num-read
          kv-string
          =-position)
      (unless (eql num-bytes :eof)
        (setf buffer (make-array (- num-bytes bytes-read) :element-type '(unsigned-byte 8) :initial-element 0))
        (setf num-read (read-sequence buffer stream))
        (unless (= num-read (- num-bytes bytes-read))
          (error 'malformed-pax-attribute-entry))
        (setf kv-string (bytevec-to-string buffer :utf-8))
        (unless (= (aref buffer (1- num-read)) +ascii-newline+)
          (error 'malformed-pax-attribute-entry))
        (setf =-position (position #\= kv-string))
        (when (null =-position)
          (error 'malformed-pax-attribute-entry))
        (values (subseq kv-string 0 =-position)
                (subseq kv-string (1+ =-position) (1- num-read))
                t)))))

(defun populate-pax-attributes (entry)
  (let ((stream (make-entry-stream entry))
        (ht (make-hash-table :test 'equal)))
    (loop
      (multiple-value-bind (key value exists-p)
          (read-attribute stream)
        (unless exists-p (return))
        (setf (gethash key ht) value)))
    (setf (attributes entry) ht)))

(defmethod slot-unbound (class (entry pax-attributes-entry) (slot-name (eql 'attributes)))
  (populate-pax-attributes entry))

(defclass pax-extended-attributes-entry (pax-attributes-entry)
  ()
  (:documentation
   "Extended attributes for the subsequent record."))

(defmethod user-attributes-to-alist ((attributes hash-table))
  (alexandria:hash-table-alist attributes))

(defmethod user-attributes-to-alist ((attributes list))
  attributes)

(defun attribute-pair-to-bytevec (pair)
  (let* ((pair-string (concatenate 'string " "
                                   (car pair)
                                   "="
                                   (cdr pair)
                                   (list #\Linefeed)))
         (pair-vector (string-to-bytevec pair-string :utf-8))
         (base-length (length pair-vector)))
    (loop
      :for offset :upfrom 0 :below 2
      :for estimated-length := (+ offset (ceiling (log base-length 10)) base-length)
      :for length-vector := (string-to-bytevec (prin1-to-string estimated-length)
                                               :utf-8)
      :when (= estimated-length (+ (length length-vector) base-length))
        :return (concatenate '(vector (unsigned-byte 8)) length-vector pair-vector))))

(defun attribute-alist-to-bytevec (alist)
  (apply #'concatenate '(vector (unsigned-byte 8)) (mapcar #'attribute-pair-to-bytevec alist)))

(defmethod write-pax-extended-attributes-entry (tar-file name &rest args &key attributes)
  (let* ((alist (user-attributes-to-alist attributes))
         (data (attribute-alist-to-bytevec alist))
         (size (length data)))
    (let ((header (apply #'make-instance (header-type tar-file)
                         :name name
                         :typeflag +posix-extended-header+
                         :size size
                         (alexandria:remove-from-plist args :attributes)))
          (start-position (file-position (tar-file-stream tar-file))))
      (write-entry tar-file header :stream data)
      (make-instance 'directory-entry :header header :tar-file tar-file :start-position start-position))))

(defmethod entry-pax-extended-attributes-p ((entry pax-extended-attributes-entry))
  t)

(defclass pax-global-attributes-entry (pax-attributes-entry)
  ()
  (:documentation
   "Extended attributes for all subsequent records."))

(defmethod write-pax-global-attributes-entry (tar-file name &rest args &key attributes)
  (let* ((alist (user-attributes-to-alist attributes))
         (data (attribute-alist-to-bytevec alist))
         (size (length data)))
    (let ((header (apply #'make-instance (header-type tar-file)
                         :name name
                         :typeflag +posix-global-header+
                         :size size
                         (alexandria:remove-from-plist args :attributes)))
          (start-position (file-position (tar-file-stream tar-file))))
      (write-entry tar-file header :stream data)
      (make-instance 'directory-entry :header header :tar-file tar-file :start-position start-position))))

(defmethod entry-pax-global-attributes-p ((entry pax-global-attributes-entry))
  t)

(defclass gnu-directory-dump-entry (entry has-data-mixin)
  ())

(defmethod entry-gnu-directory-dump-p ((entry gnu-directory-dump-entry))
  t)

(defclass gnu-long-link-name-entry (entry has-data-mixin)
  ((long-link-name
    :accessor long-link-name)))

(defmethod slot-unbound (class (entry gnu-long-link-name-entry) (slot-name (eql 'long-link-name)))
  (let ((buffer (make-array (size entry) :element-type '(unsigned-byte 8)
                                         :initial-element 0))
        (stream (make-entry-stream entry)))
    (read-sequence buffer stream)
    (setf (long-link-name entry) (bytevec-to-string buffer :utf-8))))

(defmethod write-gnu-long-link-name-entry (tar-file name &rest args &key data)
  (let* ((data (etypecase data
                 (string
                  (string-to-bytevec data :utf-8))
                 ((vector (unsigned-byte 8))
                  data)))
         (size (length data))
         (header (apply #'make-instance (header-type tar-file)
                        :name name
                        :typeflag +gnutar-long-link-name+
                        :size size
                        (alexandria:remove-from-plist args :data)))
         (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header :stream data)
    (make-instance 'gnu-long-link-name-entry
                   :header header
                   :tar-file tar-file
                   :start-position start-position)))

(defmethod entry-gnu-long-link-name-p ((entry gnu-long-link-name-entry))
  t)

(defclass gnu-long-name-entry (entry has-data-mixin)
  ((long-name
    :accessor long-name)))

(defmethod slot-unbound (class (entry gnu-long-name-entry) (slot-name (eql 'long-name)))
  (let ((buffer (make-array (size entry) :element-type '(unsigned-byte 8)
                                         :initial-element 0))
        (stream (make-entry-stream entry)))
    (read-sequence buffer stream)
    (setf (long-name entry) (bytevec-to-string buffer :utf-8))))

(defmethod write-gnu-long-name-entry (tar-file name &rest args &key data)
  (let* ((data (etypecase data
                 (string
                  (string-to-bytevec data :utf-8))
                 ((vector (unsigned-byte 8))
                  data)))
         (size (length data))
         (header (apply #'make-instance (header-type tar-file)
                        :name name
                        :typeflag +gnutar-long-link-name+
                        :size size
                        (alexandria:remove-from-plist args :data)))
         (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header :stream data)
    (make-instance 'gnu-long-name-entry
                   :header header
                   :tar-file tar-file
                   :start-position start-position)))

(defmethod entry-gnu-long-name-p ((entry gnu-long-name-entry))
  t)

(defclass gnu-sparse-file-entry (entry has-data-mixin)
  ())

(defmethod entry-gnu-sparse-file-p ((entry gnu-sparse-file-entry))
  t)

(defclass gnu-volume-header-name-entry (entry)
  ())

(defmethod entry-gnu-volume-header-name-p ((entry gnu-volume-header-name-entry))
  t)

(defclass unknown-entry (entry has-data-mixin)
  ()
  (:documentation
   "An unknown entry."))

(defmethod entry-unknown-p ((entry unknown-entry))
  t)

(defgeneric entry-has-data-p (entry)
  (:documentation
   "Returns non-NIL if ENTRY has associated data that can be read using MAKE-ENTRY-STREAM.")
  (:method (entry) nil)
  (:method ((entry has-data-mixin)) t))

(defgeneric make-entry-stream (entry)
  (:documentation
   "Returns a new binary stream that contains ENTRY's data."))

(defmethod make-entry-stream ((entry has-data-mixin))
  (make-bounded-stream (tar-file-stream (entry-tar-file entry)) (size entry)
                       (+ (start-position entry) +tar-n-block-bytes+)))

(defmacro make-header-forwarder (name)
  `(progn
     (defmethod ,name ((entry entry))
       (,name (header entry)))))

(make-header-forwarder name)
(make-header-forwarder mode)
(make-header-forwarder uid)
(make-header-forwarder gid)
(make-header-forwarder size)
(make-header-forwarder mtime)
(make-header-forwarder checksum)
(make-header-forwarder typeflag)
(make-header-forwarder linkname)
(make-header-forwarder magic)
(make-header-forwarder version)
(make-header-forwarder uname)
(make-header-forwarder gname)
(make-header-forwarder devmajor)
(make-header-forwarder devminor)
(make-header-forwarder prefix)
(make-header-forwarder atime)
(make-header-forwarder ctime)
(make-header-forwarder offset)
(make-header-forwarder offset-sparse-0)
(make-header-forwarder numbytes-sparse-0)
(make-header-forwarder offset-sparse-1)
(make-header-forwarder numbytes-sparse-1)
(make-header-forwarder offset-sparse-2)
(make-header-forwarder numbytes-sparse-2)
(make-header-forwarder offset-sparse-3)
(make-header-forwarder numbytes-sparse-3)
(make-header-forwarder isextended)
(make-header-forwarder realsize)

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "Entry ~A" (name entry))))

(defmethod entry-file-p ((entry file-entry))
  t)

(defmethod entry-directory-p ((entry directory-entry))
  t)

(defmethod entry-hard-link-p ((entry hard-link-entry))
  t)

(defmethod entry-symbolic-link-p ((entry symbolic-link-entry))
  t)

(defmethod entry-character-device-p ((entry character-device-entry))
  t)

(defmethod entry-block-device-p ((entry block-device-entry))
  t)

(defmethod entry-fifo-p ((entry fifo-entry))
  t)
