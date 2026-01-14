;;;; cpio.lisp -- support for reading cpio archives
(in-package :archive)

;;; values for cpio's `mode' field
(defconstant +cpio-read-owner+ #o400)
(defconstant +cpio-write-owner+ #o200)
(defconstant +cpio-execute-owner+ #o100)
(defconstant +cpio-read-group+ #o40)
(defconstant +cpio-write-group+ #o20)
(defconstant +cpio-execute-group+ #o10)
(defconstant +cpio-read-others+ #o4)
(defconstant +cpio-write-others+ #o2)
(defconstant +cpio-execute-others+ #o1)
(defconstant +cpio-setuid+ #o4000)
(defconstant +cpio-setgid+ #o2000)
(defconstant +cpio-block-special-file+ #o60000)
(defconstant +cpio-directory-file+ #o40000)
(defconstant +cpio-character-special-file+ #o20000)
(defconstant +cpio-fifo-special-file+ #o10000)
(defconstant +cpio-symbolic-link-file+ #o120000)
(defconstant +cpio-regular-file+ #o100000)
(defconstant +cpio-socket-file+ #o140000)

(defparameter *odc-cpio-magic-vector*
  (map '(vector (unsigned-byte 8)) #'char-code "070707"))
(defparameter *svr4-nocrc-cpio-magic-vector*
  (map '(vector (unsigned-byte 8)) #'char-code "070701"))
(defparameter *svr4-crc-cpio-magic-vector*
  (map '(vector (unsigned-byte 8)) #'char-code "070702"))

(defparameter *cpio-trailer*
  (map '(vector (unsigned-byte 8)) #'char-code "TRAILER!!!"))

(defclass cpio-archive (archive) ())
(defclass odc-cpio-archive (cpio-archive) ())

(defmethod initialize-instance :after ((instance odc-cpio-archive) &rest initargs)
  (declare (ignore initargs))
  (initialize-entry-buffer instance +odc-cpio-header-length+))

(defclass svr4-cpio-archive (cpio-archive) ())

(defmethod initialize-instance :after ((instance svr4-cpio-archive) &rest initargs)
  (declare (ignore initargs))
  (initialize-entry-buffer instance +svr4-cpio-header-length+))

(defmethod name ((entry cpio-entry))
  (bytevec-to-string (%name entry)))

(defmethod size ((entry cpio-entry))
  (filesize entry))

(defmethod entry-regular-file-p ((entry cpio-entry))
  (isreg (mode entry)))

(defmethod entry-directory-p ((entry cpio-entry))
  (isdir (mode entry)))

(defmethod entry-symbolic-link-p ((entry cpio-entry))
  (islink (mode entry)))

(defmethod entry-character-device-p ((entry cpio-entry))
  (ischarfile (mode entry)))

(defmethod entry-block-device-p ((entry cpio-entry))
  (isblockfile (mode entry)))

(defmethod entry-fifo-p ((entry cpio-entry))
  (isfifo (mode entry)))

(define-condition invalid-cpio-magic-error (archive-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Magic field for entry is invalid")))
  (:documentation "Signaled when the magic field for an entry is invalid."))

(defmethod create-entry-from-pathname ((archive svr4-cpio-archive) pathname)
  (let ((stat (stat pathname)))
    ;; FIXME: should fill in other fields at some later point (e.g. CRC)
    (make-instance 'svr4-cpio-entry
                   :pathname pathname
                   :mode (stat-mode stat)
                   :ino (stat-ino stat)
                   :nlink (stat-nlink stat)
                   :uid (stat-uid stat)
                   :gid (stat-gid stat)
                   :mtime (stat-mtime stat)
                   :filesize (stat-size stat))))

(defmethod create-entry-from-pathname ((archive odc-cpio-archive) pathname)
  (let ((stat (stat pathname)))
    ;; FIXME: should fill in other fields at some later point
    (make-instance 'odc-cpio-entry
                   :pathname pathname
                   :mode (stat-mode stat)
                   :inode (stat-ino stat)
                   :nlink (stat-nlink stat)
                   :uid (stat-uid stat)
                   :gid (stat-gid stat)
                   :mtime (stat-mtime stat)
                   :filesize (stat-size stat))))

;;; FIXME: need to read the actual filenames from the archive as well.
(defmethod read-entry-from-archive ((archive odc-cpio-archive))
  (let ((entry-block (read-entry-block archive)))
    (when (mismatch *odc-cpio-magic-vector* entry-block
                    :start2 0 :end2 +odc-cpio-header-magic-length+)
      (error 'invalid-cpio-magic-error))
    (with-extracted-fields (odc-cpio-header entry-block 0
                                           dev inode mode uid gid
                                           nlink mtime namesize filesize)
      (let ((name (read-data-block archive (1- namesize))))
        (if (mismatch *cpio-trailer* name :end2 (length *cpio-trailer*))
            (make-instance 'odc-cpio-entry
                           :dev dev
                           :inode inode
                           :mode mode
                           :uid uid
                           :gid gid
                           :nlink nlink
                           :mtime mtime
                           :namesize namesize
                           :filesize filesize
                           :name name)
            nil)))))

(defun round-up-cpio-namesize (size)
  (+ size (logand (- 2 size) 3)))

(defun round-up-cpio-entry-data (num)
  (* (ceiling num 4) 4))

(defmethod read-entry-from-archive ((archive svr4-cpio-archive))
  (let ((entry-block (read-entry-block archive)))
    (when (mismatch *svr4-nocrc-cpio-magic-vector* entry-block
                    :start2 0 :end2 +svr4-cpio-header-magic-length+)
      (error 'invalid-cpio-magic-error))
    (with-extracted-fields (svr4-cpio-header entry-block 0
                                            mode uid gid mtime
                                            nlink ino
                                            filesize namesize)
      (let ((name (read-data-block archive (round-up-cpio-namesize namesize))))
        (if (mismatch *cpio-trailer* name :end2 (length *cpio-trailer*))
            (make-instance 'svr4-cpio-entry
                           :namesize namesize
                           :filesize filesize
                           :nlink nlink
                           :ino ino
                           :mode mode
                           :mtime mtime
                           :uid uid
                           :gid gid
                           :name name)
            nil)))))

(defmethod discard-entry ((archive cpio-archive) (entry cpio-entry))
  (discard-unused-entry-data archive entry #'round-up-cpio-entry-data))

(defmethod transfer-entry-data-to-stream ((archive cpio-archive)
                                          (entry cpio-entry)
                                          stream)
  (transfer-entry-data-to-stream* archive entry stream
                                  #'round-up-cpio-entry-data))


;;; writing cpio archives

;;; these need a little work, as it may not be acceptable to simply blit
;;; the name into the buffer.  this method will likely have to take the
;;; archive as a parameter one day.
(defmethod write-entry-to-buffer ((entry odc-cpio-entry) buffer
                                  &optional (start 0))
  (let* ((namestring (namestring (entry-pathname entry)))
         (bytestring (string-to-bytevec namestring)))
    (fill buffer 0 :start start :end (+ start +odc-cpio-header-length+))

    (odc-cpio-header-write-magic-to-buffer buffer start *odc-cpio-magic-vector*)
    (odc-cpio-header-write-mode-to-buffer buffer start (mode entry))
    (odc-cpio-header-write-uid-to-buffer buffer start (uid entry))
    (odc-cpio-header-write-gid-to-buffer buffer start (gid entry))
    (odc-cpio-header-write-nlink-to-buffer buffer start (nlink entry))
    (odc-cpio-header-write-namesize-to-buffer buffer start (length bytestring))
    (odc-cpio-header-write-filesize-to-buffer buffer start (filesize entry))

    (replace buffer bytestring :start1 (+ start +odc-cpio-header-length+))))

(defmethod finalize-archive ((archive odc-cpio-archive))
  (let ((entry (make-instance 'odc-cpio-entry
                              :nlink 1
                              :name *cpio-trailer*)))
    (write-entry-to-archive archive entry :stream nil)))

(defmethod write-entry-to-buffer ((entry svr4-cpio-entry) buffer
                                  &optional (start 0))
  (let* ((namestring (namestring (entry-pathname entry)))
         (bytestring (string-to-bytevec namestring)))
    (fill buffer 0 :start start :end (+ start +svr4-cpio-header-length+))

    (svr4-cpio-header-write-magic-to-buffer buffer start *svr4-nocrc-cpio-magic-vector*)
    (svr4-cpio-header-write-mode-to-buffer buffer start (mode entry))
    (svr4-cpio-header-write-uid-to-buffer buffer start (uid entry))
    (svr4-cpio-header-write-gid-to-buffer buffer start (gid entry))
    (svr4-cpio-header-write-nlink-to-buffer buffer start (nlink entry))
    (svr4-cpio-header-write-namesize-to-buffer buffer start (length bytestring))
    (svr4-cpio-header-write-filesize-to-buffer buffer start (filesize entry))

    (replace buffer bytestring :start1 (+ start +svr4-cpio-header-length+))))

(defmethod finalize-archive ((archive svr4-cpio-archive))
  (let ((entry (make-instance 'svr4-cpio-entry
                              :nlink 1
                              :name *cpio-trailer*)))
    (write-entry-to-archive archive entry :stream nil)))
