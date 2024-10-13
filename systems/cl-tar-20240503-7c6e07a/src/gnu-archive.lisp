;;;; gnu archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass gnu-archive (archive)
  ()
  (:documentation
   "An archive that uses GNU specific extensions."))

(defmethod archive-supports-property-p ((archive gnu-archive) property)
  (or (not (null (member property '(atime ctime uname gname))))
      (call-next-method)))


;; Reading

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:gnu-long-name-entry)
                                        &rest overrides)
  (let ((real-name-byte-stream (tar-file:make-entry-stream physical-entry))
        (real-name-byte-vec (make-array (tar-file:size physical-entry)
                                        :element-type '(unsigned-byte 8))))
    (read-sequence real-name-byte-vec real-name-byte-stream)
    (apply #'convert-from-physical-entry archive (tar-file:read-entry (archive-file archive))
           :name (babel:octets-to-string real-name-byte-vec :encoding :utf-8)
           overrides)))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:gnu-long-link-name-entry)
                                        &rest overrides)
  (let ((real-name-byte-stream (tar-file:make-entry-stream physical-entry))
        (real-name-byte-vec (make-array (tar-file:size physical-entry)
                                        :element-type '(unsigned-byte 8))))
    (read-sequence real-name-byte-vec real-name-byte-stream)
    (apply #'convert-from-physical-entry archive (tar-file:read-entry (archive-file archive))
           :linkname (babel:octets-to-string real-name-byte-vec :encoding :utf-8)
           overrides)))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:file-entry)
                                        &rest overrides)
  (apply #'make-instance 'file-entry
         :%archive archive
         :%physical-entry physical-entry
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :size (tar-file:size physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :atime (local-time:unix-to-timestamp (tar-file:atime physical-entry))
                  :ctime (local-time:unix-to-timestamp (tar-file:ctime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:directory-entry)
                                        &rest overrides)
  (apply #'make-instance 'directory-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :size (tar-file:size physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:symbolic-link-entry)
                                        &rest overrides)
  (apply #'make-instance 'symbolic-link-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :atime (local-time:unix-to-timestamp (tar-file:atime physical-entry))
                  :ctime (local-time:unix-to-timestamp (tar-file:ctime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :linkname (tar-file:linkname physical-entry)))))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:hard-link-entry)
                                        &rest overrides)
  (apply #'make-instance 'hard-link-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :atime (local-time:unix-to-timestamp (tar-file:atime physical-entry))
                  :ctime (local-time:unix-to-timestamp (tar-file:ctime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :linkname (tar-file:linkname physical-entry)))))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:character-device-entry)
                                        &rest overrides)
  (apply #'make-instance 'character-device-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :atime (local-time:unix-to-timestamp (tar-file:atime physical-entry))
                  :ctime (local-time:unix-to-timestamp (tar-file:ctime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :devmajor (tar-file:devmajor physical-entry)
                  :devminor (tar-file:devminor physical-entry)))))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:block-device-entry)
                                        &rest overrides)
  (apply #'make-instance 'block-device-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :atime (local-time:unix-to-timestamp (tar-file:atime physical-entry))
                  :ctime (local-time:unix-to-timestamp (tar-file:ctime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :devmajor (tar-file:devmajor physical-entry)
                  :devminor (tar-file:devminor physical-entry)))))

(defmethod convert-from-physical-entry ((archive gnu-archive)
                                        (physical-entry tar-file:fifo-entry)
                                        &rest overrides)
  (apply #'make-instance 'fifo-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file:name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :atime (local-time:unix-to-timestamp (tar-file:atime physical-entry))
                  :ctime (local-time:unix-to-timestamp (tar-file:ctime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))
