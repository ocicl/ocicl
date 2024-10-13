;;;; ustar archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass ustar-archive (archive)
  ()
  (:documentation
   "A ustar archive that adds more fields to the header when compared to
V7-ARCHIVEs."))

(defun split-name (name)
  (if (<= (length name) 100)
      (values nil name)
      (let ((position (position #\/ name
                                :from-end t
                                :end (when (>= (length name) 156) 156))))
        (if (null position)
            (values nil name)
            (values (subseq name 0 position)
                    (subseq name (1+ position)))))))

(defmethod archive-supports-property-p ((archive ustar-archive) property)
  (or (not (null (member property '(uname gname))))
      (call-next-method)))


;; Reading

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:file-entry)
                                        &rest overrides)
  (apply #'make-instance 'file-entry
         :%archive archive
         :%physical-entry physical-entry
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :size (tar-file:size physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:directory-entry)
                                        &rest overrides)
  (apply #'make-instance 'directory-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :size (tar-file:size physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:symbolic-link-entry)
                                        &rest overrides)
  (apply #'make-instance 'symbolic-link-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :linkname (tar-file:linkname physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:hard-link-entry)
                                        &rest overrides)
  (apply #'make-instance 'hard-link-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :linkname (tar-file:linkname physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:character-device-entry)
                                        &rest overrides)
  (apply #'make-instance 'character-device-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :devmajor (tar-file:devmajor physical-entry)
                  :devminor (tar-file:devminor physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:block-device-entry)
                                        &rest overrides)
  (apply #'make-instance 'block-device-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))
                  :devmajor (tar-file:devmajor physical-entry)
                  :devminor (tar-file:devminor physical-entry)))))

(defmethod convert-from-physical-entry ((archive ustar-archive)
                                        (physical-entry tar-file:fifo-entry)
                                        &rest overrides)
  (apply #'make-instance 'fifo-entry
         :%archive archive
         (append overrides
                 (list
                  :name (tar-file-entry-with-prefix-name physical-entry)
                  :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                  :uid (tar-file:uid physical-entry)
                  :gid (tar-file:gid physical-entry)
                  :uname (tar-file:uname physical-entry)
                  :gname (tar-file:gname physical-entry)
                  :mode (mode-to-permissions (tar-file:mode physical-entry))))))


;; Writing

(defmethod archive-supports-sub-seconds-p ((archive ustar-archive))
  nil)

(defmethod archive-supports-negative-time-p ((archive ustar-archive))
  nil)

(defmethod integer-max-value ((archive ustar-archive) (name (eql 'size)))
  ;; Size is 12 bytes, an octal number in ascii, and terminated with a space.
  #o77777777777)

(defmethod integer-max-value ((archive ustar-archive) (name (eql 'uid)))
  ;; UID is 8 bytes, an octal number in ascii, and terminated with a space or
  ;; NULL.
  #o7777777)

(defmethod integer-max-value ((archive ustar-archive) (name (eql 'gid)))
  ;; GID is 8 bytes, an octal number in ascii, and terminated with a space or
  ;; NULL.
  #o7777777)

(defmethod integer-max-value ((archive ustar-archive) (name (eql 'devmajor)))
  ;; Devmajor is 8 bytes, an octal number in ascii, and terminated with a space or
  ;; NULL.
  #o7777777)

(defmethod integer-max-value ((archive ustar-archive) (name (eql 'devminor)))
  ;; Devminor is 8 bytes, an octal number in ascii, and terminated with a space or
  ;; NULL.
  #o7777777)

(defmethod string-max-length ((archive ustar-archive) (name (eql 'uname)))
  ;; uname is 32 bytes, null terminated.
  31)

(defmethod string-max-length ((archive ustar-archive) (name (eql 'gname)))
  ;; gname is 32 bytes, null terminated.
  31)

(defmethod string-max-length ((archive ustar-archive) (name (eql 'name)))
  ;; Name is 100 bytes, null terminated if not exactly 100 bytes.
  100)

(defmethod check-property-for-writing ((archive ustar-archive) (entry entry) (name (eql 'name)))
  (check-required-property entry name 'string)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (declare (ignore prefix))
    (check-string-length entry name (string-max-length archive 'name))))

(defmethod string-max-length ((archive ustar-archive) (name (eql 'linkname)))
  ;; Linkname is 100 bytes, null terminated if not exactly 100 bytes.
  100)

(defmethod %write-entry ((archive ustar-archive) (entry file-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-file-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :size (size entry)
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix "")
                    :data (data entry))))))

(defmethod %write-entry ((archive ustar-archive) (entry directory-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-directory-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :size (size entry)
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix ""))))))

(defmethod %write-entry ((archive ustar-archive) (entry symbolic-link-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-symbolic-link-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix "")
                    :linkname (maybe-truncate (linkname entry) (string-max-length archive 'linkname)))))))

(defmethod %write-entry ((archive ustar-archive) (entry hard-link-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-hard-link-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix "")
                    :linkname (maybe-truncate (linkname entry) (string-max-length archive 'linkname)))))))

(defmethod %write-entry ((archive ustar-archive) (entry fifo-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-fifo-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix ""))))))

(defmethod %write-entry ((archive ustar-archive) (entry block-device-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-block-device-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix "")
                    :devmajor (devmajor entry)
                    :devminor (devminor entry))))))

(defmethod %write-entry ((archive ustar-archive) (entry character-device-entry) &rest overrides)
  (multiple-value-bind (prefix name) (split-name (name entry))
    (apply #'tar-file:write-character-device-entry
           (archive-file archive)
           (maybe-truncate name (string-max-length archive 'name))
           (append overrides
                   (list
                    :gid (gid entry)
                    :uid (uid entry)
                    :uname (maybe-truncate (or (uname entry) "") 31)
                    :gname (maybe-truncate (or (gname entry) "") 31)
                    :mtime (local-time:timestamp-to-unix (mtime entry))
                    :mode (permissions-to-mode (mode entry))
                    :prefix (or prefix "")
                    :devmajor (devmajor entry)
                    :devminor (devminor entry))))))
