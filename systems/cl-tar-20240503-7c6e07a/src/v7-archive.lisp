;;;; v7 archives
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar)

(defclass v7-archive (archive)
  ()
  (:documentation
   "A very early archive format."))


;; Reading

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:file-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'file-entry
                 :%archive archive
                 :%physical-entry physical-entry
                 :name (tar-file:name physical-entry)
                 :size (tar-file:size physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))))

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:directory-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'directory-entry
                 :%archive archive
                 :name (tar-file:name physical-entry)
                 :size (tar-file:size physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))))

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:symbolic-link-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'symbolic-link-entry
                 :%archive archive
                 :name (tar-file:name physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))
                 :linkname (tar-file:linkname physical-entry)))

(defmethod convert-from-physical-entry ((archive v7-archive)
                                        (physical-entry tar-file:hard-link-entry)
                                        &rest overrides)
  (declare (ignore overrides))
  (make-instance 'hard-link-entry
                 :%archive archive
                 :name (tar-file:name physical-entry)
                 :mtime (local-time:unix-to-timestamp (tar-file:mtime physical-entry))
                 :uid (tar-file:uid physical-entry)
                 :gid (tar-file:gid physical-entry)
                 :mode (mode-to-permissions (tar-file:mode physical-entry))
                 :linkname (tar-file:linkname physical-entry)))


;; Writing

(defmethod archive-supports-sub-seconds-p ((archive v7-archive))
  nil)

(defmethod archive-supports-negative-time-p ((archive v7-archive))
  nil)

(defmethod integer-max-value ((archive v7-archive) (name (eql 'size)))
  ;; Size is 12 bytes, an octal number in ascii, and terminated with a space.
  #o77777777777)

(defmethod integer-max-value ((archive v7-archive) (name (eql 'uid)))
  ;; UID is 8 bytes, an octal number in ascii, and terminated with a space and
  ;; NULL.
  #o777777)

(defmethod integer-max-value ((archive v7-archive) (name (eql 'gid)))
  ;; GID is 8 bytes, an octal number in ascii, and terminated with a space and
  ;; NULL.
  #o777777)

(defmethod string-max-length ((archive v7-archive) (name (eql 'name)))
  ;; Name is 100 bytes, null terminated.
  99)

(defmethod string-max-length ((archive v7-archive) (name (eql 'linkname)))
  ;; Linkname is 100 bytes, null terminated.
  99)

(defmethod %write-entry ((archive v7-archive) (entry file-entry) &rest overrides)
  (declare (ignore overrides))
  (tar-file:write-file-entry (archive-file archive)
                             (maybe-truncate (name entry) (string-max-length archive 'name))
                             :size (size entry)
                             :gid (gid entry)
                             :uid (uid entry)
                             :mtime (local-time:timestamp-to-unix (mtime entry))
                             :mode (permissions-to-mode (mode entry))
                             :data (data entry)))

(defmethod %write-entry ((archive v7-archive) (entry directory-entry) &rest overrides)
  (declare (ignore overrides))
  (let ((name (name entry)))
    (unless (uiop:string-suffix-p name "/")
      (setf name (concatenate 'string name "/")))
    (tar-file:write-file-entry (archive-file archive)
                               (maybe-truncate name (string-max-length archive 'name))
                               :size (size entry)
                               :gid (gid entry)
                               :uid (uid entry)
                               :mtime (local-time:timestamp-to-unix (mtime entry))
                               :mode (permissions-to-mode (mode entry)))))

(defmethod %write-entry ((archive v7-archive) (entry symbolic-link-entry)
                         &rest overrides)
  (declare (ignore overrides))
  (tar-file:write-symbolic-link-entry
   (archive-file archive)
   (maybe-truncate (name entry) (string-max-length archive 'name))
   :gid (gid entry)
   :uid (uid entry)
   :mtime (local-time:timestamp-to-unix (mtime entry))
   :mode (permissions-to-mode (mode entry))
   :linkname (maybe-truncate (linkname entry) (string-max-length archive 'linkname))))

(defmethod %write-entry ((archive v7-archive) (entry hard-link-entry)
                         &rest overrides)
  (declare (ignore overrides))
  (tar-file:write-hard-link-entry
   (archive-file archive)
   (maybe-truncate (name entry) (string-max-length archive 'name))
   :gid (gid entry)
   :uid (uid entry)
   :mtime (local-time:timestamp-to-unix (mtime entry))
   :mode (permissions-to-mode (mode entry))
   :linkname (maybe-truncate (linkname entry) (string-max-length archive 'linkname))))
