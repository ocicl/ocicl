;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-create)

(defparameter *seconds-from-windows-epoch-to-unix-epoch* 11644473600)

(defun filetime-to-timestamp (filetime)
  (unless (zerop filetime)
    (let* ((unix-100ns-increments (- filetime
                                     (* *seconds-from-windows-epoch-to-unix-epoch*
                                        10000000))))
      (multiple-value-bind (secs 100ns) (truncate unix-100ns-increments 10000000)
        (local-time:unix-to-timestamp secs :nsec (* 100 100ns))))))

(defun check-for-hardlink (info)
  (uiop:nest
   (when (hash-table-p *hard-links*))
   (let ((nlinks (win:file-information-number-of-links info))))
   (when (> nlinks 1))
   (gethash (list (win:file-information-volume-serial-number info)
                  (win:file-information-file-index info))
            *hard-links*)))

(defun maybe-register-hardlink (info namestring)
  (uiop:nest
   (when (hash-table-p *hard-links*))
   (let ((nlinks (win:file-information-number-of-links info))))
   (when (> nlinks 1))
   (setf (gethash (list (win:file-information-volume-serial-number info)
                        (win:file-information-file-index info))
                  *hard-links*)
         namestring)))

(defun collect-file (archive prefix pn info)
  ;; UNWIND-PROTECT is difficult to shove in the NEST, so just do it here.
  (let ((stream nil))
    (unwind-protect
         (uiop:nest
          ;; Compute the common fields.
          (let* ((namestring (concatenate 'string prefix (namestring pn)))
                 (uid 0)
                 (gid 0)
                 (mode '(:user-read :user-write :user-exec
                         :group-read :group-write :group-exec))
                 (mtime (filetime-to-timestamp (win:file-information-last-write-time info)))
                 (ctime (filetime-to-timestamp (win:file-information-creation-time info)))
                 (atime (filetime-to-timestamp (win:file-information-last-access-time info)))))
          ;; Build up the args
          (let* ((args (append
                        (list :name namestring
                              :uid uid :gid gid
                              :mode mode)
                        (unless (null mtime) (list :mtime mtime))
                        (unless (null atime) (list :atime atime))
                        (unless (null ctime) (list :ctime ctime))))
                 (hardlink-target (check-for-hardlink info))
                 (file-kind (osicat:file-kind pn))
                 type)
            ;; Handle the type specific parts.
            (cond
              ;; Test for hardlinks first
              ((not (null hardlink-target))
               (setf type 'tar:hard-link-entry
                     args (append args
                                  (list :linkname hardlink-target))))
              ((eql file-kind :symbolic-link)
               (setf type 'tar:symbolic-link-entry
                     args (append args
                                  (list :linkname (namestring (osicat:read-link pn))))))
              ((eql file-kind :regular-file)
               (setf stream (open pn :element-type '(unsigned-byte 8)))
               (setf type 'tar:file-entry
                     args (append args
                                  (list :size (file-length stream)
                                        :data stream))))
              (t
               (error "unknown entry type"))))
          ;; Make the entry and add it to the archive
          (let ((entry (apply #'make-instance type args)))
            (tar:write-entry archive entry)
            ;; Record for hardlink detection
            (maybe-register-hardlink info namestring)))
      (unless (null stream)
        (close stream)))))

(defun collect-directory (archive prefix pn recursep filter info)
  ;; Make an entry for the directory itself.
  (uiop:nest
   (let* ((namestring (concatenate 'string prefix (namestring pn)))
          (uid 0)
          (gid 0)
          (mode '(:user-read :user-write :user-exec
                  :group-read :group-write :group-exec))
          (mtime (filetime-to-timestamp (win:file-information-last-write-time info)))
          (ctime (filetime-to-timestamp (win:file-information-creation-time info)))
          (atime (filetime-to-timestamp (win:file-information-last-access-time info)))))
   (let ((entry (apply #'make-instance
                       'tar:directory-entry
                       :ctime ctime
                       :atime atime
                       :mtime mtime
                       :uid uid
                       :gid gid
                       :name namestring
                       :mode mode
                       (append
                        (unless (null mtime) (list :mtime mtime))
                        (unless (null atime) (list :atime atime))
                        (unless (null ctime) (list :ctime ctime)))))))
   (tar:write-entry archive entry))
  ;; Contents
  (uiop:nest
   (when recursep)
   (let ((contents (sort (osicat:list-directory (merge-pathnames pn) :bare-pathnames t)
                         #'string<
                         :key #'namestring))))
   (dolist (file contents)
     (add-path-to-archive archive prefix (merge-pathnames file pn) recursep filter))))

(defun add-path-to-archive (archive prefix pathname recursep filter)
  (let ((handle nil))
    (unwind-protect
         (uiop:nest
          (when (funcall filter pathname)
            ;; Since we don't request :DELETE as part of sharing, that means no
            ;; one can rename or delete this file while we hold the handle open.
            (setf handle (win:create-file (merge-pathnames pathname) 0 '(:read :write)
                                          (cffi:null-pointer)
                                          :open-existing
                                          ;; backup semantics needed to open directories.
                                          '(:flag-open-reparse-point :flag-backup-semantics))))
          ;; Explicit merge, otherwise relative names would be relative to the current
          ;; directory.
          (let* ((info (win:get-file-information-by-handle handle))
                 (attributes (win:file-information-file-attributes info))))
          (cond
            ((and (member :attribute-directory attributes)
                  (not (win:handle-is-symbolic-link-p handle)))
             (collect-directory archive prefix pathname recursep filter info))
            (t
             (collect-file archive prefix pathname info))))
      (unless (null handle)
        (win:close-handle handle)))))
