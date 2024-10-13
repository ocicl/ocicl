;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-create)

(defun check-for-hardlink (stat)
  (uiop:nest
   (when (hash-table-p *hard-links*))
   (let ((nlinks (nix:stat-nlink stat))))
   (when (> nlinks 1))
   (gethash (list (nix:stat-dev stat)
                  (nix:stat-ino stat))
            *hard-links*)))

(defun maybe-register-hardlink (stat namestring)
  (uiop:nest
   (when (hash-table-p *hard-links*))
   (let ((nlinks (nix:stat-nlink stat))))
   (when (> nlinks 1))
   (setf (gethash (list (nix:stat-dev stat)
                        (nix:stat-ino stat))
                  *hard-links*)
         namestring)))

(defun collect-file (archive prefix pn stat raw-mode)
  ;; UNWIND-PROTECT is difficult to shove in the NEST, so just do it here.
  (let ((stream nil))
    (unwind-protect
         (uiop:nest
          ;; Compute the common fields.
          (let* ((namestring (concatenate 'string prefix (uiop:native-namestring pn)))
                 (uid (nix:stat-uid stat))
                 (gid (nix:stat-gid stat))
                 (mode (tar::mode-to-permissions raw-mode))
                 (uname (nix:getpwuid uid))
                 (gname (nix:getgrgid gid))
                 (mtime (local-time:unix-to-timestamp (nix:stat-mtime-sec stat)
                                                      :nsec (nix:stat-mtime-nsec stat)))
                 (ctime (local-time:unix-to-timestamp (nix:stat-ctime-sec stat)
                                                      :nsec (nix:stat-ctime-nsec stat)))
                 (atime (local-time:unix-to-timestamp (nix:stat-atime-sec stat)
                                                      :nsec (nix:stat-atime-nsec stat)))))
          ;; Build up the args
          (let* ((args (append
                        (list :name namestring
                              :uid uid :gid gid
                              :mode mode
                              :mtime mtime :ctime ctime :atime atime)
                        (unless (null uname) (list :uname uname))
                        (unless (null gname) (list :gname gname))))
                 (hardlink-target (check-for-hardlink stat))
                 type)
            ;; Handle the type specific parts.
            (cond
              ;; Test for hardlinks first
              ((not (null hardlink-target))
               (setf type 'tar:hard-link-entry
                     args (append args
                                  (list :linkname hardlink-target))))
              ((nix:s-ischr raw-mode)
               (setf type 'tar:character-device-entry
                     args (append args
                                  (list :devmajor (nix:major (nix:stat-rdev stat))
                                        :devminor (nix:minor (nix:stat-rdev stat))))))
              ((nix:s-isblk raw-mode)
               (setf type 'tar:block-device-entry
                     args (append args
                                  (list :devmajor (nix:major (nix:stat-rdev stat))
                                        :devminor (nix:minor (nix:stat-rdev stat))))))
              ((nix:s-isfifo raw-mode)
               (setf type 'tar:fifo-entry))
              ((nix:s-islnk raw-mode)
               (setf type 'tar:symbolic-link-entry
                     args (append args
                                  (list :linkname (nix:readlink (merge-pathnames pn))))))
              ((nix:s-isreg raw-mode)
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
            (maybe-register-hardlink stat namestring)))
      (unless (null stream)
        (close stream)))))

(defun collect-directory (archive prefix pn recursep filter stat raw-mode)
  ;; Make an entry for the directory itself.
  (uiop:nest
   (let* ((namestring (concatenate 'string prefix (uiop:native-namestring pn)))
          (uid (nix:stat-uid stat))
          (gid (nix:stat-gid stat))
          (mode (tar::mode-to-permissions raw-mode))
          (uname (nix:getpwuid uid))
          (gname (nix:getgrgid gid))
          (mtime (local-time:unix-to-timestamp (nix:stat-mtime-sec stat)
                                                      :nsec (nix:stat-mtime-nsec stat)))
          (ctime (local-time:unix-to-timestamp (nix:stat-ctime-sec stat)
                                               :nsec (nix:stat-ctime-nsec stat)))
          (atime (local-time:unix-to-timestamp (nix:stat-atime-sec stat)
                                               :nsec (nix:stat-atime-nsec stat)))))
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
                        (unless (null uname) (list :uname uname))
                        (unless (null gname) (list :gname gname)))))))
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
  (uiop:nest
   (when (funcall filter pathname))
   ;; Explicit merge, otherwise relative names would be relative to the current
   ;; directory.
   (let* ((stat (nix:lstat (merge-pathnames pathname)))
          (mode (nix:stat-mode stat))))
   (cond
     ((nix:s-isdir mode)
      (collect-directory archive prefix pathname recursep filter stat mode))
     (t
      (collect-file archive prefix pathname stat mode)))))
