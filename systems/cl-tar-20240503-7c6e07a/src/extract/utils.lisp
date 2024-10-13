;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)


;;; Unix utils

#-windows
(defun rootp ()
  (not (zerop (nix:getuid))))

#-windows
(defun safe-getpwnam (uname)
  (unless (null uname)
    (nix:getpwnam uname)))

#-windows
(defun safe-getgrnam (gname)
  (unless (null gname)
    (nix:getgrnam gname)))


;;; Permissions

(defgeneric set-permissions (entry mask &key fd pn))

(defmethod set-permissions (entry mask &key fd pn)
  #-windows (declare (ignore pn))
  #-windows
  (nix:fchmod fd (tar::permissions-to-mode (set-difference (tar:mode entry)
                                                           mask)))
  #+windows
  (nix:chmod (merge-pathnames pn) (tar::permissions-to-mode
                                   (intersection (list :user-read :user-write)
                                                 (set-difference (tar:mode entry) mask)))))


;;; utimes

(defgeneric set-utimes (entry &key fd pn dirfd))

(defmethod set-utimes (entry &key fd pn dirfd)
  (let ((atime (and (slot-boundp entry 'tar:atime) (tar:atime entry)))
        (mtime (tar:mtime entry))
        atime-sec atime-nsec
        mtime-sec mtime-nsec)
    (if (null atime)
        #+tar-extract-use-utimens
        (setf atime-sec 0
              atime-nsec nix:utime-omit)
        #-tar-extract-use-utimens
        (setf atime-sec (nix:stat-atime (nix:fstat fd)))
        (setf atime-sec (local-time:timestamp-to-unix atime)
              atime-nsec (local-time:nsec-of atime)))
    (if (null mtime)
        #+tar-extract-use-utimens
        (setf mtime-sec 0
              mtime-nsec nix:utime-omit)
        #-tar-extract-use-utimens
        (setf mtime-sec (nix:stat-mtime (nix:fstat fd)))
        (setf mtime-sec (local-time:timestamp-to-unix mtime)
              mtime-nsec (local-time:nsec-of mtime)))
    #+tar-extract-use-utimens
    (if (not (null dirfd))
        (nix:utimensat dirfd pn atime-sec atime-nsec mtime-sec mtime-nsec nix:at-symlink-nofollow)
        (nix:futimens fd atime-sec atime-nsec mtime-sec mtime-nsec))
    #-tar-extract-use-utimens
    (futime fd atime-sec mtime-sec)))


;;; Owner

#-windows
(defgeneric set-owner (entry numeric-uid &key fd pn))

#-windows
(defmethod set-owner (entry numeric-uid &key fd pn dirfd)
  (let ((owner (if numeric-uid
                   (tar:uid entry)
                   (or (nth-value 2 (safe-getpwnam (tar:uname entry)))
                       (tar:uid entry))))
        (group (if numeric-uid
                   (tar:gid entry)
                   (or (nth-value 2 (safe-getgrnam (tar:gname entry)))
                       (tar:gid entry)))))
    (if (null dirfd)
        (nix:fchown fd owner group)
        (nix:fchownat dirfd pn owner group nix:at-symlink-nofollow))))


;;; File Descriptors

(defun call-with-fd (thunk fd)
  (unwind-protect
       (funcall thunk fd)
    (nix:close fd)))

(defmacro with-fd ((fd &optional (default fd))
                   &body body)
  `(call-with-fd (lambda (,fd) ,@body) ,default))
