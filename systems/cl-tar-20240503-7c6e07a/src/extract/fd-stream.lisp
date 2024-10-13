;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)


(define-condition fd-stream-error (stream-error)
  ())

(define-condition simple-fd-stream-error (fd-stream-error simple-error)
  ())

(defclass directory-fd-stream ()
  ((fd
    :initarg :fd
    :reader fd
    :documentation
    "The underlying file descriptor.")))

(defclass fd-stream (trivial-gray-streams:trivial-gray-stream-mixin
                     trivial-gray-streams:fundamental-binary-stream)
  ((fd
    :initarg :fd
    :reader fd
    :documentation
    "The underlying file descriptor.")))

(defclass fd-output-stream (fd-stream
                            trivial-gray-streams:fundamental-binary-output-stream)
  ()
  (:documentation
   "A FD-STREAM used for output."))

(defmethod trivial-gray-streams:stream-write-byte ((stream fd-output-stream) byte)
  (cffi:with-foreign-array (buf (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-element byte)
                                '(unsigned-byte 8))
    (nix:write (fd stream) buf 1)
    byte))

(defmethod trivial-gray-streams:stream-write-sequence ((stream fd-output-stream)
                                                       sequence start end
                                                       &key &allow-other-keys)
  (let ((num-bytes (- (or end (length sequence)) start)))
    (cffi:with-foreign-array (buf (subseq sequence start end) `(:array :uchar ,num-bytes))
      (nix:write (fd stream) buf num-bytes)))
  sequence)

(defmethod stream-element-type ((stream fd-stream))
  '(unsigned-byte 8))

(defmethod close ((stream fd-stream) &key abort)
  (declare (ignore abort))
  (nix:close (fd stream)))

#+tar-extract-use-openat
(defun openat-random (dir-handle pathname mode)
  (loop
    :for random := (random 10000000000)
    :for name := (concatenate 'string "." (file-namestring pathname)
                              "." (princ-to-string random))
    :for stream := (handler-case
                       (openat dir-handle name mode)
                     (extraction-through-symbolic-link-error () nil)
                     (destination-exists-error () nil))
    :when stream
      :return (values stream name)))

(defun open-random (pathname mode)
  (loop
    :for random := (random 10000000000)
    :for name := (concatenate 'string (namestring pathname)
                              "-" (princ-to-string random))
    :for stream := (handler-case
                       (my-open name mode)
                     (destination-exists-error () nil))
    :when stream
      :return (values stream name)))

#+tar-extract-use-openat
(defun openat (cwdfd pathname mode &optional (path-so-far (list :relative)))
  "This is a slightly safer version of openat that checks for symlinks along the entire path.

Returns an FD-STREAM or OUTPUT-FD-STREAM."
  (let* ((name (file-namestring pathname))
         (directory (pathname-directory pathname))
         (absolute-directory-p (eql (first directory) :absolute))
         flags
         stat)
    (cond
      (absolute-directory-p
       (with-fd (rootfd (nix:open "/" nix:o-rdonly))
         (openat rootfd (make-pathname :directory (list* :relative (rest directory))
                                       :defaults pathname)
                 mode
                 (list :absolute))))
      ;; Still directories to traverse.
      ((and (not (null directory))
            (not (equal directory '(:relative))))
       (let ((next-dir-name (second directory)))
         (when (eql next-dir-name :back)
           (setf next-dir-name ".."))
         (tagbody
          :retry
            (setf flags (logior nix:o-rdonly
                                nix:o-nofollow))
            (handler-case
                (setf stat (nix:fstatat cwdfd next-dir-name nix:at-symlink-nofollow))
              (nix:enoent ()
                (handler-case
                    (nix:mkdirat cwdfd next-dir-name nix:s-irwxu)
                  (nix:eexist () (go :retry)))
                (go :open)))
            (cond
              ((nix:s-islnk (nix:stat-mode stat))
               (let (target)
                 (handler-case
                     (setf target (uiop:parse-unix-namestring
                                   (nix:readlinkat cwdfd next-dir-name)
                                   :dot-dot :back))
                   ;; The link got deleted between the stat and readlink
                   (nix:einval () (go :retry)))
                 (restart-case
                     (error 'extraction-through-symbolic-link-error
                            :target target
                            :pathname (make-pathname :name nil
                                                     :type nil
                                                     :directory (reverse path-so-far)
                                                     :defaults pathname))
                   (follow-symbolic-link ()
                     (return-from openat
                       (openat cwdfd (merge-pathnames
                                      (make-pathname :directory (list* :relative (cddr directory))
                                                     :defaults pathname)
                                      (uiop:ensure-directory-pathname target))
                               mode)))
                   (replace-symbolic-link ()
                     ;; Sadly, there's no non-Linux-specific way to atomically
                     ;; replace the symlink with a directory.
                     (nix:unlinkat cwdfd next-dir-name 0)
                     (go :retry)))))
              ((not (nix:s-isdir (nix:stat-mode stat)))
               (restart-case
                   (error 'file-exists-in-place-of-directory-error
                          :pathname (make-pathname :name nil
                                                   :type nil
                                                   :directory (reverse path-so-far)
                                                   :defaults pathname))
                 (remove-file ()
                   (nix:unlinkat cwdfd next-dir-name 0)
                   (go :retry)))))
          :open
            (let (nextfd)
              (handler-case
                  (setf nextfd (nix:openat cwdfd next-dir-name flags mode))
                (nix:enoent () (go :retry))
                (nix:eloop () (go :retry)))
              (with-fd (nextfd)
                (return-from openat
                  (openat nextfd (make-pathname :directory (list* :relative (cddr directory))
                                                :defaults pathname)
                          mode
                          (list* (second directory) path-so-far))))))))
      ((or (null name)
           (equal name ""))
       (make-instance 'directory-fd-stream :fd (nix:dup cwdfd)))
      (t
       (tagbody
        :retry
          (setf flags (logior nix:o-wronly
                              nix:o-creat
                              nix:o-nofollow))
          (handler-case
              (setf stat (nix:fstatat cwdfd name nix:at-symlink-nofollow))
            ;; If the file doesn't seem to exist, add O_EXCL to our flags and
            ;; try to open it. The O_EXCL ensures we get an error if the file
            ;; is created between the stat and open calls
            (nix:enoent ()
              (setf flags (logior flags nix:o-excl))
              (go :open)))
          (cond
            ;; The file exists and is a symlink.
            ((nix:s-islnk (nix:stat-mode stat))
             (let (target)
               ;; Try reading where it points to, so we can ask the user what
               ;; to do.
               (handler-case
                   (setf target (uiop:parse-unix-namestring
                                 (nix:readlinkat cwdfd name)
                                 :dot-dot :back))
                 ;; The link got deleted between the stat and readlink
                 ;; calls. Just retry from scratch.
                 (nix:einval () (go :retry)))
               (restart-case
                   (error 'extraction-through-symbolic-link-error
                          :pathname (make-pathname :directory (reverse path-so-far)
                                                   :defaults pathname)
                          :target target)
                 ;; Follow the symlink! We resolve the symlink destination
                 ;; ourselves. This is because our API tells the user where the
                 ;; symlink points and POSIX has no way to say "follow the
                 ;; symlink, but only if it points to X still" (well, Linux
                 ;; sort of does, but not Darwin nor BSD (pass a file
                 ;; descriptor to readlinkat, not a dirfd))
                 (follow-symbolic-link ()
                   (return-from openat
                     (openat cwdfd target mode)))
                 ;; Replace the symbolic link! Create a temporary file, rename
                 ;; it on top of the symlink, and then return a stream to the
                 ;; new file. This ensures that the link is atomically
                 ;; replaced.
                 (replace-symbolic-link ()
                   (multiple-value-bind (stream tmp-name)
                       (openat-random cwdfd name mode)
                     (nix:renameat cwdfd tmp-name cwdfd name)
                     (return-from openat stream))))))
            ;; File exists, but is not a symlink. Ask the user what to do.
            (t
             (restart-case
                 (error 'destination-exists-error
                        :mtime (local-time:unix-to-timestamp (nix:stat-mtime stat)
                                                             :nsec (nix:stat-mtime-nsec stat))
                        :pathname pathname)
               ;; User wants us to overwrite it. So add O_TRUNC to the flags
               ;; and get going.
               (supersede-file ()
                 (setf flags (logior flags nix:o-trunc))
                 (go :open))
               ;; User wants us to rename and replace the file. This keeps
               ;; processes that already have the file open happier. Take the
               ;; same approach as replacing a symlink, make a new file and
               ;; rename it.
               (remove-file ()
                 (multiple-value-bind (stream tmp-name) (openat-random cwdfd name mode)
                   (nix:renameat cwdfd tmp-name cwdfd name)
                   (return-from openat stream))))))
        :open
          ;; Try opening the file!
          (handler-case
              (return-from openat
                (make-instance 'fd-output-stream :fd (nix:openat cwdfd name flags mode)))
            ;; Someone snuck in and created a file between the stat and open!
            (nix:eexist () (go :retry))
            ;; Someone snuck in and made a symlink on us!
            (nix:eloop () (go :retry))))))))

(defun my-open (pn mode)
  (ensure-directories-exist (merge-pathnames pn))
  (let (flags
        stat)
    (tagbody
     :retry
       (setf flags (logior nix:o-wronly
                           nix:o-creat))
       (handler-case
           (setf stat (nix:stat (merge-pathnames pn)))
         ;; If the file doesn't seem to exist, add O_EXCL to our flags and try
         ;; to open it. The O_EXCL ensures we get an error if the file is
         ;; created between the stat and open calls
         (nix:enoent ()
           (setf flags (logior flags nix:o-excl))
           (go :open)))
       ;; File exists, ask the user what to do.
       (restart-case
           (error 'destination-exists-error
                  :mtime (local-time:unix-to-timestamp (nix:stat-mtime stat))
                  :pathname pn)
         ;; User wants us to overwrite it. So add O_TRUNC to the flags
         ;; and get going.
         (supersede-file ()
           (setf flags (logior flags nix:o-trunc))
           (go :open))
         ;; User wants us to rename and replace the file. This keeps
         ;; processes that already have the file open happier. Take the
         ;; same approach as replacing a symlink, make a new file and
         ;; rename it.
         (remove-file ()
           (multiple-value-bind (stream tmp-name) (open-random pn mode)
             (nix:rename tmp-name (merge-pathnames pn))
             (return-from my-open stream))))
     :open
       ;; Try opening the file!
       (handler-case
           (return-from my-open
             (make-instance 'fd-output-stream :fd (nix:open (merge-pathnames pn) flags mode)))
         ;; Someone snuck in and created a file between the stat and open!
         (nix:eexist () (go :retry))))))
