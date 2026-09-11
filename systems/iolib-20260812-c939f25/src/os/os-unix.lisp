;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- OS interface.
;;;

(in-package :iolib/os)

;;;; Environment access

(defclass environment ()
  ((variables :initarg :variables
              :initform (make-hash-table :test #'equal)
              :accessor environment-variables)))

(defmethod print-object ((env environment) stream)
  (print-unreadable-object (env stream :type t :identity nil)
    (let ((keys (sort (hash-table-keys (environment-variables env))
                      #'string-lessp)))
      (if keys
          (format stream "~A variables: ~S ... ~S"
                  (length keys)
                  (car keys) (lastcar keys))
          (format stream "empty")))))

(declaim (inline %obj-getenv %obj-setenv %obj-unsetenv %obj-clearenv))

(defun %obj-getenv (env name)
  (gethash name (environment-variables env)))

(defun %obj-setenv (env name value overwrite)
  (when (or overwrite
            (not (nth-value 1 (%obj-getenv env name))))
    (setf (gethash name (environment-variables env))
          value)))

(defun %obj-unsetenv (env name)
  (remhash name (environment-variables env)))

(defun %obj-clearenv (env)
  (clrhash (environment-variables env)))

(defun environment-variable (name &optional env)
  "ENVIRONMENT-VARIABLE returns the environment variable
identified by NAME, or NIL if one does not exist. NAME can
either be a symbol or a string."
  (let ((name (string name)))
    (etypecase env
      (null
       (isys:getenv name))
      (environment
       (%obj-getenv env name)))))

(defun (setf environment-variable) (value name &optional env
                                    &key (overwrite t))
  "SETF ENVIRONMENT-VARIABLE sets the environment variable
identified by NAME to VALUE. Both NAME and VALUE can be either a
symbols or strings. Signals an error on failure."
  (let ((value (string value))
        (name (string name)))
    (etypecase env
      (null
       (isys:setenv name value overwrite))
      (environment
       (%obj-setenv env name value overwrite)))
    value))

(defun makunbound-environment-variable (name &optional env)
  "Removes the environment variable identified by NAME from the
current environment. NAME can be either a string or a symbol.
Returns the string designated by NAME. Signals an error on
failure."
  (let ((name (string name)))
    (etypecase env
      (null
       (isys:unsetenv name))
      (environment
       (%obj-unsetenv env name)))
    name))

(defun clear-environment (&optional env)
  "Removes all variables from an environment."
  (etypecase env
    (null
     (isys:clearenv))
    (environment
     (%obj-clearenv env)
     env)))

(defun environment ()
  "Return the current global environment."
  (let ((env (make-instance 'environment))
        (envptr (isys:os-environ)))
    (if (null-pointer-p envptr)
        ()
        (loop :for i :from 0 :by 1
              :for string := (mem-aref envptr :string i)
              :for split := (position #\= string)
              :while string :do
              (let ((name (subseq string 0 split))
                    (value (subseq string (1+ split))))
                (%obj-setenv env name value t))))
    env))

(defun (setf environment) (newenv)
  "SETF ENVIRONMENT replaces the contents of the global environment
with that of its argument.

Often it is preferable to use SETF ENVIRONMENT-VARIABLE and
MAKUNBOUND-ENVIRONMENT-VARIABLE to modify the environment instead
of SETF ENVIRONMENT."
  (check-type newenv environment)
  (isys:clearenv)
  (maphash (lambda (name value)
             (isys:setenv name value t))
           (environment-variables newenv))
  newenv)

(defun allocate-env (argv variables)
  (let ((offset -1))
    ;; copy variables
    (maphash (lambda (k v)
               (setf (mem-aref argv :pointer (incf offset))
                     (foreign-string-alloc (concatenate 'string k "=" v))))
             variables)))

(defun delocate-null-ended-list (argv)
  (loop :for i :from 0
        :for ptr := (mem-aref argv :pointer i)
        :if (null-pointer-p ptr) :do (loop-finish)
        :else :do (foreign-free ptr)))

(defmacro with-c-environment ((var environment) &body body)
  (with-gensyms (body-fn ptr count)
    `(flet ((,body-fn (,ptr)
              (let ((,var ,ptr))
                ,@body)))
       (etypecase ,environment
         (null
          (,body-fn (null-pointer)))
         ((eql t)
          (,body-fn (isys:os-environ)))
         (environment
          (let ((,count (1+ (hash-table-count
                             (environment-variables ,environment)))))
            (with-foreign-object (,ptr :pointer ,count)
              (isys:bzero ,ptr (* ,count (isys:sizeof :pointer)))
              (unwind-protect
                   (progn
                     (allocate-env ,ptr (environment-variables ,environment))
                     (,body-fn ,ptr))
                (delocate-null-ended-list ,ptr)))))))))


;;;; Current directory

(defun current-directory ()
  "CURRENT-DIRECTORY returns the operating system's current
directory, which may or may not correspond to
*DEFAULT-FILE-PATH-DEFAULTS*."
  (let ((cwd (isys:getcwd)))
    (if cwd
        (parse-file-path cwd :expand-user nil)
        (isys:syscall-error "Could not get current directory."))))

(defun (setf current-directory) (pathspec)
  "SETF CURRENT-DIRECTORY changes the operating system's current
directory to the PATHSPEC. An error is signalled if PATHSPEC
is not a directory."
  (let ((path (file-path pathspec)))
    (isys:chdir (file-path-namestring path))))

(defmacro with-current-directory (pathspec &body body)
  (with-gensyms (old)
    `(let ((,old (current-directory)))
       (unwind-protect
            (progn
              (setf (current-directory) (file-path ,pathspec))
              ,@body)
         (setf (current-directory) ,old)))))


;;;; File-path manipulations

(defun absolute-file-path (pathspec &optional
                           (defaults *default-file-path-defaults*))
  (let ((path (file-path pathspec)))
    (if (absolute-file-path-p path)
        path
        (let ((tmp (merge-file-paths path defaults)))
          (if (absolute-file-path-p tmp)
              tmp
              (merge-file-paths tmp (current-directory)))))))

(defun strip-dots (path)
  (multiple-value-bind (root nodes)
      (split-root/nodes (file-path-components path))
    (let (new-components)
      (dolist (n nodes)
        (cond
          ((string= n "."))
          ((string= n "..")
           (pop new-components))
          (t (push n new-components))))
      (make-file-path :components (if root
                                      (cons root (nreverse new-components))
                                      (nreverse new-components))
                      :defaults path))))

(defun resolve-symlinks (path)
  (let* ((namestring (file-path-namestring path))
         (realpath (isys:realpath namestring)))
    (parse-file-path realpath)))

(defun resolve-file-path (pathspec &key
                          (defaults *default-file-path-defaults*)
                          (canonicalize t))
  "Returns an absolute file-path corresponding to PATHSPEC by
merging it with DEFAULT, and (CURRENT-DIRECTORY) if necessary.
If CANONICALIZE is non-NIL, the path is canonicalised: if it is :STRIP-DOTS,
then just remove \".\" and \"..\", otherwise symlinks are resolved too."
  (let ((absolute-file-path (absolute-file-path pathspec defaults)))
    (case canonicalize
      ((nil)       absolute-file-path)
      (:strip-dots (strip-dots absolute-file-path))
      (t           (resolve-symlinks absolute-file-path)))))


;;;; File kind

;;; FIXME: make sure that GET-FILE-KIND be able to signal
;;;        only conditions of type FILE-ERROR, either by
;;;        wrapping POSIX-ERRORs or making sure that some
;;;        POSIX-ERRORS subclass FILE-ERROR
(defun get-file-kind (file follow-p)
  (let ((namestring (file-path-namestring file)))
    (handler-case
        (let ((mode (isys:stat-mode
                     (if follow-p
                         (isys:stat namestring)
                         (isys:lstat namestring)))))
          (switch ((logand isys:s-ifmt mode) :test #'=)
            (isys:s-ifdir  :directory)
            (isys:s-ifchr  :character-device)
            (isys:s-ifblk  :block-device)
            (isys:s-ifreg  :regular-file)
            (isys:s-iflnk  :symbolic-link)
            (isys:s-ifsock :socket)
            (isys:s-ififo  :pipe)
            (t (bug "Unknown file mode: ~A." mode))))
      ((or isys:enoent isys:eloop) ()
        (cond
          ;; stat() returned ENOENT: either FILE does not exist
          ;; or it is a broken symlink
          (follow-p
           (handler-case
               (isys:lstat namestring)
             ((or isys:enoent isys:eloop) ())
             (:no-error (stat)
               (declare (ignore stat))
               (values :symbolic-link :broken))))
          ;; lstat() returned ENOENT: FILE does not exist
          (t nil))))))

(defun file-kind (pathspec &key follow-symlinks)
  "Returns a keyword indicating the kind of file designated by PATHSPEC,
or NIL if the file does not exist. Does not follow symbolic
links by default.

Possible file-kinds in addition to NIL are: :REGULAR-FILE,
:SYMBOLIC-LINK, :DIRECTORY, :PIPE, :SOCKET, :CHARACTER-DEVICE, and
:BLOCK-DEVICE.
If FOLLOW-SYMLINKS is non-NIL and PATHSPEC designates a broken symlink
returns :BROKEN as second value."
  (get-file-kind (merge-file-paths pathspec) follow-symlinks))

(defun file-exists-p (pathspec &optional file-kind)
  "Checks whether the file named by the file-path designator
PATHSPEC exists, if this is the case and FILE-KIND is specified
it also checks the file kind. If the tests succeed, return two values:
truename and file kind of PATHSPEC, NIL otherwise.
Follows symbolic links."
  (let* ((path (file-path pathspec))
         (follow (if (eql :symbolic-link file-kind) nil t))
         (actual-kind (file-kind path :follow-symlinks follow)))
    (when (and actual-kind
               (if file-kind (eql file-kind actual-kind) t))
      (values (resolve-file-path path)
              actual-kind))))

(defun regular-file-exists-p (pathspec)
  "Checks whether the file named by the file-path designator
PATHSPEC exists and is a regular file. Returns its truename
if this is the case, NIL otherwise. Follows symbolic links."
  (nth-value 0 (file-exists-p pathspec :regular-file)))

(defun directory-exists-p (pathspec)
  "Checks whether the file named by the file-path designator
PATHSPEC exists and is a directory. Returns its truename
if this is the case, NIL otherwise. Follows symbolic links."
  (nth-value 0 (file-exists-p pathspec :directory)))

(defun good-symlink-exists-p (pathspec)
  "Checks whether the file named by the file-path designator
PATHSPEC exists and is a symlink pointing to an existent file."
  (eql :broken (nth-value 1 (file-kind pathspec :follow-symlinks t))))


;;;; Temporary files

(defvar *temporary-directory*
  (let ((system-tmpdir (or (environment-variable "TMPDIR")
                           (environment-variable "TMP")
                           "/tmp")))
    (parse-file-path system-tmpdir :expand-user nil)))


;;;; Symbolic and hard links

(defun read-symlink (pathspec)
  "Returns the file-path pointed to by the symbolic link
designated by PATHSPEC. If the link is relative, then the
returned file-path is relative to the link, not
*DEFAULT-FILE-PATH-DEFAULTS*.

Signals an error if PATHSPEC is not a symbolic link."
  ;; Note: the previous version tried much harder to provide a buffer
  ;; big enough to fit the link's name. OTOH, %SYS-READLINK stack
  ;; allocates on most lisps.
  (file-path (isys:readlink
              (file-path-namestring
               (absolute-file-path pathspec *default-file-path-defaults*)))))

(defun make-symlink (link target)
  "Creates symbolic LINK that points to TARGET.
Returns the file-path of the link.

Relative targets are resolved against the link. Relative links
are resolved against *DEFAULT-FILE-PATH-DEFAULTS*.

Signals an error if TARGET does not exist, or LINK exists already."
  (let ((link (file-path link))
        (target (file-path target)))
    (with-current-directory
        (absolute-file-path *default-file-path-defaults* nil)
      (isys:symlink (file-path-namestring target)
                         (file-path-namestring link))
      link)))

(defun make-hardlink (link target)
  "Creates hard LINK that points to TARGET.
Returns the file-path of the link.

Relative targets are resolved against the link. Relative links
are resolved against *DEFAULT-FILE-PATH-DEFAULTS*.

Signals an error if TARGET does not exist, or LINK exists already."
  (let ((link (file-path link))
        (target (file-path target)))
    (with-current-directory
        (absolute-file-path *default-file-path-defaults* nil)
      (isys:link (file-path-namestring
                       (merge-file-paths target link))
                      link)
      link)))


;;;; File permissions

(defconstant (+permissions+ :test #'equal)
  `((:user-read    . ,isys:s-irusr)
    (:user-write   . ,isys:s-iwusr)
    (:user-exec    . ,isys:s-ixusr)
    (:group-read   . ,isys:s-irgrp)
    (:group-write  . ,isys:s-iwgrp)
    (:group-exec   . ,isys:s-ixgrp)
    (:other-read   . ,isys:s-iroth)
    (:other-write  . ,isys:s-iwoth)
    (:other-exec   . ,isys:s-ixoth)
    (:set-user-id  . ,isys:s-isuid)
    (:set-group-id . ,isys:s-isgid)
    (:sticky       . ,isys:s-isvtx)))

(defun file-permissions (pathspec)
  "FILE-PERMISSIONS returns a list of keywords identifying the
permissions of PATHSPEC.

SETF FILE-PERMISSIONS sets the permissions of PATHSPEC as
identified by the symbols in list.

If PATHSPEC designates a symbolic link, that link is implicitly
resolved.

Permission symbols consist of :USER-READ, :USER-WRITE, :USER-EXEC,
:GROUP-READ, :GROUP-WRITE, :GROUP-EXEC, :OTHER-READ, :OTHER-WRITE,
:OTHER-EXEC, :SET-USER-ID, :SET-GROUP-ID, and :STICKY.

Both signal an error if PATHSPEC doesn't designate an existing file."
  (let ((mode (isys:stat-mode
               (isys:stat (file-path-namestring pathspec)))))
    (loop :for (name . value) :in +permissions+
          :when (plusp (logand mode value))
          :collect name)))

(defun (setf file-permissions) (perms pathspec)
  (isys:chmod (file-path-namestring pathspec)
                   (reduce (lambda (a b)
                             (logior a (cdr (assoc b +permissions+))))
                           perms :initial-value 0)))


;;;; Directory access

(defmacro with-directory-iterator ((iterator pathspec) &body body)
  "PATHSPEC must be a valid directory designator:
*DEFAULT-FILE-PATH-DEFAULTS* is bound, and (CURRENT-DIRECTORY) is set
to the designated directory for the dynamic scope of the body.

Within the lexical scope of the body, ITERATOR is defined via
macrolet such that successive invocations of (ITERATOR) return
the directory entries, one by one. Both files and directories
are returned, except '.' and '..'. The order of entries is not
guaranteed. The entries are returned as relative file-paths
against the designated directory. Entries that are symbolic
links are not resolved, but links that point to directories are
interpreted as directory designators. Once all entries have been
returned, further invocations of (ITERATOR) will all return NIL.

The value returned is the value of the last form evaluated in
body. Signals an error if PATHSPEC is not a directory."
  (with-unique-names (one-iter)
    `(call-with-directory-iterator
      ,pathspec
      (lambda (,one-iter)
        (declare (type function ,one-iter))
        (macrolet ((,iterator ()
                     `(funcall ,',one-iter)))
          ,@body)))))

(defun call-with-directory-iterator (pathspec fn)
  (let* ((dir (resolve-file-path pathspec :canonicalize nil))
         (dp (isys:opendir (file-path-namestring dir))))
    (labels ((one-iter ()
               (let ((name (isys:readdir dp)))
                 (unless (null name)
                   (cond
                     ((member name '("." "..") :test #'string=)
                      (one-iter))
                     (t
                      (parse-file-path name)))))))
      (unwind-protect
           (let ((*default-file-path-defaults* dir))
             (funcall fn #'one-iter))
        (isys:closedir dp)))))

(defun mapdir (function pathspec)
  "Applies function to each entry in directory designated by
PATHSPEC in turn and returns a list of the results. Binds
*DEFAULT-FILE-PATH-DEFAULTS* to the directory designated by
pathspec round to function call.

If PATHSPEC designates a symbolic link, it is implicitly resolved.

Signals an error if PATHSPEC is not a directory."
  (with-directory-iterator (next pathspec)
    (loop :for entry := (next)
          :while entry
          :collect (funcall function entry))))

(defun list-directory (pathspec)
  "Returns a fresh list of file-paths corresponding to all files
within the directory named by PATHSPEC."
  (with-directory-iterator (next pathspec)
    (loop :for entry := (next)
          :while entry :collect entry)))

(defun walk-directory (directory fn &key (if-does-not-exist :error)
                       follow-symlinks (directories :before)
                       (mindepth 1) (maxdepth 65535)
                       (test (constantly t)) (key #'identity))
  "Recursively applies the function FN to all files within the
directory named by the FILE-PATH designator DIRNAME and all of
the files and directories contained within. Returns T on success."
  (assert (<= 0 mindepth maxdepth))
  (labels ((walk (name depth parent)
             (let* ((kind
                     (file-kind name :follow-symlinks follow-symlinks))
                    (name-key (funcall key name)))
               (flet ((maybe-callfn ()
                        (when (and (<= mindepth depth maxdepth)
                                   (funcall test name-key kind))
                          (callfn name-key kind parent depth)))
                      (maybe-walkdir ()
                        (when (or (< depth mindepth)
                                  (and (< depth maxdepth)
                                       (funcall test name-key kind)))
                          (walkdir name depth parent))))
                 (case kind
                   (:directory
                    (when (eql :before directories) (maybe-callfn))
                    (maybe-walkdir)
                    (when (eql :after directories) (maybe-callfn)))
                   (t (maybe-callfn))))))
           (walkdir (name depth parent)
             (mapdir (lambda (dir)
                       (walk dir (1+ depth)
                             (cond
                               ((zerop depth) (list "."))
                               ((plusp depth)
                                (cons (file-path-file name) parent))
                               (t parent))))
                     name))
           (callfn (key kind parent depth)
             (restart-case
                 (let ((parent
                        (and parent (make-file-path :components (reverse parent)))))
                   (funcall fn key kind parent depth))
               (ignore-file-system-error ()
                 :report "Ignore file system error and continue"))))
    (let* ((directory (file-path directory))
           (kind
            (handler-case
                (file-kind directory :follow-symlinks t)
              (isys:enoent ()
                (ecase if-does-not-exist
                  (:error (isys:syscall-error "Directory ~S does not exist"
                                              directory))
                  ((nil)  (return* nil))))
              (isys:eacces ()
                (isys:syscall-error "Search permission is denied for ~S"
                                    directory)))))
      (unless (eql :directory kind)
        (isys:syscall-error "~S is not a directory" directory))
      (walk directory 0 nil)
      t)))

(defun delete-files (pathspec &key recursive follow-symlinks)
  (labels ((%delete-file (file)
             (isys:unlink (file-path-namestring
                                (absolute-file-path file))))
           (%delete-directory (directory)
             (isys:rmdir (file-path-namestring
                               (absolute-file-path directory)))))
    (let* ((pathspec (file-path pathspec))
           (kind (file-kind pathspec :follow-symlinks follow-symlinks)))
      (case kind
        (:directory
         (if recursive
             (walk-directory pathspec
                             (lambda (name kind parent depth)
                               (declare (ignore parent depth))
                               (case kind
                                 (:directory (%delete-directory name))
                                 (t          (%delete-file name))))
                             :directories :after
                             :mindepth 0)
             (%delete-directory pathspec)))
        (t (%delete-file pathspec))))))


;;;; User information

(defun user-info (id)
  "USER-INFO returns the password entry for the given name or
numerical user ID, as an assoc-list."
  (multiple-value-bind (name password uid gid gecos home shell)
      (etypecase id
        (string  (isys:getpwnam id))
        (integer (isys:getpwuid id)))
    (declare (ignore password))
    (if name
        (list (cons :name name)
              (cons :user-id uid)
              (cons :group-id gid)
              (cons :gecos gecos)
              (cons :home home)
              (cons :shell shell))
        nil)))
