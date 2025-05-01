;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- *UNIX foreign function definitions.
;;;

(in-package :iolib/syscalls)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1) (debug 1))))

;; FIXME: move this into an ASDF operation
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library
      (libfixposix :canary "lfp_buildinfo")
    (t (:default "libfixposix")))
  (load-foreign-library 'libfixposix))


;;;-------------------------------------------------------------------------
;;; LibFixPOSIX build info
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((version-string (n)
             (format nil "~A.~A.~A"
                     (logand #xff (ash n -16))
                     (logand #xff (ash n -8))
                     (logand #xff n)))
           (version-int (v)
             (logior (ash (first v) 16)
                     (ash (second v) 8)
                     (third v)))
           (buildinfo ()
             (with-foreign-object (info '(:struct lfp-buildinfo))
               (foreign-funcall "lfp_buildinfo" :pointer info :int)
               (foreign-slot-value info '(:struct lfp-buildinfo) 'release)))
           (ensure-minver (minver)
             (let ((version (buildinfo))
                   (minint (version-int minver)))
               (when (< version minint)
                 (error "The minimum required LibFixPOSIX version is ~A ~
                         but ~A was loaded"
                        (version-string minint) (version-string version))))))
    ;; Minimum viable LibFixPOSIX version.
    (ensure-minver '(0 4 3))))


;;;-------------------------------------------------------------------------
;;; ERRNO-related functions
;;;-------------------------------------------------------------------------

(defcfun (errno "lfp_errno") :int)

(defun (setf errno) (value)
  (foreign-funcall "lfp_set_errno" :int value :int))

(defsyscall (%strerror "lfp_strerror")
    :int
  (errnum :int)
  (buf    :pointer)
  (buflen size-t))

(defentrypoint strerror (&optional (err (errno)))
  "Look up the error message string for ERRNO (reentrant)."
  (let ((errno
         (if (keywordp err)
             (foreign-enum-value 'errno-values err)
             err)))
    (with-foreign-pointer-as-string ((buf bufsiz) 1024)
      (%strerror errno buf bufsiz))))

(defmethod print-object ((e syscall-error) s)
  (with-slots (syscall code identifier message handle handle2) e
    (print-unreadable-object (e s :type nil :identity nil)
      (cond
        (message
         (format s "~A" message))
        (t
         (format s "Syscall ~S signalled error ~A(~S) ~S"
                 syscall identifier (or code "[No code]")
                 (or (strerror code) "[Can't get error string.]"))
         (when handle (format s " FD=~A" handle))
         (when handle2 (format s " FD2=~A" handle2)))))))


;;;-------------------------------------------------------------------------
;;; Memory manipulation
;;;-------------------------------------------------------------------------

(defcfun (memset "memset") :pointer
  "Fill the first COUNT bytes of BUFFER with the constant VALUE."
  (buffer :pointer)
  (value  :int)
  (count  size-t))

(defentrypoint bzero (buffer count)
  "Fill the first COUNT bytes of BUFFER with zeros."
  (memset buffer 0 count))

(defcfun (memcpy "memcpy") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas must not overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))

(defcfun (memmove "memmove") :pointer
  "Copy COUNT octets from SRC to DEST.
The two memory areas may overlap."
  (dest :pointer)
  (src  :pointer)
  (count size-t))


;;;-------------------------------------------------------------------------
;;; Files
;;;-------------------------------------------------------------------------

(defsyscall (%open "lfp_open")
    (:int :restart t)
  (path  sstring)
  (flags :uint64)
  (mode  mode-t))

(defentrypoint open (path flags &optional (mode #o666))
  "Open a file descriptor for PATH using FLAGS and permissions MODE(#o666 by default)."
  (%open path flags mode))

(defsyscall (creat "lfp_creat")
    (:int :restart t)
  "Create file PATH with permissions MODE and return the new FD."
  (path sstring)
  (mode mode-t))

(defsyscall (%pipe "pipe") :int
  (fds :pointer))

(defentrypoint pipe ()
  "Create pipe, returns two values with the new FDs."
  (with-foreign-object (fds :int 2)
    (%pipe fds)
    (values (mem-aref fds :int 0)
            (mem-aref fds :int 1))))

(defsyscall (mkfifo "mkfifo") :int
  "Create a FIFO (named pipe) with name PATH and permissions MODE."
  (path sstring)
  (mode mode-t))

(defsyscall (umask "umask") mode-t
  "Sets the umask to NEW-MODE and returns the old one."
  (new-mode mode-t))

(defsyscall (lseek "lfp_lseek")
    (off-t :handle fd)
  "Reposition the offset of the open file associated with the file descriptor FD
to the argument OFFSET according to the directive WHENCE."
  (fd     :int)
  (offset off-t)
  (whence :int))

(defsyscall (access "access") :int
  "Check whether the file PATH can be accessed using mode MODE."
  (path sstring)
  (mode :int))

(defsyscall (truncate "lfp_truncate")
    (:int :restart t)
  "Truncate the file PATH to a size of precisely LENGTH octets."
  (path   sstring)
  (length off-t))

(defsyscall (ftruncate "lfp_ftruncate")
    (:int :restart t :handle fd)
  "Truncate the file referenced by FD to a size of precisely LENGTH octets."
  (fd     :int)
  (length off-t))

(defsyscall (rename "rename") :int
  "Rename file named by OLDPATH to NEWPATH."
  (oldpath sstring)
  (newpath sstring))

(defsyscall (link "link") :int
  "Create a hard link from file OLDPATH to NEWPATH."
  (oldpath sstring)
  (newpath sstring))

(defsyscall (symlink "symlink") :int
  "Create a symbolic link from file OLDPATH to NEWPATH."
  (oldpath sstring)
  (newpath sstring))

(defsyscall (%readlink "readlink") ssize-t
  (path    sstring)
  (buf     :pointer)
  (bufsize size-t))

(defentrypoint readlink (path)
  "Read the file name pointed by the symbolic link PATH."
  (with-foreign-pointer (buf +cstring-path-max+ bufsize)
    (let ((count (%readlink path buf bufsize)))
      (cstring-to-sstring buf count))))

(defsyscall (%realpath "realpath") sstring
  (path          sstring)
  (resolved-path :pointer))

(defentrypoint realpath (path)
  "Read the file name pointed by the symbolic link PATH."
  (with-foreign-pointer (buf +cstring-path-max+)
    (%realpath path buf)))

(defsyscall (unlink "unlink") :int
  "Delete the file PATH from the file system."
  (path sstring))

(defsyscall (chown "chown")
    (:int :restart t)
  "Change ownership of file PATH to uid OWNER and gid GROUP(dereferences symlinks)."
  (path  sstring)
  (owner uid-t)
  (group uid-t))

(defsyscall (fchown "fchown")
    (:int :restart t :handle fd)
  "Change ownership of an open file referenced by FD to uid OWNER and gid GROUP."
  (fd    :int)
  (owner uid-t)
  (group uid-t))

(defsyscall (lchown "lchown")
    (:int :restart t)
  "Change ownership of a file PATH to uid OWNER and gid GROUP(does not dereference symlinks)."
  (path  sstring)
  (owner uid-t)
  (group uid-t))

(defsyscall (chmod "chmod")
    (:int :restart t)
  "Change permissions of file PATH to mode MODE."
  (path sstring)
  (mode mode-t))

(defsyscall (fchmod "fchmod")
    (:int :restart t :handle fd)
  "Change permissions of open file referenced by FD to mode MODE."
  (fd   :int)
  (mode mode-t))


;;;-------------------------------------------------------------------------
;;; I/O
;;;-------------------------------------------------------------------------

(defsyscall (read "read")
    (ssize-t :restart t :handle fd)
  "Read at most COUNT bytes from FD into the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall (write "write")
    (ssize-t :restart t :handle fd)
  "Write at most COUNT bytes to FD from the foreign area BUF."
  (fd    :int)
  (buf   :pointer)
  (count size-t))

(defsyscall (readv "readv")
    (ssize-t :restart t :handle fd)
  "Read from FD into the first IOVCNT buffers of the IOV array."
  (fd     :int)
  (iov    :pointer)
  (iovcnt :int))

(defsyscall (writev "writev")
    (ssize-t :restart t :handle fd)
  "Writes to FD the first IOVCNT buffers of the IOV array."
  (fd     :int)
  (iov    :pointer)
  (iovcnt :int))

(defsyscall (pread "lfp_pread")
    (ssize-t :restart t :handle fd)
  "Read at most COUNT bytes from FD at offset OFFSET into the foreign area BUF."
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(defsyscall (pwrite "lfp_pwrite")
    (ssize-t :restart t :handle fd)
  "Write at most COUNT bytes to FD at offset OFFSET from the foreign area BUF."
  (fd     :int)
  (buf    :pointer)
  (count  size-t)
  (offset off-t))

(defsyscall (sendfile "lfp_sendfile")
    (ssize-t :restart t :handle infd :handle2 outfd)
  (infd   :int)
  (outfd  :int)
  (offset off-t)
  (nbytes size-t))


;;;-------------------------------------------------------------------------
;;; Stat()
;;;-------------------------------------------------------------------------

(define-c-struct-wrapper stat ())

(defsyscall (%stat "lfp_stat")
    :int
  (file-name sstring)
  (buf       :pointer))

(defsyscall (%fstat "lfp_fstat")
    (:int :handle fd)
  (fd      :int)
  (buf     :pointer))

(defsyscall (%lstat "lfp_lstat")
    :int
  (file-name sstring)
  (buf       :pointer))

;;; If necessary for performance reasons, we can add an optional
;;; argument to this function and use that to reuse a wrapper object.
(defentrypoint funcall-stat (fn arg)
  (with-foreign-object (buf '(:struct stat))
    (funcall fn arg buf)
    (make-instance 'stat :pointer buf)))

(defentrypoint stat (path)
  "Get information about file PATH(dereferences symlinks)."
  (funcall-stat #'%stat path))

(defentrypoint fstat (fd)
  "Get information about file descriptor FD."
  (funcall-stat #'%fstat fd))

(defentrypoint lstat (path)
  "Get information about file PATH(does not dereference symlinks)."
  (funcall-stat #'%lstat path))

(defsyscall (sync "sync") :void
  "Schedule all file system buffers to be written to disk.")

(defsyscall (fsync "fsync")
    (:int :restart t)
  "Schedule a file's buffers to be written to disk."
  (fd :int))

(defsyscall (%mkstemp "lfp_mkstemp") :int
  (template :pointer))

(defentrypoint mkstemp (&optional (template ""))
  "Generate a unique temporary filename from TEMPLATE.
Return two values: the file descriptor and the path of the temporary file."
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-sstring-to-cstring (ptr template)
      (values (%mkstemp ptr) (cstring-to-sstring ptr)))))

(defsyscall (%mkostemp "lfp_mkostemp") :int
  (template :pointer)
  (flags    :uint64))

(defentrypoint mkostemp (&optional (template "") (flags 0))
  "Generate a unique temporary filename from TEMPLATE.
FLAGS are used to open the temporary file.
Return two values: the file descriptor and the path of the temporary file."
  (let ((template (concatenate 'string template "XXXXXX")))
    (with-sstring-to-cstring (ptr template)
      (values (%mkostemp ptr flags) (cstring-to-sstring ptr)))))


;;;-------------------------------------------------------------------------
;;; Directories
;;;-------------------------------------------------------------------------

(defsyscall (mkdir "mkdir") :int
  "Create directory PATH with permissions MODE."
  (path sstring)
  (mode mode-t))

(defsyscall (rmdir "rmdir") :int
  "Delete directory PATH."
  (path sstring))

(defsyscall (chdir "chdir") :int
  "Change the current working directory to PATH."
  (path sstring))

(defsyscall (fchdir "fchdir")
    (:int :restart t :handle fd)
  "Change the current working directory to the directory referenced by FD."
  (fd :int))

(defsyscall (%getcwd "getcwd") :pointer
  (buf :pointer)
  (size size-t))

(defentrypoint getcwd ()
  "Return the current working directory as a string."
  (with-cstring-to-sstring (buf +cstring-path-max+ bufsize)
    (%getcwd buf bufsize)))

(defsyscall (%mkdtemp "mkdtemp") sstring
  (template sstring))

(defentrypoint mkdtemp (&optional (template ""))
  "Generate a unique temporary filename from TEMPLATE."
  (let ((template (concatenate 'string template "XXXXXX")))
    (%mkdtemp template)))


;;;-------------------------------------------------------------------------
;;; File Descriptors
;;;-------------------------------------------------------------------------

(defsyscall (close "close")
    (:int :handle fd)
  "Close open file descriptor FD."
  (fd :int))

(defsyscall (dup "dup")
    (:int :handle fd)
  "Duplicate file descriptor FD."
  (fd :int))

(defsyscall (dup2 "dup2")
    (:int :restart t :handle oldfd :handle2 newfd)
  "Make NEWFD be the copy of OLDFD, closing NEWFD first if necessary."
  (oldfd :int)
  (newfd :int))

(defsyscall (%fcntl/noarg "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int))

;;; FIXME: Linux/glibc says ARG's type is long, POSIX says it's int.
;;; Is this an issue?
(defsyscall (%fcntl/int "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int)
  (arg :int))

(defsyscall (%fcntl/pointer "fcntl")
    (:int :handle fd)
  (fd  :int)
  (cmd :int)
  (arg :pointer))

(defentrypoint fcntl (fd cmd &optional (arg nil argp))
  (cond
    ((not argp)     (%fcntl/noarg   fd cmd))
    ((integerp arg) (%fcntl/int     fd cmd arg))
    ((pointerp arg) (%fcntl/pointer fd cmd arg))
    (t (error 'type-error :datum arg
              :expected-type '(or null integer foreign-pointer)))))

(defsyscall (%ioctl/noarg "ioctl")
    (:int :handle fd)
  "Send request REQUEST to file referenced by FD."
  (fd      :int)
  (request :unsigned-int))

(defsyscall (%ioctl/pointer "ioctl")
    (:int :handle fd)
  "Send request REQUEST to file referenced by FD using argument ARG."
 (fd      :int)
 (request :unsigned-int)
 (arg     :pointer))

(defsyscall (%ioctl/integer "ioctl")
    (:int :handle fd)
  "Send request REQUEST to file referenced by FD using argument ARG."
 (fd      :int)
 (request :unsigned-int)
 (arg     :unsigned-int))

(defentrypoint ioctl (fd request &optional (arg nil argp))
  "Control an I/O device."
  (cond
    ((not argp)     (%ioctl/noarg   fd request))
    ((pointerp arg) (%ioctl/pointer fd request arg))
    ((integerp arg) (%ioctl/integer fd request arg))
    (t (error 'type-error :datum arg
              :expected-type '(or null integer foreign-pointer)))))

(defsyscall (fd-cloexec-p "lfp_is_fd_cloexec") bool-designator
  (fd :int))

(defsyscall (%set-fd-cloexec "lfp_set_fd_cloexec") :int
  (fd      :int)
  (enabled bool-designator))

(defentrypoint (setf fd-cloexec-p) (enabled fd)
  (%set-fd-cloexec fd enabled))

(defsyscall (fd-nonblock-p "lfp_is_fd_nonblock") bool-designator
  (fd :int))

(defsyscall (%set-fd-nonblock "lfp_set_fd_nonblock") :int
  (fd      :int)
  (enabled bool-designator))

(defentrypoint (setf fd-nonblock-p) (enabled fd)
  (%set-fd-nonblock fd enabled))

(defsyscall (fd-open-p "lfp_is_fd_open") bool-designator
  (fd :int))

(defsyscall (fd-tty-p "isatty") bool-designator
  (fd :int))



;;;-------------------------------------------------------------------------
;;; TTYs
;;;-------------------------------------------------------------------------

(defsyscall (openpt "lfp_openpt") :int
  (flags :uint64))

(defsyscall (grantpt "grantpt")
    (:int :handle fd)
  (fd :int))

(defsyscall (unlockpt "unlockpt")
    (:int :handle fd)
  (fd :int))

(defsyscall (%ptsname "lfp_ptsname")
    (:int :handle fd)
  (fd     :int)
  (buf    :pointer)
  (buflen size-t))

(defentrypoint ptsname (fd)
  (with-foreign-pointer (buf +cstring-path-max+ bufsize)
    (%ptsname fd buf bufsize)
    (nth-value 0 (foreign-string-to-lisp buf))))


;;;-------------------------------------------------------------------------
;;; I/O polling
;;;-------------------------------------------------------------------------

(defsyscall (select "lfp_select") :int
  "Scan for I/O activity on multiple file descriptors."
  (nfds      :int)
  (readfds   :pointer)
  (writefds  :pointer)
  (exceptfds :pointer)
  (timeout   :pointer))

(defentrypoint copy-fd-set (from to)
  (memcpy to from (sizeof '(:struct fd-set)))
  to)

(defcfun (fd-clr "lfp_fd_clr") :void
  (fd     :int)
  (fd-set :pointer))

(defcfun (fd-isset "lfp_fd_isset") :bool
  (fd     :int)
  (fd-set :pointer))

(defcfun (fd-set "lfp_fd_set") :void
  (fd     :int)
  (fd-set :pointer))

(defcfun (fd-zero "lfp_fd_zero") :void
  (fd-set :pointer))

;;; FIXME: Until a way to autodetect platform features is implemented
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'pollrdhup)
    (defconstant pollrdhup 0)))

(defsyscall (poll "poll") :int
  "Scan for I/O activity on multiple file descriptors."
  (fds     :pointer)
  (nfds    nfds-t)
  (timeout :int))

#+linux
(progn
  (defsyscall (epoll-create "epoll_create") :int
    "Open an epoll file descriptor."
    (size :int))

  (defsyscall (epoll-ctl "epoll_ctl")
      (:int :handle epfd :handle2 fd)
    "Control interface for an epoll descriptor."
    (epfd  :int)
    (op    :int)
    (fd    :int)
    (event :pointer))

  (defsyscall (epoll-wait "epoll_wait")
      (:int :handle epfd)
    "Wait for an I/O event on an epoll file descriptor."
    (epfd      :int)
    (events    :pointer)
    (maxevents :int)
    (timeout   :int)))

#+bsd
(progn
  (defsyscall (kqueue "kqueue") :int
    "Open a kernel event queue.")

  (defsyscall (kevent "kevent")
      (:int :handle fd)
    "Control interface for a kernel event queue."
    (fd         :int)
    (changelist :pointer)               ; const struct kevent *
    (nchanges   :int)
    (eventlist  :pointer)               ; struct kevent *
    (nevents    :int)
    (timeout    :pointer))              ; const struct timespec *

  (defentrypoint ev-set (%kev %ident %filter %flags %fflags %data %udata)
    (with-foreign-slots ((ident filter flags fflags data udata) %kev kevent)
      (setf ident %ident filter %filter flags %flags
            fflags %fflags data %data udata %udata))))


;;;-------------------------------------------------------------------------
;;; Socket message readers
;;;-------------------------------------------------------------------------

(defcfun (cmsg.firsthdr "lfp_cmsg_firsthdr") :pointer
  (msgh :pointer))

(defcfun (cmsg.nxthdr "lfp_cmsg_nxthdr") :pointer
  (msgh :pointer)
  (cmsg :pointer))

(defcfun (cmsg.space "lfp_cmsg_space") size-t
  (length size-t))

(defcfun (cmsg.len "lfp_cmsg_len") size-t
  (length size-t))

(defcfun (cmsg.data "lfp_cmsg_data") :pointer
  (cmsg :pointer))


;;;-------------------------------------------------------------------------
;;; Directory walking
;;;-------------------------------------------------------------------------

(defsyscall (opendir "opendir") :pointer
  "Open directory PATH for listing of its contents."
  (path sstring))

(defsyscall (closedir "closedir") :int
  "Close directory DIR when done listing its contents."
  (dirp :pointer))

(defsyscall (%readdir "lfp_readdir") :int
  (dirp   :pointer)
  (entry  :pointer)
  (result :pointer))

(defentrypoint readdir (dir)
  "Reads an item from the listing of directory DIR (reentrant)."
  (with-foreign-objects ((entry '(:struct dirent))
                         (result :pointer))
    (%readdir dir entry result)
    (if (null-pointer-p (mem-ref result :pointer))
        nil
        (with-foreign-slots ((name type fileno) entry (:struct dirent))
          (values (cstring-to-sstring name) type fileno)))))

(defsyscall (rewinddir "rewinddir") :void
  "Rewind directory DIR."
  (dirp :pointer))

(defsyscall (seekdir "seekdir") :void
  "Seek into directory DIR to position POS(as returned by TELLDIR)."
  (dirp :pointer)
  (pos  :long))

;;; FIXME: According to POSIX docs "no errors are defined" for
;;; telldir() but Linux manpages specify a possible EBADF.
(defsyscall (telldir "telldir") off-t
  "Return the current location in directory DIR."
  (dirp :pointer))


;;;-------------------------------------------------------------------------
;;; Memory mapping
;;;-------------------------------------------------------------------------

(defsyscall (mmap "lfp_mmap")
    (:pointer :handle fd)
  "Map file referenced by FD at offset OFFSET into address space of the
calling process at address ADDR and length LENGTH.
PROT describes the desired memory protection of the mapping.
FLAGS determines whether updates to the mapping are visible to other
processes mapping the same region."
  (addr   :pointer)
  (length size-t)
  (prot   :int)
  (flags  :int)
  (fd     :int)
  (offset off-t))

(defsyscall (munmap "munmap") :int
  "Unmap pages of memory starting at address ADDR with length LENGTH."
  (addr   :pointer)
  (length size-t))


;;;-------------------------------------------------------------------------
;;; Process creation and info
;;;-------------------------------------------------------------------------

(defsyscall (fork "fork") pid-t)

(defsyscall (execv "execv") :int
  (path sstring)
  (argv :pointer))

(defsyscall (execvp "execvp") :int
  (file sstring)
  (argv :pointer))

(defsyscall (execve "execve") :int
  (file sstring)
  (argv :pointer)
  (envp :pointer))

(defsyscall (%waitpid "waitpid") pid-t
  (pid     pid-t)
  (status  :pointer)
  (options :int))

(defentrypoint waitpid (pid options)
  (with-foreign-pointer (status (sizeof :int))
    (let ((ret (%waitpid pid status options)))
      (values ret (mem-ref status :int)))))

(defsyscall (getpid "getpid") pid-t
  "Returns the process id of the current process")

(defsyscall (getppid "getppid") pid-t
  "Returns the process id of the current process's parent")

#+linux
(defentrypoint gettid ()
  (foreign-funcall "syscall" :int sys-gettid :int))

(defsyscall (getuid "getuid") uid-t
  "Get real user id of the current process.")

(defsyscall (setuid "setuid") :int
  "Set real user id of the current process to UID."
  (uid uid-t))

(defsyscall (geteuid "geteuid") uid-t
  "Get effective user id of the current process.")

(defsyscall (seteuid "seteuid") :int
  "Set effective user id of the current process to UID."
  (uid uid-t))

(defsyscall (getgid "getgid") gid-t
  "Get real group id of the current process.")

(defsyscall (setgid "setgid") :int
  "Set real group id of the current process to GID."
  (gid gid-t))

(defsyscall (getegid "getegid") gid-t
  "Get effective group id of the current process.")

(defsyscall (setegid "setegid") :int
  "Set effective group id of the current process to GID."
  (gid gid-t))

(defsyscall (setreuid "setreuid") :int
  "Set real and effective user id of the current process to RUID and EUID."
  (ruid uid-t)
  (euid uid-t))

(defsyscall (setregid "setregid") :int
  "Set real and effective group id of the current process to RGID and EGID."
  (rgid gid-t)
  (egid gid-t))

(defsyscall (getpgid "getpgid") pid-t
  "Get process group id of process PID."
  (pid pid-t))

(defsyscall (setpgid "setpgid") :int
  "Set process group id of process PID to value PGID."
  (pid  pid-t)
  (pgid pid-t))

(defsyscall (getpgrp "getpgrp") pid-t
  "Get process group id of the current process.")

(defsyscall (setpgrp "setpgrp") pid-t
  "Set process group id of the current process.")

(defsyscall (setsid "setsid") pid-t
  "Create session and set process group id of the current process.")

(defsyscall (%getrlimit "lfp_getrlimit")
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint getrlimit (resource)
  "Return soft and hard limit of system resource RESOURCE."
  (with-foreign-object (rl '(:struct rlimit))
    (with-foreign-slots ((cur max) rl (:struct rlimit))
      (%getrlimit resource rl)
      (values cur max))))

(defsyscall (%setrlimit "lfp_setrlimit")
    :int
  (resource :int)
  (rlimit   :pointer))

(defentrypoint setrlimit (resource soft-limit hard-limit)
  "Set SOFT-LIMIT and HARD-LIMIT of system resource RESOURCE."
  (with-foreign-object (rl '(:struct rlimit))
    (with-foreign-slots ((cur max) rl (:struct rlimit))
      (setf cur soft-limit
            max hard-limit)
      (%setrlimit resource rl))))

(defsyscall (%getrusage "getrusage") :int
  (who   :int)
  (usage :pointer))

;;; TODO: it might be more convenient to return a wrapper object here
;;; instead like we do in STAT.
(defentrypoint getrusage (who)
  "Return resource usage measures of WHO."
  (with-foreign-object (ru '(:struct rusage))
    (%getrusage who ru)
    (with-foreign-slots ((maxrss ixrss idrss isrss minflt majflt nswap inblock
                          oublock msgsnd msgrcv nsignals nvcsw nivcsw)
                         ru (:struct rusage))
      (values (foreign-slot-value
               (foreign-slot-pointer ru '(:struct rusage) 'utime)
               '(:struct timeval) 'sec)
              (foreign-slot-value
               (foreign-slot-pointer ru '(:struct rusage) 'utime)
               '(:struct timeval) 'usec)
              (foreign-slot-value
               (foreign-slot-pointer ru '(:struct rusage) 'stime)
               '(:struct timeval) 'sec)
              (foreign-slot-value
               (foreign-slot-pointer ru '(:struct rusage) 'stime)
               '(:struct timeval) 'usec)
              maxrss ixrss idrss isrss minflt majflt
              nswap inblock oublock msgsnd
              msgrcv nsignals nvcsw nivcsw))))

(defsyscall (getpriority "getpriority") :int
  "Get the scheduling priority of a process, process group, or user,
as indicated by WHICH and WHO."
  (which :int)
  (who   :int))

(defsyscall (setpriority "setpriority") :int
  "Set the scheduling priority of a process, process group, or user,
as indicated by WHICH and WHO to VALUE."
  (which :int)
  (who   :int)
  (value :int))

(defentrypoint nice (&optional (increment 0))
  "Get or set process priority."
  ;; FIXME: race condition. might need WITHOUT-INTERRUPTS on some impl.s
  (setf (errno) 0)
  (let ((retval (foreign-funcall "nice" :int increment :int))
        (errno (errno)))
    (if (and (= retval -1) (/= errno 0))
        (signal-syscall-error errno "nice")
        retval)))

(defsyscall (exit "_exit") :void
  "terminate the calling process"
  (status :int))



;;;-------------------------------------------------------------------------
;;; Signals
;;;-------------------------------------------------------------------------

(defsyscall (kill "kill") :int
  "Send signal SIG to process PID."
  (pid    pid-t)
  (signum signal))

(defsyscall (sigaction "sigaction") :int
  (signum :int)
  (act    :pointer)
  (oldact :pointer))

(defentrypoint wifexited (status)
  (plusp (foreign-funcall "lfp_wifexited" :int status :int)))

(defentrypoint wexitstatus (status)
  (foreign-funcall "lfp_wexitstatus" :int status :int))

(defentrypoint wifsignaled (status)
  (plusp (foreign-funcall "lfp_wifsignaled" :int status :int)))

(defentrypoint wtermsig (status)
  (foreign-funcall "lfp_wtermsig" :int status :int))

(defentrypoint wtermsig* (status)
  (foreign-enum-keyword 'signal (wtermsig status)))

(defentrypoint wcoredump (status)
  (plusp (foreign-funcall "lfp_wcoredump" :int status :int)))

(defentrypoint wifstopped (status)
  (plusp (foreign-funcall "lfp_wifstopped" :int status :int)))

(defentrypoint wstopsig (status)
  (foreign-funcall "lfp_wstopsig" :int status :int))

(defentrypoint wifcontinued (status)
  (plusp (foreign-funcall "lfp_wifcontinued" :int status :int)))


;;;-------------------------------------------------------------------------
;;; Time
;;;-------------------------------------------------------------------------

(defsyscall (usleep "usleep") :int
  "Suspend execution for USECONDS microseconds."
  (useconds useconds-t))

(defsyscall (%clock-getres "lfp_clock_getres") :int
  "Returns the resolution of the clock CLOCKID."
  (clockid clockid-t)
  (res     :pointer))

(defentrypoint clock-getres (clock-id)
  (with-foreign-object (ts '(:struct timespec))
    (with-foreign-slots ((sec nsec) ts (:struct timespec))
      (%clock-getres clock-id ts)
      (values sec nsec))))

(defsyscall (%clock-gettime "lfp_clock_gettime") :int
  (clockid clockid-t)
  (tp      :pointer))

(defentrypoint clock-gettime (clock-id)
  "Returns the time of the clock CLOCKID."
  (with-foreign-object (ts '(:struct timespec))
    (with-foreign-slots ((sec nsec) ts (:struct timespec))
      (%clock-gettime clock-id ts)
      (values sec nsec))))

(defsyscall (%clock-settime "lfp_clock_settime") :int
  (clockid clockid-t)
  (tp      :pointer))

(defentrypoint clock-settime (clock-id)
  "Sets the time of the clock CLOCKID."
  (with-foreign-object (ts '(:struct timespec))
    (with-foreign-slots ((sec nsec) ts (:struct timespec))
      (%clock-settime clock-id ts)
      (values sec nsec))))

;; FIXME: replace it with clock_gettime(CLOCK_MONOTONIC, ...)
(defentrypoint get-monotonic-time ()
  "Gets current time in seconds from a system's monotonic clock."
  (multiple-value-bind (seconds nanoseconds)
      (clock-gettime clock-monotonic)
    (+ seconds (/ nanoseconds 1d9))))


;;;-------------------------------------------------------------------------
;;; Environment
;;;-------------------------------------------------------------------------

(defsyscall (os-environ "lfp_get_environ") :pointer
  "Return a pointer to the current process environment.")

(defmacro %obsolete-*environ* ()
  (iolib/base::signal-obsolete '*environ* "use function OS-ENVIRON instead"
                               "symbol macro" :WARN)
  `(os-environ))

(define-symbol-macro *environ* (%obsolete-*environ*))

(defentrypoint getenv (name)
  "Returns the value of environment variable NAME."
  (when (and (pointerp name) (null-pointer-p name))
    (setf (errno) einval)
    (signal-syscall-error einval "getenv"))
  (foreign-funcall "getenv" :string name :string))

(defsyscall (setenv "setenv") :int
  "Changes the value of environment variable NAME to VALUE.
The environment variable is overwritten only if overwrite is not NIL."
  (name      :string)
  (value     :string)
  (overwrite bool-designator))

(defsyscall (unsetenv "unsetenv") :int
  "Removes the binding of environment variable NAME."
  (name :string))

;; FIXME: move into libfixposix
(defentrypoint clearenv ()
  "Remove all name-value pairs from the environment set the
OS environment to NULL."
  (let ((envptr (os-environ)))
    (unless (null-pointer-p envptr)
      (loop :for i :from 0 :by 1
            :for string := (mem-aref envptr :string i)
            :for name := (subseq string 0 (position #\= string))
            :while name :do (unsetenv name))
      (setf (mem-ref envptr :pointer) (null-pointer)))
    (values)))


;;;-------------------------------------------------------------------------
;;; Hostname info
;;;-------------------------------------------------------------------------

(defsyscall (%gethostname "gethostname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint gethostname ()
  "Return the host name of the current machine."
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%gethostname cstr size)))

(defsyscall (%getdomainname "getdomainname") :int
  (name    :pointer)
  (namelen size-t))

(defentrypoint getdomainname ()
  "Return the domain name of the current machine."
  (with-foreign-pointer-as-string ((cstr size) 256)
    (%getdomainname cstr size)))

(defsyscall (%uname "uname") :int
  (buf :pointer))

(defentrypoint uname ()
  "Get name and information about current kernel."
  (with-foreign-object (buf '(:struct utsname))
    (bzero buf (sizeof '(:struct utsname)))
    (%uname buf)
    (macrolet ((utsname-slot (name)
                 `(foreign-string-to-lisp
                   (foreign-slot-pointer buf '(:struct utsname) ',name))))
      (values (utsname-slot sysname)
              (utsname-slot nodename)
              (utsname-slot release)
              (utsname-slot version)
              (utsname-slot machine)))))


;;;-------------------------------------------------------------------------
;;; User info
;;;-------------------------------------------------------------------------

(defsyscall (%getpwuid-r "getpwuid_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (uid     uid-t)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%getpwnam-r "getpwnam_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (name    :string)
  (pwd     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defun funcall-getpw (fn arg)
  (with-foreign-objects ((pw '(:struct passwd))
                         (pwp :pointer))
    (with-foreign-pointer (buf +cstring-path-max+ bufsize)
      (with-foreign-slots ((name passwd uid gid gecos dir shell)
                           pw (:struct passwd))
        (funcall fn arg pw buf bufsize pwp)
        (if (null-pointer-p (mem-ref pwp :pointer))
            nil
            (values name passwd uid gid gecos dir shell))))))

(defentrypoint getpwuid (uid)
  "Gets the passwd info of a user, by user id (reentrant)."
  (funcall-getpw #'%getpwuid-r uid))

(defentrypoint getpwnam (name)
  "Gets the passwd info of a user, by username (reentrant)."
  (funcall-getpw #'%getpwnam-r name))


;;;-------------------------------------------------------------------------
;;; Group info
;;;-------------------------------------------------------------------------

(defsyscall (%getgrgid-r "getgrgid_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (uid     uid-t)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

(defsyscall (%getgrnam-r "getgrnam_r")
    (:int
     :error-predicate plusp
     :error-location :return)
  (name    :string)
  (grp     :pointer)
  (buffer  :pointer)
  (bufsize size-t)
  (result  :pointer))

;; FIXME: return group members too
(defun funcall-getgr (fn arg)
  (with-foreign-objects ((gr '(:struct group))
                         (grp :pointer))
    (with-foreign-pointer (buf +cstring-path-max+ bufsize)
      (with-foreign-slots ((name passwd gid) gr (:struct group))
        (funcall fn arg gr buf bufsize grp)
        (if (null-pointer-p (mem-ref grp :pointer))
            nil
            (values name passwd gid))))))

(defentrypoint getgrgid (gid)
  "Gets a group info, by group id (reentrant)."
  (funcall-getgr #'%getgrgid-r gid))

(defentrypoint getgrnam (name)
  "Gets a group info, by group name (reentrant)."
  (funcall-getgr #'%getgrnam-r name))


;;;-------------------------------------------------------------------------
;;; Syslog
;;;-------------------------------------------------------------------------

(defsyscall (openlog "lfp_openlog") :void
  "Opens a connection to the system logger for a program."
  (ident    :string)
  (option   :int)
  (facility :int))

(defsyscall (%syslog "lfp_syslog") :void
  "Generates a log message, which will be distributed by syslogd."
  (priority :int)
  (format   :string)
  (message  :string))

(defentrypoint syslog (priority format &rest args)
  "Generates a log message, which will be distributed by syslogd.
Using a FORMAT string and ARGS for lisp-side message formating."
  (with-foreign-string (c-string (apply #'format nil format args))
    (%syslog priority "%s" c-string)))

(defsyscall (closelog "lfp_closelog") :void
  "Closes the descriptor being used to write to the system logger (optional).")

(defsyscall (setlogmask "lfp_setlogmask") :int
  "Set the log mask level."
  (mask :int))

(defsyscall (log-mask "lfp_log_mask") :int
  "Log mask corresponding to PRIORITY."
  (priority :int))

(defsyscall (log-upto "lfp_log_upto") :int
  "Log mask upto and including PRIORITY."
  (priority :int))

(defmacro with-syslog ((identity &key (options log-ndelay) (facility log-daemon))
                       &body body)
  `(unwind-protect
        (progn
          (openlog ,identity ,options ,facility)
          ,@body)
     (closelog)))
