;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign type definitions for *NIX systems.
;;;

(in-package :iolib/syscalls)

(pkg-config-cflags "libfixposix")

(include "lfp.h")

(include "sys/poll.h" ;; FIXME: add poll() to LFP
         "sys/ioctl.h" "sys/utsname.h"
         "pwd.h" "grp.h")

#+linux
(include "sys/epoll.h" "sys/syscall.h")

#+bsd
(include "sys/event.h" "sys/time.h")    ; for kqueue


;;;-------------------------------------------------------------------------
;;; LibFixPOSIX build info
;;;-------------------------------------------------------------------------

(cstruct lfp-buildinfo "struct lfp_buildinfo"
  (release "release" :type :uint64)
  (vcsid   "vcsid"   :type :string))



;;;-------------------------------------------------------------------------
;;; Simple POSIX types
;;;-------------------------------------------------------------------------

(ctype bool "bool")
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype intptr-t "intptr_t")
(ctype uintptr-t "uintptr_t")
(ctype pid-t "pid_t")
(ctype uid-t "uid_t")
(ctype gid-t "gid_t")
(ctype off-t "off_t")
(ctype mode-t "mode_t")
(ctype time-t "time_t")
(ctype useconds-t "useconds_t")
(ctype suseconds-t "suseconds_t")
(ctype dev-t "dev_t")
(ctype ino-t "ino_t")
(ctype nlink-t "nlink_t")
(ctype blksize-t "blksize_t")
(ctype blkcnt-t "blkcnt_t")
(ctype nfds-t "nfds_t")
(ctype rlim-t "rlim_t")
(ctype id-t "id_t")
(ctype clockid-t "clockid_t")


;;;-------------------------------------------------------------------------
;;; Structs, slots and C constants
;;;-------------------------------------------------------------------------

;;; errno.h

(constantenum (errno-values :define-constants t)
 ((:e2big "E2BIG"))
 ((:eacces "EACCES"))
 ((:eaddrinuse "EADDRINUSE"))
 ((:eaddrnotavail "EADDRNOTAVAIL"))
 ((:eadv "EADV") :optional t)
 ((:eafnosupport "EAFNOSUPPORT"))
 ((:ealready "EALREADY"))
 ((:ebade "EBADE") :optional t)
 ((:ebadf "EBADF"))
 ((:ebadfd "EBADFD") :optional t)
 ((:ebadmsg "EBADMSG"))
 ((:ebadr "EBADR") :optional t)
 ((:ebadrqc "EBADRQC") :optional t)
 ((:ebadslt "EBADSLT") :optional t)
 ((:ebfont "EBFONT") :optional t)
 ((:ebusy "EBUSY"))
 ((:ecanceled "ECANCELED"))
 ((:echild "ECHILD"))
 ((:echrng "ECHRNG") :optional t)
 ((:ecomm "ECOMM") :optional t)
 ((:econnaborted "ECONNABORTED"))
 ((:econnrefused "ECONNREFUSED"))
 ((:econnreset "ECONNRESET"))
 ((:edeadlk "EDEADLK"))
 ((:edestaddrreq "EDESTADDRREQ"))
 ((:edom "EDOM"))
 ((:edotdot "EDOTDOT") :optional t)
 ((:edquot "EDQUOT"))
 ((:eexist "EEXIST"))
 ((:efault "EFAULT"))
 ((:efbig "EFBIG"))
 ((:ehostdown "EHOSTDOWN"))
 ((:ehostunreach "EHOSTUNREACH"))
 ((:ehwpoison "EHWPOISON") :optional t)
 ((:eidrm "EIDRM"))
 ((:eilseq "EILSEQ"))
 ((:einprogress "EINPROGRESS"))
 ((:eintr "EINTR"))
 ((:einval "EINVAL"))
 ((:eio "EIO"))
 ((:eisconn "EISCONN"))
 ((:eisdir "EISDIR"))
 ((:eisnam "EISNAM") :optional t)
 ((:ekeyexpired "EKEYEXPIRED") :optional t)
 ((:ekeyrejected "EKEYREJECTED") :optional t)
 ((:ekeyrevoked "EKEYREVOKED") :optional t)
 ((:el2hlt "EL2HLT") :optional t)
 ((:el2nsync "EL2NSYNC") :optional t)
 ((:el3hlt "EL3HLT") :optional t)
 ((:el3rst "EL3RST") :optional t)
 ((:elibacc "ELIBACC") :optional t)
 ((:elibbad "ELIBBAD") :optional t)
 ((:elibexec "ELIBEXEC") :optional t)
 ((:elibmax "ELIBMAX") :optional t)
 ((:elibscn "ELIBSCN") :optional t)
 ((:elnrng "ELNRNG") :optional t)
 ((:eloop "ELOOP"))
 ((:emediumtype "EMEDIUMTYPE") :optional t)
 ((:emfile "EMFILE"))
 ((:emlink "EMLINK"))
 ((:emsgsize "EMSGSIZE"))
 ((:emultihop "EMULTIHOP") :optional t)
 ((:enametoolong "ENAMETOOLONG"))
 ((:enavail "ENAVAIL") :optional t)
 ((:enetdown "ENETDOWN"))
 ((:enetreset "ENETRESET"))
 ((:enetunreach "ENETUNREACH"))
 ((:enfile "ENFILE"))
 ((:enoano "ENOANO") :optional t)
 ((:enobufs "ENOBUFS"))
 ((:enocsi "ENOCSI") :optional t)
 ((:enodata "ENODATA") :optional t)
 ((:enodev "ENODEV"))
 ((:enoent "ENOENT"))
 ((:enoexec "ENOEXEC"))
 ((:enokey "ENOKEY") :optional t)
 ((:enolck "ENOLCK"))
 ((:enolink "ENOLINK") :optional t)
 ((:enomedium "ENOMEDIUM") :optional t)
 ((:enomem "ENOMEM"))
 ((:enomsg "ENOMSG"))
 ((:enonet "ENONET") :optional t)
 ((:enopkg "ENOPKG") :optional t)
 ((:enoprotoopt "ENOPROTOOPT"))
 ((:enospc "ENOSPC"))
 ((:enosr "ENOSR") :optional t)
 ((:enostr "ENOSTR") :optional t)
 ((:enosys "ENOSYS"))
 ((:enotblk "ENOTBLK") :optional t)
 ((:enotconn "ENOTCONN"))
 ((:enotdir "ENOTDIR"))
 ((:enotempty "ENOTEMPTY"))
 ((:enotnam "ENOTNAM") :optional t)
 ((:enotrecoverable "ENOTRECOVERABLE") :optional t)
 ((:enotsock "ENOTSOCK"))
 ((:enotsup "ENOTSUP"))
 ((:enotty "ENOTTY"))
 ((:enotuniq "ENOTUNIQ") :optional t)
 ((:enxio "ENXIO"))
 ((:eopnotsupp "EOPNOTSUPP"))
 ((:eoverflow "EOVERFLOW"))
 ((:eownerdead "EOWNERDEAD") :optional t)
 ((:eperm "EPERM"))
 ((:epfnosupport "EPFNOSUPPORT") :optional t)
 ((:epipe "EPIPE"))
 ((:eproto "EPROTO"))
 ((:eprotonosupport "EPROTONOSUPPORT"))
 ((:eprototype "EPROTOTYPE"))
 ((:erange "ERANGE"))
 ((:eremchg "EREMCHG") :optional t)
 ((:eremote "EREMOTE") :optional t)
 ((:eremoteio "EREMOTEIO") :optional t)
 ((:erestart "ERESTART") :optional t)
 ((:erfkill "ERFKILL") :optional t)
 ((:erofs "EROFS"))
 ((:eshutdown "ESHUTDOWN"))
 ((:esocktnosupport "ESOCKTNOSUPPORT") :optional t)
 ((:espipe "ESPIPE"))
 ((:esrch "ESRCH"))
 ((:esrmnt "ESRMNT") :optional t)
 ((:estale "ESTALE"))
 ((:estrpipe "ESTRPIPE") :optional t)
 ((:etime "ETIME") :optional t)
 ((:etimedout "ETIMEDOUT"))
 ((:etoomanyrefs "ETOOMANYREFS") :optional t)
 ((:etxtbsy "ETXTBSY"))
 ((:euclean "EUCLEAN") :optional t)
 ((:eunatch "EUNATCH") :optional t)
 ((:eusers "EUSERS") :optional t)
 ((:ewouldblock "EWOULDBLOCK"))
 ((:exdev "EXDEV"))
 ((:exfull "EXFULL") :optional t)
 ((:ebug "EBUG")))


;;; fcntl.h

;; Open()

(constant (o-rdonly "O_RDONLY"))
(constant (o-wronly "O_WRONLY"))
(constant (o-rdwr "O_RDWR"))
(constant (o-creat "O_CREAT"))
(constant (o-excl "O_EXCL"))
(constant (o-trunc "O_TRUNC"))
(constant (o-append "O_APPEND"))

(constant (o-noctty "O_NOCTTY"))
(constant (o-nonblock "O_NONBLOCK"))
(constant (o-ndelay "O_NDELAY"))
(constant (o-sync "O_SYNC"))
(constant (o-nofollow "O_NOFOLLOW"))
(constant (o-async "O_ASYNC"))
(constant (o-cloexec "O_CLOEXEC"))

;;; Fcntl()

(constant (f-dupfd "F_DUPFD"))
(constant (f-getfd "F_GETFD"))
(constant (f-setfd "F_SETFD"))
(constant (f-getfl "F_GETFL"))
(constant (f-setfl "F_SETFL"))
(constant (f-getlk "F_GETLK"))
(constant (f-setlk "F_SETLK"))
(constant (f-setlkw "F_SETLKW"))
(constant (f-getown "F_GETOWN"))
(constant (f-setown "F_SETOWN"))
(constant (f-rdlck "F_RDLCK"))
(constant (f-wrlck "F_WRLCK"))
(constant (f-unlck "F_UNLCK"))
#+linux
(progn
  (constant (f-getsig "F_GETSIG"))
  (constant (f-setsig "F_SETSIG"))
  (constant (f-setlease "F_SETLEASE"))
  (constant (f-getlease "F_GETLEASE")))


;;; unistd.h

;; Lseek()

(constant (seek-set "SEEK_SET"))
(constant (seek-cur "SEEK_CUR"))
(constant (seek-end "SEEK_END"))

;; Access()

(constant (r-ok "R_OK"))
(constant (w-ok "W_OK"))
(constant (x-ok "X_OK"))
(constant (f-ok "F_OK"))


;;; time.h

(constant (clock-realtime "CLOCK_REALTIME"))
(constant (clock-monotonic "CLOCK_MONOTONIC"))

(cstruct timespec "struct timespec"
  "UNIX time specification in seconds and nanoseconds."
  (sec  "tv_sec"  :type time-t)
  (nsec "tv_nsec" :type :long))


;;; sys/stat.h

(constant (path-max "PATH_MAX" "MAXPATHLEN"))

(cstruct stat "struct stat"
  (dev     "st_dev"     :type dev-t)
  (ino     "st_ino"     :type ino-t)
  (mode    "st_mode"    :type mode-t)
  (nlink   "st_nlink"   :type nlink-t)
  (uid     "st_uid"     :type uid-t)
  (gid     "st_gid"     :type gid-t)
  (rdev    "st_rdev"    :type dev-t)
  (size    "st_size"    :type off-t)
  (blksize "st_blksize" :type blkcnt-t)
  (blocks  "st_blocks"  :type blksize-t)
  (atime   "st_atime"   :type time-t)
  (mtime   "st_mtime"   :type time-t)
  (ctime   "st_ctime"   :type time-t))

(constant (s-irwxu "S_IRWXU"))
(constant (s-irusr "S_IRUSR"))
(constant (s-iwusr "S_IWUSR"))
(constant (s-ixusr "S_IXUSR"))
(constant (s-ifmt "S_IFMT"))
(constant (s-ififo "S_IFIFO"))
(constant (s-ifchr "S_IFCHR"))
(constant (s-ifdir "S_IFDIR"))
(constant (s-ifblk "S_IFBLK"))
(constant (s-ifreg "S_IFREG"))
(constant (s-ifwht "S_IFWHT") :optional t)
(constant (s-iread "S_IREAD"))
(constant (s-iwrite "S_IWRITE"))
(constant (s-iexec "S_IEXEC"))

(constant (s-irwxg "S_IRWXG"))
(constant (s-irgrp "S_IRGRP"))
(constant (s-iwgrp "S_IWGRP"))
(constant (s-ixgrp "S_IXGRP"))
(constant (s-irwxo "S_IRWXO"))
(constant (s-iroth "S_IROTH"))
(constant (s-iwoth "S_IWOTH"))
(constant (s-ixoth "S_IXOTH"))
(constant (s-isuid "S_ISUID"))
(constant (s-isgid "S_ISGID"))
(constant (s-isvtx "S_ISVTX"))
(constant (s-iflnk "S_IFLNK"))
(constant (s-ifsock "S_IFSOCK"))


;;; sys/ioctl.h

;; Ioctl()

(constant (fionbio "FIONBIO"))
(constant (fionread "FIONREAD"))


;;; sys/wait.h

(constant (wnohang "WNOHANG"))
(constant (wuntraced "WUNTRACED"))
(constant (wcontinued "WCONTINUED"))


;;;-------------------------------------------------------------------------
;;; signal.h
;;;-------------------------------------------------------------------------

;; POSIX.1-1990
(constantenum (signal :define-constants t)
 ((:sighup    "SIGHUP"))
 ((:sigint    "SIGINT"))
 ((:sigquit   "SIGQUIT"))
 ((:sigill    "SIGILL"))
 ((:sigabrt   "SIGABRT"))
 ((:sigfpe    "SIGFPE"))
 ((:sigkill   "SIGKILL"))
 ((:sigsegv   "SIGSEGV"))
 ((:sigpipe   "SIGPIPE"))
 ((:sigalrm   "SIGALRM"))
 ((:sigterm   "SIGTERM"))
 ((:sigusr1   "SIGUSR1"))
 ((:sigusr2   "SIGUSR2"))
 ((:sigchld   "SIGCHLD"))
 ((:sigcont   "SIGCONT"))
 ((:sigstop   "SIGSTOP"))
 ((:sigtstp   "SIGTSTP"))
 ((:sigttin   "SIGTTIN"))
 ((:sigttou   "SIGTTOU"))
 ;; POSIX.1-2001
 ((:sigbus    "SIGBUS"))
 ((:sigpoll   "SIGPOLL") :optional t)
 ((:sigprof   "SIGPROF"))
 ((:sigsys    "SIGSYS"))
 ((:sigtrap   "SIGTRAP"))
 ((:sigurg    "SIGURG"))
 ((:sigvtalrm "SIGVTALRM"))
 ((:sigxcpu   "SIGXCPU"))
 ((:sigxfsz   "SIGXFSZ"))
 ;; Other signals
 ((:sigemt    "SIGEMT") :optional t)
 ((:sigio     "SIGIO"))
 ((:sigcld    "SIGCLD") :optional t)
 ((:sigpwr    "SIGPWR") :optional t)
 ((:siginfo   "SIGINFO") :optional t)
 ((:siglost   "SIGLOST") :optional t)
 ((:sigwinch  "SIGWINCH")))

(constant (sig-ign "SIG_IGN"))
(constant (sig-dfl "SIG_DFL"))

(cstruct sigaction "struct sigaction"
  (handler "sa_handler" :type :pointer)
  (sigaction "sa_sigaction" :type :pointer)
  ;; actual type can be structure or array...
  (mask "sa_mask" :type :unsigned-long)
  (flags "sa_flags" :type :int))

(constant (sa-nocldstop "SA_NOCLDSTOP"))
(constant (sa-nocldwait "SA_NOCLDWAIT"))
(constant (sa-nodefer "SA_NODEFER"))
(constant (sa-onstack "SA_ONSTACK"))
(constant (sa-resethand "SA_RESETHAND"))
(constant (sa-restart "SA_RESTART"))
(constant (sa-siginfo "SA_SIGINFO"))


;;; sys/mman.h

;; Mmap()

(constant (prot-none   "PROT_NONE"))
(constant (prot-read   "PROT_READ"))
(constant (prot-write  "PROT_WRITE"))
(constant (prot-exec   "PROT_EXEC"))
(constant (map-shared  "MAP_SHARED"))
(constant (map-private "MAP_PRIVATE"))
(constant (map-fixed   "MAP_FIXED"))
(constant (map-failed  "MAP_FAILED"))


;;; sys/select.h

(cstruct fd-set "fd_set")
(constant (fd-setsize "FD_SETSIZE"))

(cstruct timeval "struct timeval"
  "UNIX time specification in seconds and microseconds."
  (sec  "tv_sec"  :type time-t)
  (usec "tv_usec" :type suseconds-t))


;;; sys/poll.h

;; Poll()

(cstruct pollfd "struct pollfd"
  "Poll file descriptor activity specification structure."
  (fd      "fd"      :type :int)
  (events  "events"  :type :short)
  (revents "revents" :type :short))

(constant (pollin "POLLIN"))
(constant (pollrdnorm "POLLRDNORM"))
(constant (pollrdband "POLLRDBAND"))
(constant (pollpri "POLLPRI"))
(constant (pollout "POLLOUT"))
(constant (pollwrnorm "POLLWRNORM"))
(constant (pollwrband "POLLWRBAND"))
(constant (pollerr "POLLERR"))
#+linux (constant (pollrdhup "POLLRDHUP"))
(constant (pollhup "POLLHUP"))
(constant (pollnval "POLLNVAL"))


;;; dirent.h

;; Apparently POSIX 1003.1-2001 (according to linux manpages) only
;; requires d_name.  Sigh.  I guess we should assemble some decent
;; wrapper functions.  No, struct members can't be optional at this
;; point.
(cstruct dirent "struct dirent"
  ;; POSIX actually requires this to be d_ino
  (fileno "d_fileno" :type #-freebsd ino-t #+freebsd :uint32)
  (type   "d_type"   :type :uint8)
  (name   "d_name"   :type :uint8 :count :auto))

;;; filetypes set in d_type slot of struct dirent
(constant (dt-unknown "DT_UNKNOWN"))
(constant (dt-fifo "DT_FIFO"))
(constant (dt-chr "DT_CHR"))
(constant (dt-dir "DT_DIR"))
(constant (dt-blk "DT_BLK"))
(constant (dt-reg "DT_REG"))
(constant (dt-lnk "DT_LNK"))
(constant (dt-sock "DT_SOCK"))
(constant (dt-wht "DT_WHT"))


;;;-------------------------------------------------------------------------
;;; sys/resource.h
;;;-------------------------------------------------------------------------

(cstruct rlimit "struct rlimit"
  (cur "rlim_cur" :type rlim-t)
  (max "rlim_max" :type rlim-t))

(cstruct rusage "struct rusage"
  (utime    "ru_utime"    :type (:struct timeval))
  (stime    "ru_stime"    :type (:struct timeval))
  (maxrss   "ru_maxrss"   :type :long)
  (ixrss    "ru_ixrss"    :type :long)
  (idrss    "ru_idrss"    :type :long)
  (isrss    "ru_isrss"    :type :long)
  (minflt   "ru_minflt"   :type :long)
  (majflt   "ru_majflt"   :type :long)
  (nswap    "ru_nswap"    :type :long)
  (inblock  "ru_inblock"  :type :long)
  (oublock  "ru_oublock"  :type :long)
  (msgsnd   "ru_msgsnd"   :type :long)
  (msgrcv   "ru_msgrcv"   :type :long)
  (nsignals "ru_nsignals" :type :long)
  (nvcsw    "ru_nvcsw"    :type :long)
  (nivcsw   "ru_nivcsw"   :type :long))

(constant (prio-process "PRIO_PROCESS"))
(constant (prio-pgrp "PRIO_PGRP"))
(constant (prio-user "PRIO_USER"))
(constant (rlim-infinity "RLIM_INFINITY"))
(constant (rusage-self "RUSAGE_SELF"))
(constant (rusage-children "RUSAGE_CHILDREN"))
(constant (rlimit-as "RLIMIT_AS"))
(constant (rlimit-core "RLIMIT_CORE"))
(constant (rlimit-cpu "RLIMIT_CPU"))
(constant (rlimit-data "RLIMIT_DATA"))
(constant (rlimit-fsize "RLIMIT_FSIZE"))
(constant (rlimit-memlock "RLIMIT_MEMLOCK"))
(constant (rlimit-nofile "RLIMIT_NOFILE"))
(constant (rlimit-nproc "RLIMIT_NPROC"))
(constant (rlimit-rss "RLIMIT_RSS"))
(constant (rlimit-stack "RLIMIT_STACK"))

#+linux
(progn
  (constant (rlim-saved-max "RLIM_SAVED_MAX"))
  (constant (rlim-saved-cur "RLIM_SAVED_CUR"))
  (constant (rlimit-locks "RLIMIT_LOCKS"))
  (constant (rlimit-msgqueue "RLIMIT_MSGQUEUE"))
  (constant (rlimit-nlimits "RLIMIT_NLIMITS"))
  (constant (rlimit-nice "RLIMIT_NICE"))
  (constant (rlimit-rtprio "RLIMIT_RTPRIO"))
  (constant (rlimit-sigpending "RLIMIT_SIGPENDING")))

#+(or dragonfly freebsd)
(constant (rlimit-sbsize "RLIMIT_SBSIZE"))


;;;-------------------------------------------------------------------------
;;; sys/utsname.h
;;;-------------------------------------------------------------------------

(cstruct utsname "struct utsname"
  (sysname  "sysname"  :type :char)
  (nodename "nodename" :type :char)
  (release  "release"  :type :char)
  (version  "version"  :type :char)
  (machine  "machine"  :type :char))


;;;-------------------------------------------------------------------------
;;; pwd.h
;;;-------------------------------------------------------------------------

(cstruct passwd "struct passwd"
  (name   "pw_name"   :type :string)
  (passwd "pw_passwd" :type :string)
  (uid    "pw_uid"    :type uid-t)
  (gid    "pw_gid"    :type gid-t)
  (gecos  "pw_gecos"  :type :string)
  (dir    "pw_dir"    :type :string)
  (shell  "pw_shell"  :type :string))


;;;-------------------------------------------------------------------------
;;; grp.h
;;;-------------------------------------------------------------------------

(cstruct group "struct group"
  (name   "gr_name"   :type :string)
  (passwd "gr_passwd" :type :string)
  (gid    "gr_gid"    :type gid-t)
  (mem    "gr_mem"    :type :pointer))


;;;-------------------------------------------------------------------------
;;; sys/syscall.h
;;;-------------------------------------------------------------------------

#+linux (constant (sys-gettid "SYS_gettid"))


;;;-------------------------------------------------------------------------
;;; sys/epoll.h
;;;-------------------------------------------------------------------------

#+linux
(progn
  (cunion epoll-data "epoll_data_t"
    (ptr "ptr" :type :pointer)
    (fd  "fd"  :type :int)
    (u32 "u32" :type :uint32)
    (u64 "u64" :type :uint64))

  (cstruct epoll-event "struct epoll_event"
    (events "events" :type :uint32)
    (data   "data"   :type (:union epoll-data)))

  (constant (epoll-ctl-add "EPOLL_CTL_ADD"))
  (constant (epoll-ctl-del "EPOLL_CTL_DEL"))
  (constant (epoll-ctl-mod "EPOLL_CTL_MOD"))

  (constant (epollin "EPOLLIN"))
  (constant (epollrdnorm "EPOLLRDNORM"))
  (constant (epollrdband "EPOLLRDBAND"))
  (constant (epollpri "EPOLLPRI"))
  (constant (epollout "EPOLLOUT"))
  (constant (epollwrnorm "EPOLLWRNORM"))
  (constant (epollwrband "EPOLLWRBAND"))
  (constant (epollerr "EPOLLERR"))
  (constant (epollhup "EPOLLHUP"))
  (constant (epollmsg "EPOLLMSG"))
  (constant (epolloneshot "EPOLLONESHOT"))
  (constant (epollet "EPOLLET")))


;;;-------------------------------------------------------------------------
;;; sys/event.h
;;;-------------------------------------------------------------------------

#+bsd
(progn
  (cstruct kevent "struct kevent"
    (ident  "ident"  :type uintptr-t)
    (filter "filter" :type #-netbsd :short
                           #+netbsd :uint32)
    (flags  "flags"  :type #-netbsd :unsigned-short
                           #+netbsd :uint32)
    (fflags "fflags" :type #-netbsd :unsigned-int
                           #+netbsd :uint32)
    (data   "data"   :type #-netbsd intptr-t
                           #+netbsd :int64)
    (udata  "udata"  :type :pointer))

  ;; kevent() flags
  (constant (ev-add "EV_ADD"))
  (constant (ev-enable "EV_ENABLE"))
  (constant (ev-disable "EV_DISABLE"))
  (constant (ev-delete "EV_DELETE"))
  (constant (ev-oneshot "EV_ONESHOT"))
  (constant (ev-clear "EV_CLEAR"))
  (constant (ev-eof "EV_EOF"))
  (constant (ev-error "EV_ERROR"))

  ;; kevent() filter flags
  (constant (evfilt-read "EVFILT_READ"))
  (constant (evfilt-write "EVFILT_WRITE"))
  (constant (evfilt-aio "EVFILT_AIO"))
  (constant (evfilt-vnode "EVFILT_VNODE"))
  (constant (evfilt-proc "EVFILT_PROC"))
  (constant (evfilt-signal "EVFILT_SIGNAL"))
  (constant (evfilt-timer "EVFILT_TIMER"))
  #-darwin (constant (evfilt-netdev "EVFILT_NETDEV"))

  ;; EVFILT_VNODE options
  (constant (note-delete "NOTE_DELETE"))
  (constant (note-write "NOTE_WRITE"))
  (constant (note-extend "NOTE_EXTEND"))
  (constant (note-attrib "NOTE_ATTRIB"))
  (constant (note-link "NOTE_LINK"))
  (constant (note-rename "NOTE_RENAME"))
  (constant (note-revoke "NOTE_REVOKE"))

  ;; EVFILT_PROC options
  (constant (note-exit "NOTE_EXIT"))
  (constant (note-fork "NOTE_FORK"))
  (constant (note-exec "NOTE_EXEC"))
  (constant (note-track "NOTE_TRACK"))
  (constant (note-trackerr "NOTE_TRACKERR"))

  ;; EVFILT_NETDEV options
  #-darwin
  (progn
    (constant (note-linkup "NOTE_LINKUP"))
    (constant (note-linkdown "NOTE_LINKDOWN"))
    (constant (note-linkinv "NOTE_LINKINV"))))


;;;-------------------------------------------------------------------------
;;; syslog.h
;;;-------------------------------------------------------------------------

;; Option flags for openlog.
(constant (log-pid "LOG_PID")
  :documentation "log the pid with each message")
(constant (log-cons "LOG_CONS")
  :documentation "log on the console if errors in sending")
(constant (log-odelay "LOG_ODELAY")
  :documentation "delay open until first syslog() (default)")
(constant (log-ndelay "LOG_NDELAY")
  :documentation "don't delay open")
(constant (log-nowait "LOG_NOWAIT")
  :documentation "don't wait for console forks: DEPRECATED")
(constant (log-perror "LOG_PERROR")
  :documentation "log to stderr as well")

;; facility codes
(constant (log-kern "LOG_KERN")
  :documentation "kernel messages")
(constant (log-user "LOG_USER")
  :documentation "random user-level messages")
(constant (log-mail "LOG_MAIL")
  :documentation "mail system")
(constant (log-daemon "LOG_DAEMON")
  :documentation "system daemons")
(constant (log-auth "LOG_AUTH")
  :documentation "security/authorization messages")
(constant (log-syslog "LOG_SYSLOG")
  :documentation "messages generated internally by syslogd")
(constant (log-lpr "LOG_LPR")
  :documentation "line printer subsystem")
(constant (log-news "LOG_NEWS")
  :documentation "network news subsystem")
(constant (log-uucp "LOG_UUCP")
  :documentation "UUCP subsystem")
(constant (log-cron "LOG_CRON")
  :documentation "clock daemon")
(constant (log-authpriv "LOG_AUTHPRIV")
  :documentation "security/authorization messages (private")
(constant (log-ftp "LOG_FTP")
  :documentation "ftp daemon")
#+bsd
(constant (log-security "LOG_SECURITY")
  :documentation "security subsystems")

;; other codes through 15 reserved for system use
(constant (log-local0 "LOG_LOCAL0"))
(constant (log-local1 "LOG_LOCAL1"))
(constant (log-local2 "LOG_LOCAL2"))
(constant (log-local3 "LOG_LOCAL3"))
(constant (log-local4 "LOG_LOCAL4"))
(constant (log-local5 "LOG_LOCAL5"))
(constant (log-local6 "LOG_LOCAL6"))
(constant (log-local7 "LOG_LOCAL7"))

;; priorities (these are ordered)
(constant (log-emerg "LOG_EMERG")
  :documentation "system is unusable")
(constant (log-alert "LOG_ALERT")
  :documentation "action must be taken immediately")
(constant (log-crit "LOG_CRIT")
  :documentation "critical conditions")
(constant (log-err "LOG_ERR")
  :documentation "error conditions")
(constant (log-warning "LOG_WARNING")
  :documentation "warning conditions")
(constant (log-notice "LOG_NOTICE")
  :documentation "normal but significant condition")
(constant (log-info "LOG_INFO")
  :documentation "informational")
(constant (log-debug "LOG_DEBUG")
  :documentation "debug-level messages")
