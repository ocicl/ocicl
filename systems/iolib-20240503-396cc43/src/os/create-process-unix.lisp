;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Wrapper over lfp_spawn(3)
;;;

(in-package :iolib/os)

(defun tty-read-fn (fd buf nbytes)
  (handler-case
      (isys:read fd buf nbytes)
    (isys:eio () 0)))

(defun tty-write-fn (fd buf nbytes)
  (handler-case
      (isys:write fd buf nbytes)
    (isys:eio ()
      (error 'isys:epipe
             :handle fd
             :syscall "write"))))

(defclass tty-stream (iolib/streams:dual-channel-gray-stream)
  ()
  (:default-initargs :read-fn  #'tty-read-fn
                     :write-fn #'tty-write-fn))

(defclass process ()
  ((pid    :initarg :pid :reader process-pid)
   (status :initform :running)
   (closed :initform nil)
   (stdin  :reader process-stdin)
   (stdout :reader process-stdout)
   (stderr :reader process-stderr)
   (pty    :reader process-pty)))

(defmethod initialize-instance :after ((process process) &key
                                       stdin stdout stderr tty external-format)
  (with-slots ((in stdin) (out stdout) (err stderr) pty)
      process
    (when stdin
      (setf in  (make-instance 'tty-stream :fd stdin
                               :external-format external-format)))
    (when stdout
      (setf out (make-instance 'tty-stream :fd stdout
                               :external-format external-format)))
    (when stderr
      (setf err (make-instance 'tty-stream :fd stderr
                               :external-format external-format)))
    (when tty
      (setf pty (make-instance 'tty-stream :fd tty
                               :external-format external-format)))))

(defmethod close ((process process) &key abort)
  (if (slot-value process 'closed)
      nil
      (macrolet ((close-process-stream (slot)
                   `(when (slot-boundp process ',slot)
                      (close (slot-value process ',slot) :abort abort)
                      (slot-makunbound process ',slot))))
        (close-process-stream stdin)
        (close-process-stream stdout)
        (close-process-stream stderr)
        (close-process-stream pty)
        (process-status process :wait (not abort))
        (setf (slot-value process 'closed) t)
        t)))

(defmethod print-object ((o process) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~S ~S ~S ~S"
            :pid (process-pid o)
            :status (process-status o))))

(defun exit-status (status)
  (cond
    ((isys:wifexited status)
     (isys:wexitstatus status))
    ((isys:wifsignaled status)
     (values (isys:wtermsig* status)
             (isys:wcoredump status)))))

(defgeneric process-status (process &key wait)
  (:method ((process process) &key wait)
    (if (integerp (slot-value process 'status))
        (exit-status (slot-value process 'status))
        (multiple-value-bind (pid status)
            (isys:waitpid (process-pid process)
                          (if wait 0 isys:wnohang))
          (cond
            ((zerop pid)
             :running)
            (t
             (setf (slot-value process 'status) status)
             (exit-status status)))))))

(defgeneric process-activep (process)
  (:method ((process process))
    (eql :running (process-status process))))

(defgeneric process-kill (process &optional signum)
  (:method ((process process) &optional (signum :sigterm))
    (isys:kill (process-pid process) signum)
    process))


(defun call-with-lfp-spawn-arguments (thunk)
  (with-foreign-objects ((attributes 'lfp-spawnattr-t)
                         (file-actions 'lfp-spawn-file-actions-t))
    (let ((spawnattr-initialized-p nil)
          (file-actions-initialized-p nil))
      (unwind-protect
           (progn
             (setf spawnattr-initialized-p
                   (lfp-spawnattr-init attributes))
             (setf file-actions-initialized-p
                   (lfp-spawn-file-actions-init file-actions))
             (funcall thunk attributes file-actions))
        (when spawnattr-initialized-p
          (lfp-spawnattr-destroy attributes))
        (when file-actions-initialized-p
          (lfp-spawn-file-actions-destroy file-actions))))))

(defmacro with-lfp-spawn-arguments ((attributes file-actions) &body body)
  `(call-with-lfp-spawn-arguments
    (lambda (,attributes ,file-actions) ,@body)))

(defun allocate-argv (argv program arglist)
  ;; copy program name
  (setf (mem-aref argv :pointer 0)
        (foreign-string-alloc program))
  ;; copy program arguments
  (loop :for i :from 1
        :for arg :in arglist :do
        (setf (mem-aref argv :pointer i)
              (foreign-string-alloc arg))))

(defun find-program (program)
  (cond
    ((eql :shell program)
     (list "/bin/sh" "-c"))
    (t
     (list (file-path-namestring program)))))

(defmacro with-argv (((arg0 argv) program arguments) &body body)
  (with-gensyms (argc)
    `(let* ((,program (find-program ,program))
            (,arguments (append (cdr ,program) ,arguments))
            (,argc (+ 2 (length ,arguments))))
       (with-foreign-object (,argv :pointer ,argc)
         (isys:bzero ,argv (* ,argc (isys:sizeof :pointer)))
         (unwind-protect
              (progn
                (allocate-argv ,argv (car ,program) ,arguments)
                (let ((,arg0 (mem-ref ,argv :pointer)))
                  ,@body))
           (delocate-null-ended-list ,argv))))))

(defun redirect-one-stream (file-actions fd stream
                            &optional flags (mode #o644) close-old-fd)
  (flet ((dup-from-path (path)
           (lfp-spawn-file-actions-addopen file-actions fd path flags mode))
         (dup-from-fd (oldfd)
           (lfp-spawn-file-actions-adddup2 file-actions oldfd fd)
           (when close-old-fd
             (lfp-spawn-file-actions-addclose file-actions oldfd))))
    (etypecase stream
      ((eql t) nil)
      ((or string file-path pathname)
       (dup-from-path (file-path-namestring stream)))
      ((eql :null)
       (dup-from-path "/dev/null"))
      (unsigned-byte
       (dup-from-fd stream))
      (iolib/streams:dual-channel-fd-mixin
       (dup-from-fd (iolib/streams:fd-of stream)))
      (null
       (lfp-spawn-file-actions-addclose file-actions fd)))))

(defun redirect-to-pipes (file-actions fd keep-write-fd)
  (multiple-value-bind (pipe-parent pipe-child)
      (isys:pipe)
    (when keep-write-fd (rotatef pipe-parent pipe-child))
    (lfp-spawn-file-actions-adddup2 file-actions pipe-child fd)
    (lfp-spawn-file-actions-addclose file-actions pipe-parent)
    (lfp-spawn-file-actions-addclose file-actions pipe-child)
    (values pipe-parent pipe-child)))

(defun setup-redirections (file-actions stdin stdout stderr ptmfd pts)
  (let (infd infd-child outfd outfd-child errfd errfd-child)
    ;; Standard input
    (case stdin
      (:pipe
       (setf (values infd infd-child)
             (redirect-to-pipes file-actions +stdin+ t)))
      (:pty
       (setf infd (isys:dup ptmfd))
       (redirect-one-stream file-actions +stdin+ pts isys:o-rdonly))
      (t (redirect-one-stream file-actions +stdin+ stdin isys:o-rdonly)))
    ;; Standard output
    (case stdout
      (:pipe
       (setf (values outfd outfd-child)
             (redirect-to-pipes file-actions +stdout+ nil)))
      (:pty
       (setf outfd (isys:dup ptmfd))
       (redirect-one-stream file-actions +stdout+ pts (logior isys:o-wronly
                                                              isys:o-creat)))
      (t (redirect-one-stream file-actions +stdout+ stdout (logior isys:o-wronly
                                                                   isys:o-creat))))
    ;; Standard error
    (case stderr
      (:pipe
       (setf (values errfd errfd-child)
             (redirect-to-pipes file-actions +stderr+ nil)))
      (:pty
       (setf errfd (isys:dup ptmfd))
       (redirect-one-stream file-actions +stderr+ pts (logior isys:o-wronly
                                                              isys:o-creat)))
      (t (redirect-one-stream file-actions +stderr+ stderr (logior isys:o-wronly
                                                                   isys:o-creat))))
    (values infd infd-child outfd outfd-child errfd errfd-child)))

(defun close-fds (&rest fds)
  (dolist (fd fds)
    (when fd (isys:close fd))))

(defun setup-slave-pty (new-ctty-p)
  (if new-ctty-p
      (let (ptmfd)
        (unwind-protect-case ()
            (progn
              (setf ptmfd (isys:openpt (logior isys:o-rdwr isys:o-noctty isys:o-cloexec)))
              (isys:grantpt ptmfd)
              (isys:unlockpt ptmfd)
              (values ptmfd (isys:ptsname ptmfd)))
          (:abort (when ptmfd (isys:close ptmfd)))))
      (values nil nil)))

(defmacro with-pty ((new-ctty-p ptmfd pts) &body body)
  `(multiple-value-bind (,ptmfd ,pts)
       (setup-slave-pty ,new-ctty-p)
     (unwind-protect
          (locally ,@body)
       (unless ,new-ctty-p
         (close-fds ,ptmfd)))))

(defmacro with-redirections (((infd outfd errfd)
                              (file-actions stdin stdout stderr ptyfd pts))
                             &body body)
  (with-gensyms (infd-child outfd-child errfd-child)
    `(multiple-value-bind (,infd ,infd-child ,outfd ,outfd-child ,errfd ,errfd-child)
         (setup-redirections ,file-actions ,stdin ,stdout ,stderr ,ptyfd ,pts)
       (unwind-protect-case ()
           (locally ,@body)
         (:always
          (close-fds ,infd-child ,outfd-child ,errfd-child))
         (:abort
          (close-fds ,infd ,outfd ,errfd))))))

(defun process-other-spawn-args (attributes new-session pts current-directory
                                 uid gid resetids vfork)
  (when new-session
    (lfp-spawnattr-setsid attributes))
  (when pts
    (lfp-spawnattr-setctty attributes pts))
  (when current-directory
    (lfp-spawnattr-setcwd attributes current-directory))
  (when uid
    (lfp-spawnattr-setuid attributes uid))
  (when gid
    (lfp-spawnattr-setgid attributes gid))
  (when resetids
    (lfp-spawnattr-setflags attributes lfp-spawn-resetids))
  (when vfork
    (lfp-spawnattr-setflags attributes lfp-spawn-usevfork)))

;; program: :shell - the system shell
;;          file-path designator - a path
;; arguments: list
;; environment: t - inherit environment
;;              nil - NULL environment
;;              alist - the environment to use
;; stdin, stdout, stderr:
;;         file-path designator - open file, redirect to it
;;         :null - redirect to /dev/null - useful because /dev/null doesn't exist on Windows
;;         file-descriptor designator(integer or stream) - file descriptor, redirecto to it
;;         :pipe - create pipe, redirect the child descriptor to one end and wrap the other end
;;                 into a stream which goes into PROCESS slot
;;         t - inherit
;;         nil - close
;; pty: boolean - spawn a new controlling tty. it is also implicitly T if
;;                either stdin, stdout or stderr is :pty
;; new-session: boolean - create a new session using setsid(). it is also implicitly T
;;                        if a PTY is requested
;; current-directory: path - a directory to switch to before executing
;; uid: user id - unsigned-byte or string
;; gid: group id - unsigned-byte or string
;; resetids: boolean - reset effective UID and GID to saved IDs

(defun create-process (program-and-args &key (environment t)
                       (stdin :pipe) (stdout :pipe) (stderr :pipe) pty
                       new-session current-directory uid gid resetids vfork
                       (external-format :utf-8))
  (let ((new-ctty-p
           (or pty
               (eql :pty stdin)
               (eql :pty stdout)
               (eql :pty stderr))))
    (destructuring-bind (program &rest arguments)
        (ensure-list program-and-args)
      (with-argv ((arg0 argv) program arguments)
        (with-c-environment (envp environment)
          (with-lfp-spawn-arguments (attributes file-actions)
            (with-pty (new-ctty-p ptyfd pts)
              (with-redirections ((infd outfd errfd)
                                  (file-actions stdin stdout stderr ptyfd pts))
                (process-other-spawn-args attributes
                                          new-session pts
                                          current-directory
                                          uid gid resetids vfork)
                (with-foreign-object (pid 'pid-t)
                  (lfp-spawnp pid arg0 argv envp file-actions attributes)
                  (make-instance 'process :pid (mem-ref pid 'pid-t)
                                          :stdin infd :stdout outfd :stderr errfd :tty ptyfd
                                          :external-format external-format))))))))))

(defun slurp-char-stream (stream)
  (with-output-to-string (s)
    (loop :for c := (read-char stream nil nil)
          :while c :do (write-char c s))))

(defun slurp-octet-stream (stream)
  (let ((dynbuffer (make-instance 'dynamic-buffer :size 4096 :growth-size 2))
        (iobuffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (handler-case
        (loop :for pos := (read-sequence iobuffer stream)
              :while (plusp pos) :do
          (write-vector dynbuffer iobuffer 0 pos))
      (end-of-file () nil))
    (subseq (sequence-of dynbuffer)
            (read-cursor-of dynbuffer)
            (write-cursor-of dynbuffer))))

(defun run-program (program-and-args &key (environment t)
                    (stdin :null) (stdout :pipe) (stderr :pipe)
                    (external-format :utf-8))
  (flet ((slurp (stream)
           (if external-format
               (slurp-char-stream stream)
               (slurp-octet-stream stream)))
         (make-empty-output ()
           (if external-format
               (make-array 0 :element-type 'character)
               (make-array 0 :element-type '(unsigned-byte 8)))))
    (let ((process
            (create-process program-and-args
                            :environment environment
                            :stdin stdin
                            :stdout stdout
                            :stderr stderr
                            :external-format external-format)))
      (unwind-protect
           (let ((stdout (if (eql :pipe stdout)
                             (slurp (process-stdout process))
                             (make-empty-output)))
                 (stderr (if (eql :pipe stderr)
                             (slurp (process-stderr process))
                             (make-empty-output))))
             (values (process-status process :wait t)
                     stdout stderr))
        (close process)))))
