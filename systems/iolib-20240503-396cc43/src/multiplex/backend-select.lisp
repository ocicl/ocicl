;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- select(2) multiplexer implementation.
;;;

(in-package :iolib/multiplex)

(defconstant +select-priority+ 3)

(define-multiplexer select-multiplexer +select-priority+ (multiplexer)
  ((max-fd :initform 0
           :accessor max-fd-of)
   (read-fd-set :initform (allocate-fd-set)
                :reader read-fd-set-of)
   (write-fd-set :initform (allocate-fd-set)
                 :reader write-fd-set-of)
   (except-fd-set :initform (allocate-fd-set)
                  :reader except-fd-set-of))
  (:default-initargs :fd-limit (1- isys:fd-setsize)))

(defun allocate-fd-set ()
  (let ((fd-set (foreign-alloc '(:struct isys:fd-set))))
    (isys:fd-zero fd-set)
    fd-set))

(defmethod print-object ((mux select-multiplexer) stream)
  (print-unreadable-object (mux stream :type nil :identity nil)
    (format stream "select(2) multiplexer")))

(defmethod close-multiplexer progn ((mux select-multiplexer))
  (with-slots ((rs read-fd-set)
               (ws write-fd-set)
               (es except-fd-set))
      mux
    (unless (null-pointer-p rs) (foreign-free rs))
    (unless (null-pointer-p ws) (foreign-free ws))
    (unless (null-pointer-p es) (foreign-free es))
    (setf rs nil ws nil es nil)))

(defun find-max-fd (fd-set end)
  (loop :for i :downfrom end :to 0
        :do (when (isys:fd-isset i fd-set) (return* i)))
  ;; this means no fd <= end is set
  -1)

(defun recalc-fd-masks (mux fd read write)
  (with-accessors ((rs read-fd-set-of)
                   (ws write-fd-set-of)
                   (es except-fd-set-of)
                   (max-fd max-fd-of))
      mux
    (cond (read
           (isys:fd-set fd rs)
           (isys:fd-set fd es))
          (t
           (isys:fd-clr fd rs)
           (isys:fd-clr fd es)))
    (if write
        (isys:fd-set fd ws)
        (isys:fd-clr fd ws))
    (let ((end (max max-fd fd)))
      (setf max-fd (max (find-max-fd rs end)
                        (find-max-fd ws end))))
    t))

(defmethod monitor-fd ((mux select-multiplexer) fd-entry)
  (recalc-fd-masks mux (fd-entry-fd fd-entry)
                   (fd-entry-read-handler fd-entry)
                   (fd-entry-write-handler fd-entry)))

(defmethod update-fd ((mux select-multiplexer) fd-entry event-type edge-change)
  (declare (ignore event-type edge-change))
  (recalc-fd-masks mux (fd-entry-fd fd-entry)
                   (fd-entry-read-handler fd-entry)
                   (fd-entry-write-handler fd-entry)))

(defmethod unmonitor-fd ((mux select-multiplexer) fd-entry)
  (recalc-fd-masks mux (fd-entry-fd fd-entry) nil nil))

(defmethod harvest-events ((mux select-multiplexer) timeout)
  (with-accessors ((rs read-fd-set-of)
                   (ws write-fd-set-of)
                   (es except-fd-set-of)
                   (max-fd max-fd-of))
      mux
    ;; if there are no fds set and timeout is NULL
    ;; select() blocks forever
    (when (and (minusp max-fd)
               (null timeout))
      (warn "Non fds to monitor and no timeout set !")
      (return* nil))
    (with-foreign-objects ((read-fds '(:struct isys:fd-set))
                           (write-fds '(:struct isys:fd-set))
                           (except-fds '(:struct isys:fd-set)))
      (isys:copy-fd-set rs read-fds)
      (isys:copy-fd-set ws write-fds)
      (isys:copy-fd-set es except-fds)
      (handler-case
          (with-foreign-object (ts '(:struct isys:timespec))
            (isys:repeat-upon-condition-decreasing-timeout
                ((isys:eintr) tmp-timeout timeout)
              (when tmp-timeout
                (timeout->timespec tmp-timeout ts))
              (isys:select (1+ max-fd)
                           read-fds
                           write-fds
                           except-fds
                           (if tmp-timeout ts (null-pointer)))))
        (isys:ebadf ()
          (return* (harvest-select-fd-errors rs ws max-fd))))
      (harvest-select-events max-fd read-fds write-fds except-fds))))

(defun harvest-select-events (max-fd read-fds write-fds except-fds)
  (loop :for fd :upto max-fd
        :for event := () :then ()
        :when (or (isys:fd-isset fd read-fds)
                  (isys:fd-isset fd except-fds)) :do (push :read event)
        :when (isys:fd-isset fd write-fds) :do (push :write event)
        :when event :collect (list fd event)))

;;; FIXME: I don't know whether on all *nix systems select()
;;; returns EBADF only when a given FD present in some fd-set
;;; is closed(as the POSIX docs say) or if some other kinds of
;;; errors are reported too(as the Linux manpages seem to suggest)
(defun fd-error-p (fd)
  (not (isys:fd-open-p fd)))

(defun harvest-select-fd-errors (read-fds write-fds max-fd)
  (loop :for fd :upto max-fd
        :when (and (or (isys:fd-isset fd read-fds)
                       (isys:fd-isset fd write-fds))
                   (fd-error-p fd))
        :collect (list fd (list :error))))
