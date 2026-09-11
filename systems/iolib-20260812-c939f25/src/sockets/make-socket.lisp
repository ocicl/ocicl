;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Socket creation.
;;;

(in-package :iolib/sockets)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *socket-type-map*
    '(((:ipv4  :stream   :active)  . socket-stream-internet-active)
      ((:ipv6  :stream   :active)  . socket-stream-internet-active)
      ((:ipv4  :stream   :passive) . socket-stream-internet-passive)
      ((:ipv6  :stream   :passive) . socket-stream-internet-passive)
      ((:local :stream   :active)  . socket-stream-local-active)
      ((:local :stream   :passive) . socket-stream-local-passive)
      ((:local :datagram nil)      . socket-datagram-local)
      ((:ipv4  :datagram nil)      . socket-datagram-internet)
      ((:ipv6  :datagram nil)      . socket-datagram-internet)
      ((:ipv4  :raw      nil)      . socket-raw-internet)
      #+linux
      ((:netlink :raw    nil)      . socket-raw-netlink)))

  (defun select-socket-class (address-family type connect)
    (or (loop :for ((sock-family sock-type sock-connect) . class)
                :in *socket-type-map*
              :when (and (eql sock-family address-family)
                         (eql sock-type type)
                         (if sock-connect (eql sock-connect connect) t))
              :return class)
        (error "No socket class found !!"))))

(defun create-socket (family type protocol
                      &rest args &key connect fd &allow-other-keys)
  (apply #'make-instance (select-socket-class family type connect)
         :address-family family
         :protocol protocol
         :file-descriptor fd
         (remove-from-plist args :connect)))

(define-compiler-macro create-socket (&whole form &environment env
                                      family type protocol
                                      &rest args &key connect fd &allow-other-keys)
  (cond
    ((and (constantp family env) (constantp type env) (constantp connect env))
     `(make-instance ',(select-socket-class family type connect)
                     :file-descriptor ,fd
                     :address-family ,family
                     :protocol ,protocol
                     ,@(remove-from-plist args :connect)))
    (t form)))

(defmacro with-close-on-error ((var value) &body body)
  "Bind `VAR' to `VALUE' and execute `BODY' as implicit PROGN.
If a non-local exit occurs during the execution of `BODY',
call CLOSE with :ABORT T on `VAR'."
  `(let ((,var ,value))
     (unwind-protect-case () ,@body
       (:abort (close ,var :abort t)))))

(defmacro %create-internet-socket (family &rest args)
  `(case ,family
     (:ipv4 (create-socket :ipv4 ,@args))
     (:ipv6 (create-socket :ipv6 ,@args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-first-level-name (family type connect)
    (if (eql :stream type)
        (format-symbol :iolib/sockets "%~A/~A-~A-~A" :make-socket family type connect)
        (format-symbol :iolib/sockets "%~A/~A-~A" :make-socket family type))))

(defmacro define-socket-creator ((socket-family socket-type &optional socket-connect)
                                 (family protocol key &rest parameters) &body body)
  (assert (eql '&key key))
  (flet ((maybe-quote-default-value (arg)
           (cond ((symbolp arg) arg)
                 ((consp arg)   (list (first arg) `(quote ,(second arg))))))
         (arg-name (arg)
           (car (ensure-list arg)))
         (quotify (form)
           `(list (quote ,(car form)) ,@(cdr form))))
    (let* ((parameter-names (mapcar #'arg-name parameters))
           (first-level-function (make-first-level-name socket-family socket-type socket-connect))
           (second-level-function (format-symbol t "%~A" first-level-function)))
      (flet ((make-first-level-body (family protocol)
               `(,second-level-function ,family ,protocol ,@parameter-names)))
        `(progn
           (declaim (inline ,second-level-function))
           (defun ,second-level-function (,family ,protocol ,@parameter-names) ,@body)
           (defun ,first-level-function (arguments family protocol)
             (destructuring-bind (&key ,@parameters)
                 arguments
               ,(make-first-level-body family protocol)))
           (define-compiler-macro ,first-level-function (&whole form arguments family protocol)
             ;; Must quote default values in order for them not to be evaluated
             ;; in the compilation environment
             (if (listp arguments)
                 (destructuring-bind (&key ,@(mapcar #'maybe-quote-default-value parameters))
                     (cdr arguments)
                   ,(quotify (make-first-level-body family protocol)))
                 form)))))))


;;; Internet Stream Active Socket creation

(defun %%init-socket/internet-stream-active (socket keepalive nodelay reuse-address
                                             local-host local-port remote-host remote-port)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when keepalive (setf (socket-option socket :keep-alive) t))
  (when nodelay (setf (socket-option socket :tcp-nodelay) t))
  (when local-host
    (bind-address socket (ensure-hostname local-host)
                  :port local-port
                  :reuse-address reuse-address))
  (when remote-host
    (connect socket (ensure-hostname remote-host)
             :port remote-port))
  (values socket))

(define-socket-creator (:internet :stream :active)
    (family protocol &key external-format
                          keepalive nodelay (reuse-address t)
                          local-host local-port remote-host remote-port
                          input-buffer-size output-buffer-size)
  (with-close-on-error (socket (%create-internet-socket family :stream protocol
                                                        :connect :active 
                                                        :external-format external-format
                                                        :input-buffer-size input-buffer-size
                                                        :output-buffer-size output-buffer-size))
    (%%init-socket/internet-stream-active socket keepalive nodelay reuse-address
                                          local-host (or local-port 0) remote-host remote-port)))


;;; Internet Stream Passive Socket creation

(defun %%init-socket/internet-stream-passive (socket interface reuse-address
                                              local-host local-port backlog)
  (when local-host
    (when interface
      (setf (socket-option socket :bind-to-device) interface))
    (bind-address socket (ensure-hostname local-host)
                  :port local-port
                  :reuse-address reuse-address)
    (listen-on socket :backlog backlog))
  (values socket))

(define-socket-creator (:internet :stream :passive)
    (family protocol &key external-format
                          interface (reuse-address t)
                          local-host local-port backlog)
  (with-close-on-error (socket (%create-internet-socket family :stream protocol
                                                        :connect :passive
                                                        :external-format external-format))
    (%%init-socket/internet-stream-passive socket interface reuse-address
                                           local-host (or local-port 0)
                                           (or backlog *default-backlog-size*))))


;;; Local Stream Active Socket creation

(defun %%init-socket/local-stream-active (socket local-filename remote-filename)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)))
  (when remote-filename
    (connect socket (ensure-address remote-filename :family :local)))
  (values socket))

(define-socket-creator (:local :stream :active)
    (family protocol &key external-format local-filename remote-filename
                          input-buffer-size output-buffer-size)
  (with-close-on-error (socket (create-socket family :stream protocol
                                              :connect :active
                                              :external-format external-format
                                              :input-buffer-size input-buffer-size
                                              :output-buffer-size output-buffer-size))
    (%%init-socket/local-stream-active socket local-filename remote-filename)))


;;; Local Stream Passive Socket creation

(defun %%init-socket/local-stream-passive (socket local-filename reuse-address backlog)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)
                  :reuse-address reuse-address)
    (listen-on socket :backlog backlog))
  (values socket))

(define-socket-creator (:local :stream :passive)
    (family protocol &key external-format local-filename (reuse-address t) backlog)
  (with-close-on-error (socket (create-socket family :stream protocol
                                              :connect :passive
                                              :external-format external-format))
    (%%init-socket/local-stream-passive socket local-filename reuse-address
                                        (or backlog *default-backlog-size*))))


;;; Internet Datagram Socket creation

(defun %%init-socket/internet-datagram (socket broadcast interface reuse-address
                                        local-host local-port remote-host remote-port)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when broadcast (setf (socket-option socket :broadcast) t))
  (when local-host
    (bind-address socket (ensure-hostname local-host)
                  :port local-port
                  :reuse-address reuse-address)
    (when interface
      (setf (socket-option socket :bind-to-device) interface)))
  (when remote-host
    (connect socket (ensure-hostname remote-host)
             :port remote-port))
  (values socket))

(define-socket-creator (:internet :datagram)
    (family protocol &key broadcast interface (reuse-address t)
                          local-host local-port remote-host remote-port)
  (with-close-on-error (socket (%create-internet-socket family :datagram protocol))
    (%%init-socket/internet-datagram socket broadcast interface reuse-address
                                     local-host (or local-port 0)
                                     remote-host (or remote-port 0))))


;;; Local Datagram Socket creation

(defun %%init-socket/local-datagram (socket local-filename remote-filename)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (when local-filename
    (bind-address socket (ensure-address local-filename :family :local)))
  (when remote-filename
    (connect socket (ensure-address remote-filename :family :local)))
  (values socket))

(define-socket-creator (:local :datagram)
    (family protocol &key local-filename remote-filename)
  (with-close-on-error (socket (create-socket family :datagram protocol))
    (%%init-socket/local-datagram socket local-filename remote-filename)))


;;; Raw Socket creation

(defun %%init-socket/internet-raw (socket include-headers)
  (setf (socket-option socket :no-sigpipe :if-does-not-exist nil) t)
  (setf (socket-option socket :ip-header-include) include-headers)
  (values socket))

(define-socket-creator (:internet :raw)
    (family protocol &key include-headers)
  (with-close-on-error (socket (create-socket family :raw protocol))
    (%%init-socket/internet-raw socket include-headers)))


;;; Netlink Socket creation

#+linux
(defun %%init-socket/netlink-raw (socket local-port multicast-groups)
  (when local-port
    (bind-address socket
                  (make-instance 'netlink-address
                                 :multicast-groups multicast-groups)
                  :port local-port))
  (values socket))

#+linux
(define-socket-creator (:netlink :raw)
    (family protocol &key (local-port 0) (multicast-groups 0))
  (with-close-on-error (socket (create-socket family :raw protocol))
    (%%init-socket/netlink-raw socket local-port multicast-groups)))

#-linux
(define-socket-creator (:netlink :raw)
    (family protocol &key local-port multicast-groups)
  (declare (ignore family protocol local-port multicast-groups))
  (error 'socket-address-family-not-supported-error))


;;; MAKE-SOCKET

(defmethod make-socket (&rest args &key (address-family :internet) (type :stream) (protocol :default)
                        (connect :active) (ipv6 *ipv6*) &allow-other-keys)
  (when (eql :file address-family) (setf address-family :local))
  (check-type address-family (member :internet :local :ipv4 :ipv6 :netlink)
              "one of :INTERNET, :LOCAL(or :FILE), :IPV4, :IPV6 or :NETLINK")
  (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
  (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
  (let ((args (remove-from-plist args :address-family :type :protocol :connect :ipv6)))
    (when (eql :ipv4 address-family) (setf ipv6 nil))
    (let ((*ipv6* ipv6))
      (when (eql :internet address-family) (setf address-family +default-inet-address-family+))
      (multiple-value-case ((address-family type connect))
        ((:ipv4 :stream :active)
         (%make-socket/internet-stream-active   args :ipv4  :default))
        ((:ipv6 :stream :active)
         (%make-socket/internet-stream-active   args :ipv6  :default))
        ((:ipv4 :stream :passive)
         (%make-socket/internet-stream-passive  args :ipv4  :default))
        ((:ipv6 :stream :passive)
         (%make-socket/internet-stream-passive  args :ipv6  :default))
        ((:local :stream :active)
         (%make-socket/local-stream-active      args :local :default))
        ((:local :stream :passive)
         (%make-socket/local-stream-passive     args :local :default))
        ((:ipv4 :datagram)
         (%make-socket/internet-datagram        args :ipv4  :default))
        ((:ipv6 :datagram)
         (%make-socket/internet-datagram        args :ipv6  :default))
        ((:local :datagram)
         (%make-socket/local-datagram           args :local :default))
        ((:ipv4 :raw)
         (%make-socket/internet-raw             args :ipv4  protocol))
        ((:netlink :raw)
         (%make-socket/netlink-raw              args :netlink protocol))))))

(define-compiler-macro make-socket (&whole form &environment env &rest args
                                    &key (address-family :internet) (type :stream) (protocol :default)
                                    (connect :active) (ipv6 '*ipv6* ipv6p) &allow-other-keys)
  (when (eql :file address-family) (setf address-family :local))
  (cond
    ((and (constantp address-family env) (constantp type env) (constantp connect env))
     (check-type address-family (member :internet :local :ipv4 :ipv6 :netlink)
                 "one of :INTERNET, :LOCAL(or :FILE), :IPV4, :IPV6 or :NETLINK")
     (check-type type (member :stream :datagram :raw) "either :STREAM, :DATAGRAM or :RAW")
     (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
     (let* ((family (if (member address-family '(:ipv4 :ipv6)) :internet address-family))
            (lower-function (make-first-level-name family type connect))
            (args (remove-from-plist args :address-family :type :protocol :connect :ipv6)))
       (case address-family
         (:internet (setf address-family '+default-inet-address-family+))
         (:ipv4     (setf ipv6 nil ipv6p t)))
       (let ((expansion `(,lower-function (list ,@args) ,address-family ,protocol)))
         (if ipv6p `(let ((*ipv6* ,ipv6)) ,expansion) expansion))))
    (t form)))

(defmacro with-open-socket ((var &rest args) &body body)
  "Bind VAR to a socket created by passing ARGS to MAKE-SOCKET and execute BODY as implicit PROGN.
The socket is automatically closed upon exit."
  `(with-open-stream (,var (make-socket ,@args)) ,@body))

(defmacro with-accept-connection ((var passive-socket &rest args) &body body)
  "Bind VAR to a socket created by passing PASSIVE-SOCKET and ARGS to ACCEPT-CONNECTION and execute BODY as implicit PROGN.
The socket is automatically closed upon exit."
  `(with-open-stream (,var (accept-connection ,passive-socket ,@args)) ,@body))


;;; MAKE-SOCKET-FROM-FD

;;; FIXME: must come up with a way to find out
;;; whether a socket is active or passive
(defmethod make-socket-from-fd ((fd integer) &key (dup t) (connect :active) (external-format :default)
                                input-buffer-size output-buffer-size)
  (flet ((%get-address-family (fd)
           (with-sockaddr-storage-and-socklen (ss size)
             (%getsockname fd ss size)
             (eswitch ((foreign-slot-value ss '(:struct sockaddr-storage) 'family)
                       :test #'=)
               (af-inet  :ipv4)
               (af-inet6 :ipv6)
               (af-local :local)
               #+linux
               (af-netlink :netlink))))
         (%get-type (fd)
           (eswitch ((get-socket-option-int fd sol-socket so-type) :test #'=)
             (sock-stream :stream)
             (sock-dgram  :datagram)
             (sock-raw    :raw))))
    (create-socket (%get-address-family fd)
                   (%get-type fd)
                   :default
                   :connect connect
                   :fd fd
                   :dup dup
                   :external-format external-format
                   :input-buffer-size input-buffer-size
                   :output-buffer-size output-buffer-size)))


;;; MAKE-SOCKET-PAIR

(defmethod make-socket-pair (&key (type :stream) (protocol :default) (external-format :default)
                             input-buffer-size output-buffer-size)
  (flet ((%make-socket-pair (fd)
           (make-socket-from-fd fd :dup nil
                                :external-format external-format
                                :input-buffer-size input-buffer-size
                                :output-buffer-size output-buffer-size)))
    (multiple-value-bind (fd1 fd2)
        (multiple-value-call #'%socketpair
          (translate-make-socket-keywords-to-constants :local type protocol))
      (values (%make-socket-pair fd1)
              (%make-socket-pair fd2)))))


;;; SEND/RECEIVE-FILE-DESCRIPTOR

(defun call-with-buffers-for-fd-passing (fn)
  (with-foreign-object (msg '(:struct msghdr))
    (isys:bzero msg (isys:sizeof '(:struct msghdr)))
    (with-foreign-pointer (buffer #.(isys:cmsg.space (isys:sizeof :int))
                           buffer-size)
      (isys:bzero buffer buffer-size)
      (with-foreign-slots ((control controllen) msg (:struct msghdr))
        (setf control    buffer
              controllen buffer-size)
        (let ((cmsg (isys:cmsg.firsthdr msg)))
          (with-foreign-slots ((len level type) cmsg (:struct cmsghdr))
            (setf len (isys:cmsg.len (isys:sizeof :int))
                  level sol-socket
                  type scm-rights)
            (funcall fn msg cmsg)))))))

(defmacro with-buffers-for-fd-passing ((msg-var cmsg-var) &body body)
  `(call-with-buffers-for-fd-passing (lambda (,msg-var ,cmsg-var) ,@body)))

(defmethod send-file-descriptor ((socket local-socket) file-descriptor)
  (with-buffers-for-fd-passing (msg cmsg)
    (let ((data (isys:cmsg.data cmsg)))
      (setf (mem-aref data :int) file-descriptor)
      (%sendmsg (fd-of socket) msg 0)
      (values))))

(defmethod receive-file-descriptor ((socket local-socket))
  (with-buffers-for-fd-passing (msg cmsg)
    (let ((data (isys:cmsg.data cmsg)))
      (%recvmsg (fd-of socket) msg 0)
      (mem-aref data :int))))
