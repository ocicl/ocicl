;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various socket methods.
;;;

(in-package :iolib/sockets)

;;;-------------------------------------------------------------------------
;;; Shared Initialization
;;;-------------------------------------------------------------------------

(defun translate-make-socket-keywords-to-constants (address-family type protocol)
  (let ((sf (ecase address-family
              (:ipv4  af-inet)
              (:ipv6  af-inet6)
              (:local af-local)
              #+linux
              (:netlink af-netlink)))
        (st (ecase type
              (:stream   sock-stream)
              (:datagram sock-dgram)
              (:raw      sock-raw)))
        (sp (etypecase protocol
              ((eql :default) 0)
              (integer        protocol))))
    (values sf st sp)))

(defmethod socket-os-fd ((socket socket))
  (fd-of socket))

(defmethod shared-initialize :after
    ((socket socket) slot-names
     &key file-descriptor (dup t) address-family type protocol)
  (declare (ignore slot-names))
  (with-accessors ((fd fd-of) (fam socket-address-family) (proto socket-protocol))
      socket
    (setf fd (or (and file-descriptor (if dup
                                          (isys:dup file-descriptor)
                                          file-descriptor))
                 (multiple-value-call #'%socket
                   (translate-make-socket-keywords-to-constants
                    address-family type protocol))))
    (setf fam address-family
          proto protocol)))

(defmethod (setf external-format-of) (external-format (socket passive-socket))
  (setf (slot-value socket 'external-format)
        (babel:ensure-external-format (or external-format :default))))

(defmethod shared-initialize :after ((socket passive-socket) slot-names
                                     &key external-format
                                     input-buffer-size output-buffer-size)
  ;; Makes CREATE-SOCKET simpler
  (declare (ignore slot-names input-buffer-size output-buffer-size))
  (setf (external-format-of socket) (or external-format :default)))


;;;-------------------------------------------------------------------------
;;; Misc
;;;-------------------------------------------------------------------------

(defmethod socket-type ((socket stream-socket))
  :stream)

(defmethod socket-type ((socket datagram-socket))
  :datagram)

(defun socket-ipv6-p (socket)
  "Return T if SOCKET is an AF_INET6 socket."
  (eql :ipv6 (socket-address-family socket)))

(defun ipv6-socket-p (&rest args)
  (apply #'socket-ipv6-p args))

(defobsolete ipv6-socket-p socket-ipv6-p)


;;;-------------------------------------------------------------------------
;;; PRINT-OBJECT
;;;-------------------------------------------------------------------------

(defun sock-fam (socket)
  (ecase (socket-address-family socket)
    (:ipv4 "IPv4")
    (:ipv6 "IPv6")))

(defmethod print-object ((socket socket-stream-internet-active) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "active ~A stream socket" (sock-fam socket))
    (if (socket-connected-p socket)
        (multiple-value-bind (host port) (remote-name socket)
          (format stream " connected to ~A/~A"
                  (address-to-string host) port))
        (format stream ", ~:[closed~;unconnected~]" (fd-of socket)))))

(defmethod print-object ((socket socket-stream-internet-passive) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "passive ~A stream socket" (sock-fam socket))
    (if (socket-bound-p socket)
        (multiple-value-bind (host port) (local-name socket)
          (format stream " ~:[bound to~;waiting @~] ~A/~A"
                  (socket-listening-p socket)
                  (address-to-string host) port))
        (format stream ", ~:[closed~;unbound~]" (fd-of socket)))))

(defmethod print-object ((socket socket-stream-local-active) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "active local stream socket")
    (if (socket-connected-p socket)
        (format stream " connected to ~S"
                (address-to-string (remote-filename socket)))
        (format stream ", ~:[closed~;unconnected~]" (fd-of socket)))))

(defmethod print-object ((socket socket-stream-local-passive) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "passive local stream socket")
    (if (socket-bound-p socket)
        (format stream " ~:[bound to~;waiting @~] ~A"
                  (socket-listening-p socket)
                  (address-to-string (local-filename socket)))
        (format stream ", ~:[closed~;unbound~]" (fd-of socket)))))

(defmethod print-object ((socket socket-datagram-local) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "local datagram socket")
    (if (socket-connected-p socket)
        (format stream " connected to ~S"
                (address-to-string (remote-filename socket)))
        (if (fd-of socket)
            (format stream " waiting @ ~S" (address-to-string (local-filename socket)))
            (format stream ", closed" )))))

(defmethod print-object ((socket socket-datagram-internet) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "~A datagram socket" (sock-fam socket))
    (if (socket-connected-p socket)
        (multiple-value-bind (host port) (remote-name socket)
          (format stream " connected to ~A/~A"
                  (address-to-string host) port))
        (if (fd-of socket)
            (multiple-value-bind (host port) (local-name socket)
              (format stream " waiting @ ~A/~A"
                      (address-to-string host) port))
            (format stream ", closed" )))))

#+linux
(defmethod print-object ((socket socket-raw-netlink) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "netlink socket")
    (if (socket-bound-p socket)
        (multiple-value-bind (address port)
            (local-name socket)
          (format stream " bound to ~A@~A"
                  port (address-to-string address)))
        (format stream ", ~:[closed~;unbound~]" (fd-of socket)))))


;;;-------------------------------------------------------------------------
;;; CLOSE
;;;-------------------------------------------------------------------------

(defmethod close :before ((socket socket) &key abort)
  (declare (ignore abort))
  (setf (slot-value socket 'bound) nil))

(defmethod close ((socket socket) &key abort)
  (declare (ignore abort))
  (when (next-method-p)
    (call-next-method))
  (socket-open-p socket))

(defmethod close :before ((socket passive-socket) &key abort)
  (declare (ignore abort))
  (setf (slot-value socket 'listening) nil))

(defmethod socket-open-p ((socket socket))
  (if (null (fd-of socket))
      nil
      (with-sockaddr-storage-and-socklen (ss size)
        (handler-case
            (%getsockname (fd-of socket) ss size)
          (isys:ebadf () nil)
          (socket-connection-reset-error () nil)
          (:no-error (_) (declare (ignore _)) t)))))


;;;-------------------------------------------------------------------------
;;; GETSOCKNAME
;;;-------------------------------------------------------------------------

(defun %local-name (socket)
  (with-sockaddr-storage-and-socklen (ss size)
    (%getsockname (fd-of socket) ss size)
    (sockaddr-storage->sockaddr ss)))

(defmethod local-name ((socket socket))
  (%local-name socket))

(defmethod local-host ((socket internet-socket))
  (nth-value 0 (%local-name socket)))

(defmethod local-port ((socket internet-socket))
  (nth-value 1 (%local-name socket)))

#+linux
(defmethod local-port ((socket netlink-socket))
  (nth-value 1 (%local-name socket)))

(defmethod local-filename ((socket local-socket))
  (%local-name socket))


;;;-------------------------------------------------------------------------
;;; GETPEERNAME
;;;-------------------------------------------------------------------------

(defun %remote-name (socket)
  (with-sockaddr-storage-and-socklen (ss size)
    (%getpeername (fd-of socket) ss size)
    (sockaddr-storage->sockaddr ss)))

(defmethod remote-name ((socket socket))
  (%remote-name socket))

(defmethod remote-host ((socket internet-socket))
  (nth-value 0 (%remote-name socket)))

(defmethod remote-port ((socket internet-socket))
  (nth-value 1 (%remote-name socket)))

(defmethod remote-filename ((socket local-socket))
  (%remote-name socket))


;;;-------------------------------------------------------------------------
;;; BIND
;;;-------------------------------------------------------------------------

(defmethod bind-address :before ((socket internet-socket) address
                                 &key (reuse-address t))
  (declare (ignore address))
  (when reuse-address
    (setf (socket-option socket :reuse-address) t)))

(defun bind-ipv4-address (fd address port)
  (with-sockaddr-in (sin address port)
    (%bind fd sin (isys:sizeof '(:struct sockaddr-in)))))

(defun bind-ipv6-address (fd address port)
  (with-sockaddr-in6 (sin6 address port)
    (%bind fd sin6 (isys:sizeof '(:struct sockaddr-in6)))))

(defmethod bind-address ((socket internet-socket) (address ipv4-address)
                         &key (port 0))
  (let ((port (ensure-numerical-service port)))
    (if (socket-ipv6-p socket)
        (bind-ipv6-address (fd-of socket)
                           (map-ipv4-vector-to-ipv6 (address-name address))
                           port)
        (bind-ipv4-address (fd-of socket) (address-name address) port)))
  (values socket))

(defmethod bind-address ((socket internet-socket) (address ipv6-address)
                         &key (port 0))
  (bind-ipv6-address (fd-of socket)
                     (address-name address)
                     (ensure-numerical-service port))
  (values socket))

(defmethod bind-address ((socket local-socket) (address local-address) &key)
  (with-sockaddr-un (sun (address-name address) (abstract-address-p address))
    (%bind (fd-of socket) sun (actual-size-of-sockaddr-un sun)))
  (values socket))

#+linux
(defmethod bind-address ((socket netlink-socket) (address netlink-address)
                         &key (port 0))
  (with-sockaddr-nl (snl (netlink-address-multicast-groups address) port)
    (%bind (fd-of socket) snl (isys:sizeof '(:struct sockaddr-nl))))
  (values socket))

(defmethod bind-address :after ((socket socket) (address address) &key)
  (setf (slot-value socket 'bound) t))


;;;-------------------------------------------------------------------------
;;; LISTEN
;;;-------------------------------------------------------------------------

(defmethod listen-on ((socket socket) &key backlog)
  (unless backlog (setf backlog (min *default-backlog-size*
                                     +max-backlog-size+)))
  (check-type backlog unsigned-byte "a non-negative integer")
  (%listen (fd-of socket) backlog)
  (setf (slot-value socket 'listening) t)
  (values socket))


;;;-------------------------------------------------------------------------
;;; ACCEPT
;;;-------------------------------------------------------------------------

(defmethod accept-connection ((socket passive-socket) &key external-format
                              input-buffer-size output-buffer-size (wait t))
  (check-type wait timeout-designator)
  (flet ((make-client-socket (fd)
           (make-instance (active-class socket)
                          :address-family (socket-address-family socket)
                          :file-descriptor fd :dup nil
                          :external-format (or external-format
                                               (external-format-of socket))
                          :input-buffer-size input-buffer-size
                          :output-buffer-size output-buffer-size)))
    (ignore-some-conditions (isys:ewouldblock iomux:poll-timeout)
      (iomux:wait-until-fd-ready (fd-of socket) :input (wait->timeout wait) t)
      (with-sockaddr-storage-and-socklen (ss size)
        (multiple-value-call #'values
          (make-client-socket (%accept (fd-of socket) ss size))
          (sockaddr-storage->sockaddr ss))))))


;;;-------------------------------------------------------------------------
;;; CONNECT
;;;-------------------------------------------------------------------------

(defun ipv4-connect (fd address port)
  (with-sockaddr-in (sin address port)
    (%connect fd sin (isys:sizeof '(:struct sockaddr-in)))))

(defun ipv6-connect (fd address port)
  (with-sockaddr-in6 (sin6 address port)
    (%connect fd sin6 (isys:sizeof '(:struct sockaddr-in6)))))

(defun call-with-socket-to-wait-connect (socket thunk wait)
  (check-type wait timeout-designator)
  (let ((timeout (wait->timeout wait)))
    (flet
        ((wait-connect ()
           (when (or (null  timeout)
                     (plusp timeout))
             (handler-case
                 (iomux:wait-until-fd-ready (fd-of socket) :output timeout t)
               (iomux:poll-error ()
                 (let ((errcode (socket-option socket :error)))
                   (if (zerop errcode)
                       (bug "Polling socket signalled an error but SO_ERROR is 0")
                       (signal-socket-error errcode "connect" (fd-of socket)))))))))
      (ignore-some-conditions (iomux:poll-timeout)
        (handler-case
            (funcall thunk)
          ((or isys:ewouldblock
               isys:einprogress) ()
            (wait-connect)))))))

(defmacro with-socket-to-wait-connect ((socket wait) &body body)
  `(call-with-socket-to-wait-connect ,socket (lambda () ,@body) ,wait))

(defmethod connect ((socket internet-socket) (address inet-address)
                    &key (port 0) (wait t))
  (let ((name (address-name address))
        (port (ensure-numerical-service port)))
    (with-socket-to-wait-connect (socket wait)
      (cond
        ((socket-ipv6-p socket)
         (when (ipv4-address-p address)
           (setf name (map-ipv4-vector-to-ipv6 name)))
         (ipv6-connect (fd-of socket) name port))
        (t (ipv4-connect (fd-of socket) name port)))))
  (values socket))

(defmethod connect ((socket local-socket) (address local-address) &key (wait t))
  (with-socket-to-wait-connect (socket wait)
    (with-sockaddr-un (sun (address-name address) (abstract-address-p address))
        (%connect (fd-of socket) sun (actual-size-of-sockaddr-un sun))))
  (values socket))

(defmethod socket-connected-p ((socket socket))
  (if (fd-of socket)
      (with-sockaddr-storage-and-socklen (ss size)
        (handler-case
            (%getpeername (fd-of socket) ss size)
          ((or isys:enotconn isys:einval) ()  nil)
          (:no-error (_) (declare (ignore _)) t)))
      nil))


;;;-------------------------------------------------------------------------
;;; DISCONNECT
;;;-------------------------------------------------------------------------

(defmethod disconnect ((socket datagram-socket))
  (with-foreign-object (sin '(:struct sockaddr-in))
    (isys:bzero sin (isys:sizeof '(:struct sockaddr-in)))
    (setf (foreign-slot-value sin '(:struct sockaddr-in) 'addr) af-unspec)
    (%connect (fd-of socket) sin (isys:sizeof '(:struct sockaddr-in)))
    (values socket)))


;;;-------------------------------------------------------------------------
;;; SHUTDOWN
;;;-------------------------------------------------------------------------

(defmethod shutdown ((socket socket) &key read write)
  (assert (or read write) (read write)
          "You must select at least one direction to shut down.")
  (%shutdown (fd-of socket)
             (multiple-value-case ((read write))
               ((*   nil) shut-rd)
               ((nil *)   shut-wr)
               (t         shut-rdwr)))
  (values socket))


;;;-------------------------------------------------------------------------
;;; Socket flag definition
;;;-------------------------------------------------------------------------

(defmacro define-socket-flag (place name value platform)
  (let ((val (cond ((or (not platform)
                        (featurep platform)) value)
                   ((not (featurep platform)) 0))))
    `(pushnew (cons ,name ,val) ,place)))

(defmacro define-socket-flags (place &body definitions)
  (flet ((dflag (form)
           (destructuring-bind (name value &optional platform) form
             `(define-socket-flag ,place ,name ,value ,platform))))
    `(progn
       ,@(mapcar #'dflag definitions))))


;;;-------------------------------------------------------------------------
;;; SENDTO
;;;-------------------------------------------------------------------------

(defvar *sendto-flags* ())

(define-socket-flags *sendto-flags*
  (:dont-route    msg-dontroute)
  (:dont-wait     msg-dontwait  (:not :windows))
  (:out-of-band   msg-oob)
  (:more          msg-more      :linux)
  (:confirm       msg-confirm   :linux))

(defun %%send-to (fd ss got-peer buff-sap start length flags)
  (incf-pointer buff-sap start)
  (loop
    (restart-case
        (return*
         (%sendto fd buff-sap length flags
                  (if got-peer ss (null-pointer))
                  (if got-peer (sockaddr-size ss) 0)))
      (ignore-syscall-error ()
        :report "Ignore this socket condition"
        :test isys:syscall-error-p
        (return* 0))
      (retry-syscall (&optional (timeout 15.0d0))
        :report "Try to send data again"
        :test isys:syscall-error-p
        (when (plusp timeout)
          (iomux:wait-until-fd-ready fd :output timeout nil))))))

(defun %send-to (fd ss got-peer buffer start end flags)
  (etypecase buffer
    (ub8-sarray
     (check-bounds buffer start end)
     (with-pointer-to-vector-data (buff-sap buffer)
       (%%send-to fd ss got-peer buff-sap start (- end start) flags)))
    ((or ub8-vector (vector t))
     (check-bounds buffer start end)
     (with-pointer-to-vector-data (buff-sap (coerce buffer 'ub8-sarray))
       (%%send-to fd ss got-peer buff-sap start (- end start) flags)))
    (foreign-pointer
     (check-type start unsigned-byte)
     (check-type end   unsigned-byte)
     (%%send-to fd ss got-peer buffer start (- end start) flags))))

(defmethod send-to ((socket internet-socket) buffer &rest args
                    &key (start 0) end remote-host (remote-port 0) flags (ipv6 *ipv6*))
  (let ((*ipv6* ipv6))
    (with-sockaddr-storage (ss)
      (when remote-host
        (sockaddr->sockaddr-storage ss (ensure-hostname remote-host)
                                    (ensure-numerical-service remote-port)))
      (%send-to (fd-of socket) ss (if remote-host t) buffer start end
                (or flags (compute-flags *sendto-flags* args))))))

(defmethod send-to ((socket local-socket) buffer &rest args
                    &key (start 0) end remote-filename flags)
  (with-sockaddr-storage (ss)
    (when remote-filename
      (sockaddr->sockaddr-storage ss (ensure-address remote-filename :family :local) 0))
    (%send-to (fd-of socket) ss (if remote-filename t) buffer start end
              (or flags (compute-flags *sendto-flags* args)))))

(define-compiler-macro send-to (&whole form &environment env socket buffer &rest args
                                &key (start 0) end (remote-host nil host-p) (remote-port 0 port-p)
                                (remote-filename nil file-p) flags (ipv6 '*ipv6* ipv6-p) &allow-other-keys)
  (let ((flags-val (compute-flags *sendto-flags* args env)))
    (cond
      ((and (not flags) flags-val)
       (append
        `(send-to ,socket ,buffer :start ,start :end ,end :flags ,flags-val)
        (when host-p `(:remote-host ,remote-host))
        (when port-p `(:remote-port ,remote-port))
        (when ipv6-p `(:ipv6 ,ipv6))
        (when file-p `(:remote-filename ,remote-filename))))
      (t
       form))))


;;;-------------------------------------------------------------------------
;;; RECVFROM
;;;-------------------------------------------------------------------------

(defvar *recvfrom-flags* ())

(define-socket-flags *recvfrom-flags*
  (:out-of-band msg-oob)
  (:peek        msg-peek)
  (:wait-all    msg-waitall  (:not :windows))
  (:dont-wait   msg-dontwait (:not :windows)))

(defun %%receive-from (fd ss size buffer start length flags)
  (with-pointer-to-vector-data (buff-sap buffer)
    (incf-pointer buff-sap start)
    (loop
       (restart-case
           (return* (%recvfrom fd buff-sap length flags ss size))
         (ignore-syscall-error ()
           :report "Ignore this socket condition"
           :test isys:syscall-error-p
           (return* 0))
         (retry-syscall (&optional (timeout 15.0d0))
           :report "Try to receive data again"
           :test isys:syscall-error-p
           (when (plusp timeout)
             (iomux:wait-until-fd-ready fd :input timeout nil)))))))

(defun %receive-from (fd ss size buffer start end flags)
  (check-bounds buffer start end)
  (flet ((%do-recvfrom (buff start length)
           (%%receive-from fd ss size buff start length flags)))
    (let (nbytes)
      (etypecase buffer
        (ub8-sarray
         (setf nbytes (%do-recvfrom buffer start (- end start))))
        ((or ub8-vector (vector t))
         (let ((tmpbuff (make-array (- end start) :element-type 'ub8)))
           (setf nbytes (%do-recvfrom tmpbuff 0 (- end start)))
           (replace buffer tmpbuff :start1 start :end1 end :start2 0 :end2 nbytes))))
      (values nbytes))))

(defmethod receive-from :around ((socket socket) &rest args
                                 &key buffer size (start 0) end flags &allow-other-keys)
  (let ((flags-val (or flags (compute-flags *recvfrom-flags* args))))
    (cond
      (buffer
       (call-next-method socket :buffer buffer :start start :end end :flags flags-val))
      (t
       (check-type size unsigned-byte "a non-negative integer")
       (call-next-method socket :buffer (make-array size :element-type 'ub8)
                         :start 0 :end size :flags flags-val)))))

(defmethod receive-from ((socket stream-socket) &key buffer start end flags)
  (with-sockaddr-storage-and-socklen (ss size)
    (let ((nbytes (%receive-from (fd-of socket) ss size buffer start end flags)))
      (values buffer nbytes))))

(defmethod receive-from ((socket raw-socket) &key buffer start end flags)
  (with-sockaddr-storage-and-socklen (ss size)
    (let ((nbytes (%receive-from (fd-of socket) ss size buffer start end flags)))
      (values buffer nbytes))))

(defmethod receive-from ((socket datagram-socket) &key buffer start end flags)
  (with-sockaddr-storage-and-socklen (ss size)
    (let ((nbytes (%receive-from (fd-of socket) ss size buffer start end flags)))
      (multiple-value-call #'values buffer nbytes
                           (sockaddr-storage->sockaddr ss)))))

(define-compiler-macro receive-from (&whole form &environment env socket &rest args
                                     &key buffer size (start 0) end flags &allow-other-keys)
  (let ((flags-val (compute-flags *recvfrom-flags* args env)))
    (cond
      ((and (not flags) flags-val)
       `(receive-from ,socket :buffer ,buffer :start ,start :end ,end
                      :size ,size :flags ,flags-val))
      (t
       form))))
