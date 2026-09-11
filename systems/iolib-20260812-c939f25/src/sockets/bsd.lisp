;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Bindings for BSD sockets.
;;;

(in-package :iolib/sockets)

(defmacro define-socket-creation-call (name return-type &body args)
  `(defsyscall ,name
       (,return-type
        :error-generator signal-socket-error)
     ,@args))

(defmacro define-socket-call (name return-type &body args)
  (let ((forms (alexandria:parse-body args)))
    `(defsyscall ,name
         (,return-type
          :handle ,(caar forms) ; the socket FD
          :error-generator signal-socket-error)
       ,@forms)))


;;;; sys/socket.h

(define-socket-call (%accept "accept") :int
  (socket  :int)
  (address :pointer) ; sockaddr-foo
  (addrlen :pointer))

(define-socket-call (%bind "bind") :int
  (socket  :int)
  (address :pointer)
  (addrlen socklen-t))

(define-socket-call (%connect "connect") :int
  (socket  :int)
  (address :pointer) ; sockaddr-foo
  (addrlen socklen-t))

(define-socket-call (%getpeername "getpeername") :int
  (socket  :int)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call (%getsockname "getsockname") :int
  (socket  :int)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call (%getsockopt "getsockopt") :int
  (socket  :int)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  :pointer))

(define-socket-call (%listen "listen") :int
  (socket  :int)
  (backlog :int))

(define-socket-call (%recvfrom "recvfrom") ssize-t
  (socket  :int)
  (buffer  :pointer)
  (length  size-t)
  (flags   :int)
  (address :pointer)
  (addrlen :pointer))

(define-socket-call (%sendto "sendto") ssize-t
  (socket   :int)
  (buffer   :pointer)
  (length   size-t)
  (flags    :int)
  (destaddr :pointer)
  (destlen  socklen-t))

(define-socket-call (%recvmsg "recvmsg") ssize-t
  (socket  :int)
  (message :pointer)
  (flags   :int))

(define-socket-call (%sendmsg "sendmsg") ssize-t
  (socket  :int)
  (message :pointer)
  (flags   :int))

(define-socket-call (%setsockopt "setsockopt") :int
  (socket  :int)
  (level   :int)
  (optname :int)
  (optval  :pointer)
  (optlen  socklen-t))

(define-socket-call (%shutdown "shutdown") :int
  (socket :int)
  (how    :int))

;;; SOCKET is emulated in winsock.lisp.
(define-socket-creation-call (%socket "socket") :int
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int))

#-(and) ; unused
(define-socket-call (%sockatmark "sockatmark") :int
  (socket :int))

(define-socket-creation-call (%%socketpair "socketpair") :int
  (domain   :int)  ; af-*
  (type     :int)  ; sock-*
  (protocol :int)  ; usually 0 - "default protocol", whatever that is
  (filedes  :pointer))

(defun %socketpair (domain type protocol)
  (with-foreign-object (filedes :int 2)
    (%%socketpair domain type protocol filedes)
    (values (mem-aref filedes :int 0)
            (mem-aref filedes :int 1))))

;;;; netinet/un.h

(defconstant unix-path-max
  (- (isys:sizeof '(:struct sockaddr-un))
     (foreign-slot-offset '(:struct sockaddr-un) 'path)))

;;;; net/if.h

(defsyscall (%if-nametoindex "if_nametoindex")
    (:unsigned-int
     :error-predicate zerop
     :error-generator (lambda (r syscall h h2)
                        (declare (ignore r h h2))
                        (isys:signal-syscall-error-kw :enxio syscall)))
  (ifname :string))

(defsyscall (%if-indextoname "if_indextoname")
    :string
  (ifindex :unsigned-int)
  (ifname  :pointer))

(defsyscall (%if-nameindex "if_nameindex")
    :pointer)

(defsyscall (%if-freenameindex "if_freenameindex") :void
  (ptr :pointer))
