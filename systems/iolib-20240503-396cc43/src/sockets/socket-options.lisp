;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Setter and getters for various socket options.
;;;

(in-package :iolib/sockets)

;;;; Macrology

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *socket-option-types* (make-hash-table :test #'eq))
  (defvar *set-socket-options* (make-hash-table :test #'eq))
  (defun socktype-args (type)
    (first (gethash type *socket-option-types*)))
  (defun socktype-getter (type)
    (second (gethash type *socket-option-types*)))
  (defun socktype-setter (type)
    (third (gethash type *socket-option-types*))))

(defmacro define-socket-option-type (name args)
  (flet ((make-helper-name (action value-type)
           (format-symbol t "~A~A~A"
                          action '#:-socket-option- value-type)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,name *socket-option-types*)
             (list ',args
                   ',(make-helper-name :get name)
                   ',(make-helper-name :set name))))))

(defmacro define-socket-option-helper (helper args &body body)
  (destructuring-bind (action type) helper
    (assert (gethash type *socket-option-types*))
    (assert (= (length args)
               (+ 3 (ecase action
                      (:get 0)
                      (:set (length (socktype-args type)))))))
    (multiple-value-bind (forms decls) (parse-body body)
      `(defun ,(ecase action
                      (:get (socktype-getter type))
                      (:set (socktype-setter type)))
           ,args ,@decls ,@forms))))

(defmacro define-get-sockopt (os name type level optname)
  `(defmethod socket-option ((socket socket) (option-name (eql ,name)))
     (declare (ignorable socket option-name))
     ,(if (or (eql :any os) (featurep os))
          (let ((getter (socktype-getter type)))
            `(,getter (fd-of socket) ,level ,optname))
          `(error 'socket-option-not-supported-error
                  :message ,(format nil "Unsupported socket option: ~S" name)))))

(defmacro define-set-sockopt (os name type level optname)
  (when (or (eql :any os) (featurep os))
    `(setf (gethash ,name *set-socket-options*)
           (list ,type ,level ,optname))))

(define-setf-expander socket-option (socket option-name &key (if-does-not-exist :error))
  (flet ((%make-arglist (type-args expanded-args)
           (mapcar (lambda (targ earg)
                     (if (consp targ)
                         `(or ,earg ,(cadr targ))
                         earg))
                   type-args expanded-args)))
    (if-let (data (gethash option-name *set-socket-options*))
      (destructuring-bind (type level optname) data
        (let ((glist (make-gensym-list (length (socktype-args type)))))
          (values nil nil glist
                  `(,(socktype-setter type)
                     (fd-of ,socket) ,level ,optname
                     ,@(%make-arglist (socktype-args type) glist))
                  socket)))
      (values nil nil nil
              (case if-does-not-exist
                (:error
                 `(error 'socket-option-not-supported-error
                         :message ,(format nil "Unsupported socket option: ~S"
                                           option-name)))
                (nil nil))))))

(defmacro define-socket-option (name action optname level argtype os)
  (let ((eql-name (make-keyword name)))
    `(progn
       ,(when (member action (list :get :get-and-set))
          `(define-get-sockopt ,os ,eql-name ,argtype ,level ,optname))
       ,(when (member action (list :set :get-and-set))
          `(define-set-sockopt ,os ,eql-name ,argtype ,level ,optname)))))

(defmacro define-socket-options (action level os &body options)
  `(progn
     ,@(loop :for (name optname argtype) :in options :collect
             `(define-socket-option ,name ,action
                ,optname ,level ,argtype ,os))))

;;;; Types

;;; BOOL

(define-socket-option-type :bool (value))

(define-socket-option-helper (:get :bool) (fd level option)
  (with-foreign-object (optval :int)
    (with-socklen (optlen (isys:sizeof :int))
      (%getsockopt fd level option optval optlen)
      (mem-aref optval :boolean))))

(define-socket-option-helper (:set :bool) (fd level option value)
  (with-foreign-object (optval :int)
    (setf (mem-aref optval :int) (lisp->c-bool value))
    (%setsockopt fd level option optval (isys:sizeof :int))
    (values value)))

;;; INT

(define-socket-option-type :int (value))

(define-socket-option-helper (:get :int) (fd level option)
  (with-foreign-object (optval :int)
    (with-socklen (optlen (isys:sizeof :int))
      (%getsockopt fd level option optval optlen)
      (mem-aref optval :int))))

(define-socket-option-helper (:set :int) (fd level option value)
  (with-foreign-object (optval :int)
    (setf (mem-aref optval :int) value)
    (%setsockopt fd level option optval (isys:sizeof :int))
    (values value)))

;;; LINGER

(define-socket-option-type :linger (onoff (linger *default-linger-seconds*)))

(define-socket-option-helper (:get :linger) (fd level option)
  (with-foreign-object (optval '(:struct linger))
    (with-socklen (optlen (isys:sizeof '(:struct linger)))
      (%getsockopt fd level option optval optlen)
      (with-foreign-slots ((linger onoff) optval (:struct linger))
        (values (not (zerop onoff)) linger)))))

(define-socket-option-helper (:set :linger) (fd level option new-onoff new-linger)
  (with-foreign-object (optval '(:struct linger))
    (with-foreign-slots ((linger onoff) optval (:struct linger))
      (setf onoff (lisp->c-bool new-onoff)
            linger new-linger))
    (%setsockopt fd level option optval (isys:sizeof '(:struct linger)))
    (values new-onoff new-linger)))

;;; TIMEVAL

(define-socket-option-type :timeval (sec))

(define-socket-option-helper (:get :timeval) (fd level option)
  (with-foreign-object (optval '(:struct timeval))
    (with-socklen (optlen (isys:sizeof '(:struct timeval)))
      (%getsockopt fd level option optval optlen)
      (with-foreign-slots ((sec usec) optval (:struct timeval))
        (values sec usec)))))

(define-socket-option-helper (:set :timeval) (fd level option new-sec)
  (with-foreign-object (optval '(:Struct timeval))
    (with-foreign-slots ((sec usec) optval (:struct timeval))
      (setf (values sec usec) (decode-timeout new-sec)))
    (%setsockopt fd level option optval (isys:sizeof '(:struct timeval)))
    (values new-sec)))

;;; IFREQ-NAME

(define-socket-option-type :ifreq-name (value))

#+linux
(define-socket-option-helper (:set :ifreq-name) (fd level option interface)
  (with-foreign-object (optval '(:struct ifreq))
    (isys:bzero optval (isys:sizeof '(:struct ifreq)))
    (with-foreign-slots ((name) optval (:struct ifreq))
      (with-foreign-string (ifname interface)
        (isys:memcpy name ifname (min (length interface) (1- ifnamsiz)))))
    (%setsockopt fd level option optval (isys:sizeof '(:struct ifreq)))
    (values interface)))

;;;; Option Definitions

;;; Generic options

(define-socket-options :get sol-socket :any
  (accept-connections so-acceptconn :bool)
  (error              so-error      :int)
  (type               so-type       :int))

(define-socket-options :get-and-set sol-socket :any
  (broadcast         so-broadcast :bool)
  (debug             so-debug     :bool)
  (dont-route        so-dontroute :bool)
  (keep-alive        so-keepalive :bool)
  (linger            so-linger    :linger)
  (oob-inline        so-oobinline :bool)
  (receive-buffer    so-rcvbuf    :int)
  (send-buffer       so-sndbuf    :int)
  (receive-low-water so-rcvlowat  :int)
  (send-low-water    so-sndlowat  :int)
  (receive-timeout   so-rcvtimeo  :timeval)
  (send-timeout      so-sndtimeo  :timeval)
  (reuse-address     so-reuseaddr :bool))

;;; Linux-specific Options

(define-socket-options :set sol-socket :linux
  (bind-to-device so-bindtodevice :ifreq-name))

(define-socket-option priority :get-and-set
  so-priority sol-socket :int :linux)

;;; FreeBSD-specific options

(define-socket-options :get-and-set sol-socket (:or :freebsd :dragonfly)
  (reuse-port   so-reuseport   :bool)
  (use-loopback so-useloopback :bool)
  (no-sigpipe   so-nosigpipe   :bool))

;;; TODO

;; TODO: implement "struct ucred" helpers

;; (define-socket-option pass-credentials   :get-and-set et:so-passcred     et:sol-socket :ucred   (:or :linux :freebsd))
;; (define-socket-option peer-credentials   :get         et:so-peercred     et:sol-socket :ucred   (:or :linux :freebsd))


;; TODO: implement "struct accept_filter_arg" helpers

;; (define-socket-option accept-filter      :get-and-set et:so-acceptfilter et:sol-socket :accept-filter :freebsd)

;; TODO: find out the types of these options

;; (define-socket-option bintime            :get-and-set et:so-bintime      et:sol-socket :bool    :freebsd)
;; (define-socket-option label              :get-and-set et:so-label        et:sol-socket :bool    :freebsd)
;; (define-socket-option peerlabel          :get-and-set et:so-peerlabel    et:sol-socket :bool    :freebsd)
;; (define-socket-option listen-queue-limit :get-and-set et:so-listenqlimit et:sol-socket :int     :freebsd)
;; (define-socket-option listen-queue-length :get-and-set et:so-listenqlen  et:sol-socket :int     :freebsd)
;; (define-socket-option listen-incomplete-queue-length :get-and-set et:so-listenincqlen  et:sol-socket :int :freebsd)


;;; TCP Options

(define-socket-option tcp-nodelay :get-and-set
  tcp-nodelay ipproto-tcp :bool :any)

(define-socket-option tcp-maxseg :get-and-set
  tcp-maxseg ipproto-tcp :int (:or :linux :freebsd :dragonfly))

;;; Linux-specific TCP Options

(define-socket-options :get-and-set ipproto-tcp :linux
  (tcp-cork         tcp-cork         :bool)
  (tcp-defer-accept tcp-defer-accept :int)
  (tcp-keepcnt      tcp-keepcnt      :int)
  (tcp-keepidle     tcp-keepidle     :int)
  (tcp-keepintvl    tcp-keepintvl    :int)
  (tcp-linger2      tcp-linger2      :int)
  (tcp-quickack     tcp-quickack     :bool)
  (tcp-syncnt       tcp-syncnt       :int)
  (tcp-window-clamp tcp-window-clamp :int))

;; TODO: implement "struct tcp_info" helper
;; (define-socket-option tcp-info         :get         et::tcp-info         et:ipproto-tcp :tcp-info :linux)

;;; FreeBSD-specific TCP Options

(define-socket-options :get-and-set ipproto-tcp (:or :freebsd :dragonfly)
  (tcp-noopt  tcp-noopt  :bool)
  (tcp-nopush tcp-nopush :bool))


;;; IP Options

(define-socket-option ip-header-include :get-and-set
  ip-hdrincl ipproto-ip :bool :any)

(define-socket-option ip-receive-error :get-and-set
  ip-recverr ipproto-ip :bool :linux)


;;; RAW  Options

(define-socket-option icmp-filter :get-and-set
  icmp-filter ipproto-raw :int :linux)
