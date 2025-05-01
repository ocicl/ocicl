;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Main socket methods.
;;;

(in-package :iolib/common-lisp-user)

(defpackage :iolib/trivial-sockets
  (:nicknames :iolib.trivial-sockets)
  (:use :iolib/base :iolib/sockets)
  (:shadow #:socket-error #:accept-connection)
  (:export #:open-stream #:socket-error #:socket-nested-error
           #:unsupported #:unsupported-feature
           #:open-server #:close-server #:accept-connection
           #:with-server))

(in-package :iolib/trivial-sockets)

;;;;
;;;; ERRORS
;;;;

;; you're using a part of the interface that the implementation doesn't do
(define-condition unsupported (error)
  ((feature :initarg :feature :reader unsupported-feature))
  (:report (lambda (c s)
             (format s "~S does not support trivial-socket feature ~S."
                     (lisp-implementation-type) (unsupported-feature c)))))

;; all-purpose error: host not found, host not responding,
;; no service on that port, etc
(define-condition socket-error (error)
  ((nested-error :initarg :nested-error :reader socket-nested-error)))

;;;;
;;;; Main implementation
;;;;

(defun resolve-hostname (name)
  (let ((*ipv6* nil))
    (cond
      ((eql :any name) +ipv4-unspecified+)
      (t (nth-value 0 (ensure-hostname name))))))

(defun open-stream (peer-host peer-port &key
                    (local-host :any) (local-port 0)
                    (external-format :default)
                    (element-type 'character)
                    (protocol :tcp))
  (declare (ignore element-type))
  (unless (eql :tcp protocol)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (let ((*ipv6* nil))
    (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
      (make-socket :address-family :internet
                   :connect :active
                   :type :stream
                   :remote-host (resolve-hostname peer-host)
                   :remote-port peer-port
                   :local-host (resolve-hostname local-host)
                   :local-port local-port
                   :external-format external-format))))

(defun open-server (&key (host :any) (port 0)
                    (reuse-address t)
                    (backlog 1)
                    (protocol :tcp))
  "Returns a SERVER object and the port that was bound, as multiple values."
  (unless (eql :tcp protocol)
    (error 'unsupported :feature `(:protocol ,protocol)))
  (let ((*ipv6* nil))
    (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
      (let* ((host (if (eql :any host) +ipv4-unspecified+ host))
             (socket (make-socket :address-family :internet
                                  :type :stream
                                  :connect :passive
                                  :local-host host
                                  :local-port port
                                  :reuse-address reuse-address
                                  :backlog backlog)))
        (values socket (local-port socket))))))

(defun close-server (server)
  (close server))

(defun accept-connection (socket &key
                          (external-format :default)
                          (element-type 'character))
  (declare (ignore element-type))       ; bivalent streams
  (let ((*ipv6* nil))
    (handler-bind ((error (lambda (c) (error 'socket-error :nested-error c))))
      (iolib.sockets:accept-connection socket :external-format external-format))))

;;;;
;;;; Utilities
;;;;

(defmacro with-server ((name arguments) &body forms)
  `(with-open-stream (,name (open-server ,@arguments))
     (locally ,@forms)))
