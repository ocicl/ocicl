;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; services.lisp --- Service lookup.
;;;

(in-package :iolib/sockets)

(defvar *services-file* "/etc/services")

(defclass service ()
  ((name :initarg :name :reader service-name
         :documentation "The service name.")
   (port :initarg :port :reader service-port
         :documentation "The service's default port.")
   (protocol :initarg :protocol :reader service-protocol
             :documentation "The service's protocol, :TCP or :UDP."))
  (:documentation "Class representing a service."))

(defun make-service (name port protocol)
  "Constructor for SERVICE objects."
  (make-instance 'service :name name :port (parse-integer port)
                 :protocol (make-keyword (string-upcase protocol))))

(defmethod print-object ((service service) stream)
  (print-unreadable-object (service stream :type t :identity nil)
    (with-slots (name port protocol) service
      (format stream "Name: ~A Port: ~A Protocol: ~A" name port protocol))))

(defun split-port/proto (port/proto)
  (let ((pos (position #\/ port/proto)))
    (unless pos (error 'parse-error))
    (values (subseq port/proto 0 pos)
            (subseq port/proto (1+ pos)))))

(defun protocol-compatible-p (protocol thing)
  (case protocol
    (:any t)
    (:tcp (eql :tcp (make-keyword (string-upcase thing))))
    (:udp (eql :udp (make-keyword (string-upcase thing))))))

(defun find-service-in-parsed-lines (tokens predicate)
  (when (< (length tokens) 2) (error 'parse-error))
  (destructuring-bind (name port/proto &rest aliases) tokens
    (multiple-value-bind (port proto) (split-port/proto port/proto)
      (when (funcall predicate name port proto aliases)
        (make-service name port proto)))))

(defun lookup-service-on-disk-by-number (file service protocol)
  (flet ((good-proto-p (name port proto aliases)
           (declare (ignore name aliases))
           (let ((pnum (parse-integer port)))
             (and (protocol-compatible-p protocol proto)
                  (= pnum service)))))
    (map-etc-file (lambda (tokens)
                    (ignore-errors
                      (let ((proto (find-service-in-parsed-lines tokens #'good-proto-p)))
                        (when proto (return* proto)))))
                  file)))

(defun lookup-service-on-disk-by-name (file service protocol)
  (flet ((good-proto-p (name port proto aliases)
           (declare (ignore port))
           (and (protocol-compatible-p protocol proto)
                (or (string= service name)
                    (member service aliases :test #'string=)))))
    (map-etc-file (lambda (tokens)
                    (ignore-errors
                      (let ((proto (find-service-in-parsed-lines tokens #'good-proto-p)))
                        (when proto (return* proto)))))
                  file)))

(define-condition unknown-service ()
  ((datum :initarg :datum :initform nil :reader unknown-service-datum))
  (:report (lambda (condition stream)
             (format stream "Unknown service: ~S" (unknown-service-datum condition))))
  (:documentation "Condition raised when a network service is not found."))
(setf (documentation 'unknown-service-datum 'function)
      "Return the datum that caused the signalling of an UNKNOWN-SERVICE condition.")

(defvar *tcp-service-cache-by-name*   (make-hash-table :test #'equal))
(defvar *tcp-service-cache-by-number* (make-hash-table :test #'eql))
(defvar *udp-service-cache-by-name*   (make-hash-table :test #'equal))
(defvar *udp-service-cache-by-number* (make-hash-table :test #'eql))
(defvar *service-cache-lock* (bt:make-lock "/etc/services cache lock"))

(defun find-service-name-in-cache (thing protocol)
  (ecase protocol
    (:tcp (gethash thing *tcp-service-cache-by-name*))
    (:udp (gethash thing *udp-service-cache-by-name*))
    (:any (or (gethash thing *tcp-service-cache-by-name*)
              (gethash thing *udp-service-cache-by-name*)))))

(defun find-service-number-in-cache (thing protocol)
  (ecase protocol
    (:tcp (gethash thing *tcp-service-cache-by-number*))
    (:udp (gethash thing *udp-service-cache-by-number*))
    (:any (or (gethash thing *tcp-service-cache-by-number*)
              (gethash thing *udp-service-cache-by-number*)))))

(defun find-service (thing protocol cache-fn disk-fn)
  (or (funcall cache-fn thing protocol)
      (let ((service (funcall disk-fn *services-file* thing protocol)))
        (flet ((get-cache (type)
                 (ecase type
                   (:name (ecase (service-protocol service)
                            (:tcp *tcp-service-cache-by-name*)
                            (:udp *udp-service-cache-by-name*)))
                   (:number (ecase (service-protocol service)
                              (:tcp *tcp-service-cache-by-number*)
                              (:udp *udp-service-cache-by-number*))))))
          (when service
            (setf (gethash (service-name service) (get-cache :name))
                  service)
            (setf (gethash (service-port service) (get-cache :number))
                  service)
            (values service))))))

(defun lookup-service-by-name (thing protocol)
  (bt:with-lock-held (*service-cache-lock*)
    (find-service thing protocol
                  #'find-service-name-in-cache
                  #'lookup-service-on-disk-by-name)))

(defun lookup-service-by-number (thing protocol)
  (bt:with-lock-held (*service-cache-lock*)
    (find-service thing protocol
                  #'find-service-number-in-cache
                  #'lookup-service-on-disk-by-number)))

(defun purge-service-cache (&optional file)
  (declare (ignore file))
  (clrhash *tcp-service-cache-by-name*)
  (clrhash *tcp-service-cache-by-number*)
  (clrhash *udp-service-cache-by-name*)
  (clrhash *udp-service-cache-by-number*))

(defvar *services-monitor*
  (make-instance 'file-monitor
                 :file *services-file*
                 :update-fn 'purge-service-cache
                 :lock *service-cache-lock*))

(deftype tcp-port ()
  '(unsigned-byte 16))

(defun lookup-service (service &optional (protocol :tcp))
  "Lookup a service by port or name.  PROTOCOL should be one
of :TCP, :UDP or :ANY."
  (check-type service (or tcp-port string symbol) "a valid port number, a string or a symbol")
  (check-type protocol (member :tcp :udp :any) "one of :TCP, :UDP or :ANY")
  (update-monitor *services-monitor*)
  (let* ((parsed (ensure-string-or-unsigned-byte service :type 'tcp-port :errorp t))
         (serv (typecase parsed
                 (tcp-port (lookup-service-by-number parsed protocol))
                 (string   (lookup-service-by-name parsed protocol)))))
    (if serv
        (values (service-port serv)
                (service-name serv)
                (service-protocol serv))
        (error 'unknown-service :datum service))))

(defun ensure-numerical-service (service &optional (protocol :tcp))
  (typecase service
    (tcp-port service)
    (t        (nth-value 0 (lookup-service service protocol)))))
