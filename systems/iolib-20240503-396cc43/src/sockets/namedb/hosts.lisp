;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; hosts.lisp --- Static host lookup.
;;;

(in-package :iolib/sockets)

(defvar *hosts-file* "/etc/hosts")

(defclass host ()
  ((truename :initform nil :initarg :truename
             :accessor host-truename
             :documentation "The name of the host.")
   (aliases :initform nil :initarg :aliases
            :accessor host-aliases
            :documentation "A list of aliases.")
   (addresses :initform nil :initarg :addresses
              :accessor host-addresses
              :documentation "A list of addresses."))
  (:documentation "Class representing a host: name, aliases and addresses."))

(defmethod initialize-instance :after ((host host) &key)
  (with-accessors ((name host-truename) (aliases host-aliases)
                   (addresses host-addresses)) host
    (flet ((namep (h) (and (stringp h) (plusp (length h)))))
      (assert (namep name) (name) "Invalid host truename: ~A" name)
      (assert (every #'namep aliases) (aliases) "Invalid host aliases: ~A" aliases)
      (assert addresses (addresses) "A host must have at least one address.")
      (setf addresses (ensure-list addresses))
      (map-into addresses #'ensure-address addresses))))

(defun make-host (truename addresses &optional aliases)
  "Instantiates a HOST object."
  (make-instance 'host
                 :truename truename
                 :aliases aliases
                 :addresses addresses))

(defmethod print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity nil)
    (with-slots (truename aliases addresses) host
      (format stream "Canonical name: ~S. Aliases: ~:[None~;~:*~{~S~^, ~}~]. Addresses: ~{~A~^, ~}"
              truename aliases addresses))))

(defvar *hosts-cache* ())
(defvar *hosts-cache-lock* (bt:make-lock "/etc/hosts cache lock"))

(defun parse-/etc/hosts (file)
  (let (hosts)
    (flet ((parse-one-line (tokens)
             (when (< (length tokens) 2) (error 'parse-error))
             (destructuring-bind (address cname &rest aliases) tokens
               (push (make-host cname (ensure-address address) aliases)
                     hosts))))
      (alexandria:ignore-some-conditions (file-error)
        (map-etc-file (lambda (tokens) (ignore-errors (parse-one-line tokens)))
                      file))
      (nreverse hosts))))

(defun search-host-by-name (name ipv6)
  (labels ((compatible-address-p (address)
             (ecase ipv6
               ((t)   (inet-address-p address))
               ((nil) (ipv4-address-p address))
               (:ipv6 (ipv6-address-p address))))
           (compatible-host-p (host)
             (and (or (string= name (host-truename host))
                      (member name (host-aliases host)
                              :test #'string=))
                  (compatible-address-p (car (host-addresses host))))))
    (let ((hosts (bt:with-lock-held (*hosts-cache-lock*)
                   (remove-if-not #'compatible-host-p *hosts-cache*)))
          addresses aliases)
      (when hosts
        (mapc (lambda (host)
                (let ((address (car (host-addresses host))))
                  (push address addresses)
                  (push (cons (host-truename host) address) aliases)
                  (mapc (lambda (alias) (push (cons alias address) aliases))
                        (host-aliases host))))
              hosts)
        (let ((addresses (nreverse addresses)))
          (values (car addresses) (cdr addresses)
                  name (nreverse aliases)))))))

(defun search-host-by-address (address)
  (let* ((address (ensure-address address))
         (host (bt:with-lock-held (*hosts-cache-lock*)
                 (find-if (lambda (host)
                            (address= (car (host-addresses host))
                                      address))
                          *hosts-cache*))))
    (when host
      (values address '()
              (host-truename host)
              (list* (cons (host-truename host) address)
                     (mapcar (lambda (alias) (cons alias address))
                             (host-aliases host)))))))

(defun update-hosts-list (file)
  (setf *hosts-cache* (parse-/etc/hosts file)))

(defvar *hosts-monitor*
  (make-instance 'file-monitor
                 :file *hosts-file*
                 :update-fn 'update-hosts-list
                 :lock *hosts-cache-lock*))
