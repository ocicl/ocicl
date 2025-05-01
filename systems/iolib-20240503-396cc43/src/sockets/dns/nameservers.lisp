;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Nameservers management.
;;;

(in-package :iolib/sockets)

(defvar *resolv.conf-file* "/etc/resolv.conf")

(defvar *dns-nameservers* nil
  "List of the DNS nameservers to use.")

(defvar *dns-domain* nil
  "The current machine's domain.")

(defvar *dns-search-domain* nil
  "A domain name to be appended to the name to be searched when
the latter does not contain dots.")

(defvar *resolvconf-lock* (bt:make-lock "/etc/resolv.conf lock"))

;;; Only parses NAMESERVER, DOMAIN and SEARCH directives, for now.
(defun parse-/etc/resolv.conf (file)
  (let (nameservers domain search-domain)
    (flet ((parse-one-line (tokens)
             (when (< (length tokens) 2) (error 'parse-error))
             (destructuring-bind (option value &rest more-values) tokens
               (switch (option :test #'string-equal)
                 ("nameserver" (ignore-parse-errors
                                 (push (ensure-address value)
                                       nameservers)))
                 ("domain" (setf domain value))
                 ("search" (setf search-domain (cons value more-values)))))))
      (map-etc-file (lambda (tokens)
                      (ignore-errors (parse-one-line tokens)))
                    file)
      (values (nreverse nameservers) domain search-domain))))

(defun update-dns-parameters (file)
  (multiple-value-bind (ns domain search)
      (parse-/etc/resolv.conf file)
    (setf *dns-nameservers* (or ns +ipv4-loopback+)
          ;; everything after the first dot
          *dns-domain* (cdr (split-sequence #\. domain :count 2))
          *dns-search-domain* search)))

(defvar *resolv.conf-monitor*
  (make-instance 'file-monitor
                 :file *resolv.conf-file*
                 :update-fn 'update-dns-parameters
                 :lock *resolvconf-lock*))
