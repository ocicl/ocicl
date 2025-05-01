;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Special variable definitions.
;;;

(in-package :iolib/sockets)

(defvar *ipv6* nil
  "Specifies the default behaviour with respect to IPv6:
- nil   : Only IPv4 addresses are used.
- :ipv6 : Only IPv6 addresses are used.
- t     : If both IPv4 and IPv6 addresses are found they are returned in the best order possible (see RFC 3484).
Default value is NIL.")

(deftype *ipv6*-type ()
  '(member t nil :ipv6))

(defconstant +max-backlog-size+ somaxconn
  "Maximum length of the pending connections queue (hard limit).")

(defvar *default-backlog-size* 5
  "Default length of the pending connections queue (soft limit).")

(defvar *default-linger-seconds* 15
  "Default linger timeout when enabling SO_LINGER option on a socket.")
