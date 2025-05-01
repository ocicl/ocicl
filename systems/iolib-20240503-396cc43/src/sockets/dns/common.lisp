;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- DNS client constants.
;;;

(in-package :iolib/sockets)

(defconstant +dns-max-datagram-size+ 4096)

(defconstant +opcode-standard+ 0)

;;; Query types

(defconstant (+query-type-map+ :test 'equal)
  '((:a . 1) (:ns . 2) (:cname . 5) (:soa . 6)
    (:wks . 11) (:ptr . 12) (:hinfo . 13) (:mx . 15)
    (:txt . 16) (:aaaa . 28) (:srv . 33) (:any . 255)))

(defun query-type-number (id)
  (cdr (assoc id +query-type-map+)))

(defun query-type-id (number)
  (car (rassoc number +query-type-map+)))

(defun dns-record-type-p (id)
  (query-type-number id))

;;; Query classes

(defconstant (+query-class-map+ :test 'equal)
  '((:in . 1) (:any . 255)))

(defun query-class-number (id)
  (cdr (assoc id +query-class-map+)))

(defun query-class-id (number)
  (car (rassoc number +query-class-map+)))

;;; Error codes

(defconstant (+rcode-map+ :test 'equal)
  '((:no-error . 0) (:format-error . 1)
    (:server-failure . 2) (:name-error . 3)
    (:not-implemented . 4) (:refused . 5)))

(defun rcode-number (id)
  (cdr (assoc id +rcode-map+)))

(defun rcode-id (number)
  (car (rassoc number +rcode-map+)))
