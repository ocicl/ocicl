;;;; constants.lisp -- tar constants
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

;;; constants and class definitions
(defconstant +tar-n-block-bytes+ 512
  "The number of bytes in a single tar block.")

;;; values for tar's `typeflag' field
(defconstant +tar-regular-file+ #x30)
;;; backwards compatibility
(defconstant +tar-regular-alternate-file+ #x00)
(defconstant +tar-hard-link+ #x31)
(defconstant +tar-symbolic-link+ #x32)
(defconstant +tar-character-device+ #x33)
(defconstant +tar-block-device+ #x34)
(defconstant +tar-directory-file+ #x35)
(defconstant +tar-fifo-device+ #x36)
(defconstant +tar-implementation-specific-file+ #x37)

(defconstant +posix-extended-header+ #x78)
(defconstant +posix-global-header+ #x67)

;;; non-standard typeflags
(defconstant +gnutar-long-link-name+ #x4b)
(defconstant +gnutar-long-name+ #x4c)
(defconstant +gnutar-sparse+ #x53)
(defconstant +gnutar-directory-dump+ #x44)
(defconstant +gnutar-volume-header-name+ #x56)

(defconstant +ascii-space+ #x20)
(defconstant +ascii-zero+ #x30)
(defconstant +ascii-nine+ #x39)
(defconstant +ascii-a+ #x61)
(defconstant +ascii-z+ #x7a)
(defconstant +ascii-/+ #x29)
(defconstant +ascii-newline+ #xa)
