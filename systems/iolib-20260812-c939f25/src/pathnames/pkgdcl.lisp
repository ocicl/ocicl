;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :iolib/common-lisp-user)

(defpackage :iolib/pathnames
  (:nicknames :ipath :iolib.pathnames)
  (:use :iolib/base)
  (:export
   ;; Classes and types
   #:file-path #:file-path-designator
   #+unix    #:unix-path
   #+windows #:unc-path

   ;; Accessors
   #:file-path-host
   #:file-path-device
   #:file-path-components
   #:file-path-directory
   #:file-path-file
   #:file-path-file-name
   #:file-path-file-type
   #:file-path-namestring
   #:file-path-trailing-delimiter

   ;; Constructors
   #:file-path
   #:make-file-path
   #:parse-file-path

   ;; Named reader
   #:p

   ;; Operations
   #:merge-file-paths
   #:enough-file-path

   ;; Predicates
   #:file-path-p
   #:absolute-file-path-p
   #:relative-file-path-p

   ;; Conditions
   #:invalid-file-path

   ;; Constants
   #:+directory-delimiter+
   #:+alternative-delimiter+
   #:+execution-path-delimiter+

   ;; Specials
   #:*default-file-path-defaults*
   #:*default-execution-path*))
