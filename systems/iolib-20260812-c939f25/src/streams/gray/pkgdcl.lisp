;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :iolib/common-lisp-user)

(defpackage :iolib/streams
  (:nicknames :iolib.streams)
  (:use :iolib/base :cffi)
  (:export
   ;; Classes
   #:dual-channel-fd-mixin
   #:dual-channel-gray-stream

   ;; Conditions
   #:hangup
   #:no-characters-to-unread

   ;; Accessors
   #:external-format-of
   #:fd-non-blocking
   #:fd-of
   #:read-buffer-size
   #:read-buffer-empty-p
   #:write-buffer-size
   #:write-buffer-empty-p

   #:read-sequence*
   #:write-sequence*
   #:drain-input-buffer
   ))
