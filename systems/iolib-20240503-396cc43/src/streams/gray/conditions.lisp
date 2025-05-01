;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Gray stream conditions.
;;;

(in-package :iolib/streams)

(define-condition hangup (stream-error)
  ()
  (:report (lambda (c s)
             (format s "Stream ~S hang up."
                     (stream-error-stream c))))
  (:documentation "Condition signaled when the underlying device of a stream
is closed by the remote end while writing to it."))

(define-condition no-characters-to-unread (stream-error)
  ()
  (:report (lambda (c s)
             (format s "No uncommitted character to unread on stream ~S."
                     (stream-error-stream c))))
  (:documentation "Condition signaled when UNREAD-CHAR is called on a stream either:
1) without having been preceded by a READ-CHAR, or
2) after a PEEK-CHAR"))
