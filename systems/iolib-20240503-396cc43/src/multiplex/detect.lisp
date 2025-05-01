;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Detect available multiplexers.
;;;

(in-package :iolib/multiplex)

;;; TODO: do real detecting here
(setf *default-multiplexer*
      (cdar (sort *available-multiplexers* #'< :key #'car)))
