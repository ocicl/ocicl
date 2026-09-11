;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; etc-files.lisp --- Common parsing routines for /etc namedb files.
;;;

(in-package :iolib/sockets)

(defun space-char-p (char)
  (declare (type character char))
  (or (char= char #\Space)
      (char= char #\Tab)))

(defun split-etc-tokens (line)
  (declare (type string line))
  (let ((comment-start (position #\# line)))
    (split-sequence-if #'space-char-p line
                       :remove-empty-subseqs t
                       :start 0 :end comment-start)))

(defun map-etc-file (thunk file)
  (with-open-file (fin file :external-format :latin-1)
    (loop :for line := (read-line fin nil nil)
       :while line :do (funcall thunk (split-etc-tokens line)))))
