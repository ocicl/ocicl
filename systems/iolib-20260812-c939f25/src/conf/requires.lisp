;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Load all non-ASDF deps - usually implementation-specific REQUIREs
;;;

(in-package :iolib/conf)

(defun load-gray-streams ()
  #+(and allegro (not (version>= 9 0)))
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc"))
  #+(or cmu abcl)
  (require :gray-streams)
  #+ecl
  (when (fboundp 'gray::redefine-cl-functions)
    (gray::redefine-cl-functions)))
