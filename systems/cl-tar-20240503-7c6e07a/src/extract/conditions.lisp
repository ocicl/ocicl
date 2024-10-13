;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

(define-condition destination-exists-error (extraction-entry-error file-error)
  ((mtime
    :initarg :mtime
    :reader destination-exists-error-mtime))
  (:documentation
   "Signaled when the destination of an entry exists."))

(define-condition extraction-through-symbolic-link-error (extraction-entry-error)
  ((target
    :initarg :target)
   (pathname
    :reader extraction-through-symbolic-link-error-pathname
    :initarg :pathname))
  (:documentation
   "Signaled when attempting to extract through a symbolic link."))

(define-condition file-exists-in-place-of-directory-error (extraction-entry-error)
  ((pathname
    :initarg :pathname))
  (:documentation
   "Signaled when attempting to create a directory when a file already
exists."))

(defun remove-file (&optional c)
  "Handle C by deleting the file."
  (invoke-restart (find-restart 'remove-file c)))

(defun follow-symbolic-link (&optional c)
  "Handle C by following the symbolic link."
  (invoke-restart (find-restart 'follow-symbolic-link c)))

(defun replace-symbolic-link (&optional c)
  "Handle C by replacing the symbolic link"
  (invoke-restart (find-restart 'replace-symbolic-link c)))

(defun supersede-file (&optional c)
  "Handle C by truncating the file and overwriting it."
  (invoke-restart (find-restart 'supersede-file c)))

(defun extract-link (&optional c)
  "Handle C by extracting the link."
  (invoke-restart (find-restart 'extract-link c)))

(defun extract-device (&optional c)
  "Handle C by extracting the device."
  (invoke-restart (find-restart 'extract-device c)))

(defun extract-fifo (&optional c)
  "Handle C by extracting the fifo."
  (invoke-restart (find-restart 'extract-fifo c)))
