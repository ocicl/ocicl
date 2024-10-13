;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-common-extract)

(defvar *current-entry*)

(define-condition extraction-error (error)
  ()
  (:documentation
   "Base class of all errors signaled during archive extraction."))

(define-condition extraction-entry-error (extraction-error)
  ((entry
    :initarg :entry
    :initform *current-entry*
    :reader extraction-entry-error-entry))
  (:documentation
   "An extraction error that is specific to an ENTRY."))

(define-condition entry-name-contains-device-error (extraction-entry-error)
  ()
  (:documentation
   "An entry contains a device in its pathname."))

(define-condition entry-name-contains-..-error (extraction-entry-error)
  ()
  (:documentation
   "An entry contains .. in its pathname."))

(define-condition entry-name-is-absolute-error (extraction-entry-error)
  ()
  (:documentation
   "An entry pathname is absolute."))

(define-condition extract-fifo-entry-error (extraction-entry-error)
  ()
  (:documentation
   "A FIFO that cannot be extracted."))

(define-condition extract-symbolic-link-entry-error (extraction-entry-error)
  ()
  (:documentation
   "A symbolic link that cannot be extracted."))

(define-condition extract-hard-link-entry-error (extraction-entry-error)
  ()
  (:documentation
   "A hard link that cannot be extracted."))

(define-condition extract-block-device-entry-error (extraction-entry-error)
  ()
  (:documentation
   "A block device that cannot be extracted."))

(define-condition extract-character-device-entry-error (extraction-entry-error)
  ()
  (:documentation
   "A character device that cannot be extracted."))

(define-condition broken-or-circular-links-error (extraction-error)
  ()
  (:documentation
   "A series of symbolic or hard links is broken or circular."))

(defun dereference-link (&optional c)
  "Handle condition C by dereferencing the link."
  (invoke-restart (find-restart 'dereference-link c)))

(defun skip-entry (&optional c)
  "Handle condition C by skipping the entry."
  (invoke-restart (find-restart 'skip-entry c)))

(defun relativize-entry-name (&optional c)
  "Handle condition C by relativizing the pathname."
  (invoke-restart (find-restart 'relativize-entry-name c)))

(defun treat-..-as-back (&optional c)
  "Handle condition C by treating .. as :BACK."
  (invoke-restart (find-restart 'treat-..-as-back c)))
