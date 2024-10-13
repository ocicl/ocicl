;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(uiop:define-package #:tar-common-extract
    (:use :cl)
  ;; Conditions
  (:export
   #:*current-entry*

   #:extraction-error

   #:extraction-entry-error
   #:extraction-entry-error-entry

   #:entry-name-contains-device-error

   #:entry-name-contains-..-error

   #:entry-name-is-absolute-error

   #:extract-entry-error
   #:extract-character-device-entry-error
   #:extract-block-device-entry-error
   #:extract-fifo-entry-error
   #:extract-symbolic-link-entry-error
   #:extract-hard-link-entry-error

   #:broken-or-circular-links-error

   #:dereference-link

   #:skip-entry
   #:relativize-entry-name
   #:treat-..-as-back)

  ;; Utils
  (:export
   #:pathname-parent-directory-pathname)

  ;; pathname computation
  (:export
   #:compute-extraction-pathname))
