;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(uiop:define-package #:tar-simple-extract
    (:use :cl
          #:tar-common-extract)
  ;; From TAR-COMMON-EXTRACT
  (:export
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

  ;; extract
  (:export
   #:simple-extract-archive))
