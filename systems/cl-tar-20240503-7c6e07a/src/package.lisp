;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(uiop:define-package #:tar
    (:use :cl)
  (:export
   #:archive
   #:gnu-archive
   #:pax-archive
   #:ustar-archive
   #:v7-archive

   #:open-archive
   #:close-archive
   #:with-open-archive

   #:read-entry
   #:write-entry
   #:do-entries

   #:mode-list

   #:name
   #:size
   #:uname
   #:gname
   #:mode
   #:mtime
   #:atime
   #:ctime
   #:uid
   #:gid
   #:linkname
   #:devmajor
   #:devminor
   #:make-entry-stream

   #:entry
   #:directory-entry
   #:file-entry
   #:symbolic-link-entry
   #:hard-link-entry
   #:fifo-entry
   #:block-device-entry
   #:character-device-entry

   #:tar-condition
   #:tar-error
   #:unsupported-property
   #:ignore-unsupported-property
   #:required-property-missing
   #:required-property-missing-name
   #:unsupported-property-value
   #:unsupported-property-value-name
   #:unsupported-property-value-value
   #:property-value-too-long
   #:truncate-value
   #:with-ignored-unsupported-properties
   #:with-truncated-unsupported-values))
