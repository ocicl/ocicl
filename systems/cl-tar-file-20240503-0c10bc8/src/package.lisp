;;;; This is part of tar-file. See README.org and LICENSE for more information.

(uiop:define-package #:tar-file
    (:use :cl)
  (:export
   ;; Classes
   #:tar-file
   #:gnu-tar-file
   #:ustar-tar-file
   #:v7-tar-file

   #:*default-header-encoding*

   #:open-tar-file
   #:close-tar-file
   #:finalize-tar-file

   ;; conditions
   #:tar-file-error
   #:invalid-checksum-error
   #:malformed-pax-attribute-entry

   #:entry
   #:file-entry
   #:hard-link-entry
   #:symbolic-link-entry
   #:character-device-entry
   #:block-device-entry
   #:directory-entry
   #:fifo-entry
   #:pax-extended-attributes-entry
   #:pax-global-attributes-entry
   #:gnu-directory-dump-entry
   #:gnu-long-link-name-entry
   #:gnu-long-name-entry
   #:gnu-sparse-file-entry
   #:gnu-volume-header-name-entry
   #:unknown-entry
   #:entry-has-data-p
   #:attribute
   #:attribute-names
   #:do-attributes

   #:write-file-entry
   #:write-symbolic-link-entry
   #:write-hard-link-entry
   #:write-fifo-entry
   #:write-block-device-entry
   #:write-character-device-entry
   #:write-directory-entry
   #:write-pax-extended-attributes-entry
   #:write-pax-global-attributes-entry
   #:write-gnu-long-link-name-entry
   #:write-gnu-long-name-entry

   #:entry-file-p
   #:entry-symbolic-link-p
   #:entry-hard-link-p
   #:entry-fifo-p
   #:entry-block-device-p
   #:entry-character-device-p
   #:entry-directory-p
   #:entry-pax-extended-attributes-p
   #:entry-pax-global-attributes-p
   #:entry-gnu-long-link-name-p
   #:entry-gnu-long-name-p
   #:entry-gnu-directory-dump-p
   #:entry-gnu-sparse-file-p
   #:entry-gnu-volume-header-name-p
   #:entry-unknown-p

   #:with-open-tar-file
   #:do-entries

   #:name
   #:entry-type
   #:size
   #:uname
   #:gname
   #:mode
   #:mtime
   #:uid
   #:gid
   #:linkname
   #:devmajor
   #:devminor
   #:prefix
   #:atime
   #:ctime
   #:offset
   #:offset-sparse-0
   #:numbytes-sparse-0
   #:offset-sparse-1
   #:numbytes-sparse-1
   #:offset-sparse-2
   #:numbytes-sparse-2
   #:offset-sparse-3
   #:numbytes-sparse-4
   #:isextended
   #:realsize


   #:read-entry
   #:make-entry-stream))
