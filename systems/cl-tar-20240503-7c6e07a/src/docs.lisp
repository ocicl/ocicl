;;;; Documentation
;;;;
;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(uiop:define-package #:tar-docs
    (:use #:cl
          #:tar
          #:tar-create
          #:tar-extract
          #:tar-simple-extract))

(in-package #:tar-docs)

(40ants-doc:defsection @manual
    (:title "Tar"
     :export nil
     :package-symbol (find-package :tar))
  #.(uiop:read-file-string (asdf:system-relative-pathname :tar "README.md"))
  (@tar 40ants-doc/locatives:section)
  (@tar-simple-extract 40ants-doc/locatives:section)
  (@tar-extract 40ants-doc/locatives:section)
  (@tar-create 40ants-doc/locatives:section))

(40ants-doc:defsection @tar-simple-extract
    (:title "Simple Extraction"
     :export nil
     :package-symbol (find-package :tar-simple-extract))
  "This section describes the support for simple extraction to the filesystem."
  (simple-extract-archive 40ants-doc/locatives:function)
  (@tar-simple-extract-conditions 40ants-doc/locatives:section))

(40ants-doc:defsection @tar-simple-extract-conditions
    (:title "Simple Extraction Conditions"
     :export nil
     :package-symbol (find-package :tar-simple-extract))
  "This section describes the conditions that can occur during
SIMPLE-EXTRACT-ARCHIVE."
  (extraction-error 40ants-doc/locatives:class)
  (extraction-entry-error 40ants-doc/locatives:class)
  (extraction-entry-error-entry (40ants-doc/locatives:reader extraction-entry-error))
  (entry-name-contains-device-error 40ants-doc/locatives:class)
  (entry-name-contains-..-error 40ants-doc/locatives:class)
  (entry-name-is-absolute-error 40ants-doc/locatives:class)
  (extract-character-device-entry-error 40ants-doc/locatives:class)
  (extract-block-device-entry-error 40ants-doc/locatives:class)
  (extract-fifo-entry-error 40ants-doc/locatives:class)
  (extract-symbolic-link-entry-error 40ants-doc/locatives:class)
  (extract-hard-link-entry-error 40ants-doc/locatives:class)
  (broken-or-circular-links-error 40ants-doc/locatives:class)

  (dereference-link 40ants-doc/locatives:function)
  (skip-entry 40ants-doc/locatives:function)
  (relativize-entry-name 40ants-doc/locatives:function)
  (treat-..-as-back 40ants-doc/locatives:function))

(40ants-doc:defsection @tar-extract
    (:title "Extraction"
     :export nil
     :package-symbol (find-package :tar-extract))
  "This section describes the support for non-portable extraction to the
filesystem."
  (extract-archive 40ants-doc/locatives:function)
  (@tar-extract-conditions 40ants-doc/locatives:section))

(40ants-doc:defsection @tar-extract-conditions
    (:title "Extraction Conditions"
     :export nil
     :package-symbol (find-package :tar-extract))
  "This section describes the conditions that can occur during
EXTRACT-ARCHIVE."
  (extraction-error 40ants-doc/locatives:class)
  (extraction-entry-error 40ants-doc/locatives:class)
  (extraction-entry-error-entry (40ants-doc/locatives:reader extraction-entry-error))
  (entry-name-contains-device-error 40ants-doc/locatives:class)
  (entry-name-contains-..-error 40ants-doc/locatives:class)
  (entry-name-is-absolute-error 40ants-doc/locatives:class)
  (extract-character-device-entry-error 40ants-doc/locatives:class)
  (extract-block-device-entry-error 40ants-doc/locatives:class)
  (extract-fifo-entry-error 40ants-doc/locatives:class)
  (extract-symbolic-link-entry-error 40ants-doc/locatives:class)
  (extract-hard-link-entry-error 40ants-doc/locatives:class)
  (broken-or-circular-links-error 40ants-doc/locatives:class)

  (destination-exists-error 40ants-doc/locatives:class)
  (extraction-through-symbolic-link-error 40ants-doc/locatives:class)
  (file-exists-in-place-of-directory-error 40ants-doc/locatives:class)

  (dereference-link 40ants-doc/locatives:function)
  (skip-entry 40ants-doc/locatives:function)
  (relativize-entry-name 40ants-doc/locatives:function)
  (treat-..-as-back 40ants-doc/locatives:function)

  (remove-file 40ants-doc/locatives:function)
  (follow-symbolic-link 40ants-doc/locatives:function)
  (replace-symbolic-link 40ants-doc/locatives:function)
  (supersede-file 40ants-doc/locatives:function)
  (extract-link 40ants-doc/locatives:function)
  (extract-device 40ants-doc/locatives:function)
  (extract-fifo 40ants-doc/locatives:function))

(40ants-doc:defsection @tar-create
    (:title "Create"
     :export nil
     :package-symbol (find-package :tar-create))
  "This section describes the support for non-portable creation of archives
from the filesystem."
  (create-archive 40ants-doc/locatives:function))

(40ants-doc:defsection @tar
    (:title "Tar"
     :export nil
     :package-symbol (find-package :tar))
  "This section describes the high level tar archive support."
  (@tar-archives 40ants-doc/locatives:section)
  (@tar-entries 40ants-doc/locatives:section)
  (@tar-conditions 40ants-doc/locatives:section))

(40ants-doc:defsection @tar-archives
    (:title "Tar Archives"
     :export nil
     :package-symbol (find-package :tar))
  (tar:archive 40ants-doc/locatives:class)
  (tar:gnu-archive 40ants-doc/locatives:class)
  (tar:pax-archive 40ants-doc/locatives:class)
  (tar:ustar-archive 40ants-doc/locatives:class)
  (tar:v7-archive 40ants-doc/locatives:class)

  (tar:open-archive 40ants-doc/locatives:function)
  (tar:close-archive 40ants-doc/locatives:generic-function)
  (tar:with-open-archive 40ants-doc/locatives:macro))

(40ants-doc:defsection @tar-entries
    (:title "Tar Entries"
     :export nil
     :package-symbol (find-package :tar))
  "This section describes the various entry types and how to read/write them
  from/to an archive."

  (tar:read-entry 40ants-doc/locatives:generic-function)
  (tar:write-entry 40ants-doc/locatives:generic-function)
  (tar:do-entries 40ants-doc/locatives:macro)

  (tar:mode-list 40ants-doc/locatives:type)
  (tar:entry 40ants-doc/locatives:class)
  (tar:directory-entry 40ants-doc/locatives:class)
  (tar:file-entry 40ants-doc/locatives:class)
  (tar:symbolic-link-entry 40ants-doc/locatives:class)
  (tar:hard-link-entry 40ants-doc/locatives:class)
  (tar:fifo-entry 40ants-doc/locatives:class)
  (tar:block-device-entry 40ants-doc/locatives:class)
  (tar:character-device-entry 40ants-doc/locatives:class)

  (tar:name 40ants-doc/locatives:generic-function)
  (tar:size 40ants-doc/locatives:generic-function)
  (tar:uname 40ants-doc/locatives:generic-function)
  (tar:gname 40ants-doc/locatives:generic-function)
  (tar:mode 40ants-doc/locatives:generic-function)
  (tar:mtime 40ants-doc/locatives:generic-function)
  (tar:atime 40ants-doc/locatives:generic-function)
  (tar:ctime 40ants-doc/locatives:generic-function)
  (tar:uid 40ants-doc/locatives:generic-function)
  (tar:gid 40ants-doc/locatives:generic-function)
  (tar:linkname 40ants-doc/locatives:generic-function)
  (tar:devmajor 40ants-doc/locatives:generic-function)
  (tar:devminor 40ants-doc/locatives:generic-function)
  (tar:make-entry-stream 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @tar-conditions
    (:title "Tar Conditions"
     :export nil
     :package-symbol (find-package :tar))
  "This section describes the various conditions and restarts in the tar
system."
  (tar:tar-condition 40ants-doc/locatives:class)
  (tar:tar-error 40ants-doc/locatives:class)
  (tar:unsupported-property 40ants-doc/locatives:class)
  (tar:required-property-missing 40ants-doc/locatives:class)
  (tar:required-property-missing-name (40ants-doc/locatives:reader tar:required-property-missing))
  (tar:unsupported-property-value 40ants-doc/locatives:class)
  (tar:unsupported-property-value-name (40ants-doc/locatives:reader tar:unsupported-property-value))
  (tar:unsupported-property-value-value (40ants-doc/locatives:reader tar:unsupported-property-value))
  (tar:property-value-too-long 40ants-doc/locatives:class)

  (tar:ignore-unsupported-property 40ants-doc/locatives:function)
  (tar:truncate-value 40ants-doc/locatives:function)

  (tar:with-ignored-unsupported-properties 40ants-doc/locatives:macro)
  (tar:with-truncated-unsupported-values 40ants-doc/locatives:macro))
