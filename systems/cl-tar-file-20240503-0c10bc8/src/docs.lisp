;;;; Documentation
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(40ants-doc:defsection @manual (:title "Tar File Manual"
                                :export nil)
  #.(uiop:read-file-string (asdf:system-relative-pathname :tar-file "README.md"))
  (@opening-tar-files 40ants-doc/locatives:section)
  (@tar-archive-types 40ants-doc/locatives:section)
  (@entries 40ants-doc/locatives:section)
  (@conditions 40ants-doc/locatives:section))

(40ants-doc:defsection @opening-tar-files (:title "Opening and Closing Tar Files"
                                           :export nil)
  (*default-header-encoding* 40ants-doc/locatives:variable)
  (with-open-tar-file 40ants-doc/locatives:macro)
  (open-tar-file 40ants-doc/locatives:function)
  (finalize-tar-file 40ants-doc/locatives:generic-function)
  (close-tar-file 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @tar-archive-types (:title "Tar Archive Types"
                                           :export nil)
  "There are three different types of tar archives that this system can handle,
with a common base class."
  (tar-file 40ants-doc/locatives:class)
  (ustar-tar-file 40ants-doc/locatives:class)
  (gnu-tar-file 40ants-doc/locatives:class)
  (v7-tar-file 40ants-doc/locatives:class))

(40ants-doc:defsection @entries (:title "Entries"
                                 :export nil)
  (do-entries 40ants-doc/locatives:macro)
  (read-entry 40ants-doc/locatives:generic-function)
  (entry 40ants-doc/locatives:class)
  (entry-has-data-p 40ants-doc/locatives:generic-function)
  (make-entry-stream 40ants-doc/locatives:generic-function)
  (name 40ants-doc/locatives:generic-function)
  (size 40ants-doc/locatives:generic-function)
  (uname 40ants-doc/locatives:generic-function)
  (gname 40ants-doc/locatives:generic-function)
  (mode 40ants-doc/locatives:generic-function)
  (mtime 40ants-doc/locatives:generic-function)
  (uid 40ants-doc/locatives:generic-function)
  (gid 40ants-doc/locatives:generic-function)
  (linkname 40ants-doc/locatives:generic-function)
  (devmajor 40ants-doc/locatives:generic-function)
  (devminor 40ants-doc/locatives:generic-function)
  (prefix 40ants-doc/locatives:generic-function)
  (atime 40ants-doc/locatives:generic-function)
  (ctime 40ants-doc/locatives:generic-function)
  (offset 40ants-doc/locatives:generic-function)
  (offset-sparse-0 40ants-doc/locatives:generic-function)
  (numbytes-sparse-0 40ants-doc/locatives:generic-function)
  (offset-sparse-1 40ants-doc/locatives:generic-function)
  (numbytes-sparse-1 40ants-doc/locatives:generic-function)
  (offset-sparse-2 40ants-doc/locatives:generic-function)
  (numbytes-sparse-2 40ants-doc/locatives:generic-function)
  (offset-sparse-3 40ants-doc/locatives:generic-function)
  (numbytes-sparse-3 40ants-doc/locatives:generic-function)
  (isextended 40ants-doc/locatives:generic-function)
  (realsize 40ants-doc/locatives:generic-function)
  (attribute 40ants-doc/locatives:generic-function)
  (attribute-names 40ants-doc/locatives:generic-function)
  (do-attributes 40ants-doc/locatives:macro)

  (@file-entry 40ants-doc/locatives:section)
  (@symbolic-link-entry 40ants-doc/locatives:section)
  (@hard-link-entry 40ants-doc/locatives:section)
  (@character-device-entry 40ants-doc/locatives:section)
  (@block-device-entry 40ants-doc/locatives:section)
  (@fifo-entry 40ants-doc/locatives:section)
  (@directory-entry 40ants-doc/locatives:section)
  (@pax-extended-attributes-entry 40ants-doc/locatives:section)
  (@pax-global-attributes-entry 40ants-doc/locatives:section)
  (@gnu-long-link-name-entry 40ants-doc/locatives:section)
  (@gnu-long-name-entry 40ants-doc/locatives:section)
  (@gnu-directory-dump-entry 40ants-doc/locatives:section)
  (@gnu-sparse-file-entry 40ants-doc/locatives:section)
  (@gnu-volume-header-name-entry 40ants-doc/locatives:section)
  (@unknown-entry 40ants-doc/locatives:section))

(40ants-doc:defsection @file-entry (:title "File Entry"
                                    :export nil)
  (file-entry 40ants-doc/locatives:class)
  (write-file-entry 40ants-doc/locatives:generic-function)
  (entry-file-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @symbolic-link-entry (:title "Symbolic Link Entry"
                                             :export nil)
  (symbolic-link-entry 40ants-doc/locatives:class)
  (write-symbolic-link-entry 40ants-doc/locatives:generic-function)
  (entry-symbolic-link-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @hard-link-entry (:title "Hard Link Entry"
                                         :export nil)
  (hard-link-entry 40ants-doc/locatives:class)
  (write-hard-link-entry 40ants-doc/locatives:generic-function)
  (entry-hard-link-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @character-device-entry (:title "Character Device Entry"
                                                :export nil)
  (character-device-entry 40ants-doc/locatives:class)
  (write-character-device-entry 40ants-doc/locatives:generic-function)
  (entry-character-device-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @block-device-entry (:title "Block Device Entry"
                                            :export nil)
  (block-device-entry 40ants-doc/locatives:class)
  (write-block-device-entry 40ants-doc/locatives:generic-function)
  (entry-block-device-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @fifo-entry (:title "Fifo Entry"
                                    :export nil)
  (fifo-entry 40ants-doc/locatives:class)
  (write-fifo-entry 40ants-doc/locatives:generic-function)
  (entry-fifo-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @directory-entry (:title "Directory Entry"
                                         :export nil)
  (directory-entry 40ants-doc/locatives:class)
  (write-directory-entry 40ants-doc/locatives:generic-function)
  (entry-directory-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @pax-extended-attributes-entry (:title "Pax Extended Attributes Entry"
                                                       :export nil)
  (pax-extended-attributes-entry 40ants-doc/locatives:class)
  (write-pax-extended-attributes-entry 40ants-doc/locatives:generic-function)
  (entry-pax-extended-attributes-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @pax-global-attributes-entry (:title "Pax Global Attributes Entry"
                                    :export nil)
  (pax-global-attributes-entry 40ants-doc/locatives:class)
  (write-pax-global-attributes-entry 40ants-doc/locatives:generic-function)
  (entry-pax-global-attributes-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @gnu-long-link-name-entry (:title "Gnu Long Link Name Entry"
                                                  :export nil)
  (gnu-long-link-name-entry 40ants-doc/locatives:class)
  (write-gnu-long-link-name-entry 40ants-doc/locatives:generic-function)
  (entry-gnu-long-link-name-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @gnu-long-name-entry (:title "Gnu Long Name Entry"
                                             :export nil)
  (gnu-long-name-entry 40ants-doc/locatives:class)
  (write-gnu-long-name-entry 40ants-doc/locatives:generic-function)
  (entry-gnu-long-name-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @gnu-directory-dump-entry (:title "Gnu Directory Dump Entry"
                                                  :export nil)
  (gnu-directory-dump-entry 40ants-doc/locatives:class)
  (entry-gnu-directory-dump-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @gnu-sparse-file-entry (:title "Gnu-Sparse-File Entry"
                                               :export nil)
  (gnu-sparse-file-entry 40ants-doc/locatives:class)
  (entry-gnu-sparse-file-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @gnu-volume-header-name-entry (:title "Gnu Volume Header Name Entry"
                                                      :export nil)
  (gnu-volume-header-name-entry 40ants-doc/locatives:class)
  (entry-gnu-volume-header-name-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @unknown-entry (:title "Unknown Entry"
                                       :export nil)
  (unknown-entry 40ants-doc/locatives:class)
  (entry-unknown-p 40ants-doc/locatives:generic-function))

(40ants-doc:defsection @conditions (:title "Conditions"
                                    :export nil)
  (tar-file-error condition)
  (invalid-checksum-error condition)
  (malformed-pax-attribute-entry condition))
