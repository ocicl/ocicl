;;;; gnu.lisp -- header format for GNU tar-files
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defparameter *gnu-magic-vector*
  (coerce `(,@(map 'list #'char-code "ustar "))
          '(vector (unsigned-byte 8)))
  "The contents of the magic field for gnu tar-files.")

(defparameter *gnu-version-vector*
  (coerce `(,@(map 'list #'char-code " ") 0) '(vector (unsigned-byte 8)))
  "The contents of the version field for gnu tar-files.")

(define-octet-header gnu-header
    (name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  (magic 6 :string *gnu-magic-vector*)
  (version 2 :string *gnu-version-vector*)
  (uname 32 :string-null)
  (gname 32 :string-null)
  (devmajor 8 :octnum)
  (devminor 8 :octnum)
  (atime 12 :octnum)
  (ctime 12 :octnum)
  (offset 12 :octnum)
  (longnames 4 :string)
  (unused 1 :byte)
  (offset-sparse-0 12 :octnum)
  (numbytes-sparse-0 12 :octnum)
  (offset-sparse-1 12 :octnum)
  (numbytes-sparse-1 12 :octnum)
  (offset-sparse-2 12 :octnum)
  (numbytes-sparse-2 12 :octnum)
  (offset-sparse-3 12 :octnum)
  (numbytes-sparse-3 12 :octnum)
  (isextended 1 :byte)
  (realsize 12 :octnum)
  (%%padding 17 :string))

(defclass gnu-tar-file (tar-file) ()
  (:documentation
   "A gnu tar file."))

(defmethod header-type ((tar-file gnu-tar-file))
  'gnu-header)

(defun detect-gnu-tar-file (buffer)
  (let ((offset (field-offset 'gnu-header 'magic))
        (length (field-length 'gnu-header 'magic)))
    (when (equalp *gnu-magic-vector*
                  (subseq buffer offset (+ offset length)))
      'gnu-tar-file)))

(register-type-detector 'detect-gnu-tar-file)

(defmethod entry-type ((tar-file gnu-tar-file) header)
  (alexandria:switch ((typeflag header))
    (+tar-regular-file+
     'file-entry)
    (+tar-regular-alternate-file+
     'file-entry)
    (+tar-hard-link+
     'hard-link-entry)
    (+tar-symbolic-link+
     'symbolic-link-entry)
    (+tar-character-device+
     'character-device-entry)
    (+tar-block-device+
     'block-device-entry)
    (+tar-directory-file+
     'directory-entry)
    (+tar-fifo-device+
     'fifo-entry)
    (+gnutar-long-name+
     'gnu-long-name-entry)
    (+gnutar-long-link-name+
     'gnu-long-link-name-entry)
    (+gnutar-sparse+
     'gnu-sparse-file-entry)
    (+gnutar-directory-dump+
     'gnu-directory-dump-entry)
    (+gnutar-volume-header-name+
     'gnu-volume-header-name-entry)
    (t
     'unknown-entry)))
