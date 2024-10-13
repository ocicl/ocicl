;;;; ustar.lisp -- header formats for ustar
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defparameter *ustar-magic-vector*
  (coerce `(,@(map 'list #'char-code "ustar") 0)
          '(vector (unsigned-byte 8)))
  "The contents of the magic field for ustar tar-files.")

(defparameter *ustar-version-vector*
  (coerce (map 'list #'char-code "00") '(vector (unsigned-byte 8)))
  "The contents of the version field for ustar tar-files.")

;;; definitions taken from the FreeBSD 5.1 manpage
(define-octet-header ustar-header
    (name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  (magic 6 :bytes *ustar-magic-vector*)
  (version 2 :bytes *ustar-version-vector*)
  ;; to be used in preference to uid and gid, of course
  (uname 32 :string-null)
  (gname 32 :string-null)
  (devmajor 8 :octnum)
  (devminor 8 :octnum)
  (prefix 155 :string-null)
  ;; not part of the tar format, but it makes defined constants come out right
  (%%padding 12 :string))

(defclass ustar-tar-file (tar-file) ()
  (:documentation
   "A ustar tar file."))

(defmethod header-type ((tar-file ustar-tar-file))
  'ustar-header)

(defun detect-ustar-tar-file (buffer)
  (let ((offset (field-offset 'ustar-header 'magic))
        (length (field-length 'ustar-header 'magic)))
    (when (equalp *ustar-magic-vector*
                  (subseq buffer offset (+ offset length)))
      'ustar-tar-file)))

(register-type-detector 'detect-ustar-tar-file)

(defmethod entry-type ((tar-file ustar-tar-file) header)
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
    (+posix-extended-header+
     'pax-extended-attributes-entry)
    (+posix-global-header+
     'pax-global-attributes-entry)
    (t
     'unknown-entry)))
