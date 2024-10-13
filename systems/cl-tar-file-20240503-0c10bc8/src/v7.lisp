;;;; v7.lisp -- header formts for v7tar
;;;;
;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(define-octet-header v7-header
    (name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  ;; not part of the tar format, but it makes defined constants come out right
  (%%padding 255 :string))

(defclass v7-tar-file (tar-file) ()
  (:documentation
   "A v7 tar file."))

(defmethod header-type ((tar-file v7-tar-file))
  'v7-header)

(defmethod entry-type ((tar-file v7-tar-file) header)
  (if (alexandria:ends-with #\/ (name header))
      'directory-entry
      (alexandria:switch ((typeflag header))
        (+tar-regular-file+
         'file-entry)
        (+tar-regular-alternate-file+
         'file-entry)
        (+tar-hard-link+
         'hard-link-entry)
        (+tar-symbolic-link+
         'symbolic-link-entry)
        (+tar-directory-file+
         'directory-entry)
        (t
         'unknown-entry))))
