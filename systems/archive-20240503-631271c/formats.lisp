;;;; formats.lisp -- header formts for cpio and tar

(in-package :archive)

(defun make-field-map ()
  (make-hash-table :test #'equalp))

(defclass archive-entry ()
  ((stream :initform nil)
   (pathname :initarg :pathname :accessor entry-pathname)
   (fields :initform (make-field-map) :reader fields)
   (data-discarded-p :accessor data-discarded-p :initform nil)))

(defclass directory-entry-mixin ()
  ())

(defclass cpio-entry (archive-entry)
  ())

(define-octet-header odc-cpio-header
  (magic 6 :octnum)
  (dev 6 :octnum)
  (inode 6 :octnum)
  (mode 6 :octnum)
  (uid 6 :octnum)
  (gid 6 :octnum)
  (nlink 6 :octnum)
  (rdev 6 :octnum)
  (mtime 11 :octnum)
  (namesize 6 :octnum)
  (filesize 11 :octnum))

(defclass odc-cpio-entry (cpio-entry odc-cpio-header)
  ((%name :initarg :name :reader %name)))

(define-octet-header svr4-cpio-header
  (magic 6 :hexnum)
  (ino 8 :hexnum)
  (mode 8 :hexnum)
  (uid 8 :hexnum)
  (gid 8 :hexnum)
  (nlink 8 :hexnum)
  (mtime 8 :hexnum)
  (filesize 8 :hexnum)
  (devmajor 8 :hexnum)
  (devminor 8 :hexnum)
  (rdevmajor 8 :hexnum)
  (rdevminor 8 :hexnum)
  (namesize 8 :hexnum)
  (crc 8 :hexnum))

(defclass svr4-cpio-entry (cpio-entry svr4-cpio-header)
  ((%name :initarg :name :reader %name)))

;;; definitions taken from the FreeBSD 5.1 manpage
(define-octet-header tar-header
  (%name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  (magic 6 :string)
  (version 2 :string)
   ;; to be used in preference to uid and gid, of course
  (uname 32 :string-null)
  (gname 32 :string-null)
  (devmajor 8 :octnum)
  (devminor 8 :octnum)
  (%prefix 155 :string-null)
  ;; not part of the tar format, but it makes defined constants come out right
  (%%padding 12 :string))

(defclass tar-entry (archive-entry tar-header)
  ())

(defclass tar-longname-entry (tar-entry)
  ((base-pathname :reader base-pathname :initarg :base-pathname)))

(defclass directory-tar-entry (tar-entry directory-entry-mixin)
  ())
