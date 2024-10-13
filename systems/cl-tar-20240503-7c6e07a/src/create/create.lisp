;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-create)

(defvar *hard-links*)

(defun create-archive (archive
                       file-list
                       &key
                         prefix

                         recursep

                         dereference-hardlinks-p

                         (filter (constantly t)))
  "Add all files in FILE-LIST to ARCHIVE. FILE-LIST may be a string, pathname,
or a list of strings or pathnames. All relative pathname designators are
relative to *DEFAULT-PATHNAME-DEFAULTS*. If an item in FILE-LIST designates an
absolute pathname, the corresponding entry in the tar file will be absolute as
well.

PREFIX must be a string which is prepended to every entry name in the archive.

If RECURSEP is true, then any entries in FILE-LIST naming directories (and not
symlinks to directories) will be recursed into.

If DEREFERENCE-HARDLINKS-P is true, then hardlinks will be followed and saved
as regular file entries instead of hardlink entries.

The following option controls what entries are created.

FILTER defaults to (CONSTANTLY T). Must be a function designator that takes two
arguments (the entry and the pathname were it will be extracted) and returns
non-NIL if the entry should be extracted."
  (unless (listp file-list)
    (setf file-list (list file-list)))
  (tar:with-ignored-unsupported-properties ()
    (tar:with-truncated-unsupported-values (:properties (list 'tar:atime 'tar:ctime 'tar:mtime))
      (let ((*hard-links* (unless dereference-hardlinks-p (make-hash-table :test 'equal))))
        (dolist (file file-list)
          (when (uiop:file-pathname-p file)
            ;; The user *might* have meant a directory instead...
            (when (uiop:directory-exists-p (uiop:ensure-directory-pathname file))
              (setf file (uiop:ensure-directory-pathname file))))
          (add-path-to-archive archive (or prefix "") file recursep filter)))))
  (values))
