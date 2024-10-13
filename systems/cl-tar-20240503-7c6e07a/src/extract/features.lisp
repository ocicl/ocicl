;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-extract)

#+(and (not windows) (not darwin))
(progn
  (pushnew :tar-extract-use-mkfifoat *features*))

#-windows
(progn
  (pushnew :tar-extract-use-openat *features*)
  (pushnew :tar-extract-use-utimens *features*))

#+windows
(progn
  ;; HACK Should eventually make a windows-only extraction file that uses the
  ;; Win32 API, just like the creation code does.
  (nix::defsyscall ("futime" %futime) :int
    (fd    nix::file-descriptor-designator)
    (times (:pointer (:struct nix::utimbuf))))

  (defun futime (fd atime mtime)
    (cffi:with-foreign-object (times '(:struct nix::utimbuf))
      (setf (cffi:foreign-slot-value times '(:struct nix::utimbuf) 'nix::actime) atime
            (cffi:foreign-slot-value times '(:struct nix::utimbuf) 'nix::modtime) mtime)
      (%futime fd times))))
