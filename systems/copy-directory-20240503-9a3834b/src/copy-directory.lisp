(in-package :cl-user)
(defpackage copy-directory
  (:use :cl)
  (:export :copy)
  (:documentation "Copy a directory."))
(in-package :copy-directory)

(defun subtract-pathname (root pathname)
  "root is an absolute directory, and pathname is an absolute pathname, such
  that pathname is inside of root. Remove the common directory components,
  leaving a relative pathname."
  (assert (uiop:directory-pathname-p root))
  (assert (uiop:absolute-pathname-p root))
  (assert (uiop:absolute-pathname-p pathname))
  (assert (uiop:subpathp pathname root))
  (uiop:subpathp pathname root))

(defun cl-copy (source destination)
  "Copy everything under source to destination. Pure CL function."
  (ensure-directories-exist destination)
  (fad:walk-directory source
                      #'(lambda (pathname)
                          (unless (equal pathname source)
                            (let* ((relative-path (subtract-pathname source pathname))
                                   (target (merge-pathnames relative-path
                                                            destination)))
                              (if (uiop:directory-pathname-p pathname)
                                  ;; Ensure an equivalent directory exists
                                  (ensure-directories-exist target)
                                  ;; Copy the absolute source file to the target
                                  (uiop:copy-file pathname target)))))
                      :directories :breadth-first
                      :follow-symlinks nil)
  destination)

(defun native-copy (cp-path source destination)
  "Copy everything from source to destination using native system tools."
  (ensure-directories-exist
   (uiop:pathname-parent-directory-pathname destination))
  (uiop:run-program (format nil "~S -r ~S ~S"
                            (namestring cp-path)
                            (namestring source)
                            (namestring destination)))
  destination)

(defun copy (source destination)
  "Copy everything in the source directory to the destination directory. Both
pathnames must be absolute directory pathnames. If the cp program is available,
use it. If not, uses pure CL code. Returns the destination."
  (assert (uiop:absolute-pathname-p source))
  (assert (uiop:absolute-pathname-p destination))
  (assert (uiop:directory-pathname-p source))
  (assert (uiop:directory-pathname-p destination))
  (let ((cp (which:which "cp")))
    ;(if cp
    ;    (native-copy cp source destination)
    ;    (cl-copy source destination))))
    (cl-copy source destination)))
