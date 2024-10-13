;;;; This is part of cl-tar. See README.md and LICENSE for more information.

(in-package #:tar-common-extract)

(defun pathname-coalesce-backs-1 (components)
  (loop
    :for component := (pop components)
    :if (and (eql component :back)
             (stringp (first components)))
      :do (pop components)
    :else :collect component
    :until (member component '(:absolute :relative))))

(defun pathname-coalesce-backs (pn)
  ":BACK in a directory pathname is purely syntactic. Some
implementations (like SBCL) automatically coalesce them for you (if
possible). Other implementations (like ABCL) do not. This is a helper function
that removes all :BACKs from a PN's directory that it can."
  (let ((directory (reverse (pathname-directory pn))))
    (loop
      :for new-directory := (pathname-coalesce-backs-1 directory)
      :until (equal new-directory directory)
      :do (setf directory new-directory))
    (make-pathname
     :directory (reverse directory)
     :defaults pn)))

(defun pathname-parent-directory-pathname (pn)
  (pathname-coalesce-backs (merge-pathnames (make-pathname :directory '(:relative :back)
                                                           :name nil
                                                           :type nil)
                                            pn)))
