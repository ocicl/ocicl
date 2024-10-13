(in-package #:tar-simple-extract-test)

(defun call-with-temp-dir (thunk)
  (let* ((temp-dir (uiop:ensure-directory-pathname
                    #-windows (nix:mkdtemp (namestring (merge-pathnames "cl-tar-" (uiop:temporary-directory))))
                    #+windows (namestring (merge-pathnames (format nil "cl-tar-test-~D" (random 1000000)) (uiop:temporary-directory)))))
         (*default-pathname-defaults* (pathname temp-dir)))
    #+windows (ensure-directories-exist *default-pathname-defaults*)
    (unwind-protect
         (funcall thunk)
      (uiop:delete-directory-tree temp-dir :validate t))))

(defmacro with-temp-dir (() &body body)
  `(call-with-temp-dir (lambda () ,@body)))
