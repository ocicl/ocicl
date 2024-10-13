(in-package :cl-user)
(defpackage path-parse
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:export :path)
  (:documentation "Portably parse the PATH environment variable."))
(in-package :path-parse)

(defparameter +env-var+
  (if (uiop:os-windows-p)
      "%PATH%"
      "PATH")
  "The name of the environment variable.")

(defparameter +separator+
  (if (uiop:os-windows-p)
      #\;
      #\:)
  "The separator character.")

(defun expand-pathname (pathname)
  "Expand a pathname, resolving . and .. using the current working directory."
  (let ((*default-pathname-defaults* (uiop:getcwd)))
    ;; TODO: just shoot me now
    (uiop:merge-pathnames* pathname)))

(defun path ()
  "Return a list of absolute pathnames where executables might be located."
  (let ((path (uiop:getenv +env-var+)))
    (if path
        (mapcar #'expand-pathname
                (mapcar #'(lambda (path)
                            (uiop:parse-native-namestring path :ensure-directory t))
                        (split-sequence +separator+ path)))
        nil)))
