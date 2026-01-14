(in-package :archive)

(defun list-archive-entries (pathname)
  (with-open-archive (archive pathname)
    (do-archive-entries (entry archive)
      (format t "~A~%" (name entry)))))