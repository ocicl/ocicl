;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- New pathnames.
;;;

(in-package :iolib/pathnames)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass unix-path (file-path)
  ()
  (:default-initargs :host :unspecific
                     :device :unspecific))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

(defmethod initialize-instance :after ((path unix-path) &key)
  (with-slots (host device)
      path
    (unless device (setf device :unspecific))
    (check-type host (eql :unspecific))
    (check-type device (eql :unspecific))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun unix-path-p (thing)
  (typep thing 'unix-path))


;;;-------------------------------------------------------------------------
;;; Operations
;;;-------------------------------------------------------------------------

(defun make-file-path (&key host device (components nil componentsp)
                       (defaults nil defaultsp))
  (declare (ignore host device))
  (let ((defaults (and defaultsp (file-path defaults))))
    (make-instance 'unix-path
                   :components (cond (componentsp components)
                                     (defaultsp
                                      (file-path-components defaults))))))

(defun merge-file-paths (pathspec &optional
                         (defaults *default-file-path-defaults*))
  (let ((path (file-path pathspec))
        (defaults (file-path defaults)))
    (if (absolute-file-path-p path)
        path
        (make-instance 'unix-path
                       :components (append (file-path-components defaults)
                                           (file-path-components path))))))

(defun enough-file-path (pathspec &optional
                         (defaults *default-file-path-defaults*))
  (let ((path (file-path pathspec))
        (defaults (file-path defaults)))
    (cond
      ((or (relative-file-path-p path)
           (relative-file-path-p defaults))
       path)
      (t
       (let* ((dir (cdr (slot-value path 'components)))
              (mismatch
               (mismatch dir (cdr (slot-value defaults 'components))
                         :test #'string=)))
         (if mismatch
             (make-instance 'unix-path :components (subseq dir mismatch))
             (make-instance 'unix-path :components (list :root))))))))

(defun %file-path-host-namestring (path)
  (declare (ignore path))
  "")

(defun %file-path-device-namestring (path)
  (declare (ignore path))
  "")

(defun %components-namestring (components)
  (multiple-value-bind (root dirs)
      (split-root/nodes components)
    (let ((delimstr (string +directory-delimiter+))
          (nullstr ""))
      (concatenate 'string
                   (if (eql :root root)
                       delimstr
                       (if (null dirs)
                           "."
                           nullstr))
                   (apply #'join +directory-delimiter+ dirs)))))

(defun %file-path-components-namestring (path)
  (%components-namestring (slot-value path 'components)))

(defun %file-path-directory-namestring (path)
  (%components-namestring (%file-path-directory path)))

(defmethod file-path-namestring ((path unix-path))
  (%components-namestring (slot-value path 'components)))

(defmethod file-path-namestring (pathspec)
  (file-path-namestring (file-path pathspec)))

(defun split-directory-namestring (namestring)
  (split-sequence-if (lambda (c) (char= c +directory-delimiter+))
                     namestring
                     :remove-empty-subseqs t))

(defun absolute-namestring-p (namestring)
  (char= +directory-delimiter+ (aref namestring 0)))

(defun parse-file-path (pathspec &key (start 0) end (expand-user t))
  (check-type pathspec string)
  (when (zerop (length pathspec))
    (error 'invalid-file-path
           :path pathspec
           :reason "Null paths are not valid"))
  (let* ((actual-namestring (subseq pathspec start end))
         (expansion (or (when expand-user
                          (ignore-some-conditions (isys:syscall-error)
                            (%expand-user-directory actual-namestring)))
                        actual-namestring))
         (components (split-directory-namestring expansion)))
    (make-instance 'unix-path
                   :components (if (absolute-namestring-p expansion)
                                   (cons :root components)
                                   components))))

(defun %expand-user-directory (pathspec)
  (flet ((user-homedir (user)
           (nth-value 5 (isys:getpwnam user)))
         (uid-homedir (uid)
           (nth-value 5 (isys:getpwuid uid))))
    (unless (char= #\~ (aref pathspec 0))
      (return* pathspec))
    (destructuring-bind (first &rest rest)
        (split-directory-namestring pathspec)
      (let ((homedir
             (cond
               ((string= "~" first)
                (or (isys:getenv "HOME")
                    (let ((username
                           (or (isys:getenv "USER")
                               (isys:getenv "LOGIN"))))
                      (if username
                          (user-homedir username)
                          (uid-homedir (isys:getuid))))))
               ((char= #\~ (aref first 0))
                (user-homedir (subseq first 1)))
               (t
                (bug "The pathspec is suppose to start with a ~S" #\~)))))
        (if homedir
            (apply #'join +directory-delimiter+ homedir rest)
            pathspec)))))


;;;-------------------------------------------------------------------------
;;; Specials
;;;-------------------------------------------------------------------------

(defparameter *default-file-path-defaults*
  (or (ignore-some-conditions (isys:syscall-error)
        (parse-file-path (isys:getcwd)))
      (ignore-some-conditions (isys:syscall-error)
        (parse-file-path "~"))
      (parse-file-path "/")))

(defparameter *default-execution-path*
  (mapcar #'parse-file-path
          (split-sequence +execution-path-delimiter+
                          (isys:getenv "PATH")
                          :remove-empty-subseqs t)))
