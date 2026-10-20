;;; setup.lisp
;;;
;;; SPDX-License-Identifier: MIT

(require 'asdf)
(require 'uiop)

(in-package :cl-user)

(eval-when
    (:load-toplevel :execute)
  (progn
    (defconstant +destdir+ (cond
                             ((uiop:os-windows-p)
                              (format nil "~A\\AppData\\Local\\ocicl\\"
                                      (uiop:getenv "UserProfile")))
                             ((uiop:getenvp "OCICL_PREFIX")
                              (uiop:ensure-directory-pathname (uiop:getenv "OCICL_PREFIX")))
                             (t "~/.local/")))
    (defconstant +ocicl-bin-name+ (if (uiop:os-windows-p) "ocicl.exe" "ocicl"))))

(defmacro safe-delete-file (filename)
  "Delete FILENAME if it exists, otherwise do nothing."
  (let ((filename (pathname filename)))
    (and (probe-file filename) (delete-file filename)))) ; lint:suppress use-uiop-file-exists-p

(defmacro safe-delete-directory (dirname)
  "Delete directory DIRNAME if it exists, otherwise do nothing."
  (let ((dirname (pathname dirname)))
    (and (probe-file dirname) (uiop:delete-directory-tree dirname :validate t)))) ; lint:suppress use-uiop-file-exists-p

(defun safe-timestamp (filename)
  "Return timestamp of FILENAME if it exists, otherwise return NIL."
  (let ((filename (pathname filename)))
    (and (probe-file filename) (file-write-date filename)))) ; lint:suppress use-uiop-file-exists-p

(defun newest-file-timestamp (patterns)
  "Returns the timestamp of the newest file matching any of the given glob patterns."
  (let ((newest-time 0)
        newest-file)
    (dolist (pattern patterns (values newest-time newest-file))
      (let ((files (uiop:directory-files #p"." pattern)))
        (dolist (file files)
          (let ((time (safe-timestamp file)))
            (when (and time (> time newest-time))
              (setf newest-time time
                    newest-file file))))))))

(defun build-dynamic-space-size ()
  "Return the SBCL dynamic space size, in MB, for building ocicl."
  (if (boundp 'common-lisp-user::+dynamic-space-size+)
      (symbol-value 'common-lisp-user::+dynamic-space-size+)
      3072))

#+egcl
(defun fetch-egcl-forks ()
  "Check out the EGCL library forks pinned in egcl-forks.txt under egcl-systems/."
  (with-open-file (in "egcl-forks.txt")
    (loop for line = (read-line in nil)
          while line
          do (let ((fields (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
                                   :test #'string=)))
               (when (and fields (char/= #\# (char (first fields) 0)))
                 (destructuring-bind (name branch commit) fields
                   (declare (ignore branch))
                   (let ((dir (format nil "egcl-systems/~A" name))
                         (url (format nil "https://github.com/atgreen/~A" name)))
                     (unless (probe-file (format nil "~A/.git/" dir)) ; lint:suppress use-uiop-file-exists-p
                       (format t "Cloning ~A~%" url)
                       (uiop:run-program (list "git" "clone" "--quiet" url dir)
                                         :output *standard-output* :error-output *error-output*))
                     (unless (string= commit
                                      (uiop:run-program (list "git" "-C" dir "rev-parse" "HEAD")
                                                        :output '(:string :stripped t)))
                       (uiop:run-program (list "git" "-C" dir "fetch" "--quiet" "origin")
                                         :output *standard-output* :error-output *error-output*)
                       (uiop:run-program (list "git" "-C" dir "checkout" "--quiet" "--detach" commit)
                                         :output *standard-output* :error-output *error-output*)))))))))

(defun build-command ()
  "Return the command line that builds the ocicl binary with this Lisp implementation."
  #+egcl
  (list (or (uiop:getenv "EGCL") "egcl") ; lint:suppress
        "--no-init"
        "--eval"
        ;; The EGCL library forks shadow the vendored systems they replace.
        "(asdf:initialize-source-registry (list :source-registry (list :directory (uiop:getcwd)) (list :tree (uiop:subpathname (uiop:getcwd) \"egcl-systems/\")) (list :tree (uiop:subpathname (uiop:getcwd) \"systems/\")) :ignore-inherited-configuration))" ; lint:suppress max-line-length
        "--eval"
        "(progn (asdf:make :ocicl) (uiop:quit))")
  #-egcl
  (list (or (uiop:getenv "SBCL") "sbcl") ; lint:suppress
        "--dynamic-space-size"
        (format nil "~A" (build-dynamic-space-size))
        "--no-userinit"
        "--eval"
        "(load \"runtime/asdf.lisp\")"
        "--eval"
        "(progn (asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :also-exclude \"egcl-systems\") (list :tree (uiop:getcwd)))) (asdf:make :ocicl) (sb-ext:quit))")) ; lint:suppress max-line-length

(defun make-ocicl ()
  "Build the ocicl binary if source files are newer than the existing binary."
  (let ((ocicl-timestamp (safe-timestamp "ocicl"))
        (source-timestamp (newest-file-timestamp '("*.lisp" "*.asd" "runtime/*.lisp" "src/*.lisp" "lint/*.lisp"))))
    (when (or (not ocicl-timestamp)
              (and ocicl-timestamp (> source-timestamp ocicl-timestamp)))
      (safe-delete-file "ocicl")
      ;; (safe-delete-file "systems.csv")
      ;; (safe-delete-directory "systems/")
      #+egcl (fetch-egcl-forks)
      (let ((command (build-command)))
        (format t "~{~A~^ ~}~%" (mapcar (lambda (arg)
                                          (if (find #\Space arg) (format nil "~S" arg) arg))
                                        command))
        (uiop:run-program command :output *standard-output* :error *standard-output*)))))

(setf *random-state* (make-random-state t))

(defun random-base36-string ()
  "Return a random base36 (0-9A-Z) string of 8 characters."
  (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*)))

(defun get-temp-ocicl-dl-pathname ()
  "Generate a temporary pathname for downloading ocicl."
  (let ((rdir (format nil "ocicl-~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))
    (merge-pathnames (make-pathname :directory (list :relative rdir))
                     (uiop:default-temporary-directory))))

(defun install-ocicl ()
  "Install ocicl binary and runtime files to the user's system."
  (let ((bindir (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+)))
    (uiop:ensure-all-directories-exist (list bindir))
    (uiop:copy-file +ocicl-bin-name+ (merge-pathnames +ocicl-bin-name+ bindir))
    (unless (uiop:os-windows-p)
      (uiop:run-program (format nil "chmod +x ~A" (merge-pathnames +ocicl-bin-name+ bindir))))))

(make-ocicl)
(install-ocicl)

(uiop:run-program (format nil "~A setup"
                          (merge-pathnames (format nil "ocicl~A" (if (uiop:os-windows-p) ".exe" ""))
                                           (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+)))
                  :output *standard-output* :error-output *error-output*)

(uiop:quit)
