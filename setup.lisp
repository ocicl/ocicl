(require 'asdf)
(require 'uiop)
(load "package.lisp")
(in-package :ocicl)
(load "deflate.lisp")
(load "minitar.lisp")

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
  (let ((filename (pathname filename)))
    (and (probe-file filename) (delete-file filename))))

(defmacro safe-delete-directory (dirname)
  (let ((dirname (pathname dirname)))
    (and (probe-file dirname) (uiop:delete-directory-tree dirname :validate t))))

(defun safe-timestamp (filename)
  (let ((filename (pathname filename)))
    (and (probe-file filename) (file-write-date filename))))

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

(defun make-ocicl ()
  (let ((ocicl-timestamp (safe-timestamp "ocicl"))
        (source-timestamp (newest-file-timestamp '("*.lisp" "*.asd" "runtime/*.lisp"))))
    (when (or (not ocicl-timestamp)
              (and ocicl-timestamp (> source-timestamp ocicl-timestamp)))
      (safe-delete-file "ocicl")
      (safe-delete-file "systems.csv")
      (safe-delete-directory "systems/")
      (format t "sbcl --dynamic-space-size ~A --no-userinit --eval \"(load \\\"runtime/asdf.lisp\\\")\" --eval \"(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :ocicl) (sb-ext:quit))\""
              (if (boundp 'common-lisp-user::+dynamic-space-size+) (symbol-value 'common-lisp-user::+dynamic-space-size+) 3072))
      (terpri)
      (uiop:run-program
       (format nil "sbcl --dynamic-space-size ~A --no-userinit --eval \"(load \\\"runtime/asdf.lisp\\\")\" --eval \"(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :ocicl) (sb-ext:quit))\""
                 (if (boundp 'common-lisp-user::+dynamic-space-size+) (symbol-value 'common-lisp-user::+dynamic-space-size+) 3072))
       :output *standard-output* :error *error-output*))))

(setf *random-state* (make-random-state t))

(defun random-base36-string ()
  "Return a random base36 (0-9A-Z) string of 8 characters."
  (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*)))

(defun get-temp-ocicl-dl-pathname ()
  (let ((rdir (format nil "ocicl-~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))
    (merge-pathnames (eval `(make-pathname :directory '(:relative ,rdir)))
                     (uiop:default-temporary-directory))))

(defun install-oras (tgz-file)
  (let ((bindir (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+))
        (tmpdir (get-temp-ocicl-dl-pathname))
        (absolute-tgz-file (merge-pathnames
                            (merge-pathnames
                             tgz-file (make-pathname :directory '(:relative "oras")))
                            (uiop:getcwd))))
    (uiop:ensure-all-directories-exist (list bindir))
    (unwind-protect
         (progn
           (uiop:ensure-all-directories-exist (list tmpdir))
           (uiop:with-current-directory (tmpdir)
             (gunzip absolute-tgz-file "oras.tar")
             (unpack-tarball "oras.tar")
             (uiop:copy-file
              (format nil "oras~A" (if (uiop:os-windows-p) ".exe" ""))
              (merge-pathnames (format nil "ocicl-oras~A" (if (uiop:os-windows-p) ".exe" ""))
                               (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+)))
             (unless (uiop:os-windows-p)
               (uiop:run-program (format nil "chmod +x ~A" (merge-pathnames "ocicl-oras"
                                                                            (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+)))))))
      (uiop:delete-directory-tree tmpdir :validate t))))

(defun install-ocicl ()
  (let ((bindir (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+)))
    (uiop:ensure-all-directories-exist (list bindir))
    (uiop:copy-file +ocicl-bin-name+ (merge-pathnames +ocicl-bin-name+ bindir))
    (unless (uiop:os-windows-p)
      (uiop:run-program (format nil "chmod +x ~A" (merge-pathnames +ocicl-bin-name+ bindir)))))
  (let ((arch (if (find :X86-64 *features*) "amd64" "arm64")))
    (cond
      ((uiop:os-macosx-p)
       (install-oras (format nil "oras_1.1.0_darwin_~A.tar.gz" arch)))
      ((uiop:os-windows-p)
       (install-oras (format nil "oras_1.1.0_windows_amd64.tar.gz"))
       (format t "~%~%Be sure to add ~A\\AppData\\Local\\ocicl\\bin\\ to your path!~%~%"
               (uiop:getenv "UserProfile")))
      ((uiop:os-unix-p)
       (install-oras (format nil "oras_1.1.0_linux_~A.tar.gz" arch))))))


(make-ocicl)
(install-ocicl)

(uiop:run-program (format nil "~A setup"
                          (merge-pathnames (format nil "ocicl~A" (if (uiop:os-windows-p) ".exe" ""))
                                           (merge-pathnames (make-pathname :directory '(:relative "bin")) +destdir+)))
                  :output *standard-output* :error-output *error-output*)

(sb-ext:quit)
