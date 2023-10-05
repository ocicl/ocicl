;;; ocicl.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(in-package #:ocicl)

(require 'sb-introspect)

(defvar *ocicl-registries* (list "ghcr.io/ocicl"))
(defvar *ocicl-globaldir* nil)
(defvar *verbose* nil)
(defvar *ocicl-systems* nil)

(defparameter +version+ (uiop:read-file-form "version.sexp"))
(defparameter +runtime+
  (concatenate 'string
               (uiop:read-file-string "runtime/ocicl-runtime.lisp")
               (format nil "~%(defparameter +version+ ~S)~%" +version+)))

(define-opts
  (:oname :verbose
   :description "produce verbose output"
   :short #\v
   :long "verbose")
  (:oname :global
   :description "operate on the global system collection"
   :short #\g
   :long "global")
  (:oname :registry
   :description "use alternate oci registry"
   :short #\r
   :arg-parser (lambda (arg) (setf *ocicl-registries* (list arg)))
   :meta-var "REGISTRY"
   :default "ghcr.io/ocicl"
   :long "registry"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (option condition))
  (invoke-restart 'skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun split-on-delimeter (line delim)
  (let ((start 0)
        (end 0)
        (result '()))
    (loop for c across line
          for i from 0
          do (if (char= c delim)
                 (progn
                   (setq end i)
                   (push (string-trim " " (subseq line start end)) result)
                   (setq start (+ i 1))))
          finally (push (string-trim " " (subseq line start)) result))
    (nreverse result)))

(defun split-csv-line (line)
  (split-on-delimeter line #\,))

(defun split-lines (line)
  (split-on-delimeter line #\Newline))

(defun read-systems-csv ()
  (let ((systems-file (merge-pathnames (uiop:getcwd) "systems.csv"))
        (ht (make-hash-table :test #'equal)))
    (when (probe-file systems-file)
      (progn
        (dolist (line (uiop:read-file-lines systems-file))
          (let ((vlist (split-csv-line line)))
            (setf (gethash (car vlist) ht) (cons (cadr vlist) (caddr vlist)))
            ))))
    ht))

(defun usage ()
  (usage-describe
   :prefix (format nil "ocicl ~A - copyright (C) 2023 Anthony Green <green@moxielogic.com>" +version+)
   :suffix "Choose from the following ocicl commands:

   help                                Print this help text
   install [SYSTEM[:VERSION]]...       Install systems
   latest [SYSTEM]...                  Install latest version of systems
   list SYSTEM...                      List available system versions
   setup [GLOBALDIR]                   Mandatory ocicl configuration
   version                             Show the ocicl version information

Distributed under the terms of the MIT License"
   :usage-of "ocicl"
   :args     "command"))

(defvar *systems-dir* nil)

(defun debug-log (s)
  (when *verbose*
    (format t "; ~A~%" s)))

(defun do-list (args)
  (when args
    (dolist (system args)
      (loop for registry in *ocicl-registries*
            do (handler-case
                   (progn
                     (format t "~A(~A):~%" system registry)
                     (dolist (s (reverse (cdr (sort (split-lines (uiop:run-program (format nil "ocicl-oras repo tags ~A/~A" registry (mangle system)) :output '(:string))) #'string-lessp))))
                       (format t "~T~A~%" s))
                     (return))
                 (uiop/run-program:subprocess-error (e)
                   (declare (ignore e))
                   (format t "~T~A not found~%" system))))
      (format t "~%"))))

(defun get-ocicl-dir ()
  (let ((ocicl-dir (merge-pathnames (make-pathname :directory '(:relative "ocicl"))
                                     (uiop:xdg-data-home))))
    (uiop:ensure-all-directories-exist (list ocicl-dir))
    ocicl-dir))

(defun load-system (name)
  (if *verbose*
      (asdf:load-system name)
      (let ((*load-verbose* nil)
            (*compile-verbose* nil)
            (*load-print* nil)
            (*compile-print* nil))
        (handler-bind ((warning #'muffle-warning))
          (handler-bind ((sb-ext:compiler-note #'muffle-warning))
            (asdf:load-system name))))))

(defun do-latest (args)
  ;; Make sure the systems directory exists
  (uiop:ensure-all-directories-exist
   (list *systems-dir*))
  (if args
      ;; Download latest systems provided on the command line.
      (dolist (system args)
        (if (position #\: system)
            (progn
              (format uiop:*stderr* "Error: version tag specified for system ~A.~%" system)
              (sb-ext:quit)))
        (unless (download-system system)
            (progn
              (format uiop:*stderr* "Error: system ~A not found.~%" system)
              (sb-ext:quit))
            (load-system system)))
      ;; Download latest versions of all systems.
      (let ((blobs (make-hash-table :test #'equal)))
        (maphash (lambda (key value)
                   (setf (gethash (car value) blobs) key))
                 *ocicl-systems*)
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (let ((system (extract-between-slash-and-at key)))
                     (download-system system)))
             blobs))))

(defun do-version (args)
  (declare (ignore args))
  (format t "ocicl version:   ~A~%" +version+)
  (format t "Lisp runtime:    ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (format t "ASDF version:    ~A~%" (asdf:asdf-version)))

(defun do-setup (args)

  (with-open-file (stream (merge-pathnames (get-ocicl-dir) "ocicl-registry.cfg")
                          :direction :output
                          :if-exists :supersede)
    (write-string (first *ocicl-registries*) stream))
  (if args
    (let ((original-directory (uiop:getcwd)))
      (unwind-protect
          (handler-case
              (progn
                (uiop:chdir (car args))
                (setf args (list (namestring (uiop:getcwd))))
                (with-open-file (stream (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")
                                        :direction :output
                                        :if-exists :supersede)
                                (write-string (car args) stream)))
            (error (e)
              (declare (ignore e))
              (format uiop:*stderr* "Error: directory ~A does not exist.~%" (car args))
              (sb-ext:quit)))
        (uiop:chdir original-directory)))
    (let ((old-config-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))
      (when (probe-file old-config-file)
        (delete-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))))

  (let* ((odir (get-ocicl-dir))
         (gdir (or (car args) odir))
         (runtime-source (merge-pathnames odir "ocicl-runtime.lisp")))
    (with-open-file (stream runtime-source
                            :direction :output
                            :if-exists :supersede)
      (write-string +runtime+ stream)
      (format t ";; Add the following to your lisp startup file (.sbclrc/.eclrc/.abclrc):~%~%#-ocicl~%(when (probe-file ~S)~%  (load ~S))~%" runtime-source runtime-source)
      (format t ";; Any systems you install in ~A~%;; will be available globally unless you comment out this line:~%" gdir)
      (format t "(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration (:tree ~S)))~%~%" gdir))))


(defun do-install (args)
  ;; Make sure the systems directory exists
  (uiop:ensure-all-directories-exist
   (list *systems-dir*))
  (if args
      ;; Download the systems provided on the command line.
      (dolist (system args)
        (if (position #\@ system)
            (progn
              (unless (download-and-install system)
                (progn
                  (format uiop:*stderr* "Error: can't download ~A.~%" system)
                  (sb-ext:quit))))
            (let* ((slist (split-on-delimeter system #\:))
                   (name (car slist)))
              (if (download-system system)
                  (load-system name)
                  (progn
                    (format uiop:*stderr* "Error: system ~A not found.~%" system)
                    (sb-ext:quit))))))
      ;; Download all systems in systems.csv.
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (when (not (probe-file (concatenate 'string (namestring *systems-dir*) (cdr value))))
                   (format t "; downloading ~A~%" (car value))
                   (download-and-install (car value))))
               *ocicl-systems*)))

(defun main ()

  (let ((config-file (merge-pathnames (get-ocicl-dir) "ocicl-registry.cfg")))
    (when (probe-file config-file)
      (setf *ocicl-registries*
            (with-open-file (in config-file)
              (loop for line = (read-line in nil nil)
                    while line
                    ;; skip comments
                    unless (char= #\# (aref line 0))
                      collect (string-trim '(#\Space #\Tab #\Newline #\Return) line))))))

  (let ((config-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))
    (when (probe-file config-file)
      (setf *ocicl-globaldir* (uiop:read-file-string config-file))))

  (let ((workdir (uiop:getcwd)))

    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((unknown-option #'unknown-option))
              (get-opts))
          (missing-arg (condition)
                       (format t "fatal: option ~s needs an argument!~%"
                               (option condition)))
          (arg-parser-failed (condition)
                             (format t "fatal: cannot parse ~s as argument of ~s~%"
                                     (raw-arg condition)
                                     (option condition))))
      (when-option (options :verbose)
                   (setf *verbose* t))
      (when-option (options :global)
                   (setf workdir (or *ocicl-globaldir* (get-ocicl-dir))))

      (let ((original-directory (uiop:getcwd)))
        (unwind-protect
            (progn
              (uiop:chdir workdir)
              (setf *random-state* (make-random-state t))
              (setq *ocicl-systems* (read-systems-csv))
              (setq *systems-dir* (merge-pathnames (make-pathname :directory '(:relative "systems"))
                                                   (uiop:getcwd)))
              (if (not (> (length free-args) 0))
                  (usage)
                (let ((cmd (car free-args)))
                  (cond
                   ((string= cmd "help")
                    (usage))
                   ((string= cmd "install")
                    (do-install (cdr free-args)))
                   ((string= cmd "latest")
                    (do-latest (cdr free-args)))
                   ((string= cmd "list")
                    (do-list (cdr free-args)))
                   ((string= cmd "setup")
                    (do-setup (cdr free-args)))
                   ((string= cmd "version")
                    (do-version (cdr free-args)))
                   (t (usage))))))
          (uiop:chdir original-directory))))))

(defun replace-plus-with-string (str)
  (let ((mangled (with-output-to-string (s)
                    (loop for c across str do
                          (if (char= c #\+)
                              (write-string "_plus_" s)
                            (write-char c s))))))
    (if (char= (char mangled (- (length mangled) 1)) #\_)
        (subseq mangled 0 (- (length mangled) 1))
      mangled)))

(defun mangle (str)
  (replace-plus-with-string str))

(defun get-temp-ocicl-dl-pathname ()
  (let ((rdir (format nil "ocicl-~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))
    (merge-pathnames (eval `(make-pathname :directory '(:relative ,rdir)))
                     (uiop:default-temporary-directory))))

(defun find-asd-files (dir)
  "Recursively find all files with the .asd extension in a directory."
  (let ((systems (list)))
    (uiop:collect-sub*directories dir
                                  t t
                                  (lambda (d)
                                    (dolist (f (uiop:directory-files d))
                                      (when (equal "asd" (pathname-type f))
                                        (push f systems)))))
    systems))

(defun extract-sha256 (str)
  (let* ((start (search "sha256:" str))
         (end (+ start 71))) ;; 7 for "sha256:" and 64 for the hash
    (when start
      (subseq str (+ start 7) end))))

(defun extract-between-slash-and-at (input)
  (let* ((reversed (reverse input))
         (pos-at (position #\@ reversed))
         (pos-slash (position #\/ reversed :start pos-at)))
    (subseq input (- (length input) pos-slash) (- (length input) (1+ pos-at)))))

(defvar *ocicl-systems* nil)

(defun write-systems-csv ()
  (with-open-file (stream (merge-pathnames (uiop:getcwd) "systems.csv")
                          :direction :output
                          :if-exists :supersede)
    (maphash (lambda (key value)
               (format stream "~A, ~A, ~A~%" key (car value) (cdr value)))
             *ocicl-systems*)))

(defun download-and-install (name)
  (let ((dl-dir (get-temp-ocicl-dl-pathname)))
    (unwind-protect
         (progn
           (uiop:ensure-all-directories-exist (list dl-dir))
           (uiop:with-current-directory (dl-dir)
             (handler-case
                 (progn
                   (debug-log (format nil "ocicl-oras pull ~A" name))
                   (let ((output (uiop:run-program (format nil "ocicl-oras pull ~A" name) :output '(:string))))
                     (let ((fpath (car (uiop:directory-files dl-dir))))
                       (gunzip fpath "package.tar")
                       (uiop:with-current-directory (*systems-dir*)
                         (unpack-tarball (merge-pathnames dl-dir "package.tar"))))
                     output))
               (uiop/run-program:subprocess-error (e)
                 (debug-log e)
                 nil))))
      (uiop:delete-directory-tree dl-dir :validate t))))

(defun download-system (system)
  (let* ((slist (split-on-delimeter system #\:))
         (name (car slist))
         (version (or (cadr slist) "latest")))
    (let ((dl-dir (get-temp-ocicl-dl-pathname)))
      (unwind-protect
           (progn
             (uiop:ensure-all-directories-exist (list dl-dir))
             (uiop:with-current-directory (dl-dir)
               (loop for registry in *ocicl-registries*
                     do (handler-case
                            (progn
                              (debug-log (format nil "ocicl-oras pull ~A/~A:~A" registry (mangle name) version))
                              (let ((sha256
                                      (format nil "~A/~A@sha256:~A" registry (mangle name)
                                              (extract-sha256
                                               (uiop:run-program (format nil "ocicl-oras pull ~A/~A:~A" registry (mangle name) version) :output '(:string))))))
                                (format t "; downloaded ~A~%" sha256)
                                (let ((fpath (car (uiop:directory-files dl-dir))))
                                  (gunzip fpath "package.tar")
                                  (let ((dirname (car (contents "package.tar"))))
                                    (uiop:with-current-directory (*systems-dir*)
                                      (unpack-tarball (merge-pathnames dl-dir "package.tar"))
                                      (dolist (s (find-asd-files (merge-pathnames dirname *systems-dir*)))
                                        (setf (gethash (pathname-name s) *ocicl-systems*) (cons sha256 (subseq (namestring s) (length (namestring *systems-dir*))))))
                                      (return))))))
                          (uiop/run-program:subprocess-error (e)
                            (debug-log e)
                            nil)))))
        (uiop:delete-directory-tree dl-dir :validate t)))
    (write-systems-csv)
    (gethash name *ocicl-systems*)))

(defun find-asdf-system-file (name)
  (pathname
   (concatenate 'string
                (namestring *systems-dir*)
                (or (cdr (gethash name *ocicl-systems*))
                    (handler-case
                        (cdr (download-system name))
                      (uiop/run-program:subprocess-error (e)
                        (declare (ignore e))
                        nil))))))

(defun system-definition-searcher (name)
  (unless (or (string= name "asdf") (string= name "uiop"))
    (let ((system-file (find-asdf-system-file name)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))
