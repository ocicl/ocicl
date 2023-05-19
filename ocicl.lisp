;;; ocicl.lisp
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

(defvar *verbose* nil)

(defparameter +runtime+ (uiop:read-file-string "runtime/ocicl-runtime.lisp"))

(define-opts
  (:oname :verbose
   :description "produce verbose output"
   :short #\v
   :long "verbose"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (option condition))
  (invoke-restart 'skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun usage ()
  (usage-describe
   :prefix "ocicl 1.0.0 - copyright (C) 2023 Anthony Green <green@moxielogic.com>"
   :suffix "Choose from the following ocicl commands:

   help                                Print this help text
   install [SYSTEM[:VERSION]]...       Install systems
   latest [SYSTEM]...                  Install latest version of systems
   list SYSTEM...                      List available system versions
   setup                               Mandatory ocicl configuration
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
      (handler-case
          (progn
            (format t "~A:~%" system)
            (dolist (s (reverse (cdr (sort (split-lines (uiop:run-program (format nil "ocicl-oras repo tags ghcr.io/ocicl/~A" (mangle system)) :output '(:string))) #'string-lessp))))
              (format t "~T~A~%" s)))
        (uiop/run-program:subprocess-error (e)
          (declare (ignore e))
          (format t "~T~A not found~%" system)))
      (format t "~%"))))

(defun get-ocicl-dir ()
  (let ((ocicl-dir (merge-pathnames (make-pathname :directory '(:relative "ocicl"))
                                     (uiop:xdg-data-home))))
    (uiop:ensure-all-directories-exist (list ocicl-dir))
    ocicl-dir))

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
            (asdf:load-system system)))
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
  (format t "ocicl version:   1.0.0~%")
  (format t "Lisp runtime:    ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (format t "ASDF version:    ~A~%" (asdf:asdf-version)))

(defun do-setup (args)
  (declare (ignore args))
  (let* ((odir (get-ocicl-dir))
         (runtime-source (merge-pathnames odir "ocicl-runtime.lisp")))
    (with-open-file (stream runtime-source
                            :direction :output
                            :if-exists :supersede)
      (write-string +runtime+ stream)
      (format t "; Add the following to your ${HOME}/.sbclrc file:~%~%#-ocicl~%(when (probe-file ~S)~%  (load ~S))~%~%" runtime-source runtime-source))))

(defun do-install (args)
  ;; Make sure the systems directory exists
  (uiop:ensure-all-directories-exist
   (list *systems-dir*))
  (if args
      ;; Download the systems provided on the command line.
      (dolist (system args)
        (if (position #\@ system)
            (progn
              (format t "; downloading ~A~%" system)
              (unless (download-and-install system)
                (progn
                  (format uiop:*stderr* "Error: can't download ~A.~%" system)
                  (sb-ext:quit))))
            (let* ((slist (split-on-delimeter system #\:))
                   (name (car slist)))
              (format t "; downloading ~A~%" system)
              (if (download-system system)
                  (asdf:load-system name)
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

  (setf *random-state* (make-random-state t))
  (setq *ocicl-systems* (read-systems-csv))
  (setq *systems-dir* (merge-pathnames (make-pathname :directory '(:relative "systems"))
                                       (uiop:getcwd)))

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
            (t (usage)))))))

(defun replace-plus-with-string (str)
  (with-output-to-string (s)
    (loop for c across str do
      (if (char= c #\+)
          (write-string "_plus_" s)
          (write-char c s)))))

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
                   (let ((output (uiop:run-program (format nil "ocicl-oras pull ~A" name :output '(:string)))))
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
               (handler-case
                   (progn
                     (debug-log (format nil "ocicl-oras pull ghcr.io/ocicl/~A:~A" (mangle name) version))
                     (let ((sha256
                             (format nil "ghcr.io/ocicl/~A@sha256:~A" (mangle name)
                                     (extract-sha256
                                      (uiop:run-program (format nil "ocicl-oras pull ghcr.io/ocicl/~A:~A" (mangle name) version) :output '(:string))))))
                       (format t "; downloaded ~A~%" sha256)
                       (let ((fpath (car (uiop:directory-files dl-dir))))
                         (gunzip fpath "package.tar")
                         (let ((dirname (car (contents "package.tar"))))
                           (uiop:with-current-directory (*systems-dir*)
                             (unpack-tarball (merge-pathnames dl-dir "package.tar"))
                             (dolist (s (find-asd-files (merge-pathnames dirname *systems-dir*)))
                               (setf (gethash (pathname-name s) *ocicl-systems*) (cons sha256 (subseq (namestring s) (length (namestring *systems-dir*)))))))))))
                 (uiop/run-program:subprocess-error (e)
                   (debug-log e)
                   nil))))
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
