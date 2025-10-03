;;; ocicl-runtime.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; Check if the ASDF version is satisfied. Compile/load the bundled ASDF version if not.
(require 'asdf)

;; Except that clasp and ECL need their own ASDFs for now.
#-(or clasp ecl)
(unless (asdf:version-satisfies (asdf:asdf-version) "3.3.5")
  (let* ((asdf-file (merge-pathnames "asdf.lisp" *load-truename*))
         (orig-asdf-fasl (compile-file-pathname asdf-file))
         (asdf-fasl (make-pathname :defaults orig-asdf-fasl
                                   :name (format nil "asdf-~A-~A-~A"
                                                 (lisp-implementation-type)
                                                 (lisp-implementation-version)
                                                 (machine-type)))))
    (handler-bind ((warning #'muffle-warning))
      (cond
        ((uiop:file-exists-p asdf-fasl)
         (load asdf-fasl :verbose nil))
        ((uiop:file-exists-p asdf-file)
         (load (compile-file asdf-file :verbose nil :output-file asdf-fasl) :verbose nil))
        (t nil)))
    (unless (asdf:version-satisfies (asdf:asdf-version) "3.3.5")
      (warn "OCICL: ASDF version not satisfied. Found v~A but v3.3.5 is required." (asdf:asdf-version)))))

;; Temporary fix for a mysterious problem
#+clasp
(when (find-package :sb-bsd-sockets)
  (asdf:register-immutable-system :sb-bsd-sockets))

(defpackage #:ocicl-runtime
  (:use #:cl)
  (:documentation "Runtime support for ocicl system discovery and installation")
  (:export #:*download* #:*verbose* #:*force-global* #:+version+ #:system-list))

(in-package #:ocicl-runtime)

(defvar *download* t)
(defvar *verbose* nil)
(defvar *force-global* nil)
(defconstant +version+ "UNKNOWN")
(defconstant +required-programs+ (list "ocicl"))

(defvar *systems-csv* "ocicl.csv")
(defvar *relative-systems-dir* (make-pathname :directory '(:relative "ocicl")))

(defvar *local-ocicl-systems* nil)
(defvar *local-systems-dir* nil)
(defvar *local-systems-csv* nil)
(defvar *local-systems-csv-timestamp* 0)

(defvar *global-ocicl-systems* nil)
(defvar *global-systems-dir* nil)
(defvar *global-systems-csv* nil)
(defvar *global-systems-csv-timestamp* 0)

(defun split-on-delimiter (line delim)
  "Split LINE on DELIM character, returning list of trimmed substrings."
  (let ((start 0)
        (end 0)
        (result nil))
    (loop for c across line
          for i from 0
          do (when (char= c delim)
               (setf end i)
               (push (string-trim " " (subseq line start end)) result)
               (setf start (1+ i)))
          finally (push (string-trim " " (subseq line start)) result))
    (nreverse result)))

(defun replace-plus-with-string (str)
  "Replace + characters with _plus_ and strip trailing underscore if present."
  (let ((mangled (with-output-to-string (s)
                    (loop for c across str do
                          (if (char= c #\+)
                              (write-string "_plus_" s)
                              (write-char c s))))))
    (if (char= (char mangled (1- (length mangled))) #\_)
        (subseq mangled 0 (1- (length mangled)))
      mangled)))

(defun mangle (str)
  "Mangle system name STR for filesystem use (handle + and / characters)."
  (replace-plus-with-string (first (split-on-delimiter str #\/))))

(defun split-csv-line (line)
  "Split a CSV line on commas."
  (split-on-delimiter line #\,))

(defun should-log ()
  "Whether or not OCICL should output useful log info to *VERBOSE*."
  (and *verbose* (or (eq t *verbose*) (output-stream-p *verbose*))))

(defun read-systems-csv (systems-csv)
  "Read SYSTEMS-CSV file and return hash table mapping system names to (version . path)."
  (when (should-log)
    (format *verbose* "; loading ~A~%" systems-csv))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (line (uiop:read-file-lines systems-csv))
      (let ((vlist (split-csv-line line)))
        (setf (gethash (first vlist) ht) (cons (cadr vlist) (caddr vlist)))))
    ht))

(defun program-exists-p (program-name)
  "Check if PROGRAM-NAME exists and is executable."
  (handler-case
      (multiple-value-bind (_ err code)
          (uiop:run-program (list program-name) :ignore-error-status t)
        (declare (ignore _ err))
        (not (member code '(126 127))))
    (error ()
      ;; If we can't run the program at all, it doesn't exist
      nil)))

(defun warn-if-program-doesnt-exist (program-name)
  "If VERBOSE, print a warning if PROGRAM-NAME doesn't exist or isn't executable."
  (when (and (should-log) (not (program-exists-p program-name)))
    (format *verbose* "~&; ***************************************************************~%")
    (format *verbose* "; WARNING: `~A` could not be found!~%" program-name)
    (format *verbose* "; ***************************************************************~%")
    (terpri)))

(defun warn-if-missing-required-programs ()
  "Invoke WARN-IF-PROGRAM-DOESNT-EXIST on +REQUIRED-PROGRAMS+."
  (mapc #'warn-if-program-doesnt-exist +required-programs+))

(defun verify-ocicl-version ()
  "Verify ocicl binary version matches runtime version and warn if out of sync."
  (warn-if-missing-required-programs)
  (handler-case
      (let ((ocicl-version-output (uiop:run-program '("ocicl" "version")
                                                    :output '(:string)
                                                    :error-output *error-output*))
            (ocicl-version-string (format nil "ocicl version:   ~A~%"  ocicl-runtime:+version+)))
        (unless (string= ocicl-version-string (subseq ocicl-version-output 0 (length ocicl-version-string)))
          (format t "~&; ***************************************************************~%")
          (format t "; WARNING: Your ocicl binary and ocicl-runtime are out of sync.~%")
          (format t ";          Run `ocicl setup` and restart.~%")
          (format t "; ***************************************************************~%")
          (terpri)))
    (error (e)
      (when (should-log)
        (format *verbose* "; Error checking ocicl version: ~A~%" e)))))

(defun sanitize-system-name (name)
  "Sanitize system name to prevent command injection."
  (let ((name-str (princ-to-string name)))
    ;; Only allow alphanumeric, dash, underscore, dot, plus, and slash
    (if (every (lambda (c) (or (alphanumericp c) (find c "-_.+/"))) name-str)
        name-str
        (error "Invalid system name: ~A" name-str))))

(defun ocicl-install (name)
  "Install system NAME using the ocicl command."
  (verify-ocicl-version)
  (let* ((safe-name (sanitize-system-name name))
         (cmd `("ocicl" ,@(when *verbose* '("-v"))
                        ,@(when *force-global* '("--global"))
                        "install"
                        ,safe-name)))
    (warn-if-missing-required-programs)
    (when (should-log)
      (format *verbose* "; running: ~A~%" cmd))
    (handler-case
        (uiop:run-program cmd
                          :output (or *verbose* '(:string))
                          :error-output *error-output*)
      (error (e)
        (when (should-log)
          (format *verbose* "; Error installing ~A: ~A~%" safe-name e))
        (error e))))
  (setf *local-systems-csv-timestamp* 0))

(defun get-ocicl-dir ()
  "Find the ocicl directory."
  (let ((ocicl-dir (merge-pathnames (make-pathname :directory '(:relative "ocicl"))
                                     (uiop:xdg-data-home))))
    (uiop:ensure-all-directories-exist (list ocicl-dir))
    ocicl-dir))

(defmethod parent ((file pathname))
  "Return the parent directory of FILE."
  (if (uiop:directory-pathname-p file)
      (uiop:pathname-parent-directory-pathname file)
      (uiop:pathname-directory-pathname file)))

(defun find-workdir (workdir)
  "Search for ocicl.csv or systems.csv starting from WORKDIR and moving up the directory chain.
   Returns the directory containing ocicl.csv or systems.csv if found.  If none is
   found, return WORKDIR."
  (loop for dir = (truename workdir) :then parent-dir
        for parent-dir = (parent dir)
        for systems-csv = (merge-pathnames (make-pathname :name "systems" :type "csv") dir)
        for ocicl-csv = (merge-pathnames (make-pathname :name "ocicl" :type "csv") dir)
        ;; Need pathname return value, not just boolean
        for existing-csv = (or (probe-file ocicl-csv) (probe-file systems-csv)) ; lint:suppress use-uiop-file-exists-p
        until (or existing-csv
                  (null parent-dir)
                  (equal parent-dir dir))
        finally (return
                  (uiop:ensure-directory-pathname
                   (cond (existing-csv
                          (setf *systems-csv* existing-csv
                                *relative-systems-dir* (make-pathname
                                                        :directory
                                                        `(:relative ,(pathname-name existing-csv))))
                          dir)
                         (t workdir))))))

(defun initialize-globals ()
  "Initialize global variables for local and global system directories and CSV files."
  (unless *local-systems-dir*
    (let ((workdir (find-workdir (uiop:getcwd))))
      (setf *local-systems-dir* (merge-pathnames *relative-systems-dir* workdir))
      (setf *local-systems-csv* (merge-pathnames *systems-csv* workdir))))

  (unless *global-systems-dir*
    (let* ((config-file (merge-pathnames "ocicl-globaldir.cfg" (get-ocicl-dir)))
           (globaldir (if (uiop:file-exists-p config-file)
                          (handler-case
                              (uiop:ensure-absolute-pathname (uiop:read-file-line config-file))
                            (error (e)
                              (when (should-log)
                                (format *verbose* "; Error reading config file ~A: ~A~%" config-file e))
                              (get-ocicl-dir)))
                          (get-ocicl-dir))))

      (setf *global-systems-dir* (merge-pathnames *relative-systems-dir* globaldir))
      (setf *global-systems-csv* (merge-pathnames *systems-csv* globaldir))))

  (when (uiop:file-exists-p *local-systems-csv*)
    (let ((timestamp (file-write-date *local-systems-csv*)))
      (when (> timestamp *local-systems-csv-timestamp*)
        (handler-case
            (progn
              (setf *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
              (setf *local-systems-csv-timestamp* timestamp))
          (error (e)
            (when (should-log)
              (format *verbose* "; Error reading local systems CSV ~A: ~A~%" *local-systems-csv* e)))))))

  (when (uiop:file-exists-p *global-systems-csv*)
    (let ((timestamp (file-write-date *global-systems-csv*)))
      (when (> timestamp *global-systems-csv-timestamp*)
        (handler-case
            (progn
              (setf *global-ocicl-systems* (read-systems-csv *global-systems-csv*))
              (setf *global-systems-csv-timestamp* timestamp))
          (error (e)
            (when (should-log)
              (format *verbose* "; Error reading global systems CSV ~A: ~A~%" *global-systems-csv* e))))))))

(defun find-asdf-system-file (name download-p)
  "Find ASDF system file for NAME, optionally downloading if DOWNLOAD-P is true."
  (initialize-globals)
  (labels ((try-load (systems systems-dir)
             (let ((match (and systems (gethash (mangle name) systems)))) ; lint:suppress
               (when match
                   (let ((pn (merge-pathnames (rest match) systems-dir)))
                     (when (should-log)
                       (format *verbose* "; checking for ~A: " pn))
                     (if (uiop:file-exists-p pn)
                         (progn
                           (when (should-log) (format *verbose* "found~%"))
                           pn)
                         (when (should-log) (format *verbose* "missing~%"))))))))
    (or (try-load *local-ocicl-systems* *local-systems-dir*)
        (try-load *global-ocicl-systems* *global-systems-dir*)
        (when download-p
          (ocicl-install name)
          (setf *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
          (find-asdf-system-file name nil)))))

(defun starts-with-p (prefix string)
  "Return true if STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun system-definition-searcher (name)
  "Search for ASDF system definition file for NAME, using ocicl if needed."
  (unless (or (starts-with-p "asdf/" name) (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* asdf:*verbose-out*))
           (system-file (find-asdf-system-file name *download*)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))

(defun system-list ()
  "Return list of all known system names from local and global registries."
  (initialize-globals)
  (append (when *local-ocicl-systems*
            (loop for key being the hash-keys of *local-ocicl-systems*
                  collect key))
          (when *global-ocicl-systems*
            (loop for key being the hash-keys of *global-ocicl-systems*
                  collect key))))

(pushnew :OCICL *features*)
