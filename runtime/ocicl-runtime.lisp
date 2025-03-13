;;; ocicl-runtime.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024  Anthony Green <green@moxielogic.com>
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
        ((probe-file asdf-fasl)
         (load asdf-fasl :verbose nil))
        ((probe-file asdf-file)
         (load (compile-file asdf-file :verbose nil :output-file asdf-fasl) :verbose nil))))
    (unless (asdf:version-satisfies (asdf:asdf-version) "3.3.5")
      (warn "OCICL: ASDF version not satisfied. Found v~A but v3.3.5 is required." (asdf:asdf-version)))))

;; Temporary fix for a mysterious problem
#+clasp
(when (find-package :sb-bsd-sockets)
  (asdf:register-immutable-system :sb-bsd-sockets))

(defpackage #:ocicl-runtime
  (:use #:cl)
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
  (replace-plus-with-string (car (split-on-delimeter str #\/))))

(defun split-csv-line (line)
  (split-on-delimeter line #\,))

(defun should-log ()
  "Whether or not OCICL should output useful log info to *VERBOSE*."
  (and *verbose* (or (eq t *verbose*) (output-stream-p *verbose*))))

(defun read-systems-csv (systems-csv)
  (when (should-log)
    (format *verbose* "; loading ~A~%" systems-csv))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (line (uiop:read-file-lines systems-csv))
      (let ((vlist (split-csv-line line)))
        (setf (gethash (car vlist) ht) (cons (cadr vlist) (caddr vlist)))))
    ht))

(defun check-if-program-exists (program-name)
  "Check if PROGRAM-NAME exists and is executable."
  (multiple-value-bind (out error exit-code)
      (uiop:run-program program-name :force-shell nil :ignore-error-status t)
    (declare (ignore out error))
    (not (or (= exit-code 127) (= exit-code 126)))))

(defun warn-if-program-doesnt-exist (program-name)
  "If VERBOSE, print a warning if PROGRAM-NAME doesn't exist or isn't executable."
  (when (and (should-log) (not (check-if-program-exists program-name)))
    (format *verbose* "~&; ***************************************************************~%")
    (format *verbose* "; WARNING: `~A` could not be found!~%" program-name)
    (format *verbose* "; ***************************************************************~%")
    (terpri)))

(defun warn-if-missing-required-programs ()
  "Invoke WARN-IF-PROGRAM-DOESNT-EXIST on +REQUIRED-PROGRAMS+."
  (dolist (program +required-programs+)
    (warn-if-program-doesnt-exist program)))

(defun check-ocicl-version ()
  (warn-if-missing-required-programs)
  (let ((ocicl-version-output (uiop:run-program "ocicl version"
                                                :output '(:string)
                                                :error-output *error-output*))
        (ocicl-version-string (format nil "ocicl version:   ~A~%"  ocicl-runtime:+version+)))
    (unless (string= ocicl-version-string (subseq ocicl-version-output 0 (length ocicl-version-string)))
      (format t "~&; ***************************************************************~%")
      (format t "; WARNING: Your ocicl binary and ocicl-runtime are out of sync.~%")
      (format t ";          Run `ocicl setup` and restart.~%")
      (format t "; ***************************************************************~%")
      (terpri))))

(defun ocicl-install (name)
  (check-ocicl-version)
  (let ((cmd (format nil "ocicl ~A ~A install ~A"
                     (if *verbose* "-v" "")
                     (if *force-global* "--global" "")
                     name)))
    (warn-if-missing-required-programs)
    (when (should-log)
      (format *verbose* "; running: ~A~%" cmd))
    (uiop:run-program cmd
                      :output (or *verbose* '(:string))
                      :error-output *error-output*))
  (setq *local-systems-csv-timestamp* 0))

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
        for existing-csv = (or (probe-file ocicl-csv) (probe-file systems-csv))
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
  (unless *local-systems-dir*
    (let ((workdir (find-workdir (uiop:getcwd))))
      (setq *local-systems-dir* (merge-pathnames *relative-systems-dir* workdir))
      (setq *local-systems-csv* (merge-pathnames *systems-csv* workdir))))

  (unless *global-systems-dir*
    (let* ((config-file (merge-pathnames "ocicl-globaldir.cfg" (get-ocicl-dir)))
           (globaldir (if (probe-file config-file)
                          (uiop:ensure-absolute-pathname (uiop:read-file-line config-file))
                          (get-ocicl-dir))))

      (setq *global-systems-dir* (merge-pathnames *relative-systems-dir* globaldir))
      (setq *global-systems-csv* (merge-pathnames *systems-csv* globaldir))))

  (when (probe-file *local-systems-csv*)
    (let ((timestamp (file-write-date *local-systems-csv*)))
      (when (> timestamp *local-systems-csv-timestamp*)
        (setq *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
        (setq *local-systems-csv-timestamp* timestamp))))

  (when (probe-file *global-systems-csv*)
    (let ((timestamp (file-write-date *global-systems-csv*)))
      (when (> timestamp *global-systems-csv-timestamp*)
        (setq *global-ocicl-systems* (read-systems-csv *global-systems-csv*))
        (setq *global-systems-csv-timestamp* timestamp)))))

(defun find-asdf-system-file (name download-p)
  (initialize-globals)
  (labels ((try-load (systems systems-dir)
             (let ((match (and systems (gethash (mangle name) systems))))
               (if match
                   (let ((pn (pathname (concatenate 'string (namestring systems-dir) (cdr match)))))
                     (when (should-log)
                       (format *verbose* "; checking for ~A: " pn))
                     (let ((found (probe-file pn)))
                       (if found
                           (progn
                             (when (should-log) (format *verbose* "found~%"))
                             pn)
                           (when (should-log) (format *verbose* "missing~%")))))))))
    (or (try-load *local-ocicl-systems* *local-systems-dir*)
        (try-load *global-ocicl-systems* *global-systems-dir*)
        (when download-p
          (ocicl-install name)
          (setq *local-ocicl-systems* (read-systems-csv *local-systems-csv*))
          (find-asdf-system-file name nil)))))

(defun starts-with? (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun system-definition-searcher (name)
  (unless (or (starts-with? "asdf/" name) (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* asdf:*verbose-out*))
           (system-file (find-asdf-system-file name *download*)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))

(defun system-list ()
  (initialize-globals)
  (append (loop for key being the hash-keys of *local-ocicl-systems*
                collect key)
          (when *global-ocicl-systems*
            (loop for key being the hash-keys of *global-ocicl-systems*
                  collect key))))

(pushnew :OCICL *features*)
