;;;; Copyright (C) 2023  Anthony Green <green@moxielogic.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(require 'asdf)
(require 'sb-posix)

(defpackage #:ocicl-runtime
  (:use #:cl)
  (:export #:*download* #:*verbose*))

(in-package #:ocicl-runtime)

(defvar *download* t)
(defvar *verbose* nil)

(defvar *ocicl-systems* nil)
(defvar *systems-dir* nil)
(defvar *systems-csv* nil)
(defvar *systems-csv-mtime* 0)

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

(defun read-systems-csv ()
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (line (uiop:read-file-lines *systems-csv*))
      (let ((vlist (split-csv-line line)))
        (setf (gethash (car vlist) ht) (cons (cadr vlist) (caddr vlist)))))
    ht))

(defun ocicl-install (name)
  (let* ((cmd (format nil "ocicl install ~A" name))
         (output (uiop:run-program cmd :output '(:string))))
    (setq *systems-csv-mtime* 0)
    (when *verbose*
      (format t "~A~%~A~%" cmd output))))

(defun find-asdf-system-file (name &optional (download? t))
  (unless *systems-dir*
    (let ((cwd (uiop:getcwd)))
      (setq *systems-dir* (merge-pathnames (make-pathname :directory '(:relative "systems"))
                                           cwd))
      (setq *systems-csv* (merge-pathnames cwd "systems.csv"))))

  (when (probe-file *systems-csv*)
    (let ((mtime (sb-posix:stat-mtime (sb-posix:stat *systems-csv*))))
      (when (> mtime *systems-csv-mtime*)
        (when *verbose*
          (format t "; old csv mtime = ~A~%; new csv mtime = ~A~%" *systems-csv-mtime* mtime))
        (setq *ocicl-systems* (read-systems-csv))
        (setq *systems-csv-mtime* mtime))))

  (let ((match (and *ocicl-systems* (gethash name *ocicl-systems*))))
    (if match
        (let ((pn (pathname (concatenate 'string (namestring *systems-dir*) (cdr match)))))
          (when (and (not (probe-file pn)) *download*)
            (ocicl-install (car match)))
          pn)
        (when (and *download* download?)
          (ocicl-install name)
          (find-asdf-system-file name nil)))))

(defun system-definition-searcher (name)
  (unless (or (string= "asdf" name) (string= "uiop" name))
    (let ((system-file (find-asdf-system-file name)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))

(pushnew :OCICL *features*)
