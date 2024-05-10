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

(require 'asdf)

(defpackage #:ocicl-runtime
  (:use #:cl)
  (:export #:*download* #:*verbose* #:+version+))

(in-package #:ocicl-runtime)

(defvar *download* t)
(defvar *verbose* nil)

(defvar *ocicl-systems* nil)
(defvar *systems-dir* nil)
(defvar *systems-csv* nil)
(defvar *systems-csv-timestamp* 0)

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

(defun read-systems-csv ()
  (when *verbose*
    (format t "; loading ~A~%" *systems-csv*))
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (line (uiop:read-file-lines *systems-csv*))
      (let ((vlist (split-csv-line line)))
        (setf (gethash (car vlist) ht) (cons (cadr vlist) (caddr vlist)))))
    ht))

(defun ocicl-install (name)
  (let ((cmd (format nil "ocicl ~A install ~A" (if *verbose* "-v" "") name)))
    (when *verbose* (format t "; running: ~A~%" cmd))
    (let ((output (uiop:run-program cmd
                                    :output (if *verbose* *standard-output*
                                                '(:string))
                                    :error-output *error-output*)))
      (setq *systems-csv-timestamp* 0))))

(defun find-asdf-system-file (name download-p)
  (unless *systems-dir*
    (let ((cwd (uiop:getcwd)))
      (setq *systems-dir* (merge-pathnames (make-pathname :directory '(:relative "systems"))
                                           cwd))
      (setq *systems-csv* (merge-pathnames cwd "systems.csv"))))

  (when (probe-file *systems-csv*)
    (let ((timestamp (file-write-date *systems-csv*)))
      (when (> timestamp *systems-csv-timestamp*)
        (setq *ocicl-systems* (read-systems-csv))
        (setq *systems-csv-timestamp* timestamp))))

  (let ((match (and *ocicl-systems* (gethash (mangle name) *ocicl-systems*))))
    (if match
        (let ((pn (pathname (concatenate 'string (namestring *systems-dir*) (cdr match)))))
          (when *verbose*
            (format t "; checking for ~A: " pn))
          (let ((found (probe-file pn)))
            (if found
                (progn
                  (when *verbose* (format t "found~%"))
                  pn)
              (progn
                (when *verbose* (format t "missing~%"))
                (when download-p
                  (ocicl-install (car match)))))))
        (when download-p
          (ocicl-install name)
          (find-asdf-system-file name nil)))))

(defun system-definition-searcher (name)
  (unless (or (string= "asdf" name) (string= "uiop" name))
    (let* ((*verbose* (or *verbose* (and asdf:*verbose-out* t)))
           (system-file (find-asdf-system-file name *download*)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))

(pushnew :OCICL *features*)
