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

(defpackage #:ocicl-runtime
  (:use #:cl))

(in-package #:ocicl-runtime)

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

(defvar *ocicl-systems* nil)
(defvar *systems-dir* nil)

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

(defun find-asdf-system-file (name)
  (unless *systems-dir*
    (setq *systems-dir* (merge-pathnames (make-pathname :directory '(:relative "systems"))
                                         (uiop:getcwd))))
  (unless *ocicl-systems*
    (setq *ocicl-systems* (read-systems-csv)))
  (let ((match (gethash name *ocicl-systems*)))
    (if match
        (pathname
         (concatenate 'string (namestring *systems-dir*) (cdr match)))
        nil)))

(defun system-definition-searcher (name)
  (let ((system-file (find-asdf-system-file name)))
    (when (and system-file
               (string= (pathname-name system-file) name))
      system-file)))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))

(pushnew :OCICL *features*)
