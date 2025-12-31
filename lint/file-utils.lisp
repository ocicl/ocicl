;;;; file-utils.lisp
;;;;
;;;; File handling utilities and .asd parsing
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Copyright (C) 2025 Anthony Green

(in-package #:ocicl.lint)

(defun %ensure-package (name)
  "Ensure package NAME exists, creating it if necessary."
  (unless (find-package name)
    (make-package name :use nil)))

(defun %plist-value (plist key)
  "Extract value for KEY from property list PLIST."
  (loop for (k . rest) on plist by #'cddr
        when (eql k key) do (return (first rest))
        finally (return nil)))

(defun %as-list (v)
  "Convert various forms to lists, handling (list ...) forms specially."
  (cond
    ((null v) nil)
    ((and (consp v) (eq (first v) 'list)) (rest v))
    ((consp v) v)
    (t nil)))

(defun %collect-components (components base-dir current-subdir)
  "Recursively collect file paths from ASDF component specifications."
  (let ((result nil))
    (dolist (comp components)
      (when (consp comp)
        (let* ((kind (first comp))
               (plist (rest comp)))
          (cond
            ((eql kind :file)
             (let* ((real-plist (if (and plist (not (keywordp (first plist))))
                                    (rest plist)  ; Skip the name if it's the first element
                                    plist))
                    (name (%plist-value real-plist :name))
                    (name (or name (first plist)))
                    (s (princ-to-string name))
                    (last-slash (position #\/ s :from-end t))
                    (basename (subseq s (if last-slash (1+ last-slash) 0)))
                    (s2 (if (search "." basename) s (concatenate 'string s ".lisp")))
                    (base (uiop:merge-pathnames*
                            (or current-subdir (make-pathname :directory '(:relative)))
                            base-dir))
                    (abs (uiop:merge-pathnames* s2 base)))
               (logf ";   component :file ~A -> ~A (exists=~A)~%" name abs (uiop:file-exists-p abs))
               (push abs result)))
            ((eql kind :module)
             (let* ((real-plist (if (and plist (not (keywordp (first plist))))
                                    (rest plist)  ; Skip the name if it's the first element
                                    plist))
                    (name (%plist-value real-plist :name))
                    (name (or name (first plist)))  ; Fall back to first element if no :name key
                    (pathname-opt (%plist-value real-plist :pathname))
                    (subdir (cond
                              (pathname-opt
                                (uiop:parse-unix-namestring (princ-to-string pathname-opt)))
                              (name (uiop:parse-unix-namestring (format nil "~A/" name)))
                              (t (make-pathname :directory '(:relative)))))
                    (next-sub (merge-pathnames subdir
                                               (or current-subdir
                                                   (make-pathname :directory '(:relative)))))
                    (subcomps (%plist-value real-plist :components)))
               (logf ";   component :module ~A (pathname ~A) subcomps=~S~%"
                     name (or pathname-opt "<none>") subcomps)
               (when subcomps
                 (logf ";     Processing ~D subcomponents~%" (length (%as-list subcomps)))
                 (setf result (nconc result
                                     (%collect-components (%as-list subcomps)
                                                          base-dir next-sub))))))
            (t
             (when-let ((subcomps (%plist-value plist :components)))
               (setf result (nconc result
                                   (%collect-components (%as-list subcomps)
                                                        base-dir current-subdir)))))))))
    result))

(defun collect-asd-component-files (asd-path)
  "Return a list of files referenced by ASDF defsystem components in ASD-PATH."
  (handler-case
      (let* (;; Ensure ASDF package exists so ASDF:DEFSYSTEM can be read BEFORE parsing.
             (_ (%ensure-package "ASDF"))
             (content (uiop:read-file-string asd-path))
             (base-dir (uiop:pathname-directory-pathname asd-path))
             (files nil)
             (ctx (make-lint-context-from-string content)))
        (declare (ignore _))
        ;; Get top-level forms using lint-context
        (let ((top-forms (lint-top-level-forms ctx)))
          (logf "; Reading ASD ~A: ~D toplevel form(s)~%" asd-path (length top-forms))
          (when *verbose*
            (dolist (z top-forms)
              (when-let ((form (zip-form z)))
                (when (consp form)
                  (logf ";  form head: ~A~%" (first form))))))
          (labels ((defsystem-p (f)
                     (and (consp f)
                          (let ((h (first f)))
                            (or (eq h 'asdf:defsystem)
                                (and (symbolp h) (string-equal (symbol-name h) "DEFSYSTEM"))))))
                   (walk (f)
                     (cond
                       ((defsystem-p f)
                        (let* ((plist (cddr f))
                               (comps (%plist-value plist :components)))
                          (when comps
                            (setf files (nconc files
                                               (%collect-components (%as-list comps)
                                                                    base-dir nil))))))
                       ((consp f)
                        (walk (first f))
                        (when (listp (rest f))
                          (mapc #'walk (rest f))))
                       (t nil))))
            (dolist (z top-forms)
              (when-let ((form (zip-form z)))
                (walk form)))
            (let* ((unique-files (remove-duplicates files :test #'equal))
                   (resolved (remove-if-not #'uiop:file-exists-p unique-files)))
              (when (and *verbose* (> (length unique-files) (length resolved)))
                (logf ";   Warning: ~D component file(s) not found~%"
                      (- (length unique-files) (length resolved)))
                (dolist (f unique-files)
                  (unless (uiop:file-exists-p f)
                    (logf ";     Missing: ~A~%" f))))
              (logf "; ASD ~A -> ~D component file(s)~%" asd-path (length resolved))
              (dolist (f resolved) (logf ";  - ~A~%" f))
              resolved))))
    (error (e99)
      (declare (ignore e99))
      nil)))

(defun pathname-lisp-file-p (p)
  "Check if pathname P refers to a Lisp source file based on extension."
  (let ((type (string-downcase (or (pathname-type p) ""))))
    (member type '("lisp" "lsp" "cl" "asd") :test #'string=)))

(defun collect-paths (inputs)
  "Expand a list of files/directories into a list of Lisp source pathnames.
Currently scans directories non-recursively."
  (loop for input in inputs append
        (let ((abs-input (uiop:ensure-absolute-pathname input *default-pathname-defaults*)))
          (cond
            ((uiop:directory-exists-p abs-input)
             (remove-if-not #'pathname-lisp-file-p (uiop:directory-files abs-input)))
            ((uiop:file-exists-p abs-input)
             (let ((p (uiop:ensure-pathname abs-input)))
               (if (pathname-lisp-file-p p)
                   (let ((type (string-downcase (or (pathname-type p) ""))))
                     (if (string= type "asd")
                         (remove-duplicates (cons p (collect-asd-component-files p)) :test #'equal)
                         (list p)))
                   nil)))
            (t
             (progn
               (format *error-output* "Warning: path not found: ~A~%" input)
               nil))))))

(defun read-file-lines (path)
  "Read all lines from file at PATH and return as a list."
  (with-open-file (in path :direction :input :external-format :utf-8)
    (loop with lines = nil
          for line = (read-line in nil :eof)
          until (eql line :eof)
          do (push line lines)
          finally (return (nreverse lines)))))

(defun build-line-index (content)
  "Return a vector of starting indices for each line in CONTENT."
  (let ((starts (list 0))
        (len (length content)))
    (loop for i from 0 below len
          when (char= (char content i) #\Newline)
            do (push (1+ i) starts))
    (coerce (nreverse starts) 'vector)))

(defun index->line/col (idx line-index)
  "Given an index into a string and a vector of line starts, return 1-based line and column."
  (let* ((n (length line-index))
         (line (loop for i from 0 below n
                     for start = (aref line-index i)
                     for next = (if (< (1+ i) n) (aref line-index (1+ i)) most-positive-fixnum)
                     when (and (<= start idx) (< idx next))
                       do (return (1+ i))
                     finally (return n)))
         (col (let ((start (aref line-index (1- line))))
                (1+ (- idx start)))))
    (values line col)))

