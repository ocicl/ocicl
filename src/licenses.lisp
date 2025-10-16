;;; licenses.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

;;; Constants for pattern matching

(defparameter *license-name-patterns*
  '("(?i)^.*licen[cs]e.*$" "(?i)^copying.*$" "(?i)^copyright.*$")
  "Regex patterns for matching license file names.")

(defparameter *license-keywords*
  '("Copyright" "Licen[cs]e" "Permission" "provided 'as-is'" "Public Domain" "warranty")
  "Keywords that indicate license text.")

(defparameter *exclude-names*
  '("test" "example" "docs")
  "Directory and file names to exclude from searches.")

;;; Helper functions

(defun file-exists-p (pathname)
  "Return T if PATHNAME exists and is a regular file (not a directory)."
  (and (probe-file pathname) (not (uiop:directory-exists-p pathname))))

(defun get-directory-name (directory-pathname)
  "Extract the last component of a directory pathname."
  (car (last (pathname-directory directory-pathname))))

(defun matches-any-pattern-p (string patterns)
  "Return T if STRING matches any of the PATTERNS."
  (some (lambda (pattern) (ppcre:scan pattern string)) patterns))

(defun matches-license-name-p (name)
  "Return T if NAME matches any license file pattern."
  (and name (matches-any-pattern-p name *license-name-patterns*)))

(defun should-exclude-p (name)
  "Return T if NAME should be excluded (test/example/docs)."
  (some (lambda (exclude) (search exclude name :test #'char-equal))
        *exclude-names*))

(defun filter-files-by-type (files type)
  "Return files from FILES that have the given TYPE extension."
  (remove-if-not (lambda (f) (string-equal (pathname-type f) type)) files))

(defun find-files-matching (directory predicate &optional recurse-subdirs)
  "Find files in DIRECTORY matching PREDICATE. Optionally recurse into subdirectories."
  (let ((all-files (uiop:directory-files directory))
        (result (remove-if-not predicate (uiop:directory-files directory))))
    (when recurse-subdirs
      (dolist (subdir (remove-if (lambda (d) (should-exclude-p (get-directory-name d)))
                                 (uiop:subdirectories directory)))
        (setf result (append result (remove-if-not predicate (uiop:directory-files subdir))))))
    result))

(defun make-license-keyword-pattern (&key (comment-prefix ";;;?") (flags "i"))
  "Create a regex pattern for matching license keywords in comments."
  (format nil "(?~A)^~A.*(~{~A~^|~})"
          flags
          comment-prefix
          *license-keywords*))

;;; File reading

(defun read-file-with-fallback-encoding (pathname)
  "Read file content as string, trying UTF-8 first, then falling back to Latin-1.
   This handles LICENSE files that may be encoded in various character sets.
   Returns NIL if pathname is a directory."
  (when (file-exists-p pathname)
    (handler-case
        (alexandria:read-file-into-string pathname :external-format :utf-8)
      (#+sbcl sb-int:stream-decoding-error
       #+ccl ccl:stream-decoding-error
       #+ecl ext:stream-decoding-error
       #-(or sbcl ccl ecl) error ()
        ;; Fall back to Latin-1 which can decode any byte sequence
        (alexandria:read-file-into-string pathname :external-format :latin-1)))))

;;; Finding license files

(defun find-license-file (directory)
  "Find a license file in DIRECTORY. Returns pathname or NIL.
   If a LICENSES directory is found, looks for files inside it."
  (let* ((all-files (uiop:directory-files directory))
         (all-dirs (uiop:subdirectories directory))
         (files (remove-if-not (lambda (f) (matches-license-name-p (pathname-name f))) all-files))
         (dirs (remove-if-not (lambda (d) (matches-license-name-p (get-directory-name d))) all-dirs)))
    (or (car files)
        ;; If no license file found but there's a license directory, look inside
        (when dirs
          (car (uiop:directory-files (car dirs)))))))

(defun find-readme-file (directory)
  "Find a README file in DIRECTORY. Returns pathname or NIL."
  (car (remove-if-not
        (lambda (f)
          (let ((name (pathname-name f)))
            (and name (ppcre:scan "(?i)^readme.*$" name))))
        (uiop:directory-files directory))))

(defun find-primary-asd-file (directory)
  "Find primary .asd file in DIRECTORY (excluding test/example/docs files). Returns pathname or NIL."
  (let* ((dir-name (get-directory-name directory))
         ;; Strip version suffix like -20240503-abc123f from directory name
         (base-name (ppcre:regex-replace "-\\d{8}-[a-f0-9]{7}$" dir-name ""))
         ;; Find all .asd files in root and non-excluded subdirectories
         (asd-files (find-files-matching directory
                                         (lambda (f) (string-equal (pathname-type f) "asd"))
                                         t)))
    ;; First try to find .asd file matching base directory name (without version)
    (or (find-if (lambda (f) (string-equal (pathname-name f) base-name)) asd-files)
        ;; Also try exact directory name match in case there's no version suffix
        (find-if (lambda (f) (string-equal (pathname-name f) dir-name)) asd-files)
        ;; Otherwise exclude test/example/docs files and prefer shortest name
        (let ((filtered (remove-if (lambda (f) (should-exclude-p (pathname-name f))) asd-files)))
          (when filtered
            (first (sort (copy-list filtered) #'< :key (lambda (f) (length (pathname-name f))))))))))

;;; Extracting license text from various sources

(defun extract-license-from-readme (readme-file)
  "Extract license section from README-FILE. Returns string or NIL."
  (when (file-exists-p readme-file)
    (let ((content (read-file-with-fallback-encoding readme-file)))
      ;; Look for License section - match both markdown (#) and underline (---) style headers
      (multiple-value-bind (start end)
          (ppcre:scan "(?im)^#+?\\s*Licen[cs]e\\s*$|^Licen[cs]e\\s*$\\n[-=]+" content)
        (when start
          ;; Extract from License heading to next heading or end
          (let* ((after-heading (subseq content end))
                 ;; Match next markdown or underline-style heading
                 (next-section (or (ppcre:scan "(?im)^#+?\\s*\\w|^\\w+\\s*$\\n[-=]+" after-heading)
                                   (length after-heading))))
            (string-trim '(#\Space #\Tab #\Newline)
                         (subseq after-heading 0 next-section))))))))

(defun extract-license-block-from-lines (lines pattern &optional (start-offset 0) (max-lines 50))
  "Extract a block of license text from LINES starting at START-OFFSET.
   Looks for PATTERN to identify the start, then continues until a non-comment line.
   Returns string or NIL."
  (when (> (length lines) 3)
    (let* ((end-pos (min (+ start-offset max-lines) (length lines)))
           (scan-lines (subseq lines start-offset end-pos))
           (license-start nil)
           (license-end nil))
      ;; Find where license block starts
      (loop for i from 0 below (length scan-lines)
            for line = (nth i scan-lines)
            when (ppcre:scan pattern line)
              do (setf license-start i) (return))
      (when license-start
        ;; Find where license block ends (first non-comment, non-blank line after license start)
        (setf license-end
              (or (loop for i from (1+ license-start) below (length scan-lines)
                        for line = (nth i scan-lines)
                        unless (or (ppcre:scan "^;;;" line)
                                   (ppcre:scan "^\\s*$" line))
                          return i)
                  (length scan-lines)))
        (with-output-to-string (out)
          (loop for i from license-start below license-end
                for line = (nth i scan-lines)
                do (write-line line out)))))))

(defun extract-license-from-source-file-start (source-file)
  "Extract license from beginning of SOURCE-FILE (first ~50 lines). Returns string or NIL."
  (when (file-exists-p source-file)
    ;; First check for block comment #| ... |# at start of file
    (let ((content (read-file-with-fallback-encoding source-file)))
      (when (and content (ppcre:scan "^\\s*#\\|" content))
        (multiple-value-bind (start end)
            (ppcre:scan "(?s)^\\s*#\\|.*?(Copyright|Licen[cs]e|Permission).*?\\|#" content)
          (when start
            (return-from extract-license-from-source-file-start (subseq content start end))))))
    ;; Fall back to line comment detection
    (extract-license-block-from-lines
     (uiop:read-file-lines source-file)
     (make-license-keyword-pattern))))

(defun extract-license-from-source-file-end (source-file)
  "Extract license from end of SOURCE-FILE (last ~50 lines). Returns string or NIL."
  (when (file-exists-p source-file)
    (let* ((lines (uiop:read-file-lines source-file))
           (start-offset (max 0 (- (length lines) 50))))
      (when (> (length lines) 10)
        (let* ((end-lines (subseq lines start-offset))
               (pattern (make-license-keyword-pattern))
               (license-start nil))
          ;; Find where license block starts
          (loop for i from 0 below (length end-lines)
                for line = (nth i end-lines)
                when (ppcre:scan pattern line)
                  do (setf license-start i) (return))
          (when license-start
            (with-output-to-string (out)
              (loop for i from license-start below (length end-lines)
                    for line = (nth i end-lines)
                    do (write-line line out)))))))))

(defun extract-license-from-asd-comments (asd-file)
  "Extract license text from comment header of ASD-FILE. Returns string or NIL."
  (when (file-exists-p asd-file)
    (with-output-to-string (out)
      (with-open-file (in asd-file :direction :input)
        (let ((found nil)
              (blank-count 0)
              (pattern (make-license-keyword-pattern :comment-prefix ";;;")))
          (loop for line = (read-line in nil nil)
                while line
                do (cond
                     ;; Start capturing when we see license-related keywords
                     ((and (not found) (ppcre:scan pattern line))
                      (setf found t blank-count 0)
                      (write-line line out))
                     ;; Continue capturing comment lines
                     ((and found (ppcre:scan "^;;;" line))
                      (setf blank-count 0)
                      (write-line line out))
                     ;; Count blank lines
                     ((and found (string= (string-trim '(#\Space #\Tab) line) ""))
                      (incf blank-count)
                      (when (>= blank-count 3)
                        (return)))
                     ;; Stop if we hit non-comment, non-blank line
                     (found
                      (return)))))))))

(defun extract-license-field-from-asd (asd-file)
  "Extract :license field from ASDF system definition in ASD-FILE. Returns string or NIL."
  (when (file-exists-p asd-file)
    ;; Simple text-based extraction since reading forms requires ASDF package
    (handler-case
        (with-open-file (in asd-file :direction :input)
          (loop for line = (read-line in nil nil)
                while line
                when (ppcre:scan "^\\s*:licen[cs]e\\s+" line)
                  do (let* ((match-end (nth-value 1 (ppcre:scan "^\\s*:licen[cs]e\\s+" line)))
                            (value (when match-end
                                     (string-trim '(#\Space #\Tab #\")
                                                  (subseq line match-end)))))
                       (when (and value (> (length value) 0))
                         (return-from extract-license-field-from-asd
                           (format nil ";;; License information from ~A:~%~A"
                                   (file-namestring asd-file)
                                   value))))))
      (error () nil))))

;;; License file reference handling

(defun extract-filename-references (text)
  "Extract file references from TEXT. Returns list of filenames or NIL.
   Detects patterns like 'see file X', 'See [X](X)', etc."
  (let ((filenames nil)
        (patterns '(("(?i)see\\s+file\\s+([-_.a-zA-Z0-9]+)" . 0)
                    ("(?i)see\\s+\\[([^\\]]+)\\]\\([^)]+\\)" . 0)
                    ("(?i)see\\s+([A-Z][-_.a-zA-Z0-9/]*)" . :license-only))))
    (dolist (pattern-spec patterns)
      (destructuring-bind (pattern . mode) pattern-spec
        (ppcre:do-matches-as-strings (match pattern text)
          (multiple-value-bind (whole-match groups)
              (ppcre:scan-to-strings pattern match)
            (when (and groups (> (length groups) 0))
              (let ((filename (aref groups 0)))
                (when (or (not (eq mode :license-only))
                          (ppcre:scan "(?i)(LICEN[CS]E|COPYING|COPYRIGHT|NOTICE)" filename))
                  (push filename filenames))))))))
    (remove-duplicates filenames :test #'string-equal)))

(defun resolve-license-file-reference (system-dir referenced-filename)
  "Try to find REFERENCED-FILENAME in SYSTEM-DIR. Returns pathname or NIL."
  (let ((candidate (merge-pathnames referenced-filename system-dir)))
    (when (file-exists-p candidate)
      candidate)))

(defun follow-license-references (license-text system-dir)
  "If LICENSE-TEXT contains file references, try to load and append those files.
   Returns enhanced license text."
  (let ((referenced-files (extract-filename-references license-text)))
    (if referenced-files
        (with-output-to-string (out)
          ;; Include the original text first
          (write-string license-text out)
          ;; Then append contents of referenced files
          (dolist (ref-name referenced-files)
            (let ((ref-file (resolve-license-file-reference system-dir ref-name)))
              (when ref-file
                (format out "~%~%--------------------------------------------------------------------------------~%")
                (format out "Referenced file: ~A~%" ref-name)
                (format out "--------------------------------------------------------------------------------~%~%")
                (write-string (read-file-with-fallback-encoding ref-file) out)))))
        ;; No references found, return original text
        license-text)))

;;; Main license extraction logic

(defun find-oci-url-for-directory (dir-name csv-systems)
  "Find the OCI URL for a system directory by looking up entries in the CSV hash table."
  (block found
    (maphash (lambda (system-name registry-and-path)
               (let ((path (cdr registry-and-path)))
                 (when (and path (alexandria:starts-with-subseq dir-name path))
                   (return-from found (car registry-and-path)))))
             csv-systems)
    nil))

(defun find-license-in-system (system-dir)
  "Find license information in SYSTEM-DIR using multiple strategies.
   Returns (values license-text source-description) or (values nil nil)."
  (let* ((readme-file (find-readme-file system-dir))
         (readme-text (when readme-file (extract-license-from-readme readme-file))))
    (when readme-text
      (return-from find-license-in-system
        (values readme-text (file-namestring readme-file)))))

  (let* ((asd-file (find-primary-asd-file system-dir))
         (asd-comment-text (when asd-file (extract-license-from-asd-comments asd-file)))
         (has-asd-comment (and asd-comment-text (> (length asd-comment-text) 0))))
    (when has-asd-comment
      (return-from find-license-in-system
        (values asd-comment-text (format nil "~A (header)" (file-namestring asd-file)))))

    (let ((asd-field-text (when asd-file (extract-license-field-from-asd asd-file))))
      (when asd-field-text
        (return-from find-license-in-system
          (values asd-field-text (format nil "~A (:license field)" (file-namestring asd-file))))))

    ;; Try source files
    (when asd-file
      (let* ((base-system-name (pathname-name asd-file))
             (main-source-file (merge-pathnames
                                (make-pathname :name base-system-name :type "lisp")
                                system-dir)))
        (when (probe-file main-source-file)
          (let ((source-text (or (extract-license-from-source-file-start main-source-file)
                                 (extract-license-from-source-file-end main-source-file))))
            (when source-text
              (return-from find-license-in-system
                (values source-text (file-namestring main-source-file))))))))

    ;; Try any .lisp file
    (let ((lisp-files (filter-files-by-type (uiop:directory-files system-dir) "lisp")))
      (loop for file in lisp-files
            for text = (or (extract-license-from-source-file-start file)
                           (extract-license-from-source-file-end file))
            when text
              do (return-from find-license-in-system
                   (values text (format nil "~A (header)" (file-namestring file)))))))

  ;; Nothing found
  (values nil nil))

(defun do-collect-licenses (args)
  "Collect licenses from vendored dependencies in systems/ directory and output to stdout."
  (when args
    (usage)
    (uiop:quit 1))

  (let* ((csv-systems *ocicl-systems*)
         (licenses nil)
         (missing-systems nil))

    ;; Check if systems/ directory exists
    (unless (probe-file *systems-dir*)
      (format *error-output* "No systems/ directory found. Nothing to collect.~%")
      (uiop:quit 1))

    ;; Iterate through each system directory and collect license content
    (dolist (system-dir (directory (merge-pathnames "*/" *systems-dir*)))
      (let* ((system-name (get-directory-name system-dir))
             (license-file (find-license-file system-dir))
             (oci-url (find-oci-url-for-directory system-name csv-systems)))

        (cond
          ;; Found a license file
          (license-file
           (let* ((content (read-file-with-fallback-encoding license-file))
                  (enhanced-content (follow-license-references content system-dir))
                  (source-file (file-namestring license-file)))
             (push (list :system system-name
                        :source source-file
                        :oci-url oci-url
                        :content enhanced-content)
                   licenses)))

          ;; Try other sources
          (t
           (multiple-value-bind (license-text source-desc)
               (find-license-in-system system-dir)
             (if license-text
                 (let ((enhanced-text (follow-license-references license-text system-dir)))
                   (push (list :system system-name
                               :source source-desc
                               :oci-url oci-url
                               :content enhanced-text)
                         licenses))
                 (push system-name missing-systems)))))))

    ;; Sort licenses by system name
    (setf licenses (sort licenses #'string< :key (lambda (lic) (getf lic :system))))

    ;; Print table of contents
    (format t "================================================================================~%")
    (format t "VENDORED DEPENDENCY LICENSES~%")
    (format t "================================================================================~%")
    (format t "~%Table of Contents:~%~%")
    (loop for lic in licenses
          for i from 1
          do (format t "  ~2D. ~A (~A)~%"
                     i
                     (getf lic :system)
                     (getf lic :source)))

    (when missing-systems
      (format t "~%Systems without license information (~A):~%" (length missing-systems))
      (dolist (sys (sort (copy-list missing-systems) #'string<))
        (format t "  - ~A~%" sys)))

    (format t "~%================================================================================~%")
    (format t "~%")

    ;; Print each license
    (loop for lic in licenses
          for i from 1
          do (format t "~%~%")
             (format t "================================================================================~%")
             (format t "~D. ~A~%" i (getf lic :system))
             (format t "   Source: ~A~%" (getf lic :source))
             (format t "   OCI: ~A~%" (getf lic :oci-url))
             (format t "================================================================================~%")
             (format t "~%~A~%" (getf lic :content)))

    (format t "~%~%")
    (format t "================================================================================~%")
    (format t "Total: ~A license~:P collected~%" (length licenses))
    (when missing-systems
      (format t "       ~A system~:P without license information~%" (length missing-systems)))
    (format t "================================================================================~%")))
