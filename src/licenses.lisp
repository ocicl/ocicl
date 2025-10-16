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

(defun read-file-with-fallback-encoding (pathname)
  "Read file content as string, trying UTF-8 first, then falling back to Latin-1.
   This handles LICENSE files that may be encoded in various character sets."
  (handler-case
      (alexandria:read-file-into-string pathname :external-format :utf-8)
    (#+sbcl sb-int:stream-decoding-error
     #+ccl ccl:stream-decoding-error
     #+ecl ext:stream-decoding-error
     #-(or sbcl ccl ecl) error ()
      ;; Fall back to Latin-1 which can decode any byte sequence
      (alexandria:read-file-into-string pathname :external-format :latin-1))))

(defun find-license-file (directory)
  "Find a license file in DIRECTORY. Returns pathname or NIL."
  (let* ((license-patterns '("LICENSE*" "LICENCE*" "COPYING*" "COPYRIGHT*"))
         (files (loop for pattern in license-patterns
                      append (directory (merge-pathnames pattern directory)))))
    (car files)))

(defun find-readme-file (directory)
  "Find a README file in DIRECTORY. Returns pathname or NIL."
  (let* ((readme-patterns '("README" "README.*"))
         (files (loop for pattern in readme-patterns
                      append (directory (merge-pathnames pattern directory)))))
    (car files)))

(defun extract-license-from-readme (readme-file)
  "Extract license section from README-FILE. Returns string or NIL."
  (when (probe-file readme-file)
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

(defun extract-license-from-source-file-end (source-file)
  "Extract license from end of SOURCE-FILE (last ~50 lines). Returns string or NIL."
  (when (probe-file source-file)
    (let ((lines (uiop:read-file-lines source-file)))
      (when (> (length lines) 10)
        ;; Check last 50 lines for copyright/license
        (let* ((start-pos (max 0 (- (length lines) 50)))
               (end-lines (subseq lines start-pos))
               (license-start nil))
          ;; Find where license block starts
          (loop for i from 0 below (length end-lines)
                for line = (nth i end-lines)
                when (ppcre:scan "(?i)^;;;?.*(Copyright|Licen[cs]e|Permission|provided 'as-is')" line)
                  do (setf license-start i) (return))
          (when license-start
            (with-output-to-string (out)
              (loop for i from license-start below (length end-lines)
                    for line = (nth i end-lines)
                    do (write-line line out)))))))))

(defun extract-license-from-asd-comments (asd-file)
  "Extract license text from comment header of ASD-FILE. Returns string or NIL."
  (when (probe-file asd-file)
    (with-output-to-string (out)
      (with-open-file (in asd-file :direction :input)
        (let ((found nil)
              (blank-count 0))
          (loop for line = (read-line in nil nil)
                while line
                do (cond
                     ;; Start capturing when we see license-related keywords
                     ((and (not found)
                           (ppcre:scan "(?i)^;;;.*(Copyright|License|Licence|Public Domain|warranty|Permission)" line))
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
  (when (probe-file asd-file)
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

(defun find-primary-asd-file (directory)
  "Find primary .asd file in DIRECTORY (excluding test files). Returns pathname or NIL."
  (let ((asd-files (directory (merge-pathnames "*.asd" directory))))
    (or (find-if-not (lambda (f) (search "test" (pathname-name f))) asd-files)
        (car asd-files))))

(defun find-oci-url-for-directory (dir-name csv-systems)
  "Find the OCI URL for a system directory by looking up entries in the CSV hash table."
  (block found
    (maphash (lambda (system-name registry-and-path)
               (let ((path (cdr registry-and-path)))
                 (when (and path (alexandria:starts-with-subseq dir-name path))
                   (return-from found (car registry-and-path)))))
             csv-systems)
    nil))

(defun extract-filename-references (text)
  "Extract file references from TEXT. Returns list of filenames or NIL.
   Detects patterns like 'see file X', 'See [X](X)', etc."
  (let ((filenames nil))
    ;; Pattern 1: "see file FILENAME" (case insensitive)
    (ppcre:do-matches-as-strings (match "(?i)see\\s+file\\s+([-_.a-zA-Z0-9]+)" text)
      (multiple-value-bind (whole-match groups)
          (ppcre:scan-to-strings "(?i)see\\s+file\\s+([-_.a-zA-Z0-9]+)" match)
        (when (and groups (> (length groups) 0))
          (push (aref groups 0) filenames))))
    ;; Pattern 2: "See [FILENAME](FILENAME)" (markdown links)
    (ppcre:do-matches-as-strings (match "(?i)see\\s+\\[([^\\]]+)\\]\\([^)]+\\)" text)
      (multiple-value-bind (whole-match groups)
          (ppcre:scan-to-strings "(?i)see\\s+\\[([^\\]]+)\\]\\([^)]+\\)" match)
        (when (and groups (> (length groups) 0))
          (push (aref groups 0) filenames))))
    ;; Pattern 3: "see FILENAME" where FILENAME looks like a license file
    (ppcre:do-matches-as-strings (match "(?i)see\\s+([A-Z][-_.a-zA-Z0-9]*)" text)
      (multiple-value-bind (whole-match groups)
          (ppcre:scan-to-strings "(?i)see\\s+([A-Z][-_.a-zA-Z0-9]*)" match)
        (when (and groups (> (length groups) 0))
          (let ((filename (aref groups 0)))
            ;; Only include if it looks like a license file
            (when (ppcre:scan "(?i)(LICEN[CS]E|COPYING|COPYRIGHT|NOTICE)" filename)
              (push filename filenames))))))
    (remove-duplicates filenames :test #'string-equal)))

(defun resolve-license-file-reference (system-dir referenced-filename)
  "Try to find REFERENCED-FILENAME in SYSTEM-DIR. Returns pathname or NIL."
  (let ((candidate (merge-pathnames referenced-filename system-dir)))
    (when (probe-file candidate)
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
      (let* ((system-name (car (last (pathname-directory system-dir))))
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

          ;; Try to extract from .asd file
          (t
           (let* ((readme-file (find-readme-file system-dir))
                  (readme-text (when readme-file (extract-license-from-readme readme-file)))
                  (asd-file (find-primary-asd-file system-dir))
                  (asd-comment-text (when (and asd-file (not readme-text))
                                      (extract-license-from-asd-comments asd-file)))
                  ;; Check if asd-comment-text is empty or nil
                  (has-asd-comment (and asd-comment-text (> (length asd-comment-text) 0)))
                  (asd-field-text (when (and asd-file (not readme-text) (not has-asd-comment))
                                    (extract-license-field-from-asd asd-file)))
                  ;; Use the .asd filename (without extension) to find the main source file
                  (base-system-name (when asd-file (pathname-name asd-file)))
                  (main-source-file (when base-system-name
                                      (let ((possible (merge-pathnames
                                                       (make-pathname :name base-system-name :type "lisp")
                                                       system-dir)))
                                        (when (probe-file possible) possible))))
                  (source-end-text (when (and main-source-file (not readme-text) (not has-asd-comment) (not asd-field-text))
                                     (extract-license-from-source-file-end main-source-file)))
                  (license-text (or readme-text (and has-asd-comment asd-comment-text) asd-field-text source-end-text))
                  (source-desc (cond
                                 (readme-text (file-namestring readme-file))
                                 (has-asd-comment (format nil "~A (header)" (file-namestring asd-file)))
                                 (asd-field-text (format nil "~A (:license field)" (file-namestring asd-file)))
                                 (source-end-text (file-namestring main-source-file))
                                 (t nil))))
             (cond
               ((and license-text (> (length license-text) 0))
                (let ((enhanced-text (follow-license-references license-text system-dir)))
                  (push (list :system system-name
                             :source source-desc
                             :oci-url oci-url
                             :content enhanced-text)
                        licenses)))
               (t
                (push system-name missing-systems))))))))

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
