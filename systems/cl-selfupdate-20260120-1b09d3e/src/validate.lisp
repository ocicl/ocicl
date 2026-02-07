;;; validate.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; SHA256 validation for downloaded assets.

(in-package #:cl-selfupdate)

;;; Configuration

(defvar *validate-downloads* t
  "When non-NIL, automatically validate downloads against SHA256 checksums if available.")

;;; Conditions

(define-condition validation-error (error)
  ((expected :initarg :expected :reader validation-error-expected)
   (actual :initarg :actual :reader validation-error-actual)
   (asset-name :initarg :asset-name :reader validation-error-asset-name))
  (:report (lambda (c stream)
             (format stream "SHA256 validation failed for ~A~%  Expected: ~A~%  Actual:   ~A"
                     (validation-error-asset-name c)
                     (validation-error-expected c)
                     (validation-error-actual c)))))

;;; SHA256 computation

(defun compute-sha256 (data)
  "Compute SHA256 hash of DATA (octet vector). Returns lowercase hex string."
  (let ((digest (ironclad:digest-sequence :sha256 data)))
    (ironclad:byte-array-to-hex-string digest)))

(defun compute-sha256-file (path)
  "Compute SHA256 hash of file at PATH. Returns lowercase hex string."
  (let ((digest (ironclad:digest-file :sha256 path)))
    (ironclad:byte-array-to-hex-string digest)))

;;; Checksum file parsing

(defun parse-checksum-line (line)
  "Parse a single checksum line. Returns (hash . filename) or NIL.
Supports formats:
  - 'hash  filename' (BSD style, two spaces)
  - 'hash *filename' (binary mode indicator)
  - 'hash filename' (single space)
  - 'SHA256 (filename) = hash' (BSD tag style)"
  (let ((line (string-trim '(#\Space #\Tab #\Return #\Newline) line)))
    (when (and (> (length line) 0)
               (char/= (char line 0) #\#))  ; Skip comments
      (cond
        ;; BSD tag style: SHA256 (filename) = hash
        ((and (>= (length line) 7)
              (string-equal "SHA256" (subseq line 0 6)))
         (let* ((paren-start (position #\( line))
                (paren-end (position #\) line))
                (equals (position #\= line :from-end t)))
           (when (and paren-start paren-end equals
                      (< paren-start paren-end equals))
             (let ((filename (subseq line (1+ paren-start) paren-end))
                   (hash (string-trim '(#\Space) (subseq line (1+ equals)))))
               (cons (string-downcase hash) filename)))))
        ;; Standard format: hash  filename or hash *filename
        (t
         (let ((hash-end (or (position #\Space line)
                             (position #\Tab line))))
           (when (and hash-end (= hash-end 64))  ; SHA256 is 64 hex chars
             (let* ((hash (subseq line 0 hash-end))
                    (rest (string-trim '(#\Space #\Tab #\*) (subseq line hash-end))))
               (when (> (length rest) 0)
                 (cons (string-downcase hash) rest))))))))))

(defun parse-checksum-file (content)
  "Parse checksum file content. Returns alist of (filename . hash).
Supports:
  - Per-file .sha256 files (single hash, optional filename)
  - Unified checksums.txt files (multiple lines)
  - goreleaser-style checksum files"
  (let ((lines (cl-ppcre:split "\\n" content))
        (checksums '()))
    (dolist (line lines)
      (let ((parsed (parse-checksum-line line)))
        (when parsed
          ;; Store as (filename . hash) for easier lookup
          (push (cons (cdr parsed) (car parsed)) checksums))))
    (nreverse checksums)))

(defun parse-single-hash (content filename)
  "Parse a single-hash file (just the hash, optionally with filename).
Returns the hash string or NIL."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Return #\Newline) content)))
    (cond
      ;; Just a hash (64 hex chars)
      ((and (= (length trimmed) 64)
            (every (lambda (c) (or (digit-char-p c) (find c "abcdefABCDEF"))) trimmed))
       (string-downcase trimmed))
      ;; Hash with filename
      (t
       (let ((parsed (parse-checksum-line trimmed)))
         (when parsed (car parsed)))))))

;;; Finding checksum assets

(defun find-checksum-asset (release asset)
  "Find the checksum asset for a given asset in a release.
Looks for:
  1. asset.sha256 (per-file checksum)
  2. checksums.txt (unified checksum file)
  3. SHA256SUMS (alternative unified file)
Returns the checksum asset or NIL."
  (let* ((asset-name (asset-name asset))
         (assets (release-assets release))
         (sha256-name (format nil "~A.sha256" asset-name)))
    (or
     ;; Per-file checksum
     (find sha256-name assets :key #'asset-name :test #'string-equal)
     ;; Unified checksum files
     (find "checksums.txt" assets :key #'asset-name :test #'string-equal)
     (find "SHA256SUMS" assets :key #'asset-name :test #'string-equal)
     (find "sha256sums.txt" assets :key #'asset-name :test #'string-equal))))

;;; Validation

(defun get-expected-hash (checksum-content asset-name checksum-asset-name)
  "Extract the expected hash for asset-name from checksum content."
  (if (alexandria:ends-with-subseq ".sha256" checksum-asset-name)
      ;; Per-file checksum - might just be the hash
      (parse-single-hash checksum-content asset-name)
      ;; Unified checksum file - parse and lookup
      (let ((checksums (parse-checksum-file checksum-content)))
        (cdr (assoc asset-name checksums :test #'string-equal)))))

(defun validate-sha256 (data asset-name expected-hash)
  "Validate DATA against EXPECTED-HASH.
Signals VALIDATION-ERROR if hashes don't match.
Returns T on success."
  (let ((actual-hash (compute-sha256 data)))
    (if (string-equal actual-hash expected-hash)
        t
        (error 'validation-error
               :expected expected-hash
               :actual actual-hash
               :asset-name asset-name))))

(defun download-and-validate-asset (provider asset release)
  "Download an asset and validate against SHA256 checksum if available.
Returns the downloaded data (octets).
Signals VALIDATION-ERROR if validation fails.
PROVIDER is a provider instance."
  (let ((checksum-asset (when *validate-downloads*
                          (find-checksum-asset release asset))))
    ;; Download the main asset
    (let ((data (download-asset provider asset)))
      ;; Validate if checksum available
      (when checksum-asset
        (format *error-output* "~&Validating SHA256 checksum...~%")
        (let* ((checksum-content (download-asset provider checksum-asset))
               (checksum-string (if (stringp checksum-content)
                                    checksum-content
                                    (flexi-streams:octets-to-string
                                     checksum-content :external-format :utf-8)))
               (expected-hash (get-expected-hash checksum-string
                                                 (asset-name asset)
                                                 (asset-name checksum-asset))))
          (if expected-hash
              (validate-sha256 data (asset-name asset) expected-hash)
              (format *error-output* "~&Warning: Could not find hash for ~A in checksum file~%"
                      (asset-name asset)))))
      data)))

(defun validate-file-sha256 (path asset-name expected-hash)
  "Validate file at PATH against EXPECTED-HASH.
Signals VALIDATION-ERROR if hashes don't match.
Returns T on success."
  (let ((actual-hash (compute-sha256-file path)))
    (if (string-equal actual-hash expected-hash)
        t
        (error 'validation-error
               :expected expected-hash
               :actual actual-hash
               :asset-name asset-name))))

(defun download-and-validate-asset-to-file (provider asset release output-path)
  "Stream download an asset to file and validate against SHA256 if available.
More memory-efficient for large files.
Signals VALIDATION-ERROR if validation fails.
Returns the output path."
  (let ((checksum-asset (when *validate-downloads*
                          (find-checksum-asset release asset))))
    (if checksum-asset
        ;; Download with hash computation
        (progn
          (format *error-output* "~&Downloading with SHA256 validation...~%")
          (multiple-value-bind (path actual-hash)
              (download-asset-to-file provider asset output-path :compute-hash t)
            ;; Get expected hash
            (let* ((checksum-content (download-asset provider checksum-asset))
                   (checksum-string (if (stringp checksum-content)
                                        checksum-content
                                        (flexi-streams:octets-to-string
                                         checksum-content :external-format :utf-8)))
                   (expected-hash (get-expected-hash checksum-string
                                                     (asset-name asset)
                                                     (asset-name checksum-asset))))
              (if expected-hash
                  (if (string-equal actual-hash expected-hash)
                      (progn
                        (format *error-output* "~&Checksum verified.~%")
                        path)
                      (progn
                        ;; Delete invalid file
                        (delete-file path)
                        (error 'validation-error
                               :expected expected-hash
                               :actual actual-hash
                               :asset-name (asset-name asset))))
                  (progn
                    (format *error-output* "~&Warning: Could not find hash for ~A in checksum file~%"
                            (asset-name asset))
                    path)))))
        ;; No checksum - just download
        (download-asset-to-file provider asset output-path))))
