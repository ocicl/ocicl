;;; structures.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Common data structures for releases and assets.

(in-package #:cl-selfupdate)

;;; Release structure

(defstruct release
  "A release from a provider (GitHub, GitLab, etc.)."
  version      ; semver version object
  tag          ; original tag string
  name         ; release name
  assets       ; list of asset structs
  notes        ; release body/notes
  url          ; HTML URL
  prerelease-p ; is this a prerelease?
  draft-p)     ; is this a draft?

;;; Asset structure

(defstruct asset
  "A release asset (downloadable file)."
  name         ; filename
  url          ; API URL
  size         ; size in bytes
  download-url ; browser download URL
  content-type)

;;; Asset matching

(defun checksum-asset-p (name)
  "Return T if NAME appears to be a checksum or signature file."
  (or (alexandria:ends-with-subseq ".sha256" name)
      (alexandria:ends-with-subseq ".sha512" name)
      (alexandria:ends-with-subseq ".sig" name)
      (alexandria:ends-with-subseq ".asc" name)
      (string-equal name "checksums.txt")
      (string-equal name "SHA256SUMS")
      (string-equal name "SHA512SUMS")
      (string-equal name "sha256sums.txt")
      (string-equal name "sha512sums.txt")))

(defun asset-matches-platform-p (asset-name suffixes)
  "Check if ASSET-NAME contains any of the platform SUFFIXES."
  (some (lambda (suffix)
          (search suffix asset-name :test #'char-equal))
        suffixes))

(defun asset-extension-priority (name)
  "Return priority for asset based on extension. Lower is better.
Prefers archives over raw binaries."
  (cond
    ((alexandria:ends-with-subseq ".tar.gz" name) 0)
    ((alexandria:ends-with-subseq ".tgz" name) 0)
    ((alexandria:ends-with-subseq ".zip" name) 1)
    ((alexandria:ends-with-subseq ".gz" name) 2)
    (t 3)))  ; Raw binary or unknown

(defun find-matching-asset (release &key name-pattern)
  "Find an asset in a release that matches the current platform.
Optional NAME-PATTERN is a regex to further filter assets.
Excludes checksum and signature files.
Prefers archives (.tar.gz, .zip) over raw binaries.
Returns the matching asset or NIL."
  (let* ((suffixes (platform-suffixes))
         (assets (release-assets release))
         (candidates '()))
    ;; Filter to matching assets
    (dolist (asset assets)
      (let ((name (asset-name asset)))
        (when (and (not (checksum-asset-p name))
                   (asset-matches-platform-p name suffixes)
                   (or (null name-pattern)
                       (cl-ppcre:scan name-pattern name)))
          (push asset candidates))))
    ;; Sort by extension priority and return best match
    (when candidates
      (first (sort candidates #'<
                   :key (lambda (a) (asset-extension-priority (asset-name a))))))))
