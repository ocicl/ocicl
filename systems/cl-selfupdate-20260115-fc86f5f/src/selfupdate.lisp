;;; selfupdate.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cl-selfupdate)

;;; Configuration

(defvar *current-version* nil
  "Current version of the application (semver string or version object).")

(defvar *executable-path* nil
  "Path to the current executable. Auto-detected if NIL.")

;;; Utility functions

(defun get-executable-path ()
  "Get the path to the current executable."
  (or *executable-path*
      ;; Try SBCL's core file path
      #+sbcl (or sb-ext:*core-pathname*
                 (first sb-ext:*posix-argv*))
      ;; Try CCL
      #+ccl (first (ccl::command-line-arguments))
      ;; Try ECL
      #+ecl (si:argv 0)
      ;; Fallback
      (first (uiop:raw-command-line-arguments))))

(defun ensure-version (version)
  "Ensure VERSION is a semver version object."
  (etypecase version
    (semver:version version)
    (string (handler-case
                (semver:read-version-from-string
                 (cl-ppcre:regex-replace "^v" version ""))
              (error () nil)))
    (null nil)))

(defun version-greater-p (v1 v2)
  "Return T if V1 is greater than V2."
  (when (and (typep v1 'semver:version)
             (typep v2 'semver:version))
    (semver:version> v1 v2)))

(defun safe-print-version (version)
  "Return a printable version string or \"unknown\"."
  (if version
      (semver:print-version-to-string version)
      "unknown"))

;;; Update detection

(defun update-available-p (owner repo &key (current-version *current-version*)
                                        include-prerelease
                                        (provider *default-provider*))
  "Check if an update is available.
Returns (VALUES RELEASE NEW-VERSION-P) where RELEASE is the latest release
and NEW-VERSION-P is T if it's newer than CURRENT-VERSION.
PROVIDER can be a provider instance, :github, :gitlab, or NIL for default."
  (let* ((prov (ensure-provider provider))
         (current (ensure-version current-version))
         (latest (detect-latest prov owner repo :include-prerelease include-prerelease)))
    (if (and latest (release-version latest))
        (values latest (version-greater-p (release-version latest) current))
        (values nil nil))))

;;; Download and extraction

(defun download-update (owner repo &key release output-dir executable-name
                                      (validate *validate-downloads*)
                                      (provider *default-provider*))
  "Download the latest release asset for the current platform.
Returns the path to the downloaded/extracted executable.
RELEASE - optional release struct, fetched if not provided.
OUTPUT-DIR - directory to extract to (defaults to temp dir).
EXECUTABLE-NAME - name of executable to extract from archive.
VALIDATE - if T (default), validate SHA256 checksum if available.
PROVIDER - provider instance, :github, :gitlab, or NIL for default."
  (let* ((prov (ensure-provider provider))
         (rel (or release (detect-latest prov owner repo)))
         (asset (find-matching-asset rel))
         (temp-dir (or output-dir
                       (uiop:ensure-pathname
                        (format nil "~A/cl-selfupdate-~A/"
                                (uiop:temporary-directory)
                                (get-universal-time))
                        :ensure-directory t)))
         (*validate-downloads* validate))
    (unless rel
      (error "No release found for ~A/~A" owner repo))
    (unless asset
      (error "No matching asset found for platform ~A in release ~A"
             (detect-platform) (release-tag rel)))
    ;; Download the asset (with optional validation)
    (ensure-directories-exist temp-dir)
    (let ((archive-path (merge-pathnames (asset-name asset) temp-dir)))
      (format *error-output* "~&Downloading ~A (~:D bytes)...~%"
              (asset-name asset) (asset-size asset))
      ;; Stream download directly to file (more memory efficient)
      (download-and-validate-asset-to-file prov asset rel archive-path)
      ;; Extract if it's an archive
      (let ((format (detect-archive-format (asset-name asset))))
        (if (eq format :unknown)
            ;; Raw binary - just return it
            archive-path
            ;; Extract archive
            (progn
              (format *error-output* "~&Extracting...~%")
              (extract-archive archive-path temp-dir
                               :executable-name executable-name)))))))

;;; Executable replacement

(defun make-executable (path)
  "Make a file executable (Unix only)."
  #+(or linux darwin freebsd openbsd netbsd)
  (uiop:run-program (list "chmod" "+x" (namestring path)))
  path)

(defun copy-file-with-sync (source dest)
  "Copy SOURCE to DEST and sync to ensure durability."
  (with-open-file (in source :element-type '(unsigned-byte 8))
    (with-open-file (out dest :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
        (loop for bytes-read = (read-sequence buffer in)
              while (plusp bytes-read)
              do (write-sequence buffer out :end bytes-read)))
      ;; Force sync to disk
      (finish-output out)))
  dest)

(define-condition update-failed (error)
  ((message :initarg :message :reader update-failed-message)
   (original-error :initarg :original-error :reader update-failed-original-error))
  (:report (lambda (c stream)
             (format stream "Update failed: ~A~@[~%Original error: ~A~]"
                     (update-failed-message c)
                     (update-failed-original-error c)))))

(defun atomic-replace (old-path new-path)
  "Atomically replace OLD-PATH with NEW-PATH.
Creates a backup of OLD-PATH first. Handles cross-filesystem moves
by falling back to copy+delete. Includes rollback on failure."
  (let* ((old-path (truename old-path))
         (backup-path (make-pathname :defaults old-path
                                     :type "old"))
         (backup-created nil))
    ;; Create backup of existing file
    (when (probe-file old-path)
      (when (probe-file backup-path)
        (delete-file backup-path))
      (rename-file old-path backup-path)
      (setf backup-created t))
    ;; Try to move new file into place
    (handler-case
        (progn
          ;; First try rename (fast, atomic on same filesystem)
          (handler-case
              (rename-file new-path old-path)
            ;; If rename fails (likely cross-device), fall back to copy
            (error ()
              (copy-file-with-sync new-path old-path)
              (delete-file new-path)))
          ;; Make executable
          (make-executable old-path)
          old-path)
      ;; On any failure, attempt rollback
      (error (e)
        (when backup-created
          (format *error-output* "~&Update failed, rolling back...~%")
          (handler-case
              (rename-file backup-path old-path)
            (error ()
              (format *error-output* "~&WARNING: Rollback failed! Backup at: ~A~%"
                      backup-path))))
        (error 'update-failed
               :message "Failed to replace executable"
               :original-error e)))))

(defun apply-update (new-executable-path &key (target-path (get-executable-path)))
  "Apply an update by replacing the current executable.
NEW-EXECUTABLE-PATH - path to the new executable.
TARGET-PATH - path to replace (defaults to current executable)."
  (format *error-output* "~&Applying update to ~A...~%" target-path)
  (make-executable new-executable-path)
  (atomic-replace target-path new-executable-path)
  (format *error-output* "~&Update complete. Please restart the application.~%")
  target-path)

;;; High-level API

(defun update-self (owner repo &key (current-version *current-version*)
                                 executable-name
                                 (target-path (get-executable-path))
                                 include-prerelease
                                 (validate *validate-downloads*)
                                 (provider *default-provider*)
                                 (show-notes t)
                                 dry-run)
  "Check for and apply updates from a GitHub or GitLab repository.
OWNER - repository owner/namespace.
REPO - repository name.
CURRENT-VERSION - current version string or semver object.
EXECUTABLE-NAME - name of executable to extract from archive.
TARGET-PATH - path to the executable to replace.
INCLUDE-PRERELEASE - if T, include prerelease versions.
VALIDATE - if T (default), validate SHA256 checksum if available.
PROVIDER - provider instance, :github, :gitlab, or NIL for default.
SHOW-NOTES - if T (default), display release notes when updating.
DRY-RUN - if T, check and download but don't apply.

Returns (VALUES UPDATED-P NEW-VERSION OLD-VERSION RELEASE-NOTES) where:
- UPDATED-P is T if an update was applied (or would be in dry-run).
- NEW-VERSION is the new version string.
- OLD-VERSION is the old version string.
- RELEASE-NOTES is the release notes/changelog text (or NIL)."
  (let* ((prov (ensure-provider provider))
         (current (ensure-version current-version)))
    (format *error-output* "~&Checking for updates to ~A/~A (~A)...~%"
            owner repo (provider-name prov))
    (format *error-output* "~&Current version: ~A~%"
            (safe-print-version current))
    (multiple-value-bind (release newer-p)
        (update-available-p owner repo
                            :current-version current
                            :include-prerelease include-prerelease
                            :provider prov)
      (cond
        ((not release)
         (format *error-output* "~&No releases found.~%")
         (values nil nil (safe-print-version current) nil))
        ((not newer-p)
         (format *error-output* "~&Already up to date.~%")
         (values nil
                 (safe-print-version (release-version release))
                 (safe-print-version current)
                 nil))
        (t
         (let ((new-version (release-version release))
               (notes (release-notes release)))
           (format *error-output* "~&New version available: ~A~%"
                   (safe-print-version new-version))
           ;; Display release notes if available and requested
           (when (and show-notes notes (plusp (length notes)))
             (format *error-output* "~&~%Release Notes:~%~A~%~%" notes))
           (let ((new-exe (download-update owner repo
                                           :release release
                                           :executable-name executable-name
                                           :validate validate
                                           :provider prov)))
             (if dry-run
                 (progn
                   (format *error-output* "~&Dry run - not applying update.~%")
                   (format *error-output* "~&Downloaded to: ~A~%" new-exe))
                 (apply-update new-exe :target-path target-path))
             (values t
                     (safe-print-version new-version)
                     (safe-print-version current)
                     notes))))))))
