;;; test.lisp --- Simple tests for cl-selfupdate
;;;
;;; Run with:
;;;   sbcl --load test.lisp
;;;
;;; Or from REPL:
;;;   (load "test.lisp")

(require :asdf)
(push #p"/home/green/git/cl-github-selfupdate/" asdf:*central-registry*)
(push #p"/home/green/git/pure-tls/" asdf:*central-registry*)

(format t "~%Loading cl-selfupdate/dexador...~%")
(asdf:load-system :cl-selfupdate/dexador)

(defpackage #:selfupdate-test
  (:use #:cl #:cl-selfupdate))

(in-package #:selfupdate-test)

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro test (name &body body)
  `(handler-case
       (progn
         (format t "~%Test: ~A... " ,name)
         (if (progn ,@body)
             (progn
               (format t "PASS")
               (incf *tests-passed*))
             (progn
               (format t "FAIL")
               (incf *tests-failed*))))
     (error (e)
       (format t "ERROR: ~A" e)
       (incf *tests-failed*))))

(defun run-tests ()
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)

  (format t "~%========================================")
  (format t "~%  cl-selfupdate Test Suite")
  (format t "~%========================================")

  ;; HTTP backend tests
  (format t "~%~%--- HTTP Backend Tests ---")

  (test "HTTP backend is loaded"
    (eq *http-backend* :dexador))

  ;; Platform detection tests
  (format t "~%~%--- Platform Detection Tests ---")

  (test "detect-os returns keyword"
    (keywordp (detect-os)))

  (test "detect-arch returns keyword"
    (keywordp (detect-arch)))

  (test "detect-platform returns cons"
    (consp (detect-platform)))

  (test "platform-suffixes returns list"
    (and (listp (cl-selfupdate::platform-suffixes))
         (> (length (cl-selfupdate::platform-suffixes)) 0)))

  ;; Provider tests
  (format t "~%~%--- Provider Tests ---")

  (test "*github* is github-provider"
    (typep *github* 'github-provider))

  (test "*gitlab* is gitlab-provider"
    (typep *gitlab* 'gitlab-provider))

  (test "*default-provider* is set"
    (typep *default-provider* 'provider))

  (test "ensure-provider with keyword"
    (eq (ensure-provider :github) *github*))

  (test "ensure-provider with instance"
    (eq (ensure-provider *gitlab*) *gitlab*))

  ;; Asset matching tests
  (format t "~%~%--- Asset Matching Tests ---")

  (test "checksum files are excluded"
    (cl-selfupdate::checksum-asset-p "myapp_linux_amd64.tar.gz.sha256"))

  (test "regular assets are not excluded"
    (not (cl-selfupdate::checksum-asset-p "myapp_linux_amd64.tar.gz")))

  (test "checksums.txt is excluded"
    (cl-selfupdate::checksum-asset-p "checksums.txt"))

  (test "SHA256SUMS is excluded"
    (cl-selfupdate::checksum-asset-p "SHA256SUMS"))

  ;; GitHub API tests (requires network)
  (format t "~%~%--- Network Tests (GitHub API) ---")

  (test "list-releases returns list"
    (let ((releases (list-releases *github* "rhysd" "go-github-selfupdate")))
      (and (listp releases)
           (> (length releases) 0))))

  (test "get-latest-release returns release struct"
    (let ((rel (get-latest-release *github* "rhysd" "go-github-selfupdate")))
      (and rel (release-p rel))))

  (test "release has tag"
    (let ((rel (get-latest-release *github* "rhysd" "go-github-selfupdate")))
      (stringp (release-tag rel))))

  (test "release has assets"
    (let ((rel (get-latest-release *github* "rhysd" "go-github-selfupdate")))
      (and (listp (release-assets rel))
           (> (length (release-assets rel)) 0))))

  (test "find-matching-asset finds asset (not checksum)"
    (let* ((rel (get-latest-release *github* "rhysd" "go-github-selfupdate"))
           (asset (find-matching-asset rel)))
      (and asset
           (asset-p asset)
           (not (cl-selfupdate::checksum-asset-p (asset-name asset))))))

  (test "asset has download URL"
    (let* ((rel (get-latest-release *github* "rhysd" "go-github-selfupdate"))
           (asset (find-matching-asset rel)))
      (and asset (stringp (asset-download-url asset)))))

  ;; Test with a repo that has proper semver tags (BurntSushi/ripgrep)
  (test "detect-latest returns highest version"
    (let ((rel (detect-latest *github* "BurntSushi" "ripgrep")))
      (and rel (release-version rel))))

  ;; Version comparison tests
  (format t "~%~%--- Version Tests ---")

  (test "version parsing works"
    (let ((v (semver:read-version-from-string "1.2.3")))
      (typep v 'semver:version)))

  (test "version comparison works"
    (let ((v1 (semver:read-version-from-string "1.0.0"))
          (v2 (semver:read-version-from-string "2.0.0")))
      (semver:version< v1 v2)))

  ;; Update check test (using BurntSushi/ripgrep which has proper semver tags)
  (format t "~%~%--- Update Check Test ---")

  (test "update-available-p works with provider keyword"
    (multiple-value-bind (rel newer-p)
        (update-available-p "BurntSushi" "ripgrep"
                            :current-version "0.0.1"
                            :provider :github)
      (and (release-p rel) newer-p)))

  (test "update-available-p works with provider instance"
    (multiple-value-bind (rel newer-p)
        (update-available-p "BurntSushi" "ripgrep"
                            :current-version "0.0.1"
                            :provider *github*)
      (and (release-p rel) newer-p)))

  ;; Summary
  (format t "~%~%========================================")
  (format t "~%  Results: ~D passed, ~D failed" *tests-passed* *tests-failed*)
  (format t "~%========================================~%")

  (values *tests-passed* *tests-failed*))

;; Run tests
(run-tests)
