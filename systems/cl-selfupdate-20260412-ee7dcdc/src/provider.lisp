;;; provider.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; Provider abstraction using CLOS for multiple release hosting services.

(in-package #:cl-selfupdate)

;;; Base provider class

(defclass provider ()
  ((name :initarg :name :reader provider-name)
   (api-base :initarg :api-base :accessor provider-api-base)
   (token :initarg :token :accessor provider-token :initform nil))
  (:documentation "Base class for release providers."))

;;; Generic functions for provider operations

(defgeneric list-releases (provider owner repo &key per-page)
  (:documentation "List releases for a repository."))

(defgeneric get-latest-release (provider owner repo)
  (:documentation "Get the latest release for a repository."))

(defgeneric detect-latest (provider owner repo &key include-prerelease)
  (:documentation "Find the latest release with valid semver version."))

(defgeneric download-asset (provider asset &key output-path)
  (:documentation "Download an asset from the provider."))

(defgeneric download-asset-to-file (provider asset output-path &key compute-hash)
  (:documentation "Stream download an asset directly to a file.
If COMPUTE-HASH is true, returns (VALUES path hash-string).
Otherwise returns just the path. More memory-efficient for large files."))

(defgeneric get-token (provider)
  (:documentation "Get authentication token for the provider."))

;;; GitHub provider

(defclass github-provider (provider)
  ()
  (:default-initargs
   :name "GitHub"
   :api-base "https://api.github.com"))

(defvar *github* (make-instance 'github-provider)
  "Default GitHub provider instance.")

(defvar *github-token* nil
  "GitHub API token. Can also be set via GITHUB_TOKEN env var.")

(defmethod get-token ((provider github-provider))
  (or (provider-token provider)
      *github-token*
      (uiop:getenv "GITHUB_TOKEN")))

;;; GitLab provider

(defclass gitlab-provider (provider)
  ()
  (:default-initargs
   :name "GitLab"
   :api-base "https://gitlab.com/api/v4"))

(defvar *gitlab* (make-instance 'gitlab-provider)
  "Default GitLab provider instance.")

(defvar *gitlab-token* nil
  "GitLab API token. Can also be set via GITLAB_TOKEN env var.")

(defmethod get-token ((provider gitlab-provider))
  (or (provider-token provider)
      *gitlab-token*
      (uiop:getenv "GITLAB_TOKEN")))

;;; Default provider

(defvar *default-provider* *github*
  "Default provider for release operations.")

;;; Convenience function to get provider instance from keyword

(defun ensure-provider (provider-designator)
  "Convert a provider designator to a provider instance.
Accepts: provider instance, :github, :gitlab, or NIL (uses *default-provider*)."
  (etypecase provider-designator
    (provider provider-designator)
    (keyword (ecase provider-designator
               (:github *github*)
               (:gitlab *gitlab*)))
    (null *default-provider*)))
