;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(defpackage #:cl-selfupdate
  (:use #:cl)
  (:documentation "Self-update functionality for Common Lisp executables via GitHub/GitLab Releases.")
  (:export
   ;; HTTP abstraction
   #:*http-backend*
   #:no-http-backend
   #:http-request-error
   #:http-error-status-code
   #:http-error-url
   #:http-error-body
   #:http-request
   #:http-get
   #:http-get-stream
   #:do-http-request
   #:do-http-get
   #:do-http-get-stream
   ;; Platform detection
   #:detect-os
   #:detect-arch
   #:detect-platform
   ;; Provider classes
   #:provider
   #:provider-name
   #:provider-api-base
   #:provider-token
   #:github-provider
   #:gitlab-provider
   #:*github*
   #:*gitlab*
   #:*default-provider*
   #:ensure-provider
   ;; Provider generic functions
   #:list-releases
   #:get-latest-release
   #:detect-latest
   #:download-asset
   #:download-asset-to-file
   #:get-token
   #:find-matching-asset
   ;; Common data structures
   #:release
   #:release-p
   #:make-release
   #:release-version
   #:release-tag
   #:release-name
   #:release-assets
   #:release-notes
   #:release-url
   #:release-prerelease-p
   #:release-draft-p
   #:asset
   #:asset-p
   #:make-asset
   #:asset-name
   #:asset-url
   #:asset-size
   #:asset-download-url
   #:asset-content-type
   ;; Provider-specific errors
   #:github-error
   #:gitlab-error
   #:update-failed
   #:update-failed-message
   #:update-failed-original-error
   ;; Configuration
   #:*github-token*
   #:*gitlab-token*
   ;; Validation
   #:validation-error
   #:validation-error-expected
   #:validation-error-actual
   #:validation-error-asset-name
   #:compute-sha256
   #:compute-sha256-file
   #:parse-checksum-file
   #:find-checksum-asset
   #:validate-sha256
   #:*validate-downloads*
   ;; Self-update
   #:update-available-p
   #:download-update
   #:apply-update
   #:update-self
   #:*current-version*
   #:*executable-path*))
