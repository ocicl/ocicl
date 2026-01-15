;;; github.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; GitHub Releases API support - CLOS methods for github-provider.

(in-package #:cl-selfupdate)

;;; Conditions

(define-condition github-error (error)
  ((status-code :initarg :status-code :reader github-error-status-code)
   (message :initarg :message :reader github-error-message)
   (url :initarg :url :reader github-error-url))
  (:report (lambda (c stream)
             (format stream "GitHub API error (~A): ~A~@[ URL: ~A~]"
                     (github-error-status-code c)
                     (github-error-message c)
                     (github-error-url c)))))

;;; HTTP utilities

(defun github-request (provider endpoint &key (method :get))
  "Make an authenticated request to the GitHub API."
  (let* ((url (if (alexandria:starts-with-subseq "https://" endpoint)
                  endpoint
                  (format nil "~A~A" (provider-api-base provider) endpoint)))
         (token (get-token provider))
         (headers `(("Accept" . "application/vnd.github+json")
                    ("User-Agent" . "cl-selfupdate")
                    ("X-GitHub-Api-Version" . "2022-11-28")
                    ,@(when token
                        `(("Authorization" . ,(format nil "Bearer ~A" token)))))))
    (handler-case
        (multiple-value-bind (body status response-headers)
            (do-http-request url :method method :headers headers)
          (declare (ignore response-headers))
          (if (<= 200 status 299)
              (jsown:parse body)
              (error 'github-error
                     :status-code status
                     :message (format nil "HTTP ~A" status)
                     :url url)))
      (http-request-error (e)
        (error 'github-error
               :status-code (http-error-status-code e)
               :message (format nil "Request failed: ~A" e)
               :url url)))))

;;; Parsing functions

(defun parse-github-release (json)
  "Parse a GitHub release JSON object into a release struct."
  (let ((tag (jsown:val json "tag_name"))
        (assets-json (jsown:val json "assets")))
    (make-release
     :version (handler-case
                  (semver:read-version-from-string
                   (cl-ppcre:regex-replace "^v" tag ""))
                (error () nil))
     :tag tag
     :name (jsown:val json "name")
     :notes (jsown:val json "body")
     :url (jsown:val json "html_url")
     :prerelease-p (jsown:val json "prerelease")
     :draft-p (jsown:val json "draft")
     :assets (mapcar #'parse-github-asset assets-json))))

(defun parse-github-asset (json)
  "Parse a GitHub asset JSON object into an asset struct."
  (make-asset
   :name (jsown:val json "name")
   :url (jsown:val json "url")
   :size (jsown:val json "size")
   :download-url (jsown:val json "browser_download_url")
   :content-type (jsown:val json "content_type")))

;;; Generic function methods for GitHub

(defmethod list-releases ((provider github-provider) owner repo &key (per-page 30))
  "List releases for a GitHub repository."
  (let* ((endpoint (format nil "/repos/~A/~A/releases?per_page=~A" owner repo per-page))
         (json (github-request provider endpoint)))
    (mapcar #'parse-github-release json)))

(defmethod get-latest-release ((provider github-provider) owner repo)
  "Get the latest release (non-prerelease, non-draft) for a repository."
  (let* ((endpoint (format nil "/repos/~A/~A/releases/latest" owner repo))
         (json (github-request provider endpoint)))
    (parse-github-release json)))

(defmethod detect-latest ((provider github-provider) owner repo &key include-prerelease)
  "Find the latest release with a valid semver version.
Returns the release with the highest version number."
  (let* ((releases (list-releases provider owner repo))
         (valid-releases
           (remove-if-not
            (lambda (rel)
              (and (release-version rel)
                   (not (release-draft-p rel))
                   (or include-prerelease
                       (not (release-prerelease-p rel)))))
            releases)))
    ;; Sort by version descending and return the highest
    (first (sort valid-releases #'semver:version>
                 :key #'release-version))))

(defmethod download-asset ((provider github-provider) asset &key output-path)
  "Download a GitHub asset to the specified path or return as octets."
  (let* ((url (asset-download-url asset))
         (token (get-token provider))
         (headers `(("User-Agent" . "cl-selfupdate")
                    ,@(when token
                        `(("Authorization" . ,(format nil "Bearer ~A" token)))))))
    (handler-case
        (multiple-value-bind (body status)
            (do-http-get url :headers headers)
          (if (<= 200 status 299)
              (if output-path
                  (progn
                    (alexandria:write-byte-vector-into-file body output-path
                                                            :if-exists :supersede)
                    output-path)
                  body)
              (error 'github-error
                     :status-code status
                     :message "Failed to download asset"
                     :url url)))
      (http-request-error (e)
        (error 'github-error
               :status-code (http-error-status-code e)
               :message "Failed to download asset"
               :url url)))))

(defmethod download-asset-to-file ((provider github-provider) asset output-path
                                   &key compute-hash)
  "Stream download a GitHub asset to file, optionally computing SHA256 hash."
  (let* ((url (asset-download-url asset))
         (token (get-token provider))
         (headers `(("User-Agent" . "cl-selfupdate")
                    ,@(when token
                        `(("Authorization" . ,(format nil "Bearer ~A" token))))))
         (digest (when compute-hash
                   (ironclad:make-digest :sha256))))
    (handler-case
        (multiple-value-bind (stream status response-headers)
            (do-http-get-stream url :headers headers)
          (declare (ignore response-headers))
          (unless (<= 200 status 299)
            (when stream (close stream))
            (error 'github-error
                   :status-code status
                   :message "Failed to download asset"
                   :url url))
          (unwind-protect
               (with-open-file (out output-path
                                    :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
                 (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
                   (loop for bytes-read = (read-sequence buffer stream)
                         while (plusp bytes-read)
                         do (write-sequence buffer out :end bytes-read)
                            (when digest
                              (ironclad:update-digest digest buffer :end bytes-read)))))
            (close stream)))
      (http-request-error (e)
        (error 'github-error
               :status-code (http-error-status-code e)
               :message "Failed to download asset"
               :url url)))
    (if compute-hash
        (values output-path (ironclad:byte-array-to-hex-string
                             (ironclad:produce-digest digest)))
        output-path)))
