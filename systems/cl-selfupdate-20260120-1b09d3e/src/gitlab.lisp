;;; gitlab.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; GitLab Releases API support.

(in-package #:cl-selfupdate)

;;; Conditions

(define-condition gitlab-error (error)
  ((status-code :initarg :status-code :reader gitlab-error-status-code)
   (message :initarg :message :reader gitlab-error-message)
   (url :initarg :url :reader gitlab-error-url))
  (:report (lambda (c stream)
             (format stream "GitLab API error (~A): ~A~@[ URL: ~A~]"
                     (gitlab-error-status-code c)
                     (gitlab-error-message c)
                     (gitlab-error-url c)))))

;;; HTTP utilities

(defun gitlab-project-id (owner repo)
  "Create URL-encoded project ID from owner and repo.
GitLab uses 'owner/repo' as the project identifier."
  (quri:url-encode (format nil "~A/~A" owner repo)))

(defun gitlab-request (provider endpoint &key (method :get))
  "Make an authenticated request to the GitLab API."
  (let* ((url (if (alexandria:starts-with-subseq "https://" endpoint)
                  endpoint
                  (format nil "~A~A" (provider-api-base provider) endpoint)))
         (token (get-token provider))
         (headers `(("User-Agent" . "cl-selfupdate")
                    ,@(when token
                        `(("PRIVATE-TOKEN" . ,token))))))
    (handler-case
        (multiple-value-bind (body status response-headers)
            (do-http-request url :method method :headers headers)
          (declare (ignore response-headers))
          (if (<= 200 status 299)
              (jsown:parse body)
              (error 'gitlab-error
                     :status-code status
                     :message (format nil "HTTP ~A" status)
                     :url url)))
      (http-request-error (e)
        (error 'gitlab-error
               :status-code (http-error-status-code e)
               :message (format nil "Request failed: ~A" e)
               :url url)))))

;;; Parsing functions

(defun parse-gitlab-release (json)
  "Parse a GitLab release JSON object into a release struct."
  (let ((tag (jsown:val json "tag_name")))
    (make-release
     :version (handler-case
                  (semver:read-version-from-string
                   (cl-ppcre:regex-replace "^v" tag ""))
                (error () nil))
     :tag tag
     :name (jsown:val json "name")
     :notes (jsown:val json "description")
     :url (ignore-errors (jsown:val json "_links" "self"))
     :prerelease-p (jsown:val json "upcoming_release")
     :draft-p nil  ; GitLab doesn't have draft releases
     :assets (parse-gitlab-assets json))))

(defun parse-gitlab-assets (release-json)
  "Parse assets from a GitLab release.
GitLab stores assets in 'assets.links' and 'assets.sources'."
  (let ((assets-json (ignore-errors (jsown:val release-json "assets")))
        (result '()))
    (when assets-json
      ;; Parse links (uploaded files)
      (let ((links (ignore-errors (jsown:val assets-json "links"))))
        (when links
          (dolist (link links)
            (push (make-asset
                   :name (jsown:val link "name")
                   :url (jsown:val link "url")
                   :size 0  ; GitLab doesn't provide size in list
                   :download-url (or (ignore-errors (jsown:val link "direct_asset_url"))
                                     (jsown:val link "url"))
                   :content-type nil)
                  result))))
      ;; Parse sources (auto-generated archives)
      (let ((sources (ignore-errors (jsown:val assets-json "sources"))))
        (when sources
          (dolist (source sources)
            (let ((format (jsown:val source "format")))
              (push (make-asset
                     :name (format nil "source.~A" format)
                     :url (jsown:val source "url")
                     :size 0
                     :download-url (jsown:val source "url")
                     :content-type (cond
                                     ((string= format "zip") "application/zip")
                                     ((string= format "tar.gz") "application/gzip")
                                     (t nil)))
                    result))))))
    (nreverse result)))

;;; Generic function methods for GitLab

(defmethod list-releases ((provider gitlab-provider) owner repo &key (per-page 30))
  "List releases for a GitLab project."
  (let* ((project-id (gitlab-project-id owner repo))
         (endpoint (format nil "/projects/~A/releases?per_page=~A" project-id per-page))
         (json (gitlab-request provider endpoint)))
    (mapcar #'parse-gitlab-release json)))

(defmethod get-latest-release ((provider gitlab-provider) owner repo)
  "Get the latest release for a GitLab project."
  (let* ((project-id (gitlab-project-id owner repo))
         (endpoint (format nil "/projects/~A/releases/permalink/latest" project-id))
         (json (gitlab-request provider endpoint)))
    (parse-gitlab-release json)))

(defmethod detect-latest ((provider gitlab-provider) owner repo &key include-prerelease)
  "Find the latest release with a valid semver version.
Returns the release with the highest version number."
  (let* ((releases (list-releases provider owner repo))
         (valid-releases
           (remove-if-not
            (lambda (rel)
              (and (release-version rel)
                   (or include-prerelease
                       (not (release-prerelease-p rel)))))
            releases)))
    ;; Sort by version descending and return the highest
    (first (sort valid-releases #'semver:version>
                 :key #'release-version))))

(defmethod download-asset ((provider gitlab-provider) asset &key output-path)
  "Download a GitLab asset to the specified path or return as octets."
  (let* ((url (or (asset-download-url asset) (asset-url asset)))
         (token (get-token provider))
         (headers `(("User-Agent" . "cl-selfupdate")
                    ,@(when token
                        `(("PRIVATE-TOKEN" . ,token))))))
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
              (error 'gitlab-error
                     :status-code status
                     :message "Failed to download asset"
                     :url url)))
      (http-request-error (e)
        (error 'gitlab-error
               :status-code (http-error-status-code e)
               :message "Failed to download asset"
               :url url)))))

(defmethod download-asset-to-file ((provider gitlab-provider) asset output-path
                                   &key compute-hash)
  "Stream download a GitLab asset to file, optionally computing SHA256 hash."
  (let* ((url (or (asset-download-url asset) (asset-url asset)))
         (token (get-token provider))
         (headers `(("User-Agent" . "cl-selfupdate")
                    ,@(when token
                        `(("PRIVATE-TOKEN" . ,token)))))
         (digest (when compute-hash
                   (ironclad:make-digest :sha256))))
    (handler-case
        (multiple-value-bind (stream status response-headers)
            (do-http-get-stream url :headers headers)
          (declare (ignore response-headers))
          (unless (<= 200 status 299)
            (when stream (close stream))
            (error 'gitlab-error
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
        (error 'gitlab-error
               :status-code (http-error-status-code e)
               :message "Failed to download asset"
               :url url)))
    (if compute-hash
        (values output-path (ironclad:byte-array-to-hex-string
                             (ironclad:produce-digest digest)))
        output-path)))
