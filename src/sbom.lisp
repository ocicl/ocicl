;;; sbom.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024, 2025, 2026  Anthony Green <green@moxielogic.com>
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

(defun calculate-sha256 (file)
  "Calculate SHA-256 hash of FILE. Returns hex string."
  (let ((digest (ironclad:digest-file :sha256 file)))
    (ironclad:byte-array-to-hex-string digest))))

(defun extract-version-from-dirname (dirname)
  "Extract version from system directory name (e.g., 'alexandria-20240503-8514d8e' -> '20240503-8514d8e')."
  (let ((parts (uiop:split-string dirname :separator '(#\-))))
    (when (>= (length parts) 2)
      ;; Look for date-like pattern or version number
      (let ((potential-version (second parts)))
        (if (and (> (length parts) 2)
                 (or (cl-ppcre:scan "^\\d{8}$" potential-version)  ; Date format
                     (cl-ppcre:scan "^v?\\d+" potential-version))) ; Version number
            (format nil "~{~a~^-~}" (cdr parts))
            potential-version)))))

(defun extract-package-name-from-dirname (dirname)
  "Extract package name from system directory (e.g., 'alexandria-20240503-8514d8e' -> 'alexandria')."
  (let ((parts (uiop:split-string dirname :separator '(#\-))))
    (when (>= (length parts) 1)
      (first parts))))

(defun infer-license-from-text (license-text)
  "Infer SPDX license identifier from license text. Returns identifier or 'NOASSERTION'."
  (cond
    ((search "MIT License" license-text) "MIT")
    ((search "Apache License, Version 2.0" license-text) "Apache-2.0")
    ((search "GNU GENERAL PUBLIC LICENSE" license-text)
     (cond
       ((search "Version 3" license-text) "GPL-3.0-or-later")
       ((search "Version 2" license-text) "GPL-2.0-or-later")
       (t "GPL-1.0-or-later")))
    ((search "BSD" license-text)
     (cond
       ((search "3-Clause" license-text) "BSD-3-Clause")
       ((search "2-Clause" license-text) "BSD-2-Clause")
       (t "BSD-3-Clause")))
    ((search "ISC License" license-text) "ISC")
    ((search "Mozilla Public License" license-text) "MPL-2.0")
    ((search "Eclipse Public License" license-text) "EPL-1.0")
    ((search "LLGPL" license-text) "LGPL-2.1-or-later")
    ((or (search "Public Domain" license-text)
         (search "public domain" license-text))
     "CC0-1.0")
    (t "NOASSERTION")))

(defun collect-component-info (system-dir csv-systems)
  "Collect information about a vendored component. Returns property list."
  (let* ((system-name (car (last (pathname-directory system-dir))))
         (pkg-name (extract-package-name-from-dirname system-name))
         (version (extract-version-from-dirname system-name))
         (oci-url (find-oci-url-for-directory system-name csv-systems))
         (license-file (find-license-file system-dir))
         (asd-file (find-primary-asd-file system-dir))
         (readme-file (find-readme-file system-dir))
         (license-text nil)
         (license-spdx "NOASSERTION"))

    ;; Try to find license information (reuse logic from licenses.lisp)
    (cond
      (license-file
       (setf license-text (alexandria:read-file-into-string license-file))
       (setf license-spdx (infer-license-from-text license-text)))
      (t
       (let* ((readme-text (when readme-file (extract-license-from-readme readme-file)))
              (asd-comment-text (when (and asd-file (not readme-text))
                                  (extract-license-from-asd-comments asd-file)))
              (has-asd-comment (and asd-comment-text (> (length asd-comment-text) 0)))
              (asd-field-text (when (and asd-file (not readme-text) (not has-asd-comment))
                                (extract-license-field-from-asd asd-file))))
         (setf license-text (or readme-text
                               (and has-asd-comment asd-comment-text)
                               asd-field-text))
         (when license-text
           (setf license-spdx (infer-license-from-text license-text))))))

    ;; Calculate hash of primary .asd file
    (let ((checksum (when asd-file (calculate-sha256 asd-file))))
      (list :name pkg-name
            :full-name system-name
            :version (or version "unknown")
            :license license-spdx
            :oci-url oci-url
            :checksum checksum
            :directory system-dir))))

(defun generate-cyclonedx-json (project-name components)
  "Generate CycloneDX SBOM in JSON format."
  (let ((timestamp (multiple-value-bind (sec min hour day month year)
                       (get-decoded-time)
                     (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                             year month day hour min sec))))
    (with-output-to-string (out)
      (format out "{~%")
      (format out "  \"bomFormat\": \"CycloneDX\",~%")
      (format out "  \"specVersion\": \"1.5\",~%")
      (format out "  \"serialNumber\": \"urn:uuid:~a\",~%"
              (string-downcase
               (format nil "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x"
                       (random (expt 2 32)) (random (expt 2 16))
                       (random (expt 2 16)) (random (expt 2 16))
                       (random (expt 2 48)))))
      (format out "  \"version\": 1,~%")
      (format out "  \"metadata\": {~%")
      (format out "    \"timestamp\": \"~a\",~%" timestamp)
      (format out "    \"tools\": [~%")
      (format out "      {~%")
      (format out "        \"vendor\": \"ocicl\",~%")
      (format out "        \"name\": \"ocicl\",~%")
      (format out "        \"version\": \"~a\"~%" +version+)
      (format out "      }~%")
      (format out "    ],~%")
      (format out "    \"component\": {~%")
      (format out "      \"type\": \"application\",~%")
      (format out "      \"name\": \"~a\"~%" project-name)
      (format out "    }~%")
      (format out "  },~%")
      (format out "  \"components\": [~%")

      (loop for component in components
            for i from 0
            do (format out "    {~%")
               (format out "      \"type\": \"library\",~%")
               (format out "      \"name\": \"~a\",~%" (getf component :name))
               (format out "      \"version\": \"~a\",~%" (getf component :version))
               (when (getf component :oci-url)
                 (format out "      \"purl\": \"pkg:oci/~a@~a\",~%"
                         (getf component :name)
                         (getf component :version)))
               (unless (string= (getf component :license) "NOASSERTION")
                 (format out "      \"licenses\": [~%")
                 (format out "        {~%")
                 (format out "          \"license\": {~%")
                 (format out "            \"id\": \"~a\"~%" (getf component :license))
                 (format out "          }~%")
                 (format out "        }~%")
                 (format out "      ],~%"))
               (when (getf component :checksum)
                 (format out "      \"hashes\": [~%")
                 (format out "        {~%")
                 (format out "          \"alg\": \"SHA-256\",~%")
                 (format out "          \"content\": \"~a\"~%" (getf component :checksum))
                 (format out "        }~%")
                 (format out "      ],~%"))
               (format out "      \"externalReferences\": [~%")
               (when (getf component :oci-url)
                 (format out "        {~%")
                 (format out "          \"type\": \"distribution\",~%")
                 (format out "          \"url\": \"~a\"~%" (getf component :oci-url))
                 (format out "        }~%"))
               (format out "      ]~%")
               (format out "    }~a~%" (if (< i (1- (length components))) "," "")))

      (format out "  ]~%")
      (format out "}~%"))))

(defun generate-spdx-json (project-name components)
  "Generate SPDX SBOM in JSON format."
  (let ((timestamp (multiple-value-bind (sec min hour day month year)
                       (get-decoded-time)
                     (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                             year month day hour min sec))))
    (with-output-to-string (out)
      (format out "{~%")
      (format out "  \"spdxVersion\": \"SPDX-2.3\",~%")
      (format out "  \"dataLicense\": \"CC0-1.0\",~%")
      (format out "  \"SPDXID\": \"SPDXRef-DOCUMENT\",~%")
      (format out "  \"name\": \"~a\",~%" project-name)
      (format out "  \"documentNamespace\": \"https://ocicl.org/sbom/~a-~a\",~%"
              project-name timestamp)
      (format out "  \"creationInfo\": {~%")
      (format out "    \"created\": \"~a\",~%" timestamp)
      (format out "    \"creators\": [~%")
      (format out "      \"Tool: ocicl-~a\"~%" +version+)
      (format out "    ]~%")
      (format out "  },~%")
      (format out "  \"packages\": [~%")

      (loop for component in components
            for i from 0
            for spdx-id = (format nil "SPDXRef-Package-~a" (getf component :name))
            do (format out "    {~%")
               (format out "      \"SPDXID\": \"~a\",~%" spdx-id)
               (format out "      \"name\": \"~a\",~%" (getf component :name))
               (format out "      \"versionInfo\": \"~a\",~%" (getf component :version))
               (format out "      \"downloadLocation\": \"~a\",~%"
                       (or (getf component :oci-url) "NOASSERTION"))
               (format out "      \"filesAnalyzed\": false,~%")
               (format out "      \"licenseConcluded\": \"~a\",~%" (getf component :license))
               (format out "      \"licenseDeclared\": \"~a\",~%" (getf component :license))
               (format out "      \"copyrightText\": \"NOASSERTION\"")
               (when (getf component :checksum)
                 (format out ",~%")
                 (format out "      \"checksums\": [~%")
                 (format out "        {~%")
                 (format out "          \"algorithm\": \"SHA-256\",~%")
                 (format out "          \"checksumValue\": \"~a\"~%" (getf component :checksum))
                 (format out "        }~%")
                 (format out "      ]"))
               (format out "~%    }~a~%" (if (< i (1- (length components))) "," "")))

      (format out "  ]~%")
      (format out "}~%"))))

(defun do-create-sbom (args)
  "Create an SBOM document for the project including all vendored dependencies."
  (let* ((format-arg (when args (first args)))
         (output-file (when (cdr args) (second args)))
         (format-type (cond
                        ((null format-arg) :cyclonedx)  ; Default to CycloneDX
                        ((member format-arg '("cyclonedx" "cdx") :test #'string-equal) :cyclonedx)
                        ((member format-arg '("spdx") :test #'string-equal) :spdx)
                        (t
                         (format *error-output* "Unknown SBOM format: ~a. Use 'cyclonedx' or 'spdx'.~%" format-arg)
                         (uiop:quit 1))))
         (csv-systems *ocicl-systems*)
         (components nil))

    ;; Check if systems/ directory exists
    (unless (probe-file *systems-dir*)
      (format *error-output* "No systems/ directory found. Nothing to collect.~%")
      (uiop:quit 1))

    ;; Determine project name from current directory or ocicl.csv
    (let* ((project-dir (uiop:getcwd))
           (project-name (car (last (pathname-directory project-dir)))))

      ;; Collect component information
      (dolist (system-dir (directory (merge-pathnames "*/" *systems-dir*)))
        (let ((component-info (collect-component-info system-dir csv-systems)))
          (push component-info components)))

      ;; Sort components by name
      (setf components (sort components #'string< :key (lambda (c) (getf c :name))))

      ;; Generate SBOM
      (let ((sbom-content (ecase format-type
                            (:cyclonedx (generate-cyclonedx-json project-name components))
                            (:spdx (generate-spdx-json project-name components)))))

        (if output-file
            ;; Write to file
            (progn
              (with-open-file (out output-file :direction :output :if-exists :supersede)
                (write-string sbom-content out))
              (when *verbose*
                (format t "SBOM written to ~a~%" output-file)))
            ;; Write to stdout
            (write-string sbom-content *standard-output*))

        (when *verbose*
          (format *error-output* "Generated ~a SBOM with ~a component~:p~%"
                  (ecase format-type
                    (:cyclonedx "CycloneDX")
                    (:spdx "SPDX"))
                  (length components)))))))
