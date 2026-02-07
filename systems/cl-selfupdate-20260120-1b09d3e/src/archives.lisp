;;; archives.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:cl-selfupdate)

;;; Archive format detection

(defun detect-archive-format (filename)
  "Detect archive format from filename extension.
Returns one of :tar.gz, :tgz, :zip, :gz, :tar, or :unknown."
  (cond
    ((or (alexandria:ends-with-subseq ".tar.gz" filename)
         (alexandria:ends-with-subseq ".tgz" filename))
     :tar.gz)
    ((alexandria:ends-with-subseq ".tar" filename)
     :tar)
    ((alexandria:ends-with-subseq ".zip" filename)
     :zip)
    ((alexandria:ends-with-subseq ".gz" filename)
     :gz)
    (t :unknown)))

;;; Decompression

(defun decompress-gzip (input-data)
  "Decompress gzip data from byte vector or stream, return decompressed octets.
If INPUT-DATA is a stream, reads it entirely into memory first (chipz requires this)."
  (let ((data (etypecase input-data
                ((vector (unsigned-byte 8)) input-data)
                (stream
                 ;; chipz:decompress doesn't work reliably with streams,
                 ;; so read the entire stream into a byte vector first
                 (let* ((length (file-length input-data))
                        (buffer (make-array length :element-type '(unsigned-byte 8))))
                   (read-sequence buffer input-data)
                   buffer)))))
    (chipz:decompress nil 'chipz:gzip data)))

(defun decompress-gzip-file (input-path output-path)
  "Decompress a gzip file to output path."
  (with-open-file (in input-path :element-type '(unsigned-byte 8))
    (let ((decompressed (decompress-gzip in)))
      (alexandria:write-byte-vector-into-file decompressed output-path
                                              :if-exists :supersede)
      output-path)))

;;; Archive extraction

(defun extract-tar-from-stream (input-stream output-dir &key executable-name)
  "Extract a tar archive from stream to output directory.
If EXECUTABLE-NAME is provided, only extract that file and return its path."
  (ensure-directories-exist output-dir)
  (archive:with-open-archive (archive input-stream :direction :input)
    (archive:do-archive-entries (entry archive)
      (let* ((name (archive:name entry))
             (basename (file-namestring name))
             (output-path (merge-pathnames basename output-dir)))
        (when (and (archive:entry-regular-file-p entry)
                   (or (null executable-name)
                       (string= basename executable-name)
                       (search executable-name basename)))
          (with-open-file (out output-path
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
            (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
              (loop for bytes-read = (read-sequence buffer (archive:entry-stream entry))
                    while (plusp bytes-read)
                    do (write-sequence buffer out :end bytes-read))))
          (when executable-name
            (return-from extract-tar-from-stream output-path))))))
  output-dir)

(defun extract-tar-gz (archive-path output-dir &key executable-name)
  "Extract a .tar.gz archive to output directory."
  (with-open-file (gz-stream archive-path :element-type '(unsigned-byte 8))
    (let ((decompressed (decompress-gzip gz-stream)))
      (flexi-streams:with-input-from-sequence (tar-stream decompressed)
        (extract-tar-from-stream tar-stream output-dir
                                 :executable-name executable-name)))))

(defun extract-tar (archive-path output-dir &key executable-name)
  "Extract a .tar archive to output directory."
  (with-open-file (tar-stream archive-path :element-type '(unsigned-byte 8))
    (extract-tar-from-stream tar-stream output-dir
                             :executable-name executable-name)))

(defun extract-zip (archive-path output-dir &key executable-name)
  "Extract a .zip archive to output directory."
  (ensure-directories-exist output-dir)
  (zip:with-zipfile (zipfile archive-path)
    (zip:do-zipfile-entries (name entry zipfile)
      (let* ((basename (file-namestring name))
             (output-path (merge-pathnames basename output-dir)))
        (when (and (plusp (length basename))
                   (not (char= (char name (1- (length name))) #\/))
                   (or (null executable-name)
                       (string= basename executable-name)
                       (search executable-name basename)))
          (alexandria:write-byte-vector-into-file
           (zip:zipfile-entry-contents entry)
           output-path
           :if-exists :supersede)
          (when executable-name
            (return-from extract-zip output-path))))))
  output-dir)

(defun extract-single-gz (archive-path output-path)
  "Extract a single .gz file (not tar.gz) to output path."
  (decompress-gzip-file archive-path output-path))

;;; Main extraction function

(defun extract-archive (archive-path output-dir &key executable-name)
  "Extract an archive based on its format.
If EXECUTABLE-NAME is provided, only extract matching file.
Returns the path to extracted content."
  (let ((format (detect-archive-format (namestring archive-path))))
    (case format
      ((:tar.gz :tgz)
       (extract-tar-gz archive-path output-dir :executable-name executable-name))
      (:tar
       (extract-tar archive-path output-dir :executable-name executable-name))
      (:zip
       (extract-zip archive-path output-dir :executable-name executable-name))
      (:gz
       ;; Single gzip file - decompress to output dir with .gz stripped
       (let* ((name (pathname-name archive-path))
              (output-path (merge-pathnames name output-dir)))
         (extract-single-gz archive-path output-path)))
      (:unknown
       ;; Assume it's a raw binary
       (let ((output-path (merge-pathnames
                           (or executable-name (file-namestring archive-path))
                           output-dir)))
         (uiop:copy-file archive-path output-path)
         output-path))
      (t
       (error "Unsupported archive format: ~A" format)))))
