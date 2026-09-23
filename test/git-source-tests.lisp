;;; git-source-tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Unit tests for git+ sources, OCI installs, HTTP, and terminal progress.

(defpackage :ocicl-git-source-tests
  (:use :cl)
  (:export #:run-all-tests #:*test-failed*))

(in-package :ocicl-git-source-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defmacro with-environment ((&rest bindings) &body body)
  "Run BODY with each (NAME VALUE) of BINDINGS in the environment, NIL
meaning unset, restoring every previous value afterwards."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (list ,@(loop for (name nil) in bindings
                                 collect `(cons ,name (uiop:getenv ,name))))))
       (flet ((%apply-env (name value)
                (if value
                    (sb-posix:setenv name value 1)
                    (sb-posix:unsetenv name))))
         (unwind-protect
              (progn ,@(loop for (name value) in bindings
                             collect `(%apply-env ,name ,value))
                     ,@body)
           (dolist (entry ,saved)
             (%apply-env (car entry) (cdr entry))))))))

(defvar *test-failed* 0)
(defvar *test-passed* 0)

(defmacro check (label form)
  `(handler-case
       (if ,form
           (progn (incf *test-passed*)
                  (format t "PASS  ~A~%" ,label))
           (progn (incf *test-failed*)
                  (format t "FAIL  ~A~%" ,label)))
     (error (e)
       (incf *test-failed*)
       (format t "FAIL  ~A (unexpected error: ~A)~%" ,label e))))

(defmacro check-errors (label form)
  `(handler-case
       (progn ,form
              (incf *test-failed*)
              (format t "FAIL  ~A (no error signaled)~%" ,label))
     (error ()
       (incf *test-passed*)
       (format t "PASS  ~A~%" ,label))))

(defun source= (source url ref subdir)
  (let ((parsed (ocicl::parse-git-source source)))
    (and (equal (getf parsed :url) url)
         (equal (getf parsed :ref) ref)
         (equal (getf parsed :subdir) subdir))))

(defun strip-ansi-sgr (string)
  "Remove ANSI SGR color sequences from STRING for display-width assertions."
  (with-output-to-string (out)
    (loop with length = (length string)
          for index from 0 below length
          for char = (char string index)
          do (if (and (char= char (code-char 27))
                      (< (1+ index) length)
                      (char= (char string (1+ index)) #\[))
                 (progn
                   (incf index)
                   (loop while (and (< (1+ index) length)
                                    (not (char= (char string (1+ index)) #\m)))
                         do (incf index))
                   (when (< (1+ index) length)
                     (incf index)))
                 (write-char char out)))))

(defun run-all-tests ()
  (let ((*test-failed* 0))
    (declare (special *test-failed*))

    ;; resolve-dependency-name: ASDF dependency specs
    (check "plain string dependency resolves to itself"
           (equal (ocicl::resolve-dependency-name "alexandria") "alexandria"))
    (check ":version form resolves to the system name"
           (equal (ocicl::resolve-dependency-name '(:version "alexandria" "1.0"))
                  "alexandria"))
    (check ":require form resolves to the module name"
           (equal (ocicl::resolve-dependency-name '(:require "sb-posix"))
                  "sb-posix"))
    (check ":feature form resolves to NIL even when the feature is present"
           (null (ocicl::resolve-dependency-name
                  (list :feature (car *features*) "some-system"))))
    (check ":feature-wrapped :require resolves to NIL"
           (null (ocicl::resolve-dependency-name
                  '(:feature :dotcl (:require "dotcl-float")))))

    ;; parse-git-source: user-supplied sources
    (check "plain https URL"
           (source= "git+https://github.com/me/my-lib"
                    "https://github.com/me/my-lib" nil nil))
    (check ".git suffix is part of the URL"
           (source= "git+https://github.com/me/my-lib.git"
                    "https://github.com/me/my-lib.git" nil nil))
    (check "branch ref after @"
           (source= "git+https://github.com/me/my-lib@main"
                    "https://github.com/me/my-lib" "main" nil))
    (check "commit SHA after @"
           (source= "git+https://github.com/me/my-lib@6f6959222b65a3d44a818b12b3a104cf822fcd91"
                    "https://github.com/me/my-lib"
                    "6f6959222b65a3d44a818b12b3a104cf822fcd91" nil))
    (check "ref as fragment parameter"
           (source= "git+https://github.com/me/my-lib#ref=main"
                    "https://github.com/me/my-lib" "main" nil))
    (check "ref with slash via fragment parameter"
           (source= "git+https://github.com/me/my-lib#ref=feature/foo"
                    "https://github.com/me/my-lib" "feature/foo" nil))
    (check "subdirectory parameter"
           (source= "git+https://github.com/me/mono#subdirectory=libs/my-lib"
                    "https://github.com/me/mono" nil "libs/my-lib"))
    (check "ref and subdirectory together"
           (source= "git+https://github.com/me/mono@v1.0#subdirectory=libs/my-lib"
                    "https://github.com/me/mono" "v1.0" "libs/my-lib"))
    (check "ssh URL authority @ is not a ref"
           (source= "git+ssh://git@github.com/me/my-lib"
                    "ssh://git@github.com/me/my-lib" nil nil))
    (check "ssh URL with ref"
           (source= "git+ssh://git@github.com/me/my-lib@v2"
                    "ssh://git@github.com/me/my-lib" "v2" nil))
    (check "scp-style URL without ref"
           (source= "git+git@github.com:my-lib"
                    "git@github.com:my-lib" nil nil))
    (check "scp-style URL with ref"
           (source= "git+git@github.com:me/my-lib@main"
                    "git@github.com:me/my-lib" "main" nil))
    (check-errors "both @REF and #ref= is an error"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib@main#ref=dev"))
    (check-errors "comma is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib@ma,in"))
    (check-errors "dot-dot subdirectory is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/mono#subdirectory=../evil"))
    (check-errors "absolute subdirectory is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/mono#subdirectory=/etc"))
    (check-errors "unknown fragment parameter is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib#egg=foo"))
    (check-errors "non-git source is rejected"
                  (ocicl::parse-git-source "https://github.com/me/my-lib"))
    ;; Argument & transport injection guards (ocicl-j83)
    (check-errors "ext:: transport is rejected"
                  (ocicl::parse-git-source "git+ext::sh"))
    (check-errors "fd:: transport is rejected"
                  (ocicl::parse-git-source "git+fd::7"))
    (check-errors "URL beginning with '-' is rejected"
                  (ocicl::parse-git-source "git+-upload-pack=touch"))
    (check-errors "unsupported URL scheme is rejected"
                  (ocicl::parse-git-source "git+javascript://evil"))
    (check-errors "ref beginning with '-' is rejected"
                  (ocicl::parse-git-source "git+https://github.com/me/my-lib#ref=-x"))
    (check-errors "bare local path (no scheme) is rejected"
                  (ocicl::parse-git-source "git+/etc/passwd"))
    (check "https scheme accepted by validate-git-url"
           (ocicl::validate-git-url "https://github.com/me/my-lib"))
    (check "ssh scheme accepted by validate-git-url"
           (ocicl::validate-git-url "ssh://git@github.com/me/my-lib"))
    (check "file scheme accepted by validate-git-url"
           (ocicl::validate-git-url "file:///home/me/my-lib"))
    (check "scp-style accepted by validate-git-url"
           (ocicl::validate-git-url "git@github.com:me/my-lib"))

    ;; make-git-fullname / parse-git-fullname round trips
    (let ((sha "6f6959222b65a3d44a818b12b3a104cf822fcd91"))
      (check "fullname round trip: bare pin"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (ocicl::make-git-fullname "https://github.com/me/my-lib" sha))
               (and (equal url "https://github.com/me/my-lib")
                    (equal sha2 sha) (null ref) (null subdir))))
      (check "fullname round trip: ref and subdirectory"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (ocicl::make-git-fullname "https://github.com/me/mono" sha
                                            :ref "feature/foo" :subdir "libs/my-lib"))
               (and (equal url "https://github.com/me/mono")
                    (equal sha2 sha)
                    (equal ref "feature/foo")
                    (equal subdir "libs/my-lib"))))
      (check "fullname round trip: ref needing percent-encoding"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (ocicl::make-git-fullname "https://github.com/me/my-lib" sha
                                            :ref "odd&ref=x"))
               (declare (ignore url subdir))
               (and (equal sha2 sha) (equal ref "odd&ref=x"))))
      (check "fullname with subdirectory but no ref parses"
             (multiple-value-bind (url sha2 ref subdir)
                 (ocicl::parse-git-fullname
                  (format nil "git+https://github.com/me/mono@~A#subdirectory=libs/my-lib" sha))
               (and (equal url "https://github.com/me/mono")
                    (equal sha2 sha) (null ref)
                    (equal subdir "libs/my-lib"))))
      (check-errors "fullname without a full SHA pin is rejected"
                    (ocicl::parse-git-fullname "git+https://github.com/me/my-lib@main")))

    ;; git-repo-basename / git-tree-dirname
    (check "basename from https URL"
           (equal (ocicl::git-repo-basename "https://github.com/me/my-lib") "my-lib"))
    (check "basename strips .git and trailing slash"
           (equal (ocicl::git-repo-basename "https://github.com/me/my-lib.git/") "my-lib"))
    (check "basename from scp-style URL"
           (equal (ocicl::git-repo-basename "git@github.com:me/my-lib") "my-lib"))
    (check "basename prefers the subdirectory"
           (equal (ocicl::git-repo-basename "https://github.com/me/mono" "libs/inner") "inner"))
    (check "tree dirname carries a short pin"
           (equal (ocicl::git-tree-dirname "https://github.com/me/my-lib"
                                           "6f6959222b65a3d44a818b12b3a104cf822fcd91")
                  "my-lib-6f69592"))

    ;; strictly-under-systems-dir-p: the deletion guard (ocicl-hih)
    (let ((ocicl::*systems-dir* #p"/home/u/proj/ocicl/"))
      (check "guard accepts a real subdirectory"
             (ocicl::strictly-under-systems-dir-p #p"/home/u/proj/ocicl/foo-abc1234/"))
      (check "guard rejects the parent via .."
             (not (ocicl::strictly-under-systems-dir-p #p"/home/u/proj/ocicl/../")))
      (check "guard rejects a deeper .. escape"
             (not (ocicl::strictly-under-systems-dir-p #p"/home/u/proj/ocicl/../../etc/")))
      (check "guard rejects an absolute path outside"
             (not (ocicl::strictly-under-systems-dir-p #p"/etc/"))))

    ;; Temporary git clones must come back out, read-only pack files and
    ;; all (ocicl-7ql / gh#209).
    (let* ((tree (ocicl::make-temp-ocicl-dl-directory))
           (pack (merge-pathnames ".git/objects/pack/" tree))
           (idx (merge-pathnames "pack-deadbeef.idx" pack)))
      (ensure-directories-exist pack)
      (with-open-file (stream idx :direction :output :if-exists :supersede)
        (write-string "pack" stream))
      (ocicl::clear-read-only-attributes tree)
      (check "clearing read-only attributes leaves the tree in place"
             (uiop:file-exists-p idx))
      (ocicl::delete-git-tree-directory tree)
      (check "a clone with a read-only pack file is deleted"
             (not (uiop:directory-exists-p tree))))

    ;; CA discovery: the standard OpenSSL variables have to work, or one
    ;; binary answers the same question two ways (ocicl-6pg / gh#208).
    (let* ((bundle (merge-pathnames "ca-fixture.pem" (ocicl::make-temp-ocicl-dl-directory)))
           (bundle-name (namestring bundle))
           (bundle-dir (namestring (uiop:pathname-directory-pathname bundle))))
      (with-open-file (stream bundle :direction :output :if-exists :supersede)
        (write-string "-- not a real certificate --" stream))
      (with-environment (("OCICL_CA_FILE" nil) ("OCICL_CA_DIR" nil)
                         ("SSL_CERT_FILE" bundle-name) ("SSL_CERT_DIR" bundle-dir))
        (multiple-value-bind (ca-file ca-dir) (ocicl.http::%resolve-ca-locations)
          (check "SSL_CERT_FILE is used as the CA bundle"
                 (equal (namestring (pathname ca-file)) bundle-name))
          (check "SSL_CERT_DIR is used as the CA directory"
                 (equal (namestring (pathname ca-dir)) bundle-dir))))
      (with-environment (("OCICL_CA_FILE" bundle-name) ("OCICL_CA_DIR" nil)
                         ("SSL_CERT_FILE" "/nonexistent/ocicl-test.pem") ("SSL_CERT_DIR" nil))
        (check "OCICL_CA_FILE outranks SSL_CERT_FILE"
               (equal (ocicl.http::%resolve-ca-locations) bundle-name)))
      (with-environment (("OCICL_CA_FILE" nil) ("OCICL_CA_DIR" nil)
                         ("SSL_CERT_FILE" "/nonexistent/ocicl-test.pem") ("SSL_CERT_DIR" nil))
        (check "a dangling SSL_CERT_FILE falls through to the built-in locations"
               (not (equal (ocicl.http::%resolve-ca-locations) "/nonexistent/ocicl-test.pem"))))
      (with-environment (("OCICL_CA_FILE" "") ("OCICL_CA_DIR" nil)
                         ("SSL_CERT_FILE" bundle-name) ("SSL_CERT_DIR" nil))
        (check "an empty OCICL_CA_FILE is ignored, not treated as a path"
               (equal (namestring (pathname (ocicl.http::%resolve-ca-locations))) bundle-name)))
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname bundle) :validate t))

    ;; A file where the systems directory goes has to say so (ocicl-8dm).
    (let* ((parent (ocicl::make-temp-ocicl-dl-directory))
           (clear-dir (merge-pathnames "ocicl/" parent))
           (blocked-dir (merge-pathnames "blocked/" parent))
           (blocker (merge-pathnames "blocked" parent)))
      (check "a clear path is not reported as blocked"
             (not (ocicl::systems-dir-blocked-by-file clear-dir)))
      (check "ensure-systems-dir creates the directory"
             (progn (ocicl::ensure-systems-dir clear-dir)
                    (uiop:directory-exists-p clear-dir)))
      (check "an existing directory is not reported as blocked"
             (not (ocicl::systems-dir-blocked-by-file clear-dir)))
      (with-open-file (stream blocker :direction :output :if-exists :supersede)
        (write-string "a binary, say" stream))
      (check "a file holding the name is reported as the blocker"
             (equal (namestring (pathname (ocicl::systems-dir-blocked-by-file blocked-dir)))
                    (namestring blocker)))
      (check-errors "ensure-systems-dir refuses to create over a file"
                    (ocicl::ensure-systems-dir blocked-dir))
      (check "the error names the file that is in the way"
             (search (namestring blocker)
                     (handler-case (progn (ocicl::ensure-systems-dir blocked-dir) "")
                       (error (e) (princ-to-string e)))))
      (uiop:delete-directory-tree parent :validate t))

    ;; Registry digest verification helpers (ocicl-01j)
    (check "sha256 of \"abc\" matches the known vector"
           (string= (ocicl::sha256-hex-of-octets
                     (babel:string-to-octets "abc" :encoding :utf-8))
                    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (check "sha256-hex-of-file matches sha256-hex-of-octets"
           (uiop:with-temporary-file (:pathname p :type "bin")
             (with-open-file (out p :direction :output :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
               (write-sequence (babel:string-to-octets "hello ocicl" :encoding :utf-8) out))
             (string= (ocicl::sha256-hex-of-file p)
                      (ocicl::sha256-hex-of-octets
                       (babel:string-to-octets "hello ocicl" :encoding :utf-8)))))
    (check "parse-oci-digest accepts a well-formed digest"
           (equal (ocicl::parse-oci-digest
                   "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
                  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (check "parse-oci-digest uppercases-normalizes"
           (equal (ocicl::parse-oci-digest
                   "sha256:BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD")
                  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (check "parse-oci-digest rejects wrong length"
           (null (ocicl::parse-oci-digest "sha256:abcd")))
    (check "parse-oci-digest rejects non-hex"
           (null (ocicl::parse-oci-digest
                  "sha256:zzzz16bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))
    (check "parse-oci-digest rejects a mutable tag"
           (null (ocicl::parse-oci-digest "latest")))
    (check "require-oci-digest canonicalizes a valid digest"
           (equal (ocicl::require-oci-digest
                   "sha256:BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD" "x")
                  "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
    (check-errors "require-oci-digest rejects a digest with a comma"
                  (ocicl::require-oci-digest "sha256:ab,cd" "x"))
    (check-errors "require-oci-digest rejects a non-digest"
                  (ocicl::require-oci-digest "latest" "x"))
    (check "a one-byte change is detected"
           (not (string= (ocicl::sha256-hex-of-octets (babel:string-to-octets "abc" :encoding :utf-8))
                         (ocicl::sha256-hex-of-octets (babel:string-to-octets "abd" :encoding :utf-8)))))

    ;; Install download concurrency is bounded by both the terminal and CPU.
    (check "download concurrency uses 75 percent of visible rows"
           (= (ocicl::select-download-concurrency 16 24 nil) 18))
    (check "download concurrency never exceeds twice the processor count"
           (= (ocicl::select-download-concurrency 8 100 nil) 16))
    (check "non-interactive concurrency uses the processor ceiling"
           (= (ocicl::select-download-concurrency 8 nil nil) 16))
    (check "download concurrency override can lower the automatic value"
           (= (ocicl::select-download-concurrency 8 24 "5") 5))
    (check "download concurrency override cannot exceed the processor ceiling"
           (= (ocicl::select-download-concurrency 8 24 "99") 16))
    (check "download concurrency override supports serial operation"
           (= (ocicl::select-download-concurrency 8 24 "1") 1))
    (check-errors "download concurrency override rejects zero"
                  (ocicl::select-download-concurrency 8 24 "0"))
    (check-errors "download concurrency override rejects non-numbers"
                  (ocicl::select-download-concurrency 8 24 "many"))

    ;; Progress rows fill the available width without relying on color alone.
    (let* ((plain-80 (ocicl::render-download-progress-row
                      "alexandria" 512 1024 80 :state :downloading))
           (plain-60 (ocicl::render-download-progress-row
                      "a-system-with-a-very-long-name" 512 nil 60
                      :state :downloading))
           (colored (ocicl::render-download-progress-row
                     "alexandria" 1024 1024 80 :state :done :color t))
           (failed (ocicl::render-download-progress-row
                    "alexandria" 100 nil 60 :state :failed)))
      (check "determinate progress row occupies exactly 80 columns"
             (= (length plain-80) 80))
      (check "determinate progress row includes a percentage"
             (search "50%" plain-80))
      (check "indeterminate progress row occupies exactly 60 columns"
             (= (length plain-60) 60))
      (check "indeterminate progress row includes downloaded bytes"
             (search "512 B" plain-60))
      (check "colored progress preserves an 80-column visible width"
             (= (length (strip-ansi-sgr colored)) 80))
      (check "colored progress includes ANSI styling"
             (position (code-char 27) colored))
      (check "progress uses a hash track instead of dense block glyphs"
             (and (position #\# plain-80)
                  (not (position (code-char #x2588) plain-80))
                  (not (position (code-char #x2591) plain-80))))
      (check "completed progress has a textual state"
             (search "done" (strip-ansi-sgr colored) :test #'char-equal))
      (check "publish progress has an installing state"
             (search "installing"
                     (ocicl::render-download-progress-row
                      "alexandria" 1024 1024 80 :state :installing)
                     :test #'char-equal))
      (check "progress bars have one fixed width across states"
             (let ((waiting
                     (ocicl::render-download-progress-row
                      "alexandria" 0 nil 80 :state :waiting))
                   (done
                     (ocicl::render-download-progress-row
                      "alexandria" 1024 1024 80 :state :done)))
               (= (position #\] waiting) (position #\] done))))
      (check "waiting work has an empty bar rather than fake progress"
             (not (position
                   #\#
                   (ocicl::render-download-progress-row
                    "alexandria" 0 nil 80 :state :waiting))))
      (check "failed progress has a textual state without color"
             (search "failed" failed :test #'char-equal)))
    (multiple-value-bind (columns rows)
        (ocicl::parse-terminal-size "24 80")
      (check "terminal size parser returns columns then rows"
             (and (= columns 80) (= rows 24))))
    (multiple-value-bind (columns rows)
        (ocicl::parse-terminal-size "not a size")
      (check "terminal size parser rejects malformed output"
             (and (null columns) (null rows))))
    (check "new progress displays have no cached terminal dimensions"
           (null
            (ocicl::progress-display-last-size-check
             (ocicl::make-progress-display))))
    (let* ((progress
             (loop for name in '("one" "two" "three" "four" "five")
                   collect (ocicl::make-download-progress
                            :name name :downloaded 0 :state :waiting)))
           (rows (ocicl::render-download-progress-rows progress 60 4)))
      (check "resized progress display uses at most 75 percent of rows"
             (= (length rows) 3))
      (check "resized progress display keeps every line at terminal width"
             (every (lambda (row) (= (length row) 60)) rows))
      (check "resized progress display summarizes hidden downloads"
             (search "+3 more downloads" (car (last rows)))))
    (let ((summary
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (ocicl::*color* nil))
                (ocicl::report-installed-batch
                 '("one" "two" "three"))))))
      (check "interactive completion uses one compact batch summary"
             (and (search "installed 3 systems" summary)
                  (= (count #\Newline summary) 1))))

    ;; The worker pool is bounded and preserves task order for coordinator
    ;; commits, even though the work itself completes out of order.
    (let ((lock (bt:make-lock "parallel-map-test"))
          (active 0)
          (peak 0))
      (check "bounded parallel map preserves result order"
             (equal
              (ocicl::bounded-parallel-map
               (lambda (number)
                 (bt:with-lock-held (lock)
                   (incf active)
                   (setf peak (max peak active)))
                 (sleep 0.02)
                 (bt:with-lock-held (lock)
                   (decf active))
                 (* number number))
               '(1 2 3 4 5 6) 3)
              '(1 4 9 16 25 36)))
      (check "bounded parallel map performs independent work concurrently"
             (> peak 1))
      (check "bounded parallel map respects its worker limit"
             (<= peak 3)))
    (check-errors "bounded parallel map propagates worker errors"
                  (ocicl::bounded-parallel-map
                   (lambda (item)
                     (if (eq item :bad) (error "bad task") item))
                   '(:good :bad :unreached) 2))

    ;; temp download directories are created exclusively
    (let ((dir-1 (ocicl::make-temp-ocicl-dl-directory))
          (dir-2 (ocicl::make-temp-ocicl-dl-directory)))
      (unwind-protect
           (progn
             (check "temp download dir is created on the spot"
                    (uiop:directory-exists-p dir-1))
             (check "temp download dirs are distinct"
                    (not (equal dir-1 dir-2))))
        (uiop:delete-empty-directory dir-1)
        (uiop:delete-empty-directory dir-2)))
    (let ((directories
            (ocicl::bounded-parallel-map
             (lambda (number)
               (declare (ignore number))
               (ocicl::make-temp-ocicl-dl-directory))
             (loop for number below 24 collect number)
             8)))
      (unwind-protect
           (check "parallel temp download dirs are all distinct"
                  (= (length (remove-duplicates directories :test #'equal))
                     (length directories)))
        (dolist (directory (remove-duplicates directories :test #'equal))
          (when (probe-file directory)
            (uiop:delete-directory-tree directory :validate t)))))

    ;; Registry workers stage into private directories.  Only the coordinator
    ;; commit is allowed to mutate the systems table and shared systems tree.
    (let ((real-fetch (fdefinition 'ocicl::fetch-and-extract-layer))
          (destination (ocicl::make-temp-ocicl-dl-directory))
          (staged nil)
          (states nil))
      (unwind-protect
           (let ((ocicl::*systems-dir* destination)
                 (ocicl::*ocicl-systems* (make-hash-table :test #'equal))
                 (ocicl::*ocicl-registries* '("registry.invalid/ocicl")))
             (setf (fdefinition 'ocicl::fetch-and-extract-layer)
                   (lambda (registry system tag directory &key progress)
                     (declare (ignore registry system tag))
                     (let ((tree (merge-pathnames "fixture-20260913/" directory)))
                       (ensure-directories-exist tree)
                       (with-open-file
                           (out (merge-pathnames "fixture.asd" tree)
                                :direction :output :if-exists :supersede)
                         (write-line "(asdf:defsystem \"fixture\")" out)))
                     (when progress
                       (funcall progress :done 128 128))
                     (concatenate 'string "sha256:" (make-string 64 :initial-element #\a))))
             (setf staged
                   (ocicl::stage-system-download
                    "fixture"
                    (lambda (state downloaded total)
                      (declare (ignore downloaded total))
                      (push state states))))
             (check "staging does not mutate the systems table"
                    (zerop (hash-table-count ocicl::*ocicl-systems*)))
             (check "staging keeps the extracted tree private"
                    (probe-file
                     (merge-pathnames "fixture-20260913/fixture.asd"
                                      (ocicl::staged-download-directory staged))))
             (ocicl::commit-staged-download staged :update-csv nil)
             (check "coordinator commit registers the staged system"
                    (gethash "fixture" ocicl::*ocicl-systems*))
             (check "coordinator commit copies into the shared systems tree"
                    (probe-file
                     (merge-pathnames "fixture-20260913/fixture.asd" destination)))
             (check "staged download reports ready for coordinator publish"
                    (eq (first states) :ready)))
        (setf (fdefinition 'ocicl::fetch-and-extract-layer) real-fetch)
        (when (and staged
                   (probe-file (ocicl::staged-download-directory staged)))
          (uiop:delete-directory-tree
           (ocicl::staged-download-directory staged) :validate t))
        (uiop:delete-directory-tree destination :validate t)))
    (let ((real-fetch (fdefinition 'ocicl::fetch-and-extract-layer))
          (destination (ocicl::make-temp-ocicl-dl-directory))
          (staged nil)
          (seen-request nil)
          (digest (make-string 64 :initial-element #\b)))
      (unwind-protect
           (let ((ocicl::*systems-dir* destination)
                 (ocicl::*ocicl-systems* (make-hash-table :test #'equal)))
             (setf (fdefinition 'ocicl::fetch-and-extract-layer)
                   (lambda (registry system tag directory &key progress)
                     (declare (ignore progress))
                     (setf seen-request (list registry system tag))
                     (let ((tree (merge-pathnames "fixture-pinned/" directory)))
                       (ensure-directories-exist tree)
                       (with-open-file
                           (out (merge-pathnames "fixture.asd" tree)
                                :direction :output :if-exists :supersede)
                         (write-line "(asdf:defsystem \"fixture\")" out)))
                     (format nil "sha256:~A" digest)))
             (setf staged
                   (ocicl::stage-pinned-download
                    (format nil "registry.invalid/ocicl/fixture@sha256:~A"
                            digest)
                    "fixture"))
             (check "pinned staging requests the recorded registry digest"
                    (equal seen-request
                           (list "registry.invalid/ocicl" "fixture"
                                 (format nil "sha256:~A" digest))))
             (ocicl::commit-staged-download
              staged :update-csv nil :print-result nil)
             (check "pinned coordinator commit preserves its existing CSV rows"
                    (zerop (hash-table-count ocicl::*ocicl-systems*)))
             (check "pinned coordinator commit restores the recorded tree"
                    (probe-file
                     (merge-pathnames "fixture-pinned/fixture.asd"
                                      destination))))
        (setf (fdefinition 'ocicl::fetch-and-extract-layer) real-fetch)
        (when (and staged
                   (probe-file (ocicl::staged-download-directory staged)))
          (uiop:delete-directory-tree
           (ocicl::staged-download-directory staged) :validate t))
        (uiop:delete-directory-tree destination :validate t)))

    ;; Download response streams are owned by the blob copier and must be
    ;; closed after use so repeated or parallel downloads do not leak sockets.
    (uiop:with-temporary-file (:pathname source :type "bin")
      (with-open-file (out source :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
        (write-sequence #(1 2 3 4) out))
      (uiop:with-temporary-file (:pathname destination :type "bin")
        (let ((input (open source :direction :input
                                 :element-type '(unsigned-byte 8)))
              (updates nil))
          (ocicl::copy-http-response-to-file
           input destination
           :total 4
           :progress (lambda (downloaded total)
                       (push (list downloaded total) updates)))
          (check "blob copier closes the response stream"
                 (not (open-stream-p input)))
          (check "blob copier preserves response bytes"
                 (equalp (alexandria:read-file-into-byte-vector destination)
                         #(1 2 3 4)))
          (check "blob copier reports byte progress and total"
                 (equal (first updates) '(4 4))))))

    ;; http-get retry behavior (stub the single-attempt fetch and the sleep)
    (let ((headers
            (ocicl.http::header-alist->hash-table
             '((:content-length . "4096")
               ("Docker-Content-Digest" . "sha256:abc")))))
      (check "HTTP response header names are normalized to lowercase strings"
             (and (string= (gethash "content-length" headers) "4096")
                  (string= (gethash "docker-content-digest" headers)
                           "sha256:abc")))
      (check "normalized Content-Length drives determinate progress"
             (= (ocicl::response-content-length headers) 4096)))

    (let ((stream (make-string-output-stream)))
      (let ((ocicl.http::*retry-output* stream))
        (ocicl::bounded-parallel-map
         (lambda (number)
           (let ((ocicl.http::*retry-output* stream))
             (ocicl.http::%write-retry-diagnostic
              "temporary failure" (format nil "https://example/~D" number) 1)))
         (loop for number below 20 collect number)
         4))
      (let ((lines (remove ""
                           (uiop:split-string
                            (get-output-stream-string stream)
                            :separator '(#\Newline #\Return))
                           :test #'string=)))
        (check "parallel HTTP retry diagnostics remain whole lines"
               (and (= (length lines) 20)
                    (every (lambda (line)
                             (and (search "temporary failure" line)
                                  (search "retrying in 1s" line)))
                           lines)))))

    (let ((real-request (fdefinition 'drakma:http-request))
          (lock (bt:make-lock "drakma header stream test"))
          (entered 0)
          (stream-one (make-string-output-stream))
          (stream-two (make-string-output-stream)))
      (unwind-protect
           (progn
             (setf (fdefinition 'drakma:http-request)
                   (lambda (url &rest arguments &key &allow-other-keys)
                     (declare (ignore arguments))
                     (bt:with-lock-held (lock)
                       (incf entered))
                     (loop until (bt:with-lock-held (lock) (= entered 2))
                           do (sleep 0.001))
                     (values
                      (vector
                       (if (eq drakma:*header-stream*
                               (if (search "one" url) stream-one stream-two))
                           1
                           0))
                      200 nil nil nil)))
             (check "parallel HTTP requests keep verbose streams thread-local"
                    (every
                     (lambda (result) (= (aref result 0) 1))
                     (ocicl::bounded-parallel-map
                      (lambda (task)
                        (ocicl.http::%http-get-once
                         (car task) :force-binary t :verbose (cdr task)))
                      (list (cons "https://one.invalid/blob" stream-one)
                            (cons "https://two.invalid/blob" stream-two))
                      2))))
        (setf (fdefinition 'drakma:http-request) real-request)))

    (let ((real-once (fdefinition 'ocicl.http::%http-get-once))
          (real-sleep (fdefinition 'ocicl.http::%sleep-before-retry))
          (calls 0))
      (unwind-protect
           (progn
             (setf (fdefinition 'ocicl.http::%sleep-before-retry)
                   (lambda (what url attempt)
                     (declare (ignore what url attempt))))

             (setf calls 0
                   (fdefinition 'ocicl.http::%http-get-once)
                   (lambda (url &key &allow-other-keys)
                     (declare (ignore url))
                     (incf calls)
                     (if (< calls 2)
                         (error 'ocicl.http::http-fetch-error :message "boom")
                         (values "ok" 200 (make-hash-table)))))
             (check "http-get retries a transient connection error"
                    (and (equal (ocicl.http:http-get "https://x/y") "ok")
                         (= calls 2)))

             (setf calls 0
                   (fdefinition 'ocicl.http::%http-get-once)
                   (lambda (url &key &allow-other-keys)
                     (declare (ignore url))
                     (incf calls)
                     (if (< calls 3)
                         (values "err" 503 (make-hash-table))
                         (values "ok" 200 (make-hash-table)))))
             (check "http-get retries HTTP 503"
                    (and (equal (ocicl.http:http-get "https://x/y") "ok")
                         (= calls 3)))

             (setf calls 0
                   (fdefinition 'ocicl.http::%http-get-once)
                   (lambda (url &key &allow-other-keys)
                     (declare (ignore url))
                     (incf calls)
                     (values "nope" 404 (make-hash-table))))
             (check-errors "http-get signals on HTTP 404"
                           (ocicl.http:http-get "https://x/y"))
             (check "http-get does not retry HTTP 404"
                    (= calls 1))

             (setf calls 0
                   (fdefinition 'ocicl.http::%http-get-once)
                   (lambda (url &key &allow-other-keys)
                     (declare (ignore url))
                     (incf calls)
                     (error 'ocicl.http::tls-verification-failure :message "bad cert")))
             (check-errors "http-get signals on TLS verification failure"
                           (ocicl.http:http-get "https://x/y"))
             (check "http-get does not retry a TLS verification failure"
                    (= calls 1)))
        (setf (fdefinition 'ocicl.http::%http-get-once) real-once
              (fdefinition 'ocicl.http::%sleep-before-retry) real-sleep)))

    ;; with-transient-retries covers the streamed-download body phase that
    ;; http-get's internal retry cannot see (a mid-body connection reset).
    (let ((real-sleep (fdefinition 'ocicl.http::%sleep-before-retry))
          (calls 0))
      (unwind-protect
           (progn
             (setf (fdefinition 'ocicl.http::%sleep-before-retry)
                   (lambda (what url attempt)
                     (declare (ignore what url attempt))))

             (setf calls 0)
             (check "with-transient-retries retries a mid-body stream error"
                    (and (equal (ocicl.http:with-transient-retries
                                    (:url "https://x/blob")
                                  (incf calls)
                                  (if (< calls 3)
                                      (error 'stream-error :stream *terminal-io*)
                                      :done))
                                :done)
                         (= calls 3)))

             (setf calls 0)
             (check "with-transient-retries retries a digest mismatch"
                    (and (equal (ocicl.http:with-transient-retries
                                    (:url "https://x/blob")
                                  (incf calls)
                                  (if (< calls 2)
                                      (error "blob digest mismatch for x")
                                      :verified))
                                :verified)
                         (= calls 2)))

             (setf calls 0)
             (check-errors "with-transient-retries rethrows after exhausting attempts"
                           (ocicl.http:with-transient-retries
                               (:url "https://x/blob")
                             (incf calls)
                             (error "always failing")))
             (check "with-transient-retries stops after max retries"
                    (= calls (1+ ocicl.http::*http-max-retries*)))

             (setf calls 0)
             (check-errors "with-transient-retries passes through TLS failures"
                           (ocicl.http:with-transient-retries
                               (:url "https://x/blob")
                             (incf calls)
                             (error 'ocicl.http::tls-verification-failure
                                    :message "bad cert")))
             (check "with-transient-retries never retries a TLS failure"
                    (= calls 1))

             (setf calls 0)
             (check-errors "with-transient-retries passes through HTTP 404"
                           (ocicl.http:with-transient-retries
                               (:url "https://x/blob")
                             (incf calls)
                             (error 'ocicl.http::http-status-error
                                    :status 404 :message "HTTP 404")))
             (check "with-transient-retries never retries a 404"
                    (= calls 1)))
        (setf (fdefinition 'ocicl.http::%sleep-before-retry) real-sleep)))

    (format t "~%~D passed, ~D failed~%" *test-passed* *test-failed*)
    *test-failed*))
