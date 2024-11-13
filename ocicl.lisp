;;; ocicl.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024  Anthony Green <green@moxielogic.com>
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

(named-readtables:in-readtable :interpol-syntax)

(require 'sb-introspect)

(defvar *ocicl-registries* (list "ghcr.io/ocicl"))
(defvar *ocicl-globaldir* nil)
(defvar *verbose* nil)
(defvar *force* nil)
(defvar *ocicl-systems* nil)

(defparameter +version+ #.(uiop:read-file-form "version.sexp"))

(defparameter +runtime+
  (let* ((runtime #.(uiop:read-file-string "runtime/ocicl-runtime.lisp"))
         (start (search "UNKNOWN" runtime)))
    (if start
        (concatenate 'string
                     (subseq runtime 0 start)
                     +version+
                     (subseq runtime (+ start 7)))
        runtime)))

(defparameter +asdf+ #.(uiop:read-file-string "runtime/asdf.lisp"))

(opts:define-opts
  (:name :verbose
   :description "produce verbose output"
   :short #\v
   :long "verbose")
  (:name :force
   :description "force action"
   :short #\f
   :long "force")
  (:name :global
   :description "operate on the global system collection"
   :short #\g
   :long "global")
  (:name :registry
   :description "use alternate oci registry"
   :short #\r
   :arg-parser (lambda (arg) (setf *ocicl-registries* (list arg)))
   :meta-var "REGISTRY"
   :default "ghcr.io/ocicl"
   :long "registry"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (option condition))
  (invoke-restart 'skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun split-on-delimeter (line delim)
  (let ((start 0)
        (end 0)
        (result '()))
    (loop for c across line
          for i from 0
          do (if (char= c delim)
                 (progn
                   (setq end i)
                   (push (string-trim " " (subseq line start end)) result)
                   (setq start (+ i 1))))
          finally (push (string-trim " " (subseq line start)) result))
    (nreverse result)))

(defun split-csv-line (line)
  (split-on-delimeter line #\,))

(defun split-lines (line)
  (split-on-delimeter line #\Newline))

(defun read-systems-csv ()
  (let ((systems-file (merge-pathnames (uiop:getcwd) "systems.csv"))
        (ht (make-hash-table :test #'equal)))
    (when (probe-file systems-file)
      (progn
        (dolist (line (uiop:read-file-lines systems-file))
          (let ((vlist (split-csv-line line)))
            (setf (gethash (car vlist) ht) (cons (cadr vlist) (caddr vlist)))
            ))))
    ht))

(defun usage ()
  (opts:describe
   :prefix (format nil "ocicl ~A - copyright (C) 2023-2024 Anthony Green <green@moxielogic.com>" +version+)
   :suffix "Choose from the following ocicl commands:

   help                                Print this help text
   changes [SYSTEM[:VERSION]]...       Display changes
   install [SYSTEM[:VERSION]]...       Install systems
   latest [SYSTEM]...                  Install latest version of systems
   libyear                             Calculate the libyear dependency freshness metric
   list SYSTEM...                      List available system versions
   setup [GLOBALDIR]                   Mandatory ocicl configuration
   version                             Show the ocicl version information

Distributed under the terms of the MIT License"
   :usage-of "ocicl"
   :args     "command"))

(defvar *systems-dir* nil)

(defun debug-log (s)
  (when *verbose*
    (format t "; ~A~%" s)))

(defun get-up-to-first-slash (str)
  (let ((pos (position #\/ str)))
    (if pos
        (values (subseq str 0 pos) pos)
        (values str -1))))

(defun get-repository-name (url)
  (let* ((first-slash-pos (nth-value 1 (get-up-to-first-slash url)))
         (start-pos (1+ first-slash-pos))
         (pos (position #\/ url :start start-pos)))
    (subseq url start-pos (when pos pos))))

(defun get-bearer-token (registry system)
  (handler-case
      (let* ((server (get-up-to-first-slash registry))
             (repository (get-repository-name registry)))
        (cdr (assoc :token
                    (cl-json:decode-json-from-string
                     (dex:get #?"https://${server}/token?scope=repository:${repository}/${system}:pull")))))
    (error (e)
      (declare (ignore e))
      nil)))

(defun do-list (args)
  (handler-case
      (when args
        (dolist (system args)
          (loop for registry in *ocicl-registries*
                do (handler-case
                       (let ((server (get-up-to-first-slash registry))
                             (repository (get-repository-name registry)))
                         (let* ((token (get-bearer-token registry system))
                                (tags
                                  (sort
                                   (cdr (assoc :tags
                                               (cl-json:decode-json-from-string
                                                (dex:get #?"https://${server}/v2/${repository}/${system}/tags/list"
                                                         :headers `(("Authorization" . ,#?"Bearer ${token}"))))))
                                   #'string>)))
                           (format t "~A(~A):~%" system registry)
                           (dolist (tag tags)
                             (format t "~T~A~%" tag))
                           (return)))
                     (error (e)
                       (format t "~T~A(~A) not found~%" system registry))))
          (format t "~%")))
    (sb-int:broken-pipe (e)
      ())))

(defun get-ocicl-dir ()
  "Find the ocicl directory."
  (let ((ocicl-dir (merge-pathnames (make-pathname :directory '(:relative "ocicl"))
                                    (uiop:xdg-data-home))))
    (uiop:ensure-all-directories-exist (list ocicl-dir))
    ocicl-dir))

(eval-when (:load-toplevel :execute)
  (setf *random-state* (make-random-state t)))

(defun random-base36-string ()
  "Return a random base36 (0-9A-Z) string of 8 characters."
  (format nil "~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*)))

(defun get-changes (system version)
  (loop for registry in *ocicl-registries*
        do (handler-case
               (progn
                 (let* ((token (get-bearer-token registry system))
                        (server (get-up-to-first-slash registry))
                        (repository (get-repository-name registry)))
                   (multiple-value-bind (manifest manifest-digest)
                       (get-manifest registry #?"${system}-changes.txt" version)
                     (let* ((digest (cdr (assoc :digest (cadr (assoc :layers manifest)))))
                            (changes (dex:get #?"https://${server}/v2/${repository}/${system}-changes.txt/blobs/${digest}"
                                              :force-string t
                                              :headers `(("Authorization" . ,#?"Bearer ${token}")))))
                       (return-from get-changes changes)))))
             (error (e)
               )))
  (format nil "No documented changes for ~A:~A" system version))

(defun download-system-dependencies (name)
  (let ((*load-verbose* *verbose*)
        (*load-print* *verbose*))
    (let* ((s (asdf:find-system name))
           (deps (asdf:system-depends-on s)))
      (dolist (d deps)
        (unless (or (listp d) (string= "sb-" (subseq d 0 3)))
          (download-system-dependencies d))))))

(defun do-latest (args)
  ;; Make sure the systems directory exists
  (uiop:ensure-all-directories-exist
   (list *systems-dir*))
  (if args
      ;; Download latest systems provided on the command line.
      (dolist (system args)
        (if (position #\: system)
            (progn
              (format uiop:*stderr* "Error: version tag specified for system ~A.~%" system)
              (sb-ext:quit)))
        (unless (download-system (concatenate 'string system ":latest"))
            (progn
              (format uiop:*stderr* "Error: system ~A not found.~%" system)
              (sb-ext:quit))
            (download-system-dependencies system)))
      ;; Download latest versions of all systems.
      (let ((blobs (make-hash-table :test #'equal)))
        (maphash (lambda (key value)
                   (setf (gethash (car value) blobs) key))
                 *ocicl-systems*)
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (let ((system (extract-between-slash-and-at key)))
                     (download-system (concatenate 'string system ":latest"))))
             blobs))))

(defun get-memory-in-gb ()
  #+sbcl(format nil "configured with ~AGB memory" (ceiling (sb-ext:dynamic-space-size) (* 1024 1024 1024)))
  #-sbcl(format nil ""))

(defun do-version (args)
  (declare (ignore args))
  (format t "ocicl version:   ~A~%" +version+)
  (format t "Lisp runtime:    ~A ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version) (get-memory-in-gb))
  (format t "ASDF version:    ~A~%" (asdf:asdf-version)))

(defun do-setup (args)
  (if (or *force*
          (not (probe-file (merge-pathnames (get-ocicl-dir) "ocicl-registry.cfg"))))
      (with-open-file (stream (merge-pathnames (get-ocicl-dir) "ocicl-registry.cfg")
                              :direction :output
                              :if-exists :supersede)
        (write-string (first *ocicl-registries*) stream))
      (format t ";; Preserving existing ~A~%;; Use setup's --force option to override.~%~%" (merge-pathnames (get-ocicl-dir) "ocicl-registry.cfg")))
  (if args
      (let ((original-directory (uiop:getcwd)))
        (unwind-protect
             (handler-case
                 (progn
                   (uiop:chdir (car args))
                   (setf args (list (namestring (uiop:getcwd))))
                   (with-open-file (stream (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")
                                           :direction :output
                                           :if-exists :supersede)
                     (write-string (car args) stream)))
               (error (e)
                 (declare (ignore e))
                 (format uiop:*stderr* "Error: directory ~A does not exist.~%" (car args))
                 (sb-ext:quit)))
          (uiop:chdir original-directory)))
      (let ((old-config-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))
        (when (probe-file old-config-file)
          (delete-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))))

  (let* ((odir (get-ocicl-dir))
         (gdir (or (car args) odir))
         (runtime-source (merge-pathnames odir "ocicl-runtime.lisp"))
         (asdf-source (merge-pathnames odir "asdf.lisp")))
    (with-open-file (stream asdf-source
                            :direction :output
                            :if-exists :supersede)
      (write-string +asdf+ stream))
    (with-open-file (stream runtime-source
                            :direction :output
                            :if-exists :supersede)
      (write-string +runtime+ stream)
      (format t ";; Present the following code to your LISP system at startup, either~%;; by adding it to your implementation's startup file~%;;~T(~~/.sbclrc, ~~/.eclrc, ~~/.abclrc or ~~/.roswell/init.lisp)~%;; or overriding it completely on the command line~%;;~T(eg. sbcl --userinit init.lisp)~%~%#-ocicl~%(when (probe-file ~S)~%  (load ~S))~%(asdf:initialize-source-registry~%  (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))~%" runtime-source runtime-source))))

(defun filter-strings (strings)
  (remove-if (lambda (s) (string= s "latest"))
             strings))

(defun get-versions-since (system version)
  (loop for registry in *ocicl-registries*
        do (handler-case
               (return-from get-versions-since
                 (let ((server (get-up-to-first-slash registry))
                       (repository (get-repository-name registry)))
                   (let* ((token (get-bearer-token registry system))
                          (all-versions
                            (filter-strings
                             (cdr (assoc :tags
                                         (cl-json:decode-json-from-string
                                          (dex:get #?"https://${server}/v2/${repository}/${system}/tags/list"
                                                   :headers `(("Authorization" . ,#?"Bearer ${token}"))))))))
                          (p (position version all-versions :test #'string=)))
                     (when p (cdr (nthcdr p all-versions))))))
             (dexador.error:http-request-forbidden (e)
               (declare (ignore e))))))

(defun number-to-ordinal-suffix (n)
  "Convert an integer to a string with its ordinal suffix."
  (cond ((member n '(11 12 13)) (format nil "~Dth" n))
        ((= (mod n 10) 1) (format nil "~Dst" n))
        ((= (mod n 10) 2) (format nil "~Dnd" n))
        ((= (mod n 10) 3) (format nil "~Drd" n))
        (t (format nil "~Dth" n))))

(defun format-line (project-name nth-change version)
  (let* ((base (format nil "~&==== ~A~A "
                       project-name
                       (if nth-change
                           (format nil ": ~A change~A"
                                   (number-to-ordinal-suffix nth-change)
                                   (if version
                                       (format nil " (~A)" version)
                                       ""))
                           "")))
         (padding-length (max 0 (- 75 (length base))))
         (padding (make-string padding-length :initial-element #\=)))
    (concatenate 'string base padding)))

(defun top-level-directory (path)
  (let ((directory-list (pathname-directory (pathname path))))
    (if (and directory-list (> (length directory-list) 1))
        (second directory-list)
        nil)))

(defun read-file-from-directory (directory filename)
  "Reads a file from the specified directory and filename, returning the content as a string."
  (let ((full-path (uiop:merge-pathnames* filename directory)))
    (uiop:read-file-string full-path)))

(defun parse-date-to-universal-time (date-string)
  "Converts a date string in the format YYYYMMDD to universal time."
  (let ((year (parse-integer date-string :start 0 :end 4))
        (month (parse-integer date-string :start 4 :end 6))
        (day (parse-integer date-string :start 6 :end 8)))
    (encode-universal-time 0 0 0 day month year 0))) ; Assumes time at 00:00:00

(defun get-project-date (key-file)
  (let ((dpos (search "-20" key-file :test #'string=)))
    (if dpos
        (parse-date-to-universal-time (subseq key-file (1+ dpos) (+ dpos 9))))))

(defun difference-in-years (time1 time2)
  "Calculate the difference in years between two universal times."
  (float
   (let ((seconds-per-year 31557600)) ; 365.25 days per year * 24 hours/day * 60 minutes/hour * 60 seconds/minute
     (/ (abs (- time1 time2)) seconds-per-year))))

(defun get-project-name (key-file)
  (let ((tld (top-level-directory key-file)))
    (handler-case
        (let ((pfile (uiop:merge-pathnames* (make-pathname :directory `(:relative ,tld))
                                            (uiop:merge-pathnames* (make-pathname :directory '(:relative "systems"))
                                                                   "_00_OCICL_NAME"))))
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string pfile)))
      ;; If the tgz file doesn't include _00_OCICL_NAME, the infer it from the directory name.
      (file-error (e)
        (let ((last-dash-position (position #\- tld :from-end t)))
          (cond
            ((position #\. (subseq tld last-dash-position))
             (subseq tld 0 last-dash-position))
            (t
             (subseq tld 0 (position #\- (subseq tld 0 last-dash-position)
                                     :from-end t)))))))))

(defun get-project-version (key-file)
  (let ((tld (top-level-directory key-file)))
    (handler-case
        (let ((vfile (uiop:merge-pathnames* (make-pathname :directory `(:relative ,tld))
                                            (uiop:merge-pathnames* (make-pathname :directory '(:relative "systems"))
                                                                   "_00_OCICL_VERSION"))))
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string vfile)))
      (file-error (e)
        ;; If the tgz file doesn't include _00_OCICL_VERSION, the infer it from the directory name.
        (subseq tld (1+ (position #\- tld :from-end t)))))))

(defun round-up-to-decimal (number decimal-places)
  (let* ((adjustment (expt 10 (- decimal-places)))
         (adjusted-number (+ number (* 0.5 adjustment))))
    (format nil "~,vf" decimal-places adjusted-number)))

(defun do-libyear ()
  (let ((projects (make-hash-table :test #'equal)))
    (maphash (lambda (key value)
               (setf (gethash (namestring (uiop:merge-pathnames* (make-pathname :directory `(:relative ,(top-level-directory (cdr value))))
                                                                 (uiop:merge-pathnames* (make-pathname :directory '(:relative "systems"))
                                                                                        "_00_OCICL_VERSION")))
                              projects)
                     key))
             *ocicl-systems*)
    (let ((age 0))
      (maphash (lambda (skey value)
                 (let* ((asd (cdr (gethash value *ocicl-systems*)))
                        (version (get-project-version asd))
                        (project-name (get-project-name asd)))
                   (let* ((newer-versions (get-versions-since value version))
                          (most-recent-version (car (last newer-versions)))
                          (using-date (get-project-date skey)))
                     (if (and using-date
                              most-recent-version
                              (string= (subseq most-recent-version 0 2) "20"))
                         (let ((p-age
                                 (difference-in-years (parse-date-to-universal-time
                                                       (subseq most-recent-version 0 8))
                                                      using-date)))
                           (format t "~A~20T~A libyears (~A days)~%" project-name
                                   (round-up-to-decimal p-age 2)
                                   (round-up-to-decimal (* p-age 365.25) 2))
                           (incf age p-age))))))
               projects)
      (format t "~&~%TOTAL libyears: ~A (~A days)~%"
              (round-up-to-decimal age 2)
              (round-up-to-decimal (* age 365.25) 2)))))

(defun do-changes (args)
  ;; Make sure the systems directory exists
  (uiop:ensure-all-directories-exist
   (list *systems-dir*))
  (if args
      ;; Report on all the systems provided on the command line.
      (dolist (system-maybe-version args)
        (let ((system (car (split-on-delimeter system-maybe-version #\:)))
              (version (cadr (split-on-delimeter system-maybe-version #\:))))
          (let ((asd (cdr (gethash system *ocicl-systems*))))
            (let ((version (or version
                               (and asd (get-project-version asd))))
                  (project-name (or (and asd (get-project-name asd)) system)))
                (let ((versions (get-versions-since system version)))
                  (let ((nth-change 0))
                    (dolist (v versions)
                      (format t "~&~A~%~%~A~%~%" (format-line project-name (incf nth-change) v) (get-changes (mangle system) v)))))))))
      (let ((projects (make-hash-table :test #'equal)))
        (maphash (lambda (key value)
                   (setf (gethash (uiop:merge-pathnames* (make-pathname :directory `(:relative ,(top-level-directory (cdr value))))
                                                         (uiop:merge-pathnames* (make-pathname :directory '(:relative "systems"))
                                                                                "_00_OCICL_VERSION"))
                                  projects)
                         key))
                 *ocicl-systems*)
        (maphash (lambda (skey value)
                   (let ((key (subseq (namestring skey) 8)))
                     (handler-case
                         (let ((version (get-project-version key))
                               (project-name (get-project-name key)))
                           (let ((versions (get-versions-since value version)))
                             (if versions
                                 (let ((nth-change 0))
                                   (dolist (v versions)
                                     (format t "~&~A~%~%~A~%~%" (format-line project-name (incf nth-change) v) (get-changes (mangle value) v))))
                                 (when *verbose* (format t "~A~%" (format-line project-name nil nil))))))
                       (error (e) ()))))
                 projects))))

(defun do-install (args)
  ;; Make sure the systems directory exists
  (uiop:ensure-all-directories-exist
   (list *systems-dir*))
  (if args
      ;; Download the systems provided on the command line.
      (dolist (system args)
        (if (position #\@ system)
            (progn
              (unless (download-and-install system)
                (progn
                  (format uiop:*stderr* "Error: can't download ~A.~%" system)
                  (sb-ext:quit))))
          (let* ((slist (split-on-delimeter system #\:))
                 (name (car slist)))
            (if (download-system system)
                (download-system-dependencies name)
              (progn
                (format uiop:*stderr* "Error: system ~A not found.~%" system)
                (sb-ext:quit))))))
    ;; Download all systems in systems.csv.
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (not (probe-file (concatenate 'string (namestring *systems-dir*) (cdr value))))
                 (format t "; downloading ~A~%" (car value))
                 (download-and-install (car value))))
             *ocicl-systems*)))

(defmethod parent ((file pathname))
  "Return the parent directory of FILE."
  (if (uiop:directory-pathname-p file)
      (uiop:pathname-parent-directory-pathname file)
      (uiop:pathname-directory-pathname file)))

(defun find-workdir (workdir)
  "Search for systems.csv starting from WORKDIR and moving up the directory chain.
   Returns the directory containing systems.csv if found.  If none is
   found, return WORKDIR."
  (let ((dir (truename workdir)))
    (loop
       for path = (merge-pathnames "systems.csv" dir)
       until (probe-file path)
       do (let ((parent-dir (parent dir)))
            ;; Stop if we've reached the root (parent is same as current directory)
            (if (or (null parent-dir) (equal parent-dir dir))
                (return))
            (setf dir parent-dir)))
    (if (probe-file (merge-pathnames "systems.csv" dir))
        dir
      workdir)))

(defun main ()
  (setf *random-state* (make-random-state t))
  (handler-case
      (with-user-abort:with-user-abort

       (let ((config-file (merge-pathnames (get-ocicl-dir) "ocicl-registry.cfg")))
         (when (probe-file config-file)
           (setf *ocicl-registries*
                 (or (with-open-file (in config-file)
                                     (loop for line = (read-line in nil nil)
                                           while line
                                           ;; skip comments
                                           unless (char= #\# (aref line 0))
                                           collect (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                     *ocicl-registries*))))

       (let ((config-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))
         (when (probe-file config-file)
           (setf *ocicl-globaldir* (uiop:ensure-absolute-pathname (uiop:read-file-line config-file)))))

       (let ((workdir (uiop:getcwd)))

         (multiple-value-bind (options free-args)
             (handler-case
                 (handler-bind ((unknown-option #'unknown-option))
                   (opts:get-opts))
               (opts:missing-arg (condition)
                 (format t "fatal: option ~s needs an argument!~%"
                         (option condition)))
               (opts:arg-parser-failed (condition)
                 (format t "fatal: cannot parse ~s as argument of ~s~%"
                         (raw-arg condition)
                         (option condition))))
           (when-option (options :verbose)
                        (setf *verbose* t))
           (when-option (options :force)
                        (setf *force* t))
           (when-option (options :global)
                        (setf workdir (or *ocicl-globaldir* (get-ocicl-dir))))


           (setf workdir (find-workdir workdir))

           (let ((original-directory (uiop:getcwd)))
             (unwind-protect
                 (locally
                  (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
                  (handler-bind
                      (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
                    (uiop:chdir workdir)
                    (setq *ocicl-systems* (read-systems-csv))
                    (setq *systems-dir* (merge-pathnames (make-pathname :directory '(:relative "systems"))
                                                         (uiop:getcwd)))
                    (if (not (> (length free-args) 0))
                        (usage)
                      (let ((cmd (car free-args)))
                        (cond
                         ((string= cmd "help")
                          (usage))
                         ((string= cmd "libyear")
                          (do-libyear))
                         ((string= cmd "changes")
                          (do-changes (cdr free-args)))
                         ((string= cmd "install")
                          (do-install (cdr free-args)))
                         ((string= cmd "latest")
                          (do-latest (cdr free-args)))
                         ((string= cmd "list")
                          (do-list (cdr free-args)))
                         ((string= cmd "setup")
                            (do-setup (cdr free-args)))
                         ((string= cmd "version")
                          (do-version (cdr free-args)))
                         (t (usage)))))))
                   (uiop:chdir original-directory))))))
       (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defun replace-plus-with-string (str)
  (let ((mangled (with-output-to-string (s)
                    (loop for c across str do
                          (if (char= c #\+)
                              (write-string "_plus_" s)
                              (write-char c s))))))
    (if (char= (char mangled (- (length mangled) 1)) #\_)
        (subseq mangled 0 (- (length mangled) 1))
      mangled)))

(defun mangle (str)
  (replace-plus-with-string (car (split-on-delimeter str #\/))))

(defun get-temp-ocicl-dl-pathname ()
  (let ((rdir (format nil "ocicl-~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))
    (merge-pathnames (eval `(make-pathname :directory '(:relative ,rdir)))
                     (uiop:default-temporary-directory))))

(defun find-asd-files (dir)
  "Recursively find all files with the .asd extension in a directory."
  ;; Force a trailing slash to support uiop change in behavior:
  ;; https://github.com/fare/asdf/commit/6138d709eb25bf75c1d1f7dc45a63d174f982321
  (let* ((dir (namestring dir))
         (dir (if (string= (subseq dir (- (length dir) 1)) "/")
                 dir
                 (concatenate 'string dir "/")))
         (systems (list)))
    (labels ((push-asd (dir)
               (debug-log #?"searching ${dir}")
               (dolist (f (uiop:directory-files dir))
                 (when (equal "asd" (pathname-type f))
                   (pushnew f systems)))))
      (uiop:collect-sub*directories dir
                                    t t
                                    (lambda (d) (push-asd d)))
      (push-asd dir))
    systems))

(defun extract-sha256 (str)
  (let* ((start (search "sha256:" str))
         (end (+ start 71))) ;; 7 for "sha256:" and 64 for the hash
    (when start
      (subseq str (+ start 7) end))))

(defun extract-between-slash-and-at (input)
  (let* ((reversed (reverse input))
         (pos-at (position #\@ reversed))
         (pos-slash (position #\/ reversed :start pos-at)))
    (subseq input (- (length input) pos-slash) (- (length input) (1+ pos-at)))))

(defvar *ocicl-systems* nil)

(defun write-systems-csv ()
  (with-open-file (stream (merge-pathnames (uiop:getcwd) "systems.csv")
                          :direction :output
                          :if-exists :supersede)
    (maphash (lambda (key value)
               (format stream "~A, ~A, ~A~%" key (car value) (cdr value)))
             *ocicl-systems*))
  (debug-log "wrote new systems.csv"))

(defun get-manifest (registry system tag)
  (let ((token (get-bearer-token registry system))
        (server (get-up-to-first-slash registry))
        (repository (get-repository-name registry)))
    (multiple-value-bind (body status response-headers)
        (dex:get #?"https://${server}/v2/${repository}/${system}/manifests/${tag}"
                 :force-string t
                 :headers `(("Authorization" . ,#?"Bearer ${token}")
                            ("Accept" . "application/vnd.oci.image.manifest.v1+json,application/vnd.oci.image.index.v1+json")))
      (values (json:decode-json-from-string body) (gethash "docker-content-digest" response-headers)))))

(defun get-blob (registry system tag dl-dir)
  (let* ((token (get-bearer-token registry system))
         (server (get-up-to-first-slash registry))
         (repository (get-repository-name registry)))
    (multiple-value-bind (manifest manifest-digest)
        (get-manifest registry system tag)
      (let* ((digest (cdr (assoc :digest (cadr (assoc :layers manifest)))))
             (input (dex:get #?"https://${server}/v2/${repository}/${system}/blobs/${digest}"
                             :force-binary t
                             :want-stream t
                             :headers `(("Authorization" . ,#?"Bearer ${token}")))))
        (handler-bind
            ((tar-simple-extract:broken-or-circular-links-error
              (lambda (condition)
                (invoke-restart 'continue))))
          (tar:with-open-archive (a input)
                                 (tar-simple-extract:simple-extract-archive a :directory dl-dir)))
        manifest-digest))))

(defun download-and-install (fullname)
  (let ((dl-dir (get-temp-ocicl-dl-pathname)))
    (unwind-protect
         (progn
           (uiop:ensure-all-directories-exist (list dl-dir))
           (uiop:with-current-directory (dl-dir)
             (handler-case
                 (progn
                   (debug-log #?"attempting to pull ${fullname}")
                   (cl-ppcre:register-groups-bind (registry name digest)
                       ("^([^/]+/[^/]+)/([^:@]+)?(?:@sha256:([a-fA-F0-9]+))?" fullname)
                     (let ((manifest-digest (get-blob registry name #?"sha256:${digest}" dl-dir)))
                       (copy-directory:copy dl-dir *systems-dir*))))
               (error (e)
                 (format t "; error downloading and installing ~A~%" fullname)
                 (debug-log e)
                 nil))))
      (uiop:delete-directory-tree dl-dir :validate t))))

(defun download-system (system)
  (let* ((slist (split-on-delimeter system #\:))
         (name (car slist))
         (mangled-name (mangle name))
         (version (or (cadr slist) "latest")))
    (if (and (eq (length slist) 1) (gethash name *ocicl-systems*) (not *force*))
        (progn
          (format t "; ~A:~A already exists~%" system (get-project-version (cdr (gethash name *ocicl-systems*))))
          t)
        (progn
          (let ((dl-dir (get-temp-ocicl-dl-pathname)))
            (unwind-protect
                 (progn
                   (uiop:ensure-all-directories-exist (list dl-dir))
                   (uiop:with-current-directory (dl-dir)
                     (unless (loop for registry in *ocicl-registries*
                                   do (handler-case
                                          (progn
                                            (debug-log (format nil "attempting to pull ~A/~A:~A" registry mangled-name version))
                                            (let ((manifest-digest (get-blob registry mangled-name version dl-dir)))
                                              (format t "; downloaded ~A@~A~%" name manifest-digest)
                                              (let* ((abs-dirname (car (uiop:subdirectories dl-dir)))
                                                     (rel-dirname (car (last (remove-if #'(lambda (s) (string= s ""))
                                                                                        (uiop:split-string (namestring abs-dirname)
                                                                                                           :separator (list (uiop:directory-separator-for-host))))))))
                                                (copy-directory:copy dl-dir *systems-dir*)
                                                (dolist (s (find-asd-files (merge-pathnames rel-dirname *systems-dir*)))
                                                  (debug-log #?"registering ${s}")
                                                  (setf (gethash (mangle (pathname-name s)) *ocicl-systems*) (cons #?"${registry}/${mangled-name}@${manifest-digest}"
                                                                                                                   (subseq (namestring s) (length (namestring *systems-dir*))))))))
                                            (return t))
                                        (error (e) (debug-log e))))
                       (format t "; error downloading ~A~%" name))))
              (uiop:delete-directory-tree dl-dir :validate t)))
          (write-systems-csv)
          (gethash (mangle name) *ocicl-systems*)))))

(defun find-asdf-system-file (name)
  (pathname
   (concatenate 'string
                (namestring *systems-dir*)
                (or (cdr (gethash (mangle name) *ocicl-systems*))
                    (handler-case
                        (cdr (download-system name))
                      (error (e)
                        (declare (ignore e))
                        nil))))))

(defun system-definition-searcher (name)
  (unless (or (string= name "asdf") (string= name "uiop"))
    (let* ((*verbose* (or *verbose* (and asdf:*verbose-out* t)))
           (dex:*verbose* *verbose*)
           (system-file (find-asdf-system-file name)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))
