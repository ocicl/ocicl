;;; ocicl.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
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

#+sbcl(require 'sb-introspect)

(defvar *ocicl-registries* (list "ghcr.io/ocicl"))
(defvar *ocicl-globaldir* nil)
(defvar *verbose* nil)
(defvar *force* nil)
(defvar *color* nil)
(defvar *ocicl-systems* nil)
(defvar *inhibit-download-during-search* nil)
(defvar *systems-csv* "ocicl.csv")
(defvar *relative-systems-dir* (make-pathname :directory '(:relative "ocicl")))

(defvar *color-reset* #.(format nil "~c[0m" #\escape))
(defvar *color-bold* #.(format nil "~c[1m" #\escape))
(defvar *color-dim* #.(format nil "~c[2m" #\escape))
(defvar *color-bright-red* #.(format nil "~c[91m" #\escape))
(defvar *color-bright-green* #.(format nil "~c[92m" #\escape))
(defvar *color-bright-cyan* #.(format nil "~c[96m" #\escape))

(defparameter +version+ #.(uiop:read-file-form "version.sexp"))

(defparameter +runtime+
  #.(let* ((runtime (uiop:read-file-string "runtime/ocicl-runtime.lisp"))
         (start (search "UNKNOWN" runtime)))
    (if start
        (concatenate 'string
                     (subseq runtime 0 start)
                     (uiop:read-file-form "version.sexp")
                     (subseq runtime (+ start 7)))
        runtime)))

(defparameter +builtin-templates+
  #.(let* ((root  (merge-pathnames "templates/"
                     (uiop:pathname-directory-pathname
                      (or *compile-file-truename* *load-truename*))))
           (wild  (make-pathname :directory '(:relative :wild-inferiors)
                                 :name :wild :type :wild))
           (files (directory (merge-pathnames wild root)))
           (alist (loop for file in files
                        unless (uiop:directory-pathname-p file)
                          collect (cons (enough-namestring file root)
                                        (alexandria:read-file-into-byte-vector file)))))
      ;; return a *quoted* alist so that the value is data, not a form
      `(quote ,alist)))

(defparameter +asdf+ #.(uiop:read-file-string "runtime/asdf.lisp"))

(defvar *template-params* nil)

(defvar *template-dirs* nil)

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
   :long "registry")
  (:name :color
   :description "color the output WHEN (auto, always, or never)"
   :short #\c
   :long "color"
   :meta-var "WHEN"
   :arg-parser (lambda (arg)
                 (if (member arg '("auto" "always" "never")
                             :test #'equal)
                     arg
                     (progn
                       (usage)
                       (uiop:quit 1)))))
  (:name :depth
   :description "maximum depth for the tree command (integer or \"max\", default 1)"
   :short #\d
   :long "depth"
   :meta-var "NUM"
   :arg-parser (lambda (arg)
                 (cond ((equal arg "max") :max)
                       ((ignore-errors (parse-integer arg)))
                       (t (usage)
                          (uiop:quit 1)))))
  )
#|
  (:name :template-dir
   :description "prepend DIR to template search path (may be repeated)"
   :long "template-dir"
   :meta-var "DIR"
   :repeat t))
|#

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(declaim (inline split-on-delimiter))
(defun split-on-delimiter (line delim)
  (mapcar
   (lambda (string) (string-trim " " string))
   (uiop:split-string line :separator (string delim))))

(defun split-csv-line (line)
  (split-on-delimiter line #\,))

(defun split-lines (line)
  (split-on-delimiter line #\Newline))

(defun read-systems-csv ()
  (let ((systems-file (merge-pathnames (uiop:getcwd) *systems-csv*))
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
   :prefix (format nil "ocicl ~A - copyright (C) 2023-2025 Anthony Green <green@moxielogic.com>" +version+)
   :suffix "Choose from the following ocicl commands:

   help                                   Print this help text
   changes [SYSTEM[:VERSION]]...          Display changes
   clean                                  Remove system directories not listed in ocicl.csv
   diff SYSTEM                            Diff between the installed and latest versions
   diff SYSTEM VERSION                    Diff between the installed version and VERSION
   diff SYSTEM VERSION1 VERSION2          Diff between files in different system versions
   install [SYSTEM[:VERSION]]...          Install systems
   latest [SYSTEM]...                     Install latest version of systems
   libyear                                Calculate the libyear dependency freshness metric
   list SYSTEM...                         List available system versions
   new APP-NAME [TEMPLATE] [KEY=VALUE]... Create a new app
   remove [SYSTEM]...                     Remove systems
   setup [GLOBALDIR]                      Mandatory ocicl configuration
   templates [list]                       List available templates
   templates dirs                         Show template search path
   tree [SYSTEM]...                       Print tree of installed systems
   version                                Show the ocicl version information

Distributed under the terms of the MIT License"
   :usage-of "ocicl"
   :args     "command"))

(defvar *systems-dir* "")

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
        (debug-log (format nil "getting bearer token for ~A" server))
        (cdr (assoc :token
                    (cl-json:decode-json-from-string
                     (ocicl.http:http-get #?"https://${server}/token?scope=repository:${repository}/${system}:pull"
                                          :force-string t
                                          :verbose *verbose*)))))
    (error (e)
      (declare (ignore e))
      nil)))

(defun system-latest-version (system)
  (loop :for registry in *ocicl-registries*
        :as version-list := (system-version-list system registry)
        :thereis (first version-list)))

(defun system-version-list (system registry)
  (handler-case
      (let ((server (get-up-to-first-slash registry))
            (repository (get-repository-name registry)))
        (let* ((token (get-bearer-token registry system)))
          (sort
           (cdr (assoc :tags
                       (cl-json:decode-json-from-string
                        (ocicl.http:http-get #?"https://${server}/v2/${repository}/${system}/tags/list?n=1024"
                                             :force-string t
                                             :verbose *verbose*
                                             :headers `(("Authorization" . ,#?"Bearer ${token}"))))))
           #'string>)))
    (error (e)
      (declare (ignore e))
      (format t "; ~A(~A) not found~%" system registry))))

(defun do-list (args)
  (handler-case
      (when args
        (dolist (system args)
          (loop for registry in *ocicl-registries*
                do (let ((tags (system-version-list system registry)))
                     (when tags
                       (format t "~A(~A):~%" system registry)
                       (dolist (tag tags)
                         (format t "~T~A~%" tag))
                       (return))))
          (format t "~%")))
    #+sbcl
    (sb-int:broken-pipe (e)
      (declare (ignore e))
      ())

    ;; The "broken-pipe" error for sbcl is defined this way in
    ;; sbcl source code: (src/code/target-error.lisp:2282)
    ;;
    ;;   (define-condition simple-stream-error (simple-condition stream-error) ())
    ;;   (define-condition broken-pipe (simple-stream-error) ())
    ;;
    ;; Hopefully using the base stream-error will help catch similar conditions
    ;; in other implementations


    #-sbcl
    (stream-error (e)
      (declare (ignore e))
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
                     (declare (ignore manifest-digest))
                     (let* ((digest (cdr (assoc :digest (cadr (assoc :layers manifest)))))
                            (changes (ocicl.http:http-get #?"https://${server}/v2/${repository}/${system}-changes.txt/blobs/${digest}"
                                              :force-string t
                                              :verbose *verbose*
                                              :headers `(("Authorization" . ,#?"Bearer ${token}")))))
                       (return-from get-changes changes)))))
             (error (e)
               (declare (ignore e))
               )))
  (format nil "No documented changes for ~A:~A" system version))

(declaim (inline quiet-find-system))
(defun quiet-find-system (system &optional (errorp nil errorp-given))
  (let ((*load-verbose* *verbose*)
        (*compile-verbose* *verbose*)
        (*error-output* (if *verbose*
                            *error-output*
                            (make-broadcast-stream))))
    (handler-bind ((#+sbcl(or warning sb-ext:compiler-note)
                    #-sbcl(or warning)
                     (lambda (w)
                       (unless *verbose*
                         (muffle-warning w)))))
      (if errorp-given
          (asdf:find-system system errorp)
          (asdf:find-system system)))))

(defun download-system-dependencies (name)
  (let* ((s (quiet-find-system name))
         (deps (when s (asdf:system-depends-on s))))
    (dolist (d deps)
      (let ((dep (resolve-dependency-name d)))
        (handler-case
            (download-system-dependencies dep)
          (asdf/find-component:missing-component (e)
            (declare (ignore e))
            (when *verbose*
              (format t "; can't download ASDF dependency ~A~%" d)))
          (error (e)
            (when *verbose*
              (format t "; error processing ~A: ~A~%" d e))))))))

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
              (uiop:quit)))
        (unless (download-system (concatenate 'string system ":latest"))
            (progn
              (format uiop:*stderr* "Error: system ~A not found.~%" system)
              (uiop:quit))
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

(defun install-builtin-templates (&key (force nil))
  "Write the embedded templates to ~/.local/share/ocicl/templates/ .
If FORCE is NIL, skip files that already exist."
  (let* ((base (merge-pathnames "templates/" (get-ocicl-dir))))
    (dolist (tpl +builtin-templates+)
      (destructuring-bind (rel . data) tpl
        (let* ((target (merge-pathnames rel base)))
          (uiop:ensure-all-directories-exist
           (list (uiop:pathname-directory-pathname target)))
          (alexandria:write-byte-vector-into-file data target :if-exists :overwrite :if-does-not-exist :create))))))

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
                 (uiop:quit)))
          (uiop:chdir original-directory)))
      (let ((old-config-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))
        (when (probe-file old-config-file)
          (delete-file (merge-pathnames (get-ocicl-dir) "ocicl-globaldir.cfg")))))

  (install-builtin-templates :force *force*)

  (let* ((odir (get-ocicl-dir))
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
      (format t ";; Present the following code to your LISP system at startup, either~%;; by adding it to your implementation's startup file~%;;~T(~~/.sbclrc, ~~/.eclrc, ~~/.abclrc, ~~/.clinit.cl, or ~~/.roswell/init.lisp)~%;; or overriding it completely on the command line~%;;~T(eg. sbcl --userinit init.lisp)~%~%#-ocicl~%(when (probe-file ~S)~%  (load ~S))~%(asdf:initialize-source-registry~%  (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))~%" runtime-source runtime-source))))

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
                                          (ocicl.http:http-get #?"https://${server}/v2/${repository}/${system}/tags/list?n=1024&last=latest"
                                                               :force-string t
                                                               :verbose *verbose*
                                                               :headers `(("Authorization" . ,#?"Bearer ${token}"))))))))
                          (p (position version all-versions :test #'string=)))
                     (when p (cdr (nthcdr p all-versions))))))
             (drakma:drakma-error (e)
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
    (if *color*
        (concatenate 'string *color-bold* base padding *color-reset*)
        (concatenate 'string base padding))))

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
                                            (uiop:merge-pathnames* *relative-systems-dir*
                                                                   "_00_OCICL_NAME"))))
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string pfile)))
      ;; If the tgz file doesn't include _00_OCICL_NAME, the infer it from the directory name.
      (file-error (e)
        (declare (ignore e))
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
                                            (uiop:merge-pathnames* *relative-systems-dir*
                                                                   "_00_OCICL_VERSION"))))
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string vfile)))
      (file-error (e)
        (declare (ignore e))
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
                                                                 (uiop:merge-pathnames* *relative-systems-dir*
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
        (let ((system (car (split-on-delimiter system-maybe-version #\:)))
              (version (cadr (split-on-delimiter system-maybe-version #\:))))
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
                                                         (uiop:merge-pathnames* *relative-systems-dir*
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
                       (error (e)
                         (declare (ignore e))
                         ()))))
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
                  (uiop:quit))))
          (let* ((slist (split-on-delimiter system #\:))
                 (name (car slist)))
            (when (download-system system)
              (download-system-dependencies name)))))
      ;; Download all systems in systems.csv.
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (when (not (probe-file (concatenate 'string (namestring *systems-dir*) (cdr value))))
                   (format t "; downloading ~A~%" (car value))
                   (download-and-install (car value))))
               *ocicl-systems*)))

(defun subpath-p (path1 path2)
  (let ((enough-directory
          (pathname-directory
           (enough-namestring (merge-pathnames path1)
                              (merge-pathnames path2)))))
    (or (not enough-directory)
        (eql :relative (first enough-directory)))))


;; this function needs to be defined above its first use if it is inline
(declaim (inline find-asd-files))
(defun find-asd-files (dir)
  "Recursively find all files with the .asd extension in a directory."
  ;; Force a trailing slash to support uiop change in behavior:
  ;; https://github.com/fare/asdf/commit/6138d709eb25bf75c1d1f7dc45a63d174f982321
  (directory (merge-pathnames
              (make-pathname :name :wild
                             :type "asd"
                             :directory '(:relative :wild-inferiors))
              (uiop:ensure-directory-pathname dir))))

(defun system-group (system)
  "Return systems that are known to ocicl and in the same directory tree as SYSTEM"
  (let* ((*inhibit-download-during-search* t))
    (alexandria:when-let* ((asdf-system (ignore-errors
                                         ;; First check if the system is
                                         ;; already known, since otherwise
                                         ;; ocicl remove would try to load
                                         ;; dependencies again as it removes
                                         ;; them
                                         (or (asdf:registered-system system)
                                             (quiet-find-system system nil))))
                           (source-file (asdf:system-source-file asdf-system)))
      (when (subpath-p source-file *systems-dir*)
        (let ((top-systems (mapcar
                            #'pathname-name
                            (find-asd-files
                             (merge-pathnames
                              (make-pathname :directory
                                             (subseq (pathname-directory
                                                      (enough-namestring source-file *systems-dir*))
                                                     0 2))
                              *systems-dir*)))))
          (append
           top-systems
           (let ((slash-systems))
             (asdf:map-systems (lambda (system)
                                 (when (find-if
                                        (lambda (top)
                                          (uiop:string-prefix-p
                                           (concatenate 'string top "/")
                                           (asdf:component-name system)))
                                        top-systems)
                                   (push (asdf:component-name system) slash-systems))))
             slash-systems)))))))

(defun remove-system (system &key (modify-ocicl-systems t))
  (let* ((slist (split-on-delimiter system #\:))
         (name (car slist))
         (mangled-name (mangle name))
         (system-info (gethash mangled-name *ocicl-systems*)))

    (if (null system-info)
      (format t "; no system to remove: ~A~%" name) ;return here, fixes type warnings to merge-pathnames
      (let* ((fullname (car system-info))
             (relative-asd-path (cdr system-info))
             (absolute-asd-path (merge-pathnames relative-asd-path *systems-dir*))
             (system-directory (merge-pathnames
                                 (make-pathname
                                   :directory
                                   `(:relative
                                      ,(second
                                         (pathname-directory
                                           (enough-namestring absolute-asd-path *systems-dir*)))))
                                 *systems-dir*))
             (system-group (system-group system)))
        (when (and modify-ocicl-systems system-info)
          (dolist (system system-group)
            (remhash system *ocicl-systems*)))
        (when (uiop:directory-exists-p system-directory)
          (uiop:delete-directory-tree
            system-directory
            :validate (lambda (path)
                        ;; ensure directory being deleted is a subdirectory of *systems-dir*
                        (equal :relative (car (pathname-directory (enough-namestring path *systems-dir*))))))
          (format t "; removed ~A~%" (file-namestring fullname)))))))

(defun resolve-dependency-name (dependency)
  "Resolve ASDF dependency name."
  (declare (optimize (speed 3) (safety 1)))
  (if (consp dependency)
      (resolve-dependency-name (case (car dependency)
                                 (:version (second dependency))
                                 (:feature (third dependency))
                                 (:require (second dependency))))
      dependency))

(defun full-dependency-table (system
                              &optional
                                (dependency-table (make-hash-table :test #'equal)))
  (declare (optimize (speed 3) (safety 1)))
  (let ((*inhibit-download-during-search* t))
    (labels ((recurse-deps (system)
               (when (not (nth-value 1 (gethash system dependency-table)))
                 (let ((asdf-system (ignore-errors (quiet-find-system system nil))))
                   (when asdf-system
                     (let ((dependencies (append (mapcar #'resolve-dependency-name
                                                         (asdf:system-depends-on asdf-system))
                                                 (mapcar #'resolve-dependency-name
                                                         (asdf:system-defsystem-depends-on asdf-system)))))
                       (setf (gethash system dependency-table)
                             dependencies)
                       (mapc #'recurse-deps dependencies)))))))
      (recurse-deps system)
      dependency-table)))

(defun do-remove (systems)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((*inhibit-download-during-search* t)
         (dependency-table (make-hash-table :test #'equal))
         (all-systems ;; a dependency graph like (((system group-system...) dep ...) ...)
           (progn
             (maphash (lambda (system info)
                        (declare (ignore info))
                        (full-dependency-table (unmangle system) dependency-table))
                      *ocicl-systems*)
             (alexandria:hash-table-alist dependency-table)))
         (seen-system-groups (make-hash-table :test #'equal)))
    (labels ((dependency-tree (system)
               (let* ((spec (assoc system all-systems :test #'equal))
                      (system (car spec))
                      (system-deps (cdr spec)))
                 (when spec
                   (cons
                    system
                    (remove nil (mapcar #'dependency-tree system-deps))))))
             (remove-trees (depend-tree graph)
               (dolist (tree depend-tree)
                 (destructuring-bind (system . depend-tree) tree
                   (let* ((system-group (mapcar #'unmangle (system-group system)))
                          (depended-on (rassoc system-group graph
                                               :test (lambda (a b)
                                                       (intersection a b :test #'equal)))))

                     (cond (depended-on
                            (setf (gethash system-group seen-system-groups) (car depended-on)))
                           (t
                            (when system-group
                             (unless (gethash system-group seen-system-groups)
                              ;; delay modifying ocicl-systems until after tree traversal
                              (remove-system (car system-group) :modify-ocicl-systems nil)
                              (setf (gethash system-group seen-system-groups) :removed)))
                            (when depend-tree
                              (remove-trees depend-tree graph)))))))))
      (if *force*
          (mapcar #'remove-system systems)
          (let* ((existing-systems (remove-if-not (lambda (system) (quiet-find-system system nil)) systems))
                 (nonexistent-systems (remove-if (lambda (system) (quiet-find-system system nil)) systems))
                 (dependency-trees (mapcar #'dependency-tree existing-systems))
                 (flat-dependencies (remove-duplicates (alexandria:flatten dependency-trees)
                                                       :test #'equal))
                 (flat-groups (remove-duplicates
                               (mapcar (lambda (system)
                                         (mapcar #'unmangle (system-group system)))
                                       flat-dependencies)
                               :test #'equal))
                 ;; don't consider groups to remove as dependant systems
                 (graph (set-difference all-systems flat-groups
                                        :test (lambda (a b)
                                                (member (car a) b :test #'equal)))))
            (format t "~{; no system to remove: ~A~^~%~}~&" nonexistent-systems)
            (remove-trees dependency-trees graph)
            ;; modify ocicl-systems
            (maphash
             (lambda (system-group value)
               (let ((*print-pretty* nil))
                 (if (eql value :removed)
                     (mapc (lambda (system) (remhash (mangle system) *ocicl-systems*)) system-group)
                     (format t "~&; not removing systems ~a, depended on by: ~a~%" system-group value))))
             seen-system-groups)))
      (write-systems-csv))))



(defclass colorful-unified-diff-window (diff::unified-diff-window) ())

(defclass colorful-unified-diff (diff::unified-diff)
  ()
  (:default-initargs :window-class 'colorful-unified-diff-window))

(defmethod diff:render-diff-window :before ((window diff::unified-diff-window) stream)
  (let ((original-length (diff::original-window-length window))
        (modified-length (diff::modified-window-length window)))
    (when *color*
      (write-string *color-bright-cyan* stream))
    (format stream "@@ -~A" (1+ (diff::original-start-line window)))
    (unless (zerop original-length)
      (format stream ",~A" original-length))
    (format stream " +~A" (1+ (diff::modified-start-line window)))
    (unless (zerop modified-length)
      (format stream ",~A" modified-length))
    (write-string " @@" stream)
    (when *color*
      (write-string *color-reset* stream))
    (terpri stream)))

(defmethod diff:render-diff :before ((diff diff::unified-diff) stream)
  (when *color*
    (write-string *color-bold* stream))
  (format stream "--- ~A~%+++ ~A"
          (namestring (enough-namestring (diff::original-pathname diff) *systems-dir*))
          (namestring (enough-namestring (diff::modified-pathname diff) *systems-dir*)))
  (when *color*
    (write-string *color-reset* stream))
  (terpri stream))

(defmethod diff:render-diff-window ((object colorful-unified-diff-window) stream)
  (if *color*
      (dolist (chunk (diff:window-chunks object))
        (let ((prefix (ecase (diff:chunk-kind chunk)
                        (:common (concatenate 'string *color-dim* " "))
                        ((:delete :replace) (concatenate 'string *color-bright-red* "-"))
                        ((:insert :create) (concatenate 'string *color-bright-green* "+")))))
          (dolist (line (diff:chunk-lines chunk))
            (write-char #\escape stream)
            (write-string prefix stream)
            (write-string line stream)
            (write-string *color-reset* stream)
            (terpri stream))))
      (call-next-method)))

(defun list-all-files (directory)
  (remove-if
   (lambda (file)
     (uiop:directory-pathname-p file))
   (directory
    (merge-pathnames (make-pathname :name :wild
                                    :type :wild
                                    :directory '(:relative :wild-inferiors))
                     directory))))

(defun binary-file-p (pathname)
  (declare (optimize (speed 3) (safety 1)))
  (let ((buffer (make-array 3000 :element-type '(unsigned-byte 8))))
    (with-open-file (stream pathname :element-type '(unsigned-byte 8))
      (let ((bytes-read (read-sequence buffer stream)))
        (some #'zerop (subseq buffer 0 bytes-read))))))

(defun binary-files-differ-p (path1 path2)
  (declare (optimize (speed 3) (safety 1)))
  (let ((buf1 (make-array 3000 :element-type '(unsigned-byte 8)))
        (buf2 (make-array 3000 :element-type '(unsigned-byte 8))))
    (with-open-file (stream1 path1 :element-type '(unsigned-byte 8))
      (with-open-file (stream2 path2 :element-type '(unsigned-byte 8))
        (loop :as bytes-read1 := (read-sequence buf1 stream1)
              :as bytes-read2 := (read-sequence buf2 stream2)
              :unless (and (= bytes-read1 bytes-read2)
                           (equalp buf1 buf2))
                :return t
              :until (or (zerop bytes-read1)
                         (zerop bytes-read2)))))))

(defun do-diff (args)
  (declare (optimize (speed 3) (safety 1)))
  (if (fourth args)
      (progn (usage) (uiop:quit 1))
      (let* ((system-name (first args))
             (given-v1 (second args))
             (given-v2 (third args))
             (latest-version (when (or (equal given-v1 "latest")
                                       (equal given-v2 "latest")
                                       (and (null given-v1) (null given-v2)))
                               (system-latest-version system-name)))
             (version1 (cond ((not given-v1) nil)
                             ((not given-v2) nil)
                             ((string= given-v1 "latest")
                              latest-version)
                             (t given-v1)))
             (version2 (cond ((not given-v2) (or version1 latest-version))
                             ((string= given-v2 "latest")
                              latest-version)
                             (t given-v2)))
             (system-fullname-1 (concatenate 'string system-name (when version1 ":") version1))
             (system-fullname-2 (concatenate 'string system-name ":" version2)))
        (declare (type (simple-array character) system-name)
                 (type (or null (simple-array character)) given-v1 given-v2 version1 version2))
        (when (and version1 (equal version1 version2))
          (return-from do-diff))
        (let* ((version1-system-info (if version1
                                         (download-system system-fullname-1
                                                          :write-systems-csv nil
                                                          :print-error t)
                                         (gethash (mangle system-name) *ocicl-systems*)))
               (version2-system-info (when version1-system-info
                                       (download-system system-fullname-2
                                                        :write-systems-csv nil
                                                        :print-error t))))
          (when (not (or version1 version1-system-info))
            (format *error-output* "; Error: system ~A not installed. Install it or specify two versions to diff.~%" system-name)
            (uiop:quit 1))
          (if (and version1-system-info version2-system-info)
              (let* ((version1-dir (merge-pathnames
                                    (make-pathname
                                     :directory `(:relative
                                                  ,(second
                                                    (pathname-directory
                                                     (the string (cdr version1-system-info))))))
                                    *systems-dir*))
                     (version1-files (mapcar
                                      (lambda (file)
                                        (enough-namestring file version1-dir))
                                      (list-all-files version1-dir)))
                     (version2-dir (merge-pathnames
                                    (make-pathname
                                     :directory `(:relative
                                                  ,(second
                                                    (pathname-directory
                                                     (the string (cdr version2-system-info))))))
                                    *systems-dir*))
                     (version2-files (mapcar
                                      (lambda (file)
                                        (enough-namestring file version2-dir))
                                      (list-all-files version2-dir)))
                     (files-only-in-1  (set-difference version1-files version2-files :test #'equal))
                     (files-only-in-2 (set-difference version2-files version1-files :test #'equal))
                     (files-in-both (intersection version1-files version2-files :test #'equal))
                     (files (sort
                             (append
                              (mapcar (lambda (file) (cons file version1)) files-only-in-1)
                              (mapcar (lambda (file) (cons file version2)) files-only-in-2)
                              (mapcar (lambda (file) (cons file :both)) files-in-both))
                             #'string<
                             :key #'car)))
                (dolist (file files)
                  (if (eql :both (cdr file))
                      (let ((pathname-1 (merge-pathnames (car file) version1-dir))
                            (pathname-2 (merge-pathnames (car file) version2-dir)))
                        (if (or (binary-file-p pathname-1)
                                (binary-file-p pathname-2))
                            (when (binary-files-differ-p pathname-1 pathname-2)
                              (format t "~&Binary files ~a and ~a differ~%" pathname-1 pathname-2))
                            (handler-case
                                (let ((diff (diff:generate-diff 'colorful-unified-diff pathname-1 pathname-2)))
                                  (when (diff:diff-windows diff)
                                    (diff:render-diff diff *standard-output*)))
                              (stream-error ()
                                (when (binary-files-differ-p pathname-1 pathname-2)
                                  (format t "~&Binary files ~a and ~a differ~%" pathname-1 pathname-2))))))
                      (format t "~&Only in ~a: ~a~%" (cdr file) (car file)))))
              (uiop:quit 1))))))

(defun do-clean (args)
  (when args
    (usage)
    (uiop:quit 1))
  (let* ((system-directories (directory
                              (make-pathname
                               :name :wild :type :wild
                               :defaults *systems-dir*)))
         (registered-directories
           (let ((directories))
             (maphash (lambda (system values)
                        (declare (ignore system))
                        (destructuring-bind (version . asd) values
                          (declare (ignore version))
                          ;; get just the top directory
                          (push
                           (merge-pathnames
                            (make-pathname
                             :directory (subseq (pathname-directory asd) 0 2))
                            *systems-dir*)
                           directories)))
                      *ocicl-systems*)
             (remove-duplicates directories :test #'equal)))
         (directories-to-clean
           (set-difference system-directories registered-directories
                           :test #'equal)))
    (mapc
     (lambda (directory)
       (uiop:delete-directory-tree
        directory
        :validate
        (lambda (path)
          ;; ensure directory being deleted is a subdirectory of *systems-dir*
          (equal :relative (car (pathname-directory (enough-namestring path *systems-dir*)))))))
     directories-to-clean)))

(defvar *tree-depth* nil)

(defstruct tree-not-found name)

(defstruct tree-top systems)

(defvar *tree-seen* (make-hash-table))

(defmethod tree:node-children ((system tree-top))
  (let ((*inhibit-download-during-search* t))
    (mapcar
     (lambda (name)
       (or (ignore-errors (quiet-find-system name nil))
           (make-tree-not-found :name name)))
     (tree-top-systems system))))

(defmethod tree:print-node ((system tree-top) stream)
  (when *color*
    (write-string *color-reset*))
  (princ *systems-csv*)
  (when *color*
    (write-string *color-dim*)))

(defmethod tree:node-children ((system asdf:system))
  (let ((*inhibit-download-during-search* t))
    (unless (eql (gethash system *tree-seen*) :expanded)
      (setf (gethash system *tree-seen*) :expanded)
      (let ((dependency-names (sort
                               (mapcar
                                #'resolve-dependency-name
                                (asdf:system-depends-on system))
                               #'string<)))
        (mapcar
         (lambda (dependency)
           (or (ignore-errors (quiet-find-system dependency nil))
               (make-tree-not-found :name dependency)))
         dependency-names)))))

(defmethod tree:print-node ((system tree-not-found) stream)
  (when *color*
    (write-string *color-reset*)
    (write-string *color-bold*)
    (write-string *color-bright-red*))
  (princ (tree-not-found-name system))
  (princ "!")
  (when *color*
    (write-string *color-reset*)
    (write-string *color-dim*)))

(defmethod tree:print-node ((system asdf:system) stream)
  (let ((seen (gethash system *tree-seen*)))
    (when (and *color* (not seen))
      (write-string *color-reset*)
      (write-string *color-bold*)
      (write-string *color-bright-green*))
    (princ (asdf:component-name system))
    (when (and (eql seen :expanded) (asdf:system-depends-on system))
      (princ "*"))
    (when (and *color* (not seen))
      (write-string *color-reset*)
      (write-string *color-dim*))
    (unless seen
      (setf (gethash system *tree-seen*) :printed))))

(defun do-tree (args)
  (when *color*
    (write-string *color-dim*))
  (let ((top (make-tree-top
              :systems
              (or (remove-duplicates args :test #'equal)
                  (mapcar
                   #'unmangle
                   (sort (alexandria:hash-table-keys *ocicl-systems*) #'string<))))))
    (tree:print-tree
     top
     :unicode t
     :max-depth (when *tree-depth*
                  (+ *tree-depth* (if args 1 0)))))
  (when *color*
    (write-string *color-reset*)))

(defun render-template-file (in-path out-path env)
  "Read template text from IN-PATH, render with ENV, write to OUT-PATH."
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname out-path)))
  (let ((template-text (uiop:read-file-string in-path)))
    (handler-bind ((style-warning #'muffle-warning))
      (let ((fn (cl-template:compile-template template-text)))
        (with-open-file (out out-path :direction :output :if-exists :supersede)
          (format out "~A" (funcall fn env))))))
  out-path)

(defun string-replace-ci (hay token replacement)
  "Case-insensitive TOKEN → REPLACEMENT in HAY.  Returns a fresh string."
  (loop with out   = (make-array 0 :element-type 'character
                                 :adjustable t :fill-pointer 0)
        with len-h = (length hay)
        with len-t = (length token)
        for i from 0 below len-h
        do (if (and (<= (+ i len-t) len-h)
                    (string-equal hay token :start1 i :end1 (+ i len-t)))
               ;; Copy the replacement and jump ahead
               (progn
                 (loop for ch across replacement
                       do (vector-push-extend ch out))
                 (incf i (1- len-t)))           ; loop will incf once more
               ;; Normal character
               (vector-push-extend (char hay i) out))
        finally (return out)))

(defun expand-appname (path app-name)
  (string-replace-ci path "{{app-name}}" app-name))

(defun ensure-parent-dir (pathname)
  "Make sure the directory that will hold PATHNAME exists."
  (uiop:ensure-all-directories-exist
   (list (uiop:pathname-directory-pathname pathname))))

(defun copy-template-tree (src dst)
  (uiop:ensure-directory-pathname dst)          ; top-level target dir

  (let ((app-name (getf *template-params* :app-name)))
    (dolist (p (directory
                (merge-pathnames
                 (make-pathname :directory '(:relative :wild-inferiors)
                                :name      :wild
                                :type      :wild)
                 src)))

      ;; ── skip anything in a .git directory ──
      (unless (member ".git" (pathname-directory p) :test #'string=)

        ;; decide destination name
        (let* ((rel       (enough-namestring p src))
               (rendered  (expand-appname rel app-name))
               (out-path  (merge-pathnames rendered dst)))

          (cond
            ((uiop:directory-pathname-p p)
             (uiop:ensure-directory-pathname out-path))

            ((not (binary-file-p p))
             (ensure-parent-dir out-path)
             (render-template-file p out-path *template-params*))

            (t
             (ensure-parent-dir out-path)
             (uiop:copy-file p out-path))))))))

(defun do-new (args)
  "ocicl new APP [TEMPLATE] [KEY=VAL]..."
  (unless args (usage) (uiop:quit 1))

  (let* ((app-name (first args))
         (rest     (rest  args))
         ;; TEMPLATE is explicit if the next token lacks "="
         (template
           (cond
             ;; explicit on CLI
             ((and rest (not (search "=" (first rest))))
              (prog1 (first rest) (setf rest (rest rest))))
             ;; implicit "user" in any search directory
             ((assoc "user" (discover-templates) :test #'string=) "user")
             ;; final fall-back
             (t "basic")))
         (user-plist
           (loop for s in rest
                 append
                 (destructuring-bind (k v)
                     (uiop:split-string s :separator "=")
                   (list (intern (string-upcase k) :keyword) v))))
         (*template-params*
           (append (list :app-name app-name) user-plist)))

    ;; look up the template in the unified search path
    (let* ((tpl-entry (assoc template (discover-templates) :test #'equal))
           (tpl       (cdr tpl-entry)))
      (unless tpl
        (format *error-output* "ocicl: template “~A” not found~%" template)
        (uiop:quit 1))

      (let* ((dest (merge-pathnames
                    (make-pathname :directory `(:relative ,app-name))
                    (uiop:getcwd))))
        (when (probe-file dest)
          (format *error-output* "; “~A” already exists – choose another name~%" app-name)
          (uiop:quit 1))

        (copy-template-tree tpl dest)
        (format t "; created “~A” from template “~A”~%" app-name template)))))

(defmethod parent ((file pathname))
  "Return the parent directory of FILE."
  (if (uiop:directory-pathname-p file)
      (uiop:pathname-parent-directory-pathname file)
      (uiop:pathname-directory-pathname file)))

(defun find-workdir (workdir)
  "Search for ocicl.csv or systems.csv starting from WORKDIR and moving up the directory chain.
   Returns the directory containing ocicl.csv or systems.csv if found.  If none is
   found, return WORKDIR."
  (loop for dir = (truename workdir) :then parent-dir
        for parent-dir = (parent dir)
        for systems-csv = (merge-pathnames (make-pathname :name "systems" :type "csv") dir)
        for ocicl-csv = (merge-pathnames (make-pathname :name "ocicl" :type "csv") dir)
        for existing-csv = (or (probe-file ocicl-csv) (probe-file systems-csv))
        until (or existing-csv
                  (null parent-dir)
                  (equal parent-dir dir))
        finally (return
                  (uiop:ensure-directory-pathname
                   (cond (existing-csv
                          (setf *systems-csv* existing-csv
                                *relative-systems-dir* (make-pathname
                                                        :directory
                                                        `(:relative ,(pathname-name existing-csv))))
                          dir)
                         (t workdir))))))

(defun discover-templates ()
  "Return an alist (TEMPLATE-NAME . ABSOLUTE-PATH).  First hit wins."
  (labels ((dirname (pn)                 ;→ \"basic\" for .../templates/basic/
             (let* ((parts (pathname-directory pn))
                    (leaf  (first (last parts))))
               (etypecase leaf
                 (string leaf)
                 (symbol (string-downcase (symbol-name leaf)))))))
    (let ((table (make-hash-table :test #'equal)))
      (dolist (dir *template-dirs*)
        (dolist (d (directory
                    (merge-pathnames (make-pathname :directory '(:relative :wild))
                                     (uiop:ensure-directory-pathname dir))))
          (when (uiop:directory-pathname-p d)
            (let ((name (dirname d)))
              (unless (gethash name table)
                (setf (gethash name table) d))))))
      (alexandria:hash-table-alist table))))

(defun do-templates (args)
  "Without args: list all visible templates.
   With “dirs”:  show the current template search-path."
  (cond
    ((or (null args) (string= (first args) "list"))
     (dolist (tpl (sort (discover-templates) #'string< :key #'car))
       (format t "~A~30T~A~%" (car tpl) (cdr tpl))))
    ((string= (first args) "dirs")
     (dolist (d *template-dirs*)
       (format t "~A~%" d)))
    (t (usage))))

(defun main ()
  (setf *random-state* (make-random-state t))
  (ocicl.http:configure-drakma-proxy-from-env)

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
                 (handler-bind ((opts:unknown-option #'unknown-option))
                   (opts:get-opts))
               (opts:missing-arg (condition)
                 (format t "fatal: option ~s needs an argument!~%"
                         (opts:option condition)))
               (opts:arg-parser-failed (condition)
                 (format t "fatal: cannot parse ~s as argument of ~s~%"
                         (opts:raw-arg condition)
                         (opts:option condition))))
           (when-option (options :verbose)
                        (setf *verbose* t))
           (when-option (options :force)
                        (setf *force* t))
           (when-option (options :global)
                        (setf workdir (or *ocicl-globaldir* (get-ocicl-dir))))
           ;; FIXME: required because ocicl's version of unix-opts does not
           ;; yet have :default

           ;; 1.  dirs given on the CLI (earlier option instances should win → reverse)
           (setf *template-dirs*
                 (reverse (getf options :template-dir)))

           ;; 2.  config-file
           (let ((cfg (merge-pathnames (get-ocicl-dir) "ocicl-templates.cfg")))
             (when (probe-file cfg)
               (alexandria:appendf *template-dirs*
                        (uiop:read-file-lines cfg))))

           ;; 3.  environment variable
           (when (uiop:getenvp "OCICL_TEMPLATE_PATH")
             (alexandria:appendf *template-dirs*
                      (uiop:split-string (uiop:getenv "OCICL_TEMPLATE_PATH")
                                         :separator (string #\:))))

           ;; 4.  hard defaults (user dir first, then built-in share dir)
           (alexandria:appendf *template-dirs*
                    (list (merge-pathnames "templates/" (get-ocicl-dir))))

           ;; collapse duplicates while preserving order
           (setf *template-dirs* (remove-duplicates *template-dirs* :test #'equal))

           (flet ((get-output-stream ()
                    (if (typep *standard-output* 'synonym-stream)
                        (symbol-value (synonym-stream-symbol *standard-output*))
                        *standard-output*)))
             (let* ((color (getf options :color))
                    (color-allowed?
                      (or (string= color "auto")
                          (and (not color)
                               (not (uiop:getenvp "NO_COLOR")))))
                    (tty?
                      (handler-case
                          #+sbcl
                          (/= 0 (sb-unix:unix-isatty
                                  (sb-sys:fd-stream-fd
                                    (get-output-stream))))
                          #-sbcl
                          (interactive-stream-p (get-output-stream))
                        (error () nil))))
               (setf *color* (or (string= color "always")
                                 (and tty? color-allowed?)))))

           (let ((depth (getf options :depth)))
             (setf *tree-depth* (if (eql depth :max)
                                    nil
                                    (or depth 1))))


           (setf workdir (find-workdir workdir))

           (locally (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
             (handler-bind (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
               (uiop:with-current-directory (workdir)
                 (setq *ocicl-systems* (read-systems-csv))
                 (setq *systems-dir* (merge-pathnames *relative-systems-dir*
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
                         ((string= cmd "remove")
                          (do-remove (cdr free-args)))
                         ((string= cmd "latest")
                          (do-latest (cdr free-args)))
                         ((string= cmd "list")
                          (do-list (cdr free-args)))
                         ((string= cmd "new")
                          (do-new (cdr free-args)))
                         ((string= cmd "tree")
                          (do-tree (cdr free-args)))
                         ((string= cmd "templates")
                          (do-templates (cdr free-args)))
                         ((string= cmd "diff")
                          (do-diff (cdr free-args)))
                         ((string= cmd "clean")
                          (do-clean (cdr free-args)))
                         ((string= cmd "setup")
                          (do-setup (cdr free-args)))
                         ((string= cmd "version")
                          (do-version (cdr free-args)))
                         (t (usage)))))))))))
    (with-user-abort:user-abort () (uiop:quit 130))
    (stream-error (e)
      (format *error-output* "ocicl: stream error during output~%")
      (when *verbose*
        (format *error-output* "~a~&" e))
      (uiop:quit 1))))

(defun replace-plus-with-string (str)
  (let ((mangled (with-output-to-string (s)
                    (loop for c across str do
                          (if (char= c #\+)
                              (write-string "_plus_" s)
                              (write-char c s))))))
    (if (char= (char mangled (- (length mangled) 1)) #\_)
        (subseq mangled 0 (- (length mangled) 1))
      mangled)))

(defun unmangle (str)
  (let ((final-plus (ppcre:regex-replace-all "_plus$" str "+")))
    (ppcre:regex-replace-all "_plus_"  final-plus "+")))

(defun mangle (str)
  (replace-plus-with-string (car (split-on-delimiter str #\/))))

(defun get-temp-ocicl-dl-pathname ()
  (let ((rdir (format nil "ocicl-~:@(~36,8,'0R~)" (random (expt 36 8) *random-state*))))
    (merge-pathnames (eval `(make-pathname :directory '(:relative ,rdir)))
                     (uiop:default-temporary-directory))))


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
  (with-open-file (stream (merge-pathnames (uiop:getcwd) *systems-csv*)
                          :direction :output
                          :if-exists :supersede)
    (let ((systems-list (sort (alexandria:hash-table-alist *ocicl-systems*)
                              #'string<
                              :key #'car)))
      (mapc
       (lambda (system)
         (destructuring-bind (system fullname . asd) system
           (format stream "~A, ~A, ~A~%" system fullname asd)))
       systems-list)))
  (debug-log (format nil "wrote new ~a" *systems-csv*)))

(defun get-manifest (registry system tag)
  (let ((token (get-bearer-token registry system))
        (server (get-up-to-first-slash registry))
        (repository (get-repository-name registry)))
    (multiple-value-bind (body status response-headers)
        (ocicl.http:http-get #?"https://${server}/v2/${repository}/${system}/manifests/${tag}"
                 :force-string t
                 :verbose *verbose*
                 :headers `(("Authorization" . ,#?"Bearer ${token}")
                            ("Accept" . "application/vnd.oci.image.manifest.v1+json,application/vnd.oci.image.index.v1+json")))
      (declare (ignore status))
      (values (json:decode-json-from-string body) (gethash "docker-content-digest" response-headers)))))

(defun get-blob (registry system tag dl-dir)
  (let* ((token (get-bearer-token registry system))
         (server (get-up-to-first-slash registry))
         (repository (get-repository-name registry)))
    (multiple-value-bind (manifest manifest-digest)
        (get-manifest registry system tag)
      (let* ((digest (cdr (assoc :digest (cadr (assoc :layers manifest)))))
             (input (ocicl.http:http-get #?"https://${server}/v2/${repository}/${system}/blobs/${digest}"
                             :force-binary t
                             :want-stream t
                             :verbose *verbose*
                             :headers `(("Authorization" . ,#?"Bearer ${token}")))))
        (handler-bind
            ((tar-simple-extract:broken-or-circular-links-error
              (lambda (condition)
                (declare (ignore condition))
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
                       (declare (ignore manifest-digest))
                       (copy-directory:copy dl-dir *systems-dir*))))
               (error (e)
                 (format t "; error downloading and installing ~A~%" fullname)
                 (debug-log e)
                 nil))))
      (uiop:delete-directory-tree dl-dir :validate t))))

(defun download-system (system &key
                                 (write-systems-csv t)
                                 print-error)
  "Downloads SYSTEM, which may be specified as NAME[:VERSION].

If SYSTEM exists in the systems csv file and the asd file exists, does not
download the system unless a version is specified."
  (let* ((slist (split-on-delimiter system #\:))
         (name (first slist))
         (requested-version (second slist))
         (mangled-name (mangle name))
         (system-info (gethash mangled-name *ocicl-systems*))
         (fullname (car system-info))
         (relative-asd-path (cdr system-info))
         (existing-version (when system-info
                             (cl-ppcre:register-groups-bind (registry name digest)
                                 ("^([^/]+/[^/]+)/([^:@]+)?(?:@sha256:([a-fA-F0-9]+))?" fullname)
                               (declare (ignore registry name))
                               #?"sha256:${digest}")))
         (version (or requested-version existing-version "latest"))
         (asd-file (when relative-asd-path (merge-pathnames relative-asd-path *systems-dir*))))
    (if (and (not requested-version)
             system-info
             asd-file
             (probe-file asd-file)
             (not *force*))
        (progn
          (format t "; ~A:~A already exists~%" system (get-project-version relative-asd-path))
          (gethash mangled-name *ocicl-systems*))
        (let ((dl-dir (get-temp-ocicl-dl-pathname)))
          (unwind-protect
               (progn
                 (uiop:ensure-all-directories-exist (list dl-dir))
                 (when (uiop:with-current-directory (dl-dir)
                         (loop for registry in *ocicl-registries*
                               thereis (handler-case
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
                                                   (setf (gethash (mangle (pathname-name s)) *ocicl-systems*)
                                                         (cons #?"${registry}/${mangled-name}@${manifest-digest}"
                                                               (enough-namestring (namestring s) *systems-dir*))))))
                                             t)
                                         (error (e)
                                           (when (or *verbose* print-error)
                                             (format *error-output* "; error downloading ~A from registry ~A~%" system registry))
                                           (debug-log e)))))
                   (when write-systems-csv
                     (write-systems-csv))
                   (gethash mangled-name *ocicl-systems*)))
            (uiop:delete-directory-tree dl-dir :validate t))))))

(defun find-asdf-system-file (name)
  (let* ((system-info (gethash (mangle name) *ocicl-systems*))
         (system-asd (when system-info (merge-pathnames (cdr system-info) *systems-dir*))))
    (cond ((and system-asd (probe-file system-asd)))
          ((not *inhibit-download-during-search*)
           (handler-case
               (probe-file (merge-pathnames (cdr (download-system name)) *systems-dir*))
             (error (e)
               (declare (ignore e))
               nil))))))

(defun system-definition-searcher (name)
  (unless (or (string= name "asdf") (string= name "uiop"))
    (let* ((*verbose* (or *verbose* (and asdf:*verbose-out* t)))
           (system-file (find-asdf-system-file name)))
      (when (and system-file
                 (string= (pathname-name system-file) name))
        system-file))))

(setf asdf:*system-definition-search-functions*
      (append asdf:*system-definition-search-functions*
              (list 'system-definition-searcher)))

;; just to be safe, try loading internal SBCL systems in the event they're
;; actually needed by a defsystem, since we're going to make these unloadable
;; later.
#+sbcl
(handler-bind ((warning (lambda (c) (muffle-warning c))))
  (dolist
    (system
      '(:sb-aclrepl
         :sb-bsd-sockets
         :sb-capstone
         :sb-cltl2
         :sb-concurrency
         :sb-cover
         :sb-executable
         :sb-gmp
         :sb-grovel
         :sb-introspect
         :sb-md5
         #-windows
         :sb-mpfr
         :sb-posix
         :sb-queue
         :sb-rotate-byte
         :sb-rt
         :sb-simple-streams
         :sb-sprof))
    (ignore-errors (require system))))

;; Register known internal systems as "immutable" so that find-system inside
;; the ocicl executable does not try to load them

(let* ((sbcl-systems
         #+sbcl
         '(
           :sb-aclrepl
           :sb-bsd-sockets
           :sb-capstone
           :sb-cltl2
           :sb-concurrency
           :sb-cover
           :sb-executable
           :sb-gmp
           :sb-grovel
           :sb-introspect
           :sb-md5
           :sb-mpfr
           :sb-perf
           :sb-posix
           :sb-queue
           :sb-rotate-byte
           :sb-rt
           :sb-simd
           :sb-simple-streams
           :sb-sprof))
       (base-systems
         '(
           ;; Register some non-SBCL internal systems that don't exist
           ;; in the ocicl repo

           ;; corman
           :threads
           ;; clisp
           :syscalls
           ;; abcl
           :extensible-sequences
           ;; cmucl
           :unix
           ;; allegro
           :osi)))
  (dolist
    (system (append base-systems sbcl-systems))
    (asdf:register-immutable-system system)))

;; clear systems to avoid collision with systems loaded into executable
(asdf/system-registry:clear-registered-systems)
