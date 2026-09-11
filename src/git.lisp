;;; git.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
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

;;; Git package sources.
;;;
;;; 'ocicl install git+URL[@REF][#PARAMS]' installs systems from a git
;;; repository instead of an OCI registry.  The resolved commit is pinned
;;; in the systems CSV with a fullname of the form
;;;
;;;   git+URL@SHA[#ref=REF][&subdirectory=PATH]
;;;
;;; and the fetched tree is placed at <systems-dir>/<basename>-<shortsha>/,
;;; a sibling of ordinary registry trees.  Because the pin is encoded in
;;; the directory name, staleness detection, removal, grouping, and
;;; 'ocicl clean' all work through the same machinery as registry systems.
;;; 'ocicl latest' advances the pin along the recorded REF (or the
;;; remote's default branch when no REF was given).

(in-package #:ocicl)

(named-readtables:in-readtable :interpol-syntax)

;; Defined in ocicl.lisp, which is compiled after this file.
(declaim (special *ocicl-systems* *systems-dir* *systems-csv* *verbose*
                  *force* *color* *color-reset* *color-bold* *color-dim*
                  *color-bright-green*))

(defun git-source-p (string)
  "Return T if STRING denotes a git source rather than a registry system."
  (and (stringp string)
       (uiop:string-prefix-p "git+" string)))

(defun full-sha-p (string)
  "Return T if STRING is a full 40-character hexadecimal commit SHA."
  (and (stringp string)
       (= (length string) 40)
       (every (lambda (c) (digit-char-p c 16)) string)))

(defun encode-git-param (value)
  "Percent-encode characters in VALUE that would collide with fullname syntax."
  (with-output-to-string (out)
    (loop for c across value
          do (if (or (find c "%&#=, ") (char= c #\Tab))
                 (format out "%~2,'0X" (char-code c))
                 (write-char c out)))))

(defun decode-git-param (value)
  "Reverse ENCODE-GIT-PARAM."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length value))
          do (let ((c (char value i)))
               (if (and (char= c #\%) (<= (+ i 3) (length value)))
                   (progn
                     (write-char (code-char (parse-integer value
                                                           :start (1+ i)
                                                           :end (+ i 3)
                                                           :radix 16))
                                 out)
                     (incf i 3))
                   (progn (write-char c out) (incf i)))))))

(defun validate-git-subdir (subdir)
  "Validate a repository-relative SUBDIR path."
  (and (stringp subdir)
       (> (length subdir) 0)
       (not (uiop:string-prefix-p "/" subdir))
       (not (search ".." subdir))
       subdir))

(defun parse-git-parts (source)
  "Split a git+ SOURCE into its raw pieces.  Returns (values URL AT-PART
REF-PARAM SUBDIR) where AT-PART is whatever followed the last '@' in the
URL position and REF-PARAM/SUBDIR come from the ref= and subdirectory=
fragment parameters."
  (unless (git-source-p source)
    (error "not a git source: ~A" source))
  (when (or (find #\, source) (find #\Space source))
    (error "git source may not contain spaces or commas: ~A" source))
  (let* ((rest (subseq source 4))
         (hash (position #\# rest))
         (base (if hash (subseq rest 0 hash) rest))
         (fragment (when hash (subseq rest (1+ hash))))
         (at-part nil)
         (ref-param nil)
         (subdir nil))
    (when fragment
      (dolist (param (uiop:split-string fragment :separator "&"))
        (let ((eq (position #\= param)))
          (unless eq
            (error "malformed parameter ~S in ~A" param source))
          (let ((key (subseq param 0 eq))
                (value (decode-git-param (subseq param (1+ eq)))))
            (cond ((string= key "ref") (setf ref-param value))
                  ((string= key "subdirectory") (setf subdir value))
                  (t (error "unknown parameter ~S in ~A" key source)))))))
    ;; Split an @SUFFIX off the base URL.  The '@' must follow the last
    ;; '/', so user@host authorities are never mistaken for refs, and a
    ;; git ref can never contain ':', which rules out scp-style host:path
    ;; remainders.
    (let ((at (position #\@ base :from-end t))
          (slash (position #\/ base :from-end t)))
      (when (and at
                 (> at (or slash -1))
                 (not (find #\: base :start at)))
        (let ((suffix (subseq base (1+ at))))
          (when (plusp (length suffix))
            (setf at-part suffix
                  base (subseq base 0 at))))))
    (when (zerop (length base))
      (error "empty git URL in ~A" source))
    (when (and subdir (not (validate-git-subdir subdir)))
      (error "invalid subdirectory ~S in ~A" subdir source))
    (values base at-part ref-param subdir)))

(defun parse-git-source (source)
  "Parse a user-supplied git source, git+URL[@REF][#PARAMS].  REF may be a
branch, tag, or commit SHA, given after '@' or as a ref= fragment
parameter.  Returns a plist (:url URL :ref REF :subdir SUBDIR)."
  (multiple-value-bind (url at-part ref-param subdir)
      (parse-git-parts source)
    (when (and at-part ref-param)
      (error "both @~A and #ref=~A given in ~A" at-part ref-param source))
    (list :url url :ref (or at-part ref-param) :subdir subdir)))

(defun parse-git-fullname (fullname)
  "Parse a pinned systems-CSV fullname, git+URL@SHA[#ref=REF][&subdirectory=PATH].
Returns (values URL SHA REF SUBDIR)."
  (multiple-value-bind (url at-part ref-param subdir)
      (parse-git-parts fullname)
    (unless (full-sha-p at-part)
      (error "git fullname ~A is not pinned to a commit SHA" fullname))
    (values url at-part ref-param subdir)))

(defun make-git-fullname (url sha &key ref subdir)
  "Build the pinned systems-CSV fullname for a git source."
  (let ((params (append (when ref
                          (list #?"ref=${(encode-git-param ref)}"))
                        (when subdir
                          (list #?"subdirectory=${(encode-git-param subdir)}")))))
    (format nil "git+~A@~A~@[#~{~A~^&~}~]" url sha params)))

(defun git-repo-basename (url &optional subdir)
  "Derive a directory-name base for a git source: the final path segment
of SUBDIR when one is given, else of URL (without any .git suffix)."
  (let* ((trimmed (string-right-trim "/" (or subdir url)))
         (cut (max (or (position #\/ trimmed :from-end t) -1)
                   (or (position #\: trimmed :from-end t) -1)))
         (base (subseq trimmed (1+ cut)))
         (base (if (and (not subdir) (uiop:string-suffix-p base ".git"))
                   (subseq base 0 (- (length base) 4))
                   base)))
    (unless (and (plusp (length base))
                 (<= (length base) 200)
                 (every (lambda (c) (or (alphanumericp c) (find c "-_.+"))) base)
                 (not (uiop:string-prefix-p "." base)))
      (error "cannot derive a directory name from git source ~A" (or subdir url)))
    base))

(defun git-tree-dirname (url sha &optional subdir)
  "Directory name for a git source's tree pinned at SHA: <basename>-<shortsha>."
  (format nil "~A-~A" (git-repo-basename url subdir) (take 7 sha)))

(defun assert-no-git-tree-conflict (rel-dirname url subdir)
  "Signal an error if REL-DIRNAME is already claimed by a different git
source, so two sources can never silently clobber each other's tree."
  (maphash (lambda (key value)
             (declare (ignore key))
             (when (git-source-p (car value))
               (let ((dirs (pathname-directory (pathname (cdr value)))))
                 (when (and (eql (first dirs) :relative)
                            (equal (second dirs) rel-dirname))
                   (multiple-value-bind (row-url row-sha row-ref row-subdir)
                       (parse-git-fullname (car value))
                     (declare (ignore row-sha row-ref))
                     (unless (and (equal row-url url)
                                  (equal row-subdir subdir))
                       (error "directory ~A is already used by ~A; remove it first"
                              rel-dirname (car value))))))))
           *ocicl-systems*))

(defun run-git (args &key (error-p t))
  "Run git with ARGS.  Returns (values trimmed-stdout exit-code)."
  (debug-log (format nil "running: git~{ ~A~}" args))
  (multiple-value-bind (out err code)
      (handler-case
          (uiop:run-program (cons "git" args)
                            :output '(:string :stripped t)
                            :error-output (if *verbose* *error-output* '(:string))
                            :ignore-error-status t)
        (error (e)
          (declare (ignore e))
          (error "unable to run git; is it installed and on your PATH?")))
    (when (and error-p (not (zerop code)))
      (error "git~{ ~A~} failed~@[:~%~A~]" args (and (stringp err) err)))
    (values out code)))

(defun git-remote-ref-sha (url ref)
  "Resolve REF (or the remote's default branch when NIL) on URL to a
commit SHA via ls-remote, without cloning.  Annotated tags are peeled."
  (if (full-sha-p ref)
      ref
      (let* ((patterns (if ref
                           (list ref #?"${ref}^{}")
                           (list "HEAD")))
             (out (run-git (append (list "ls-remote" url) patterns)))
             (sha nil))
        ;; Each line is "SHA<tab>refname"; a peeled "^{}" line, when
        ;; present, names the commit an annotated tag points at and wins.
        (dolist (line (uiop:split-string out :separator (string #\Newline)))
          (let ((tab (position #\Tab line)))
            (when tab
              (let ((line-sha (subseq line 0 tab))
                    (line-ref (subseq line (1+ tab))))
                (when (or (null sha) (uiop:string-suffix-p line-ref "^{}"))
                  (setf sha line-sha))))))
        (unless (full-sha-p sha)
          (error "cannot resolve ~A on ~A" (or ref "HEAD") url))
        sha)))

(defun fetch-git-tree (url &key ref subdir dirname)
  "Clone URL, check out REF (a branch, tag, or commit SHA; the remote's
default branch when NIL), and place the requested tree (SUBDIR when
given) at <systems-dir>/DIRNAME/, replacing any existing tree there.
DIRNAME defaults to <basename>-<shortsha>.  Returns (values RESOLVED-SHA
RELATIVE-DIRNAME)."
  (let ((tmp-dir (get-temp-ocicl-dl-pathname)))
    (unwind-protect
         (progn
           (uiop:ensure-all-directories-exist (list tmp-dir *systems-dir*))
           (let ((tmp (uiop:native-namestring tmp-dir)))
             ;; A blobless clone is cheap and still lets us check out any
             ;; pinned commit; fall back to a full clone for servers
             ;; without partial-clone support.
             (multiple-value-bind (out code)
                 (run-git (list "clone" "--quiet" "--filter=blob:none"
                                "--no-checkout" url tmp)
                          :error-p nil)
               (declare (ignore out))
               (unless (zerop code)
                 (run-git (list "clone" "--quiet" "--no-checkout" url tmp))))
             (when subdir
               (run-git (list "-C" tmp "sparse-checkout" "set" subdir)))
             ;; A plain checkout handles branches (via git's remote-branch
             ;; guessing), tags, and commit SHAs alike.
             (if ref
                 (run-git (list "-C" tmp "checkout" "--quiet" ref))
                 (run-git (list "-C" tmp "checkout" "--quiet" "--detach" "HEAD")))
             (let* ((resolved (run-git (list "-C" tmp "rev-parse" "HEAD")))
                    (rel-dirname (or dirname (git-tree-dirname url resolved subdir)))
                    (target-dir (merge-pathnames
                                 (make-pathname :directory `(:relative ,rel-dirname))
                                 *systems-dir*))
                    (source-dir (if subdir
                                    (merge-pathnames
                                     (uiop:ensure-directory-pathname subdir)
                                     tmp-dir)
                                    tmp-dir)))
               (unless (full-sha-p resolved)
                 (error "cannot resolve ~A on ~A" (or ref "HEAD") url))
               (assert-no-git-tree-conflict rel-dirname url subdir)
               (unless (uiop:directory-exists-p source-dir)
                 (error "subdirectory ~A not found in ~A" subdir url))
               (unless subdir
                 (let ((git-meta (merge-pathnames ".git/" tmp-dir)))
                   (when (uiop:directory-exists-p git-meta)
                     (uiop:delete-directory-tree git-meta :validate t))))
               (unless (strictly-under-systems-dir-p target-dir)
                 (error "refusing to write tree outside the systems directory: ~A"
                        rel-dirname))
               (when (uiop:directory-exists-p target-dir)
                 (uiop:delete-directory-tree
                  target-dir
                  :validate #'strictly-under-systems-dir-p))
               (uiop:ensure-all-directories-exist (list target-dir))
               (copy-directory:copy source-dir target-dir)
               (values resolved rel-dirname))))
      (when (uiop:directory-exists-p tmp-dir)
        (uiop:delete-directory-tree tmp-dir :validate t)))))

(defun register-git-tree (fullname rel-dirname)
  "Register every .asd file under <systems-dir>/REL-DIRNAME/ with FULLNAME
in *OCICL-SYSTEMS*.  Signals an error if the tree defines no systems.
Returns the list of registered system names."
  (let ((tree-dir (merge-pathnames
                   (make-pathname :directory `(:relative ,rel-dirname))
                   *systems-dir*))
        (registered nil))
    (dolist (asd (find-asd-files tree-dir))
      (let* ((sysname (pathname-name asd))
             (mangled (mangle sysname))
             (existing (gethash mangled *ocicl-systems*))
             (new (cons fullname (enough-namestring (namestring asd) *systems-dir*))))
        (when (and existing
                   (not (git-source-p (car existing)))
                   (not (equal existing new)))
          (format t "; ~A now comes from ~A (previously ~A)~%"
                  sysname fullname (car existing)))
        (setf (gethash mangled *ocicl-systems*) new)
        (push sysname registered)))
    (unless registered
      (error "no system definitions found in ~A" fullname))
    (nreverse registered)))

(defun remove-git-tree-rows (rel-dirname)
  "Drop every *OCICL-SYSTEMS* row whose .asd lives under REL-DIRNAME."
  (let ((stale nil))
    (maphash (lambda (key value)
               (let ((dirs (pathname-directory (pathname (cdr value)))))
                 (when (and (eql (first dirs) :relative)
                            (equal (second dirs) rel-dirname))
                   (push key stale))))
             *ocicl-systems*)
    (dolist (key stale)
      (remhash key *ocicl-systems*))
    stale))

(defun report-git-install (rel-dirname url sha)
  (if *color*
      (format t #?"${*color-dim*};${*color-reset*} installed ~
                   ${*color-bold*}${*color-bright-green*}${rel-dirname}${*color-reset*} ~
                   ${*color-dim*}from ${url}@${(take 7 sha)}${*color-reset*}~%")
      (format t "; installed ~A from ~A@~A~%" rel-dirname url (take 7 sha))))

(defun install-git-source (source &key (write-csv t))
  "Handle 'ocicl install git+...': fetch the tree, pin the resolved
commit in the systems CSV, and register its systems.  Returns the list
of registered system names."
  (destructuring-bind (&key url ref subdir) (parse-git-source source)
    (multiple-value-bind (sha rel-dirname)
        (fetch-git-tree url :ref ref :subdir subdir)
      (let* ((fullname (make-git-fullname url sha :ref ref :subdir subdir))
             (registered (register-git-tree fullname rel-dirname)))
        (report-git-install rel-dirname url sha)
        (when write-csv
          (write-systems-csv))
        (dolist (sysname registered)
          (download-system-dependencies sysname))
        registered))))

(defun refetch-git-row (fullname rel-asd-path)
  "Re-fetch the pinned tree behind a systems-CSV row whose files are
missing, e.g. after a fresh clone.  Uses the row's own directory name so
re-fetched files land exactly where the CSV says they are."
  (multiple-value-bind (url sha ref subdir) (parse-git-fullname fullname)
    (declare (ignore ref))
    (let ((rel-dirname (second (pathname-directory (pathname rel-asd-path)))))
      (fetch-git-tree url :ref sha :subdir subdir :dirname rel-dirname)
      (report-git-install rel-dirname url sha))))

(defun git-row-groups ()
  "Group git-sourced *OCICL-SYSTEMS* rows by tree.  Returns an alist of
(REL-DIRNAME . FULLNAME)."
  (let ((groups nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (git-source-p (car value))
                 (let ((dirs (pathname-directory (pathname (cdr value)))))
                   (when (and (eql (first dirs) :relative) (second dirs))
                     (pushnew (cons (second dirs) (car value)) groups
                              :key #'car :test #'equal)))))
             *ocicl-systems*)
    groups))

(defun latest-git-tree (rel-dirname fullname)
  "Advance one git tree along its recorded ref.  Returns T if the pin moved."
  (multiple-value-bind (url sha ref subdir) (parse-git-fullname fullname)
    (cond
      ((full-sha-p ref)
       (when *verbose*
         (format t "; ~A is pinned at ~A; leaving it alone~%"
                 rel-dirname (take 7 ref)))
       nil)
      (t
       (let ((new-sha (handler-case (git-remote-ref-sha url ref)
                        (error (e)
                          (format *error-output* "; error checking ~A: ~A~%" url e)
                          (return-from latest-git-tree nil)))))
         (if (string= new-sha sha)
             (progn
               (when *verbose*
                 (format t "; ~A is up to date~%" rel-dirname))
               nil)
             (handler-case
                 (multiple-value-bind (resolved new-dirname)
                     (fetch-git-tree url :ref new-sha :subdir subdir)
                   (remove-git-tree-rows rel-dirname)
                   (unless (equal rel-dirname new-dirname)
                     (let ((old-dir (merge-pathnames
                                     (make-pathname :directory `(:relative ,rel-dirname))
                                     *systems-dir*)))
                       (when (and (strictly-under-systems-dir-p old-dir)
                                  (uiop:directory-exists-p old-dir))
                         (uiop:delete-directory-tree
                          old-dir
                          :validate #'strictly-under-systems-dir-p))))
                   (let* ((new-fullname (make-git-fullname url resolved
                                                           :ref ref :subdir subdir))
                          (registered (register-git-tree new-fullname new-dirname)))
                     (report-git-install new-dirname url resolved)
                     (dolist (sysname registered)
                       (download-system-dependencies sysname)))
                   t)
               (error (e)
                 (format *error-output* "; error updating ~A: ~A~%" rel-dirname e)
                 nil))))))))

(defun latest-git-rows ()
  "Advance every git-sourced tree along its recorded ref.  Returns T if
any pin moved."
  (let ((changed nil))
    (dolist (group (git-row-groups))
      (when (latest-git-tree (car group) (cdr group))
        (setf changed t)))
    changed))
