(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(declaim (special *document-open-linking*))

;;; @DOCUMENTABLE
(defvar *document-tight*)
(export '*document-tight*)

;;; @PACKAGE-AND-READTABLE
(defvar *document-normalize-packages*)
(export '*document-normalize-packages*)

;;; @BROWSING-WITH-OTHER-BROWSERS
(defvar *browse-html-style*)
(export '*browse-html-style*)

;;; @CODIFICATION
(defvar *document-uppercase-is-code*)
(export '*document-uppercase-is-code*)
(defvar *document-downcase-uppercase-code*)
(export '*document-downcase-uppercase-code*)

;;; @LINKING
(defvar *document-link-code*)
(export '*document-link-code*)
;;; Silence SBCL compiler notes.
#+sbcl
(define-condition unresolvable-reflink (warning) ())
(export 'unresolvable-reflink)
(export 'output-reflink)
(export 'output-label)

;;; @LINKING-TO-THE-HYPERSPEC
(defvar *document-link-to-hyperspec*)
(export '*document-link-to-hyperspec*)
(defvar *document-hyperspec-root*)
(export '*document-hyperspec-root*)

;;; @LINKING-TO-SECTIONS
(defvar *document-link-sections*)
(export '*document-link-sections*)
(defvar *document-max-numbering-level*)
(export '*document-max-numbering-level*)
(defvar *document-max-table-of-contents-level*)
(export '*document-max-table-of-contents-level*)
(defvar *document-text-navigation*)
(export '*document-text-navigation*)
(defvar *document-fancy-html-navigation*)
(export '*document-fancy-html-navigation*)

;;; @LINK-FORMAT
(defvar *document-url-versions*)
(export '*document-url-versions*)
(defvar *document-min-link-hash-length*)
(export '*document-min-link-hash-length*)
(defvar *document-base-url*)
(export '*document-base-url*)

;;; @OUTPUT-FORMATS
(defvar *document-mark-up-signatures*)
(export '*document-mark-up-signatures*)

;;; @PDF-OUTPUT
(defvar *document-pandoc-program*)
(export '*document-pandoc-program*)
(defvar *document-pandoc-pdf-options*)
(export '*document-pandoc-pdf-options*)
(defvar *document-pandoc-pdf-header-includes*)
(export '*document-pandoc-pdf-header-includes*)
(defvar *document-pandoc-pdf-metadata-block*)
(export '*document-pandoc-pdf-metadata-block*)

;;; @HTML-OUTPUT
(defvar *document-html-default-style*)
(export '*document-html-default-style*)
(defvar *document-html-max-navigation-table-of-contents-level*)
(export '*document-html-max-navigation-table-of-contents-level*)
(defvar *document-html-lang*)
(export '*document-html-lang*)
(defvar *document-html-charset*)
(export '*document-html-charset*)
(defvar *document-html-head*)
(export '*document-html-head*)
(defvar *document-html-sidebar*)
(export '*document-html-sidebar*)
(defvar *document-html-top-blocks-of-links*)
(export '*document-html-top-blocks-of-links*)
(defvar *document-html-bottom-blocks-of-links*)
(export '*document-html-bottom-blocks-of-links*)

(autoload downcasingp "mgl-pax/document" :export nil)
(autoload document "mgl-pax/document")
(autoload update-asdf-system-readmes "mgl-pax/document")
(autoload update-asdf-system-html-docs "mgl-pax/document")
;;; UPDATE-PAX-WORLD generates documentation for PAX itself, so load
;;; MGL-PAX/FULL to have all documentation. Otherwise,
;;; MGL-PAX/DOCUMENT would be enough.
(autoload update-pax-world "mgl-pax/full")


(defsection @extending-document (:title "Extending DOCUMENT")
  "For all definitions that it encounters, DOCUMENT calls
  DOCUMENT-OBJECT* to generate documentation. The following utilities
  are for writing new DOCUMENT-OBJECT* methods, which emit Markdown."
  (*format* variable)
  (with-heading macro)
  (doctitle* generic-function)
  (documenting-definition macro)
  (with-dislocated-names macro)
  (document-docstring function)
  (escape-markdown function)
  (prin1-to-markdown function))

(defvar *format*)
(setf (documentation '*format* 'variable)
      "Bound by DOCUMENT to its FORMAT argument, this allows Markdown
      output to depend on the output format.")
(declaim (special *subformat*))

(defmacro with-heading ((stream &key dref link-title-to) &body body)
  "Write a Markdown heading with the DOCTITLE of DREF to STREAM.

  - DREF defaults to the definition for which documentation is
    currently being generated.

  - Nested WITH-HEADINGs produce nested headings.

  - If *DOCUMENT-LINK-SECTIONS*, generate anchors based on DREF.

  - LINK-TITLE-TO behaves like the LINK-TITLE-TO argument of
    DEFSECTION."
  `(call-with-heading ,stream ,(if dref
                                   `(or (locate ,dref)
                                        *documenting-dref* )
                                   '*documenting-dref*)
                      ,link-title-to
                      (lambda (,stream)
                        ,@body)))
(autoload call-with-heading "mgl-pax/document" :export nil)
(declaim (special *first-pass*))

(autoload doctitle "mgl-pax/document")

(defgeneric doctitle* (object)
  (:documentation "DOCTITLE* extends DOCTITLE in the same way
  as DOCSTRING* extends DOCSTRING.

  The default method returns NIL.

  This function is for extension only. Do not call it directly.")
  (:method (object)
    (declare (ignore object))
    nil))

(defmacro documenting-definition ((stream &key dref package readtable
                                   (arglist nil arglistp))
                                  &body body)
  "Write DREF to STREAM as described in
  *DOCUMENT-MARK-UP-SIGNATURES*, and establish DREF as a
   @LOCAL-DEFINITION for the processing of BODY.

  - DREF defaults to the definition for which documentation is
    currently being generated.

  - If DREF has a DOCTITLE, then it is PRINCed after the
    LOCATIVE-TYPE (see @MARKDOWN-IN-TITLES). Else, `(DREF-NAME DREF)`
    is printed subject to *DOCUMENT-DOWNCASE-UPPERCASE-CODE* but with
    all Markdown and @MATHJAX markup escaped.

  - *PACKAGE* and *READTABLE* are bound to PACKAGE and READTABLE for
    the duration of printing the ARGLIST and the processing of BODY.
    If either is NIL, then a default value is computed as described in
    @PACKAGE-AND-READTABLE.

  - ARGLIST:

      - If it is not provided, then it defaults to (ARGLIST DREF).

      - If NIL, then it is not printed.

      - If it is a list, then it is must be a [lambda list][clhs] and
        is printed without the outermost parens and with the package
        names removed from the argument names.

      - If its is a string, then it must be valid Markdown.

  - It is not allowed to have WITH-HEADING within the [dynamic
    extent][clhs] of BODY."
  (let ((%stream (gensym))
        (%dref (gensym))
        (%arglist (gensym)))
    ;; If WITH-HEADING were allowed in BODY, then we couldn't stop if
    ;; *FIRST-PASS*.
    `(unless *first-pass*
       (let* ((,%stream ,stream)
              (,%dref ,dref)
              (,%dref (if ,%dref
                          (locate ,%dref)
                          *documenting-dref*))
              (,%arglist ,(if arglistp
                              arglist
                              (list 'arglist %dref))))
         (when (and *document-link-code*
                    (not (eq *format* :pdf)))
           (anchor ,%dref ,%stream))
         (print-dref-bullet ,%dref ,%stream)
         (when (and *document-link-code*
                    (eq *format* :pdf))
           (anchor ,%dref ,%stream))
         (multiple-value-bind (*package* *readtable*)
             ;; In apropos terse view, whatever BODY emits is to be
             ;; skipped. Do not waste time with
             ;; GUESS-PACKAGE-AND-READTABLE, which can be very
             ;; expensive.
             (if (eq *document-list-view* :terse)
                 (values *package* *readtable*)
                 (guess-package-and-readtable ,package ,readtable
                                              ,%dref ,%arglist))
           (when ,%arglist
             (write-char #\Space ,%stream)
             (print-arglist ,%arglist ,%stream))
           (print-end-bullet ,%stream)
           (unless (eq *document-list-view* :terse)
             (with-local-references
                 (if (member (dref-locative-type ,%dref)
                             '(section glossary-term))
                     ;; See @SUPPRESSED-LINKS.
                     ()
                     ,%dref)
               ,@body)))))))
(autoload print-dref-bullet "mgl-pax/document" :export nil)
(declaim (ftype function print-arglist))
(declaim (ftype function print-end-bullet))
(declaim (ftype function guess-package-and-readtable))
(declaim (ftype function anchor))
(declaim (special *document-list-view*))

(declaim (special *local-references*))
(defmacro with-local-references (refs &body body)
  `(let ((*local-references* (append (ensure-list ,refs) *local-references*)))
     ,@body))

(defmacro with-dislocated-names (names &body body)
  "For each name in NAMES, establish a @LOCAL-DEFINITION."
  `(with-local-references (mapcar (lambda (name)
                                    ;; FIXME: It is ARGUMENT not
                                    ;; DISLOCATED.
                                    (xref name 'argument))
                                  (ensure-list ,names))
     ,@body))

(autoload document-docstring "mgl-pax/document")
(autoload escape-markdown "mgl-pax/document")
(autoload prin1-to-markdown "mgl-pax/document")
(autoload escape-tex "mgl-pax/document")


;;;; Early non-exported definitions

;;; These are used only by the DOCUMENT-OBJECT* (METHOD CLASS-DREF T).
(declaim (ftype function find-link))
(declaim (ftype function link-to-definition))

;;; For DOCUMENT-OBJECT* (METHOD (INCLUDE-DREF T))
(declaim (ftype function codify-and-link))

;;; We need this for DOCUMENTING-DEFINITION.
(defvar *documenting-dref* nil)


(defsection @github-workflow (:title "GitHub Workflow")
  "It is generally recommended to commit generated readmes (see
  UPDATE-ASDF-SYSTEM-READMES), so that users have something to read
  without reading the code and sites like GitHub can display them.

  HTML documentation can also be committed, but there is an issue with
  that: when linking to the sources (see MAKE-GIT-SOURCE-URI-FN), the
  commit id is in the link. This means that code changes need to be
  committed first, and only then can HTML documentation be regenerated
  and committed in a followup commit.

  The second issue is that GitHub is not very good at serving HTML
  files from the repository itself (and
  [http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
  on links to the sources).

  The recommended workflow is to use
  [gh-pages](https://pages.github.com/), which can be made relatively
  painless with the `git worktree` command. The gist of it is to make
  the `doc/` directory a checkout of the branch named `gh-pages`.
  There is a [good
  description](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html)
  of this general process. Two commits are needed still, but it is
  somewhat less painful.

  This way the HTML documentation will be available at

      http://<username>.github.io/<repo-name>

  It is probably a good idea to add sections like the
  @LINKS-AND-SYSTEMS section to allow jumping between the repository
  and the gh-pages site."
  (make-github-source-uri-fn function)
  (make-git-source-uri-fn function))

(defun make-github-source-uri-fn (asdf-system github-uri &key git-version)
  "This function is a backward-compatibility wrapper around
  MAKE-GIT-SOURCE-URI-FN, which supersedes MAKE-GITHUB-SOURCE-URI-FN.
  All arguments are passed on to MAKE-GIT-SOURCE-URI-FN, leaving
  URI-FORMAT-STRING at its default, which is suitable for GitHub."
  (make-git-source-uri-fn asdf-system github-uri :git-version git-version))

(defvar *git-version-for-test* nil)

(defun make-git-source-uri-fn (asdf-system git-forge-uri
                               &key git-version
                                 (uri-format-string "~A/blob/~A/~A#L~S"))
  """Return an object suitable as :SOURCE-URI-FN of a page spec (see
  the PAGES argument of DOCUMENT). The function looks at the source
  location of the object passed to it, and if the location is found,
  the path is made relative to the top-level directory of the git
  checkout containing the file of the ASDF-SYSTEM and finally an \URI
  pointing to your git forge (such as GitHub) is returned. A warning
  is signalled whenever the source location lookup fails or if the
  source location points to a directory not below the directory of
  ASDF-SYSTEM.

  If GIT-FORGE-URI is `"https://github.com/melisgl/mgl-pax/"` and
  GIT-VERSION is `"master"`, then the returned \URI may look like this:

      https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12

  If GIT-VERSION is NIL, then an attempt is made to determine to
  current commit id from the `.git` in the directory holding
  ASDF-SYSTEM. If no `.git` directory is found, then no links to
  the git forge will be generated.

  URI-FORMAT-STRING is a CL:FORMAT control string for four arguments:

  - GIT-FORGE-URI,
  - GIT-VERSION,
  - the relative path to the file of the source location of the reference,
  - and the line number.

  The default value of URI-FORMAT-STRING is for GitHub. If using a
  non-standard git forge, such as Sourcehut or GitLab, simply pass a
  suitable URI-FORMAT-STRING matching the \URI scheme of your forge."""
  ;; Because the git version may change and UPDATE-PAX-WORLD gets
  ;; PAGE-SPECS (containing our return value in :SOURCE-URI-FN) via
  ;; REGISTER-DOC-IN-PAX-WORLD, we need to clear the cache at the
  ;; start of DOCUMENT.
  `(:maker
    ,(lambda ()
       (multiple-value-bind (git-root git-version)
           (asdf-system-git-root-and-version asdf-system
                                             :default-version git-version)
         (when *git-version-for-test*
           (setq git-version *git-version-for-test*))
         (if git-version
             (let ((line-file-position-cache (make-hash-table :test #'equal)))
               (lambda (object)
                 (multiple-value-bind (relative-path line-number)
                     (convert-source-location
                      (source-location object) git-root object
                      line-file-position-cache)
                   (when relative-path
                     (format nil uri-format-string git-forge-uri git-version
                             relative-path (1+ line-number))))))
             (warn "No GIT-VERSION given and can't find .git directory for ~
                   ASDF system~% ~A. Links to git forge will not be generated."
                   (asdf:component-name (dref::find-system* asdf-system))))))))

(defun asdf-system-git-root-and-version (system &key default-version)
  (let ((file (asdf:system-source-file (dref::find-system* system))))
    (when (in-git-p file)
      (values (git-root file)
              (or (git-version file) default-version)))))

(defun in-git-p (pathname)
  (zerop (nth-value
          2 (uiop:run-program (list "git" "-C" (directory-namestring pathname)
                                    "ls-files" "--error-unmatch"
                                    (file-namestring pathname))
                              :output nil
                              :ignore-error-status t))))

(defun git-root (pathname)
  (multiple-value-bind (toplevel error-output exit-code)
      (uiop:run-program (list "git" "-C" (directory-namestring pathname)
                              "rev-parse" "--show-toplevel")
                        :output '(:string :stripped t) :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop exit-code)
        (pathname-as-directory toplevel)
        nil)))

(defun pathname-as-directory (pathname)
  (make-pathname :directory (append (pathname-directory pathname)
                                    (when (pathname-name pathname)
                                      (list (file-namestring pathname))))
                 :name nil :type nil
                 :defaults pathname))

(defun git-version (pathname)
  (multiple-value-bind (version error-output exit-code)
      (uiop:run-program (list "git" "-C" (directory-namestring pathname)
                              "rev-parse" "HEAD")
                        :output '(:string :stripped t) :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop exit-code)
        version
        nil)))

(defun convert-source-location (source-location git-dir object
                                line-file-position-cache)
  (cond ((or
          ;; CCL
          (null source-location)
          ;; SBCL, AllegroCL
          (eq (first source-location) :error))
         (warn "~@<No source location found for ~:_~A: ~:_~A~%~@:>"
               object (second source-location)))
        (t
         (assert (eq (first source-location) :location))
         (let* ((filename (second (assoc :file (rest source-location))))
                (position (second (assoc :position (rest source-location))))
                (relative-path (and filename
                                    (enough-namestring filename git-dir))))
           (if (and relative-path
                    (uiop/pathname:relative-pathname-p relative-path))
               (let ((line-number (file-position-to-line-number
                                   filename position
                                   line-file-position-cache)))
                 (if line-number
                     (values relative-path line-number)
                     (warn "~@<Source location information in file ~S ~
                            is out of date.~@:>" filename)))
               (warn "~@<Source location for ~S is not below the git ~
                     top-level directory ~S.~%~@:>" object git-dir))))))

(defun file-position-to-line-number (filename file-position cache)
  (if (null file-position)
      0
      (let ((line-file-positions (or (gethash filename cache)
                                     (setf (gethash filename cache)
                                           (line-file-positions filename)))))
        (loop for line-number upfrom 0
              for line-file-position in line-file-positions
              do (when (<= file-position line-file-position)
                   (return line-number))))))

;;; This is cached because determining the line number for a given
;;; file position would need to traverse the file, which is extremely
;;; expensive. Note that position 0 is not included, but FILE-LENGTH
;;; is.
(defun line-file-positions (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          for line-number upfrom 0
          while line
          collect (file-position stream))))


;;;; Register PAX itself in PAX World.

(defun pax-sections ()
  (list @pax-manual))

(defun pax-pages ()
  `((:objects ,(pax-sections)
     :source-uri-fn ,(make-git-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))

(register-doc-in-pax-world :pax 'pax-sections 'pax-pages)

(defvar end-of-register-doc-example)
