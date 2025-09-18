(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @browsing-live-documentation (:title "Browsing Live Documentation")
  """Documentation for definitions in the running Lisp can be browsed
  directly without generating documentation in the offline manner.
  HTML documentation, complete with @CODIFICATION and @LINKING, is
  generated from docstrings of all kinds of definitions and PAX
  SECTIONs in the running Lisp on the fly. This allows ad-hoc
  exploration of the Lisp, much like `describe-function`,
  `apropos-command` and other online help commands in Emacs, for which
  direct parallels are provided.

  Still, even without Emacs and @SLIME, limited functionality can be
  accessed through @PAX-LIVE-HOME-PAGE by starting the live
  documentation web server [manually][ensure-web-server].

  If @EMACS-SETUP has been done, the Elisp function
  `mgl-pax-document` (maybe bound to `\\C-.`) generates and displays
  documentation as a single HTML page. If necessary, a disambiguation
  page is generated with the documentation of all matching
  definitions. For example, to view the documentation of this very
  SECTION, one can do:

      M-x mgl-pax-document
      View Documentation of: pax::@browsing-live-documentation

  Alternatively, pressing `\\C-.` with point over the text
  `\pax::@browsing-live-documentation` in a buffer achieves the same
  effect.

  In interactive use, `mgl-pax-document` behaves similarly to
  [`\\M-.`][@NAVIGATING-IN-EMACS] except:

  - It shows the DOCUMENTation of some definition and does not visit
    its SOURCE-LOCATION.

  - It considers definitions with all LOCATIVE-TYPES not just
    LISP-LOCATIVE-TYPES because it doesn't need SOURCE-LOCATION.

      This also means that completion works for [CLHS][locative]
      definitions:

      - `\"lambda list<TAB>` lists `"lambda list"` and `"lambda list
        keywords"`, both HyperSpec glossary entries. This is similar
        to `common-lisp-hyperspec-glossary-term` in Elisp but also
        works for HyperSpec section titles.

      - `\"#<TAB>` lists all sharpsign reader macros (similar to
        `common-lisp-hyperspec-lookup-reader-macro` in Elisp).

      - `\"~<TAB>` lists all CL:FORMAT directives (similar to
        `common-lisp-hyperspec-format` in Elisp).

      - `\"loop:~<TAB>` lists all [loop keywords][clhs].

  - It works in non-`lisp-mode` buffers by reinterpreting a few lines
    of text surrounding point as lisp code (hence the suggested
    _global_ binding).

  - It supports fragment syntax at the prompt:

          NAME LOCATIVE FRAGMENT-NAME FRAGMENT-LOCATIVE

      This is like `NAME LOCATIVE`, but the browser scrolls to the
      definition of `FRAGMENT-NAME FRAGMENT-LOCATIVE` within that
      page.

      For example, entering this at the prompt will generate the
      entire PAX manual as a single page and scroll to the very
      section you are reading within it:

          pax::@pax-manual pax:section pax::@browsing-live-documentation pax:section

  - If the empty string is entered at the prompt, and there is no
    existing w3m buffer or w3m is not used, then @PAX-LIVE-HOME-PAGE
    is visited. If there is a w3m buffer, then entering the empty
    string displays that buffer.

  The convenience function
  `mgl-pax-current-definition-toggle-view` (`C-c C-d c`) documents the
  definition with point in it."""
  (@browsing-with-w3m section)
  (@browsing-with-other-browsers section)
  (@apropos section)
  (@pax-live-home-page section))

(define-glossary-term @w3m-key-bindings
    (:title "w3m's default key bindings"
     :url "https://emacs-w3m.github.io/info/emacs-w3m_10.html#Key-Binding"))

(defsection @browsing-with-w3m (:title "Browsing with w3m")
  """When the value of the Elisp variable `mgl-pax-browser-function`
  is `w3m-browse-url` (see @EMACS-SETUP), the Emacs w3m browser is
  used without the need for a web server, and also offering somewhat
  tighter integration than @BROWSING-WITH-OTHER-BROWSERS.

  With @W3M-KEY-BINDINGS, moving the cursor between links involves
  `TAB` and `S-TAB` (or `<up>` and `<down>`). `RET` and `<right>`
  follow a link, while `B` and `<left>` go back in history.

  In addition, the following PAX-specific key bindings are available:

  - `\\M-.` visits the source location of the definition corresponding
    to the link under the point.

  - Invoking `mgl-pax-document` on a section title link will show the
    documentation of that section on its own page.

  - `n` moves to the next PAX definition on the page.

  - `p` moves to the previous PAX definition on the page.

  - `u` follows the first `Up:` link (to the first containing
    [SECTION][class]) if any.

  - `\\U` is like `u` but positions the cursor at the top of the page.

  - `v` visits the source location of the current definition (the one
    under the cursor or the first one above it).

  - `\\V` visits the source location of the first definition on the
    page.
  """)

(defsection @browsing-with-other-browsers
    (:title "Browsing with Other Browsers")
  """When the value of the Elisp variable `mgl-pax-browser-function`
  is not `w3m-browse-url` (see @EMACS-SETUP), requests are served via
  a web server started in the running Lisp, and documentation is most
  likely displayed in a separate browser window.

  By default, `mgl-pax-browser-function` is `nil`, which makes PAX use
  `browse-url-browser-function`. You may want to customize the related
  `browse-url-new-window-flag` or, for Chrome, set
  `browse-url-chrome-arguments` to `("--new-window")`.

  By default, `mgl-pax-web-server-port` is `nil`, and PAX will pick a
  free port automatically.

  In the browser, clicking on the locative on the left of the
  name (e.g. in `- [function] PRINT`) will raise and focus the Emacs
  window (if Emacs is not in text mode, and also subject to window
  manager focus stealing settings), then go to the corresponding
  source location. For sections, clicking on the lambda link will do
  the same (see *DOCUMENT-FANCY-HTML-NAVIGATION*).

  Finally, note that the URLs exposed by the web server are subject to
  change, and even the port used may vary by session if the Elisp
  variable `mgl-pax-web-server-port` is nil."""
  (*browse-html-style* variable))

(defvar/autoloaded *browse-html-style* :charter
  "The HTML style to use for browsing live documentation. Affects only
  non-w3m browsers. See *DOCUMENT-HTML-DEFAULT-STYLE* for the possible
  values.

  If you change this variable, you may need to do a hard refresh in
  the browser (often `C-<f5>`).")

(defun document-for-emacs (pax*-url output &optional *document-hyperspec-root*)
  (swank/backend:converting-errors-to-error-location
    (swank::with-buffer-syntax (swank::*buffer-package*)
      ;; This is called only for `w3m' and `precheck'. See below.
      (let ((*subformat* (if (uiop:directory-exists-p output) :w3m nil)))
        `(:url ,(document-pax*-url pax*-url output))))))

;;; Write documentation of PAX*-URL into OUTPUT. Return the (:URL
;;; <URL>) to visit or (:ERROR <STRING>). PAX*-URL may be a `pax:',
;;; `pax-eval:' or a `pax-wall' URL.
;;;
;;; - `w3m': OUTPUT denotes an existing directory. The filename within
;;;   that directory is determined by (PAX-URL-TO-FILENAME PAX*-URL)
;;;   and the returned :URL will be this `file:' (plus the
;;;   canonicalized fragment) or an external URL (e.g. to the
;;;   hyperspec). w3m then visits this URL.
;;;
;;; - `precheck': OUTPUT is NIL. No documentation is generated. This
;;;   is for redirection and error checking from Emacs before invoking
;;;   a non-w3m browser. The returned :URL is PAX*-URL canonicalized
;;;   (so that the fragment is found) or an external URL.
;;;
;;; - `web': OUTPUT denotes a file. Currently, this is used by the web
;;;    server (serving non-w3m browsers). Behaves similarly to the
;;;    `w3m' case.
(defun document-pax*-url (url output)
  (declare (type (or pathname string null) output))
  (multiple-value-bind (scheme authority path query fragment) (parse-url url)
    ;; The only QUERY is PKG handled by the web server.
    (declare (ignore query))
    (let ((place (cond ((string= scheme "pax")
                        (document-pax-url path output))
                       ((string= scheme "pax-eval")
                        (document-pax-eval-url path output))
                       ((string= scheme "pax-wall")
                        (document-pax-wall-url url path output))
                       (t
                        (assert nil)))))
      (return-url place scheme authority path fragment))))

(defun return-url (place scheme authority path fragment)
  (declare (type (or string pathname null) place))
  (let ((query
          ;; w3m doesn't like queries on file URLs, so we set
          ;; `slime-buffer-package' instead on the Elisp side.
          (unless (eq *subformat* :w3m)
            (format nil "pkg=~A" (urlencode (package-name *package*)))))
        (fragment (when fragment
                    (canonicalize-pax-url-fragment fragment))))
    (cond
      ;; Redirect to external or a PAX URL.
      ((stringp place)
       (multiple-value-bind (p-scheme p-authority p-path p-query p-fragment)
           (parse-url place)
         ;; Add the canonicalized fragment when redirecting to a PAX URL.
         (cond ((string= p-scheme "pax")
                (assert (null p-authority))
                (assert (null p-query))
                (assert (null p-fragment))
                (make-url :scheme p-scheme :path p-path
                          :encoded-query query :fragment fragment))
               (t
                place))))
      ;; Output was written to this file.
      ((pathnamep place)
       (multiple-value-bind (f-scheme f-authority f-path f-query f-fragment)
           (parse-url (pathname-to-file-url place))
         (assert (string= f-scheme "file"))
         (assert (null f-authority))
         (assert (null f-query))
         (assert (null f-fragment))
         (make-url :scheme "file" :path f-path
                   :encoded-query query :fragment fragment)))
      ;; OUTPUT was NIL and there was no redirection to an external
      ;; URL.
      ((null place)
       ;; Return the original PAX URL with the package in QUERY and a
       ;; possibly updated FRAGMENT.
       (make-url :scheme scheme :authority authority :path path
                 :encoded-query query :fragment fragment)))))

(defun canonicalize-pax-url-fragment (fragment)
  (let ((dref (parse-dref fragment)))
    (if dref
        (dref-to-anchor dref)
        fragment)))

(defun filename-for-pax-url (output pax-url)
  (let ((filename (make-pathname :name (pax-url-to-filename pax-url)
                                 :type "html")))
    (if (uiop:directory-pathname-p output)
        (merge-pathnames filename output)
        output)))

;;; Same as *UNRESERVED-URL-CHARACTERS*, but with #\* reserved. Make
;;; that #\: reserved, too, for CCL to be happy.
(defparameter *unreserved-pax-url-filename-characters*
  (let ((array (make-array 256 :element-type 'bit :initial-element 0)))
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    (_mark-range array #\0 #\9)
    (_mark-one array #\-)
    (_mark-one array #\_)
    (_mark-one array #\.)
    (_mark-one array #\@)
    (_mark-one array #\+)
    array))

;;; A PAX URL is like pax:PATH[#FRAGMENT]. When the documentation is
;;; written to a file, special characters in PATH must be somehow
;;; escaped so that the result is a valid file name. Also, since w3m's
;;; URL history is buggy with regards to URL encoding and decoding,
;;; the encoded PATH must have no #\% in it.
(defun pax-url-to-filename (pax-url)
  (let ((*unreserved-url-characters* *unreserved-pax-url-filename-characters*)
        (*url-escape-char* #\x))
    (urlencode pax-url)))

(defun pax-url-from-filename (filename)
  (let ((*unreserved-url-characters* *unreserved-pax-url-filename-characters*)
        (*url-escape-char* #\x))
    (urldecode filename)))


;;;; Handling of "pax-wall:" URLs. WALL is the acronym of
;;;; Word-And-Locatives-List (also, see the WALL type).

;;; pax-wall:<URLENCODED-STRINGIFIED-WALL>. The list is encoded in an
;;; URL to make the Elisp side simpler as what to document goes
;;; through `mgl-pax-w3m-goto-url'.
(defun document-pax-wall-url (pax-url path dirname)
  (let* ((wall (read-from-string path))
         (definitions (definitions-of-wall wall :definitions #'definitions*)))
    (case (length definitions)
      ((0) (error "No definitions for ~{~S~^ or ~} (in ~S)."
                  (mapcar #'first wall) (package-name *package*)))
      ((1) (document-for-emacs/reference (first definitions) dirname))
      (t (document-for-emacs/ambiguous
          definitions pax-url
          (format nil "~{~A~^ and ~} in buffer context" (mapcar #'first wall))
          dirname)))))


;;;; Handling of "pax-eval:" URLs

(defun document-pax-eval-url (path output)
  (let* ((form (read-from-string (urldecode path)))
         (stuff (pax-eval form)))
    (when output
      (let* ((url-path (with-standard-io-syntax*
                         (prin1-to-string form)))
             (filename (filename-for-pax-url
                        output
                        (format nil "pax-eval:~A" (urlencode url-path)))))
        (document/open/file filename stuff :title path)
        filename))))

(defun pax-eval (form)
  ;; For the sake of MGL-PAX/WEB, don't allow arbitrary evaluations.
  (unless (and (listp form)
               (member (first form) '(pax-apropos* pax-live-home-page))
               (every (lambda (arg)
                        (and (constantp arg)
                             (or (not (symbolp arg))
                                 (allowed-to-evaluate-symbol-p arg))))
                      (rest form)))
    (error "Not allowed to evaluate ~S." form))
  (eval form))

(defun allowed-to-evaluate-symbol-p (symbol)
  (or (keywordp symbol)
      (member symbol '(nil t))))

(defun make-pax-eval-url (form)
  (finalize-pax-url (format nil "pax-eval:~A"
                            (urlencode (with-standard-io-syntax*
                                         (prin1-to-string form))))))


;;;; Handling of "pax:" URLs

(define-condition pax-http-error (error)
  ((message :initarg :message :reader message-of)
   (message-args :initarg :message-args :reader message-args-of)
   (http-code :initarg :http-code :reader http-code-of))
  (:report (lambda (condition stream)
             (format stream "~@<~?~:@>" (message-of condition)
                     (message-args-of condition)))))

(defun document-pax-url (path output)
  (unless path
    (error "Nothing to document."))
  (let ((drefs (or
                ;; This handles PAX URLs generated by DREF-TO-PAX-URL,
                ;; NAME-TO-PAX-URL) (e.g for open linking or
                ;; CURRENT-DEFINITION-PAX-URL-FOR-EMACS), properly
                ;; parsing names with spaces, colons and all kind of
                ;; funny characters.
                (ignore-errors (definitions-for-pax-url-path path))
                ;; These follow @PARSING, so that `mgl-pax-document'
                ;; either with defaulting or prompting works similarly
                ;; to @NAVIGATION and @LINKING.
                (ensure-list (parse-dref path))
                (parse-definitions* path))))
    (cond ((endp drefs)
           (multiple-value-bind (dref locative junk) (parse-dref path)
             (declare (ignore dref))
             (if (or locative (null junk))
                 (error 'pax-http-error
                        :message "Could not find definitions for ~S ~
                                 (in package ~S)."
                        :message-args (list path (package-name *package*))
                        :http-code 404)
                 (error 'pax-http-error
                        :message "Bad locative ~S (in package ~S)."
                        :message-args (list junk (package-name *package*))
                        :http-code 404))))
          ((= (length drefs) 1)
           (document-for-emacs/reference (first drefs) output))
          (t
           (document-for-emacs/ambiguous
            drefs (format nil "pax:~A" (urlencode path))
            path output)))))

;;; See if (DOCUMENT REFERENCE) with *DOCUMENT-OPEN-LINKING* T would
;;; try to document an external reference, and return it.
(defun open-reference-if-external (reference)
  (let ((*document-open-linking* t))
    (let ((dref (locate reference)))
      (when (external-dref-p dref)
        dref))))

;;; E.g. "pax:foo function"
(defun document-for-emacs/reference (reference output)
  (let ((reference (replace-go-target reference)))
    (if-let (external-reference (open-reference-if-external reference))
      (external-dref-url external-reference)
      (let ((url (format nil "pax:~A" (urlencode (dref-to-anchor reference)))))
        (values (if output
                    (document-for-emacs/reference-1 reference output url)
                    url))))))

(defun document-for-emacs/reference-1 (reference output url)
  (let* ((filename (filename-for-pax-url output url))
         (packagep (packagep (resolve reference nil)))
         (*package* (if packagep
                        (resolve reference)
                        *package*))
         #+nil
         (*print-arglist-key*
           (and packagep (rcurry 'shorten-arglist reference)))
         #+nil
         (*document-docstring-key*
           (and packagep (rcurry 'shorten-docstring reference))))
    (document/open/file filename
                        (if packagep
                            (pax-apropos* nil t
                                          (make-symbol
                                           (package-name *package*)))
                            (documentable-for-reference reference))
                        :title (format nil "~A ~A"
                                       (xref-name reference)
                                       (xref-locative reference)))
    filename))

(defun documentable-for-reference (reference)
  (remove nil
          (append (format-up-links (sections-that-contain (list-all-sections)
                                                          reference)
                                   reference)
                  (list reference)
                  (format-asdf-detritus reference)
                  (format-also-see reference))))

(defun format-up-links (sections dref)
  (when sections
    (with-standard-io-syntax*
      (list
       (with-output-to-string (s)
         (format s "Up: ")
         (dolist (section (sort-by-proximity sections dref))
           (let ((section-dref (locate section)))
             (format s "[~A](~A#~A)"
                     (document-definition-title-or-anchor section-dref)
                     (finalize-pax-url (dref-to-pax-url section-dref))
                     (urlencode (dref-to-anchor dref))))))))))

(defun format-asdf-detritus (dref)
  (when (typep dref 'dref-ext:asdf-system-dref)
    (with-filename-to-asdf-system-name-map
      (let* ((name (dref-name dref))
             (related-asdf-systems (related-asdf-systems name)))
        (multiple-value-bind (packages-defined related-packages)
            (asdf-packages name)
          (flet ((format-system (system)
                   (format nil "- [~A][asdf:system]"
                           (escape-markdown (dref-name (locate system)))))
                 (format-package (package)
                   (format nil "- [~A][cl:package]"
                           (escape-codification
                            (escape-markdown (package-name package))))))
            (append
             (when packages-defined
               (list* "### Packages Defined"
                      (mapcar #'format-package packages-defined)))
             (when related-asdf-systems
               (list* "### [Related][PAX::@RELATED] ASDF Systems"
                      (mapcar #'format-system related-asdf-systems)))
             (when related-packages
               (list* "### [Related][PAX::@RELATED] Packages"
                      (mapcar #'format-package related-packages))))))))))

;;; Return the packages defined directly in files of ASDF-SYSTEM, or
;;; packages defined in files below the directory of ASDF-SYSTEM.
(defun asdf-packages (asdf-system)
  (let* ((packages ())
         (related ())
         (system (dref::find-system* asdf-system))
         (system-name (dref-name (locate system)))
         (system-dirname (asdf-system-dirname system)))
    (dolist (package (sort (list-all-packages) #'string< :key #'package-name))
      (when-let (name (asdf-system-name-of package))
        (cond ((equal system-name name)
               (push package packages))
              ((and system-dirname
                    (starts-with-subseq system-dirname
                                        (asdf-system-dirname
                                         (dref::find-system* name))))
               (push package related)))))
    (values (reverse packages) (reverse related))))

(defun related-asdf-systems (asdf-system)
  (let* ((related ())
         (system (dref::find-system* asdf-system))
         (system-dirname (asdf-system-dirname system))
         (root-dirname system-dirname))
    (when system-dirname
      (dolist (name (sort (asdf:registered-systems) #'string<))
        (let* ((system-1 (dref::find-system* name))
               (dirname-1 (asdf-system-dirname system-1)))
          (when (and (not (eq system system-1))
                     dirname-1
                     (or (starts-with-subseq system-dirname dirname-1)
                         (starts-with-subseq dirname-1 system-dirname)))
            (push system-1 related)
            (when (< (length dirname-1) (length root-dirname))
              (setq root-dirname dirname-1))))))
    (values (reverse related) root-dirname)))

(defun slot-value-if-bound (object slot-name &optional default)
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))

(defun asdf-system-dirname (system)
  (when-let (pathname (slot-value-if-bound system 'asdf/component:absolute-pathname))
    (namestring pathname)))

#+nil
(defun shorten-arglist (string &optional except-reference)
  (let* ((reference *documenting-dref*)
         (n-chars (- 64 (length (prin1-to-string
                                 (xref-locative-type reference)))
                     (length (prin1-to-string
                              (xref-name reference))))))
    (if (and except-reference
             (reference= *documenting-dref* except-reference))
        string
        (shorten-string string :n-lines 1 :n-chars n-chars :ellipsis "..."))))

#+nil
(defun shorten-docstring (docstring &optional except-reference)
  (if (or (stringp (first *objects-being-documented*))
          (and *documenting-dref* except-reference
               (reference= *documenting-dref* except-reference)))
      docstring
      (shorten-string docstring :n-lines 1 :n-chars 68 :ellipsis "...")))

(defun format-also-see (reference)
  (let ((entries ())
        ;; This will stringify reference @NAME for e.g. PACKAGEs.
        (reference (locate reference)))
    (flet ((emit (control &rest args)
             (push (cons control args) entries)))
      (assert (not (external-dref-p reference)))
      (dolist (dref (cons (clhs-dref (dref-name reference)
                                     (dref-locative reference))
                          (definitions* reference)))
        (when (external-dref-p dref)
          (emit "the [~A][~A ~A]"
                (escape-markdown (symbol-name (dref-locative-type dref)))
                (prin1-to-markdown (dref-name dref))
                (prin1-to-markdown (dref-locative dref)))))
      (let ((generic-function-name
              (and (eq (xref-locative-type reference) 'method)
                   (xref-name reference))))
        (when generic-function-name
          (emit "the generic-function `~A`"
                (prin1-to-markdown generic-function-name
                                   ;; It goes between backticks.
                                   :escape-inline nil))))
      (when (< 1 (length (definitions* (xref-name reference))))
        (emit "the [disambiguation page](~A)"
              (finalize-pax-url (name-to-pax-url (xref-name reference)))))
      (unless (eq (xref-locative-type reference) 'section)
        (multiple-value-bind (package other-packages)
            (find-reference-package reference)
          (when package
            (emit "the home package [~A][cl:package]"
                  (escape-markdown (package-name package))))
          (when other-packages
            (emit "other exporting packages ~{[~A][cl:package]~^, ~}"
                  (loop for package in other-packages
                        collect (escape-markdown (package-name package))))))
        (when-let (asdf-system-name (asdf-system-name-of* reference))
          (emit "the defining ASDF system [~A][asdf:system]"
                (escape-markdown asdf-system-name)))))
    (when entries
      (list
       (let ((n-entries (length entries)))
         (with-output-to-string (out)
           (format out "Also, see")
           (loop for entry in (reverse entries)
                 for i upfrom 0
                 do (format out (if (and (< 2 n-entries) (plusp i))
                                    ", "
                                    " "))
                    (when (and (< 1 n-entries)
                               (= i (1- n-entries)))
                      (format out "and "))
                    (apply #'format out entry))
           ;; KLUDGE: The second newline makes 3BMD emit the "Also,
           ;; see ..." string in paragraph in HTML.
           (format out ".~%~%")))))))

(defun find-reference-package (reference)
  (let ((name (xref-name reference)))
    (when (symbolp name)
      (values (symbol-package name) (symbol-other-packages name)))))

;;; E.g. "pax:foo"
(defun document-for-emacs/ambiguous (references pax-url title output)
  (assert (< 1 (length references)))
  (when output
    (let ((filename (filename-for-pax-url output pax-url)))
      (document/open/file
       filename (cons (format nil "## Disambiguation for [~A][pax:dislocated]"
                              (escape-markdown title))
                      (dref::sort-references (replace-go-targets references)))
       :title title)
      filename)))

(defun document/open/file (filename stuff &key title)
  (with-open-file (stream (ensure-directories-exist filename)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format *utf-8-external-format*)
    (when title
      (format stream "<title>~A</title>~%" (escape-html title)))
    (document/open stuff :stream stream)))

(defvar *document/open-extra-args* ())

(defun document/open (documentable &rest args)
  (let ((*document-open-linking* t)
        (*document-url-versions* '(2)))
    (apply #'document documentable
           (append args (list :format (or *subformat* :html))
                   *document/open-extra-args*))))


(defun redocument-for-emacs (file-url output
                             &optional *document-hyperspec-root*)
  (swank/backend:converting-errors-to-error-location
    (swank::with-buffer-syntax (swank::*buffer-package*)
      ;; This is called only for `w3m' and `precheck'.
      (let ((*subformat* (if (uiop:directory-exists-p output) :w3m nil)))
        (multiple-value-bind (scheme authority path query fragment)
            (parse-url file-url)
          (declare (ignore authority query))
          (when (equal scheme "file")
            (assert (null fragment))
            (let ((dir (make-pathname :name nil :type nil :defaults path)))
              (when (string= (namestring dir) output)
                (let ((new-file-url (document-pax*-url
                                     (pax-url-from-filename
                                      (pathname-name path))
                                     dir)))
                  (assert (string= new-file-url file-url)))))))))
    (values)))


;;; Find the source location of the path component of PAX-URL, and
;;; return its dspec and source location. Ignore the fragment. This is
;;; what M-. in a w3m PAX doc buffer does.
(defun locate-pax-url-for-emacs (pax-url)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (multiple-value-bind (scheme authority path) (parse-url pax-url)
          (declare (ignore authority))
          (unless (equal scheme "pax")
            (error "~S doesn't have pax: scheme." pax-url))
          (let ((drefs (definitions-for-pax-url-path path)))
            (assert (<= (length drefs) 1))
            (when drefs
              (let* ((dref (first drefs))
                     (location (source-location dref)))
                (when (eq (first location) :location)
                  ;; List of one Swank dspec and location.
                  `((,(dref::definition-to-dspec dref) ,location)))))))))))


(defun current-definition-pax-url-for-emacs (buffer filename possibilities)
  (with-swank ()
    (swank::with-buffer-syntax ()
      (let ((reference (find-current-definition buffer filename
                                                possibilities)))
        (if reference
            `(:pax-url ,(dref-to-pax-url reference))
            '(:error "Cannot determine current definition."))))))


(defsection @apropos (:title "Apropos")
  "The Elisp functions `mgl-pax-apropos`, `mgl-pax-apropos-all`, and
  `mgl-pax-apropos-package` can display the results of DREF-APROPOS in
  the [live documentation browser] [@browsing-live-documentation].
  These extend the functionality of `slime-apropos`,
  `slime-apropos-all` and `slime-apropos-package` to support more
  kinds of DREF::@DEFINITIONs in an extensible way. The correspondence
  is so close that the PAX versions might [take over the Slime key
  bindings][@EMACS-SETUP].

  Note that apropos functionality is also exposed via the
  @PAX-LIVE-HOME-PAGE.

  More concretely, the PAX versions supports the following extensions:

  - Definitions with string names. One can search for
    [ASDF:SYSTEMs][locative], [PACKAGEs][locative] and
    [CLHS][locative] sections, glossary entries, format directives,
    reader macro characters, loop keywords.

  - Exact or substring matching of the name and the package.

  - Matching only symbol or string names.

  On the @PAX-LIVE-HOME-PAGE, one may @BROWSE-BY-LOCATIVE-TYPE, which
  gives access to some of the apropos functionality via the browser
  without involving Emacs.

  On the result page:

  - A DREF-APROPOS form to reproduce the results at the REPL is shown.

  - One may toggle the EXTERNAL-ONLY and CASE-SENSITIVE boolean
    arguments.

  - One may switch between list, and detailed view. The list view only
    shows the first, [bulleted line][@markdown-output] for each
    definition, while the detailed view includes the full
    documentation of definitions with the exception of SECTIONs.

  - The returned references are presented in two groups: those with
    non-symbol and those with symbol @NAMEs. The non-symbol group is
    sorted by locative type then by name. The symbol group is sorted
    by name then by locative type.

  With `mgl-pax-apropos-all` and `mgl-pax-apropos-package` being
  simple convenience functions on top of `mgl-pax-apropos`, we only
  discuss the latter in detail here. For the others, see the Elisp
  docstrings."
  (@apropos-string-argument section)
  (@apropos-package-argument section))

;;; `mgl-pax-apropos' calls DOCUMENT-FOR-EMACS with a `pax-eval:' URL
;;; that evaluates a call to this function. NAME and PACKAGE are
;;; strings, EXTERNAL-ONLY and CASE-SENSITIVE are boolean.
(defun pax-apropos* (name &optional external-only package case-sensitive
                            (just-list t))
  (let ((name0 name)
        (package0 package))
    (multiple-value-bind (name dtype package)
        (pax-apropos*-process-args name package)
      (let* (;; Whether this is for an exact package match and no
             ;; other restrictions.
             (%packagep (and package (null name) (symbolp package)))
             (pax-entry-points
               (when (and (symbolp package) (find-package package))
                 (entry-point-sections (list-sections-in-package
                                        (find-package package)))))
             (dref-apropos-form
               `(dref-apropos ,(maybe-quote name)
                              :external-only ,external-only
                              :package ,(maybe-quote package)
                              :case-sensitive ,case-sensitive
                              :dtype ,(maybe-quote dtype))))
        (multiple-value-bind (non-symbol-definitions symbol-definitions)
            (split-apropos-definitions
             (dref-apropos name :external-only external-only
                                :package package
                                :case-sensitive case-sensitive
                                :dtype dtype))
          (values
           `((progv '(*document-max-table-of-contents-level*) '(-1))
             ,@(when %packagep
                 (documentable-for-reference (xref package 'package)))
             ((progv '(*document-list-view*)
                  '(,(if just-list :terse :detailed)))
              ,(format nil "## [Apropos][pax::@apropos]~%~%```~%~A~%```~%~%"
                       (let ((current-package *package*))
                         (with-standard-io-syntax*
                           (let ((*package* current-package)
                                 (*print-readably* nil)
                                 (*print-pretty* t)
                                 (*print-right-margin* 72))
                             (prin1-to-string* dref-apropos-form)))))
              ,(format nil "Switch to [~S ~S](~A), [~S ~S](~A), or to ~
                            [~A view](~A)."
                       :external-only (not external-only)
                       (make-pax-eval-url
                        `(pax-apropos* ,(maybe-quote name0)
                                       ,(not external-only)
                                       ,(maybe-quote package0)
                                       ,case-sensitive ,just-list))
                       :case-sensitive (not case-sensitive)
                       (make-pax-eval-url
                        `(pax-apropos* ,(maybe-quote name0) ,external-only
                                       ,(maybe-quote package0)
                                       ,(not case-sensitive) ,just-list))
                       (if just-list "detailed" "list")
                       (make-pax-eval-url
                        `(pax-apropos* ,(maybe-quote name0) ,external-only
                                       ,(maybe-quote package0)
                                       ,case-sensitive ,(not just-list))))
              ,@(when pax-entry-points
                  (list "### PAX Entry Points"
                        (break-long-list (sections-tightly pax-entry-points))))
              ,@(when (and (not %packagep) non-symbol-definitions)
                  (cons "### Non-Symbol Definitions"
                        (list `((progv '(*document-tight*) '(t))
                                ,@(break-long-list non-symbol-definitions)))))
              ,@(when symbol-definitions
                  (list "### Symbol Definitions"
                        `((progv '(*document-tight*) '(t))
                          ,@(break-long-list symbol-definitions))))))
           dref-apropos-form))))))

(defun pax-apropos*-process-args (name package)
  (multiple-value-bind (name dtype) (parse-apropos-input name)
    (let ((package (cond
                     ;; For DOCUMENT-FOR-EMACS/REFERENCE-1
                     ((symbolp package)
                      package)
                     ;; For mgl-pax-apropos-package
                     ((listp package)
                      (assert (= (length package) 1))
                      (make-symbol (first package)))
                     (t
                      (read-apropos-package-pattern-from-string package)))))
      (values name dtype package))))

(defun parse-apropos-input (string)
  (multiple-value-bind (name-pattern pos)
      (read-apropos-name-pattern-from-string string)
    (values name-pattern (read-apropos-dtype string :start pos))))

(defsection @apropos-string-argument
    (:title "The STRING Argument of `mgl-pax-apropos`")
  "The STRING argument consists of a name pattern and a [DTYPE][DREF::@DTYPES].

  [read-apropos-name-pattern-from-string function][docstring]
  [read-apropos-dtype function][docstring]")

(defun read-apropos-name-pattern-from-string (string)
  """The name pattern has the following forms.

  - `:print` matches definitions whose names are the string `\print`
    or a symbol with SYMBOL-NAME `\print`. Vertical bar form as in
    `:|prInt|` is also also supported and is useful in when
    CASE-SENSITIVE is true.

  - `"print"` matches definitions whose names contain `\print` as
    a substring.

  - `\print` is like the previous, substring matching case. Use this
    form to save typing if the pattern does not contain spaces and
    does not start with a colon.

  - The empty string matches everything."""
  (cond ((string= string "")
         (values nil 0))
        ((starts-with #\Space string)
         (values nil 1))
        ((starts-with-subseq ":" string)
         (read-uninterned-symbol-from-string string :start 1))
        ((starts-with #\" string)
         (read-from-string string t nil :preserve-whitespace t))
        (t
         (let ((n (or (position #\Space string) (length string))))
           (values (subseq string 0 n) n)))))

(defun read-apropos-dtype (string &key (start 0))
  """After the name pattern, STRING may contain a
  [DTYPE][DREF::@DTYPES] that the definitions must match.

  - `print t` matches definitions with LISP-LOCATIVE-TYPES, which is
    the default (equivalent to `\print`).

  - `print function` matches functions whose names contain
    `\print` (e.g. CL:PRINT and CL:PPRINT).

  - `:print function` is like the previous example but with exact
    name match (so it matches CL:PRINT but not CL:PPRINT).

  - `print variable` matches for example *PRINT-ESCAPE*.

  - `print (or variable function)` matches all variables and functions
    with `print` in their names.

  - `array (or type (not class))` matches DEFTYPEs and but not CLASSes
    with the string `array` in their names.

  - &nbsp;`\pax:section` (note the leading space) matches all PAX
    sections (EXTERNAL-ONLY NIL is necessary to see many of them).

  - `print dref:pseudo` matches definitions with PSEUDO-LOCATIVE-TYPES
    such as MGL-PAX:CLHS.

  - `print dref:top` matches definitions with all locative
    types (LOCATIVE-TYPES)."""
  (if (blankp string :start start)
      t
      (multiple-value-bind (dtype pos) (parse-sexp string :start start)
        (unless (blankp string :start pos)
          (error "~@<Junk following ~S from position ~S in ~S.~:@>"
                 'dtype pos string))
        dtype)))

(defsection @apropos-package-argument
    (:title "The PACKAGE Argument of `mgl-pax-apropos`")
  "[read-apropos-package-pattern-from-string function][docstring]")

(defun read-apropos-package-pattern-from-string (string)
  """When `mgl-pax-apropos` is invoked with a prefix argument, it
  prompts for a package pattern among other things. The pattern may be
  like the following examples.

  - `:none` restricts matches to non-symbol names.

  - `:any` restricts matches to symbol names.

  - `:cl` restricts matches to symbols in the CL package.

  - `:|X Y|` is similar to the previous, but the vertical bar syntax
    allows for spaces in names.

  - `mgl` restricts matches to packages whose name contains `mgl` as a
    substring.

  - `"x y"` is the same as the previous, but the explicit quotes allow
    for spaces in names.

  The above examples assume case-insensitive matching."""
  (let ((string (trim-whitespace string)))
    (multiple-value-bind (pattern pos)
        (cond ((blankp string)
               (values nil (length string)))
              ((starts-with-subseq ":" string)
               (read-uninterned-symbol-from-string string :start 1))
              ((starts-with #\" string)
               (alexandria:nth-value-or 0
                 (ignore-errors (read-from-string string t nil
                                                  :preserve-whitespace t))
                 (values (subseq string 1) (length string))))
              (t
               (let ((n (length string)))
                 (values (subseq string 0 n) n))))
      (unless (= pos (length string))
        (error "~@<Trailing junk from index ~S in ~S.~:@>" pos string))
      (cond ((and (symbolp pattern)
                  (equal (dref::adjust-string-case (symbol-name pattern))
                         (dref::adjust-string-case "none")))
             :none)
            ((and (symbolp pattern)
                  (equal (dref::adjust-string-case (symbol-name pattern))
                         (dref::adjust-string-case "any")))
             :any)
            (t
             pattern)))))

(defun read-uninterned-symbol-from-string (string &key (start 0))
  (let* ((pos (skip-sexp string :start start))
         (substring (subseq string start pos)))
    (values (make-symbol
             ;; KLUDGE: This handles |xyz| but not X|y|Z that some
             ;; lisps may print.
             (if (and (starts-with #\| substring)
                      (ends-with #\| substring))
                 (subseq substring 1 (1- (length substring)))
                 substring))
            pos)))

(defun maybe-quote (obj)
  (if (and obj
           (not (member obj '(nil t)))
           (or (symbolp obj)
               (listp obj)))
      `(quote ,obj)
      obj))

(defun split-apropos-definitions (drefs)
  ;; DREF-APROPOS returns list where non-symbol locative types are
  ;; before symbol locative types.
  (let ((pos (position-if #'symbolp drefs :key #'dref-name)))
    (if pos
        (values (subseq drefs 0 pos)
                (subseq drefs pos))
        (values drefs ()))))

;;; Workaround for PARSE-MARKDOWN-FAST being slow on large lists.
(defun break-long-list (list &key (n 10))
  (let ((len (length list)))
    (loop for i upfrom 0 by n below len
          append (let ((group (subseq list i (min (+ i n) len))))
                   (if (plusp i)
                       (cons `((progv '(*document-tight*) '(nil))
                               ,(format nil "~%~%<span/>~%"))
                             group)
                       group)))))

(defun sections-tightly (section-refs)
  `((progv '(*document-tight*) '(t))
    ,@(loop for ref in section-refs
            collect (format nil "- [~A][pax:section]"
                            (prin1-to-markdown (xref-name ref))))))

(defun list-sections-in-package (package)
  (let ((sections ()))
    (do-symbols (symbol package sections)
      (when (boundp symbol)
        (let ((value (symbol-value symbol)))
          (when (and (typep value 'section)
                     ;; Filter out normal variables with SECTION values.
                     (eq (section-name value) symbol))
            (pushnew value sections)))))))


(defsection @pax-live-home-page (:title "PAX Live Home Page")
  """When @BROWSING-LIVE-DOCUMENTATION, the home page provides
  quick access to documentation of the definitions in the system. In
  Emacs, when `mgl-pax-document` is invoked with the empty string, it
  visits the home page.

  The home page may also be accessed directly by going to the root
  page of the web server (if one is started). Here, unless the home
  page is viewed [with w3m][@BROWSING-WITH-W3M], one may directly look
  up documentation and access @APROPOS via the input boxes provided."""
  ;; FIXME: This may not be loaded.
  (ensure-web-server function)
  (@top-level-pax-sections section)
  (@asdf-systems-and-related-packages section)
  (@systemless-packages section)
  (@browse-by-locative-type section)
  (@related glossary-term))

(defvar *pax-live-home-page-override* nil)

(defun pax-live-home-page (&key (override *pax-live-home-page-override*))
  (with-filename-to-asdf-system-name-map
    (let ((*package* (find-package '#:mgl-pax) ))
      `((progv '(*package*) (list ,(find-package '#:mgl-pax)))
        ,@(list*
           "## [PAX Home][@pax-live-home-page]"
           (or override "You are @BROWSING-LIVE-DOCUMENTATION.")
           "### [Top-level PAX Sections][@top-level-pax-sections]"
           (sections-tightly (top-level-pax-sections))
           (asdf-systems-and-packages-grouped-documentable)
           (locative-types-documentable))))))


(defsection @top-level-pax-sections (:title "Top-level PAX Sections")
  "The @PAX-LIVE-HOME-PAGE lists the top-level PAX sections: those
  that have no other SECTIONs referencing them (see DEFSECTION).")

(defun top-level-pax-sections ()
  (sort (remove-duplicates
         (append (mapcar #'locate (sections-registered-in-pax-world))
                 (entry-point-sections (list-all-sections)))
         :test #'xref=)
        #'string< :key (alexandria:compose 'plain-section-title-or-name
                                           'resolve)))

(defun entry-point-sections (sections)
  (loop for section in sections
        for ref = (locate section)
        unless (or (sections-that-contain sections ref)
                   (member (package-name (symbol-package
                                          (section-name section)))
                           '(#:dref-test #:mgl-pax-test)
                           :test #'string=))
          collect ref))


(defsection @asdf-systems-and-related-packages
    (:title "ASDF:SYSTEMs and Related PACKAGEs")
  "The @PAX-LIVE-HOME-PAGE lists all ASDF:SYSTEMs and PACKAGEs in the Lisp.
  For easier overview, the they are grouped based on their
  SOURCE-LOCATIONs. Two systems are in the same group if the directory
  of one (i.e. the directory of the `.asd` file in which it was
  defined) is the same or is below the other's.

  A PACKAGE presented under a group of systems, if the SOURCE-LOCATION
  of the package is below the the top-most directory among the systems
  in the group.")

(define-glossary-term @related (:title "related")
  "Two definitions are _related_ if the directory of one's
  SOURCE-LOCATIONs contains the directory of the other's.")

(defsection @systemless-packages (:title "Systemless Packages")
  "The @PAX-LIVE-HOME-PAGE lists PACKAGEs
  [unrelated][@asdf-systems-and-related-packages] to any ASDF:SYSTEM
  as systemless.")

(defun asdf-systems-and-packages-grouped-documentable ()
  (multiple-value-bind (groups orphan-packages)
      (asdf-systems-and-packages-grouped)
    (list
     "### [ASDF:SYSTEMs and Related PACKAGEs][
     @asdf-systems-and-related-packages]"
     (loop for (root-dir systems packages) in groups
           collect (with-output-to-string (s)
                     (format s "~%- ~{[~A][asdf:system]~^, ~}~%~%"
                             (mapcar (compose 'escape-markdown 'dref-name
                                              'locate)
                                     systems))
                     (loop for package in packages
                           do (format s "    - [~A][package]~%"
                                      (escape-codification
                                       (escape-markdown
                                        (package-name package)))))))
     (when orphan-packages
       (with-output-to-string (s)
         (format s "### [Systemless Packages][@systemless-packages]~%~%")
         (loop for package in orphan-packages
               do (format s "    - [~A][cl:package]~%"
                          (escape-codification
                           (escape-markdown (package-name package))))))))))

(defun escape-codification (raw)
  (if (codifiable-word-p raw)
      (format nil "\\~A" raw)
      raw))

(defun asdf-systems-and-packages-grouped ()
  (let (;; (ROOT-DIR SYSTEMS PACKAGES)
        (groups ())
        (orphan-packages))
    (flet ((add-system (dir system)
             (when dir
               (dolist (group groups)
                 (let ((root-dir (first group)))
                   (cond ((and root-dir (starts-with-subseq root-dir dir))
                          (push system (second group))
                          (return-from add-system))
                         ((and root-dir (starts-with-subseq dir root-dir))
                          (setf (first group) dir)
                          (push system (second group))
                          (return-from add-system))))))
             (push (list dir (list system) ()) groups))
           (add-package (file package)
             (when file
               (dolist (group groups)
                 (when-let (root-dir (first group))
                   (when (starts-with-subseq root-dir file)
                     (push package (third group))
                     (return-from add-package)))))
             (push package orphan-packages)))
      ;; Sort in reverse order so that PACKAGES in GROUPS come out in
      ;; forward order.
      (dolist (asdf-system-name (sort (asdf:registered-systems) #'string>))
        (let* ((system (dref::find-system* asdf-system-name))
               (dir (asdf-system-dirname system)))
          (add-system dir system)))
      (dolist (package (sort (list-all-packages) #'string> :key #'package-name))
        (let ((file (file-of-dref package)))
          (add-package file package))))
    (values groups orphan-packages)))


(defsection @browse-by-locative-type (:title "Browse by Locative Types")
  "The @PAX-LIVE-HOME-PAGE provides quick links to @APROPOS result
  pages for all DREF::@BASIC-LOCATIVE-TYPES which may have
  definitions.")

(defun locative-types-documentable ()
  `(,(format nil "### [Locative Types][@browse-by-locative-type]")
    ,@(enumerate-locative-types-in-markdown
       (locative-types-maybe-with-definitions))))

(defun enumerate-locative-types-in-markdown (locative-types)
  (loop for locative-type in locative-types
        collect (format nil "- [`~S`](~A)" locative-type
                        (make-pax-eval-url
                         `(pax-apropos* ,(with-standard-io-syntax*
                                           (format nil " ~S" locative-type))
                                        t)))))
