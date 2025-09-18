(in-package :mgl-pax)

;;;; USE-PACKAGEs that were not available in MGL-PAX-BOOTSTRAP.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '#:named-readtables)
  (use-package '#:pythonic-string-reader))


(defun section-entries (section)
  "A list of Markdown docstrings and [XREF][class]s in the order they
  occurred in DEFSECTION."
  (let ((%entries (slot-value section '%entries)))
    (if (eq (first %entries) '%to-xref)
        (setf (slot-value section '%entries)
              (mapcar #'section-entry-to-xref (rest %entries)))
        %entries)))

(defun section-entry-to-xref (entry)
  (if (listp entry)
      ;; CHECK-SECTION-ENTRIES made sure this will work.
      (destructuring-bind (name locative) entry
        (xref name locative))
      entry))

(defun section-link-title-to (section)
  (let ((link-title-to (slot-value section '%link-title-to)))
    (when link-title-to
      (if (listp link-title-to)
          ;; CHECK-LINK-TITLE-TO made sure this will work.
          (destructuring-bind (name locative) link-title-to
            (xref name locative))
          link-title-to))))


(in-readtable pythonic-string-syntax)

(defsection @pax-manual (:title "PAX Manual")
  (@introduction section)
  (@links-and-systems section)
  (@emacs-setup section)
  (@background section)
  (@basics section)
  (@pax-locatives section)
  (@navigating-in-emacs section)
  (@generating-documentation section)
  (@transcripts section)
  (@parsing section)
  (@extension-api section))

(define-glossary-term @slime (:title "SLIME"
                              :url "https://slime.common-lisp.dev/"))

(define-glossary-term @m-.
    (:title "`\\\\M-.`"
     :url "http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions"))

(define-glossary-term @quicklisp (:title "Quicklisp"
                                  :url "https://quicklisp.org/"))

(define-glossary-term @oaoo (:title "OAOO"
                             :url "http://c2.com/cgi/wiki?OnceAndOnlyOnce"))

(define-glossary-term @markdown
    (:title "Markdown"
     :url "https://daringfireball.net/projects/markdown/"))

(define-glossary-term @w3m
    (:title "w3m"
     :url "https://emacs-w3m.github.io/info/emacs-w3m.html"))

(defsection @introduction (:title "Introduction")
  """_What if documentation really lived in the code?_

  Docstrings are already there. If some narrative glued them together,
  we'd be able develop and explore the code along with the
  documentation due to their physical proximity. The main tool that
  PAX provides for this is DEFSECTION:

  ```
  (defsection @foo-random-manual (:title "Foo Random manual")
    "Foo Random is a random number generator library."
    (foo-random-state class)
    (uniform-random function)
    (@foo-random-examples section))
  ```

  Like this one, sections can have docstrings and
  [references][dref::@dref-manual] to
  definitions (e.g. `(UNIFORM-RANDOM FUNCTION)`). These docstrings and
  references are the glue. To support interactive development, PAX

  - makes @SLIME's @M-. work with references and

  - adds a documentation browser.

  See @EMACS-SETUP.

  Beyond interactive workflows, @GENERATING-DOCUMENTATION from
  sections and all the referenced items in Markdown or HTML format is
  also implemented.

  With the simplistic tools provided, one may emphasize the narrative
  as with Literate Programming, but documentation is generated from
  code, not vice versa, and there is no support for chunking.

  _Code is first, code must look pretty, documentation is code_.

  ##### Docstrings

  PAX automatically recognizes and [marks up code][@codification] with
  backticks and [links][@linking] names in code to their definitions.
  Take, for instance, SBCL's ABORT function, whose docstring is
  written in the usual style, uppercasing names of symbols:

  ```
  (docstring #'abort)
  => "Transfer control to a restart named ABORT, signalling
  a CONTROL-ERROR if none exists."
  ```

  Note how in the generated documentation, ABORT is set with a
  monospace font, while `\CONTROL-ERROR` is @AUTOLINKed:

  - \[function\] **\ABORT** *\\&OPTIONAL \CONDITION*

      Transfer control to a restart named `ABORT`, signalling
      a [`\CONTROL-ERROR`][6bc0] if none exists.

    [6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR CONDITION"

  The following [transcript][@transcripts] shows the raw Markdown for
  the previous example.

  ```
  (document #'abort :format :markdown)
  .. - [function] **ABORT** *&OPTIONAL CONDITION*
  ..
  ..     Transfer control to a restart named `ABORT`, signalling
  ..     a [`CONTROL-ERROR`][7c2c] if none exists.
  ..
  ..   [7c2c]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR (MGL-PAX:CLHS CONDITION)"
  ..
  ```

  ##### A Complete Example

  Here is an example of how it all works together:"""
  (foo-random-example (include #.(asdf:system-relative-pathname
                                  :mgl-pax "src/base/foo-random-example.lisp")
                               :header-nl "```" :footer-nl "```"))
  """Note how `(VARIABLE *FOO-STATE*)` in the DEFSECTION form both
  exports `*FOO-STATE*` and includes its documentation in
  `@FOO-RANDOM-MANUAL`. The symbols [VARIABLE][locative] and
  [FUNCTION][locative] are just two instances of DREF::@LOCATIVEs,
  which are used in DEFSECTION to refer to definitions tied to
  symbols.

  `(DOCUMENT @FOO-RANDOM-MANUAL)` generates fancy Markdown or HTML
  output with [automatic markup][\*document-uppercase-is-code\*
  variable] and @AUTOLINKs uppercase @WORDs found in docstrings,
  numbers sections, and creates a table of contents.

  One can even generate documentation for different but related
  libraries at the same time with the output going to different files
  but with cross-page links being automatically added for symbols
  mentioned in docstrings. In fact, this is what @PAX-WORLD does. See
  @GENERATING-DOCUMENTATION for some convenience functions to cover
  the most common cases.

  The [transcript][@transcripts] in the code block tagged with
  `cl-transcript` is automatically checked for up-to-dateness when
  documentation is generated.""")

(defsection @links-and-systems (:title "Links and Systems")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
  for the latest version. There is also a [PAX channel][pax-yt] on
  youtube with a couple of videos.

    [pax-yt]: https://www.youtube.com/playlist?list=PLxbqYr4DvjX68AEdLky4IiHG69VJu6f5s

  PAX is built on top of the [DRef library]
  [DREF::@DREF-MANUAL] (bundled in the same repository).

  - _Installation for deployment_

      The base system is [mgl-pax][asdf:system]. It has very few
      dependencies and is sufficient as a dependency for systems using
      the @BASICS to add documentation. This is to keep deployed code
      small. To install only the bare minimum, with no intention of
      using @NAVIGATING-IN-EMACS, @GENERATING-DOCUMENTATION,
      @BROWSING-LIVE-DOCUMENTATION or using @TRANSCRIPTS, under
      Quicklisp for example, PAX could be installed as:

          (ql:quickload \"mgl-pax\")

  - _Installation for development_

      The heavier dependencies are on the other systems, which
      correspond to the main functionalities provided, intended to be
      used primarily during development. To install the dependencies
      for all features under Quicklisp, do

          (ql:quickload \"mgl-pax/full\")

      Having thus installed the dependencies, it is enough to load the
      base system, which will autoload the other systems as necessary."
  ("mgl-pax" asdf:system)
  ("mgl-pax/navigate" asdf:system)
  ("mgl-pax/document" asdf:system)
  ("mgl-pax/web" asdf:system)
  ("mgl-pax/transcribe" asdf:system)
  ("mgl-pax/full" asdf:system))

(defsection @emacs-setup (:title "Emacs Setup")
  """Here is a quick recipe for setting up PAX for use via @SLIME to
  take advantage of the [conveniences on offer][@EMACS-FUNCTIONALITY].
  Conversely, there is no need to do any of this just to use
  DEFSECTION, write docstrings and for @GENERATING-DOCUMENTATION.

  If PAX was installed from @QUICKLISP, then evaluate this in CL to
  [install][install-pax-elisp] the Elisp code in a stable location:

      (mgl-pax:install-pax-elisp "~/quicklisp/")

  Assuming the Elisp file is in the `~/quicklisp/` directory, add
  something like this to your `.emacs`:

  ```elisp
  (add-to-list 'load-path "~/quicklisp/")
  (require 'mgl-pax)
  (global-set-key (kbd "C-.") 'mgl-pax-document)
  (global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
  (global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
  ```

  When @BROWSING-WITH-OTHER-BROWSERS, for clicking on the locative
  next to a definition to visit the corresponding source location in
  Emacs, permission needs to be given:

  ```elisp
  (setq slime-enable-evaluate-in-emacs t)
  ```"""
  (@emacs-functionality section)
  (@emacs-quicklisp section)
  (@emacs-loading section)
  (@emacs-keys section))

(defsection @emacs-functionality (:title "Functionality Provided")
  "- For @NAVIGATING-IN-EMACS, loading `mgl-pax` extends
    `slime-edit-definitions` (@M-.) by adding
    `mgl-pax-edit-definitions` to `slime-edit-definition-hooks`. There
    are no related variables to customize.

  - For @BROWSING-LIVE-DOCUMENTATION, `mgl-pax-browser-function` and
    `mgl-pax-web-server-port` can be customized in Elisp. To browse
    within Emacs, choose `w3m-browse-url` (see @W3M), and make sure
    both the w3m binary and the w3m Emacs package are installed. On
    Debian, simply install the `w3m-el` package. With other browser
    functions, a [HUNCHENTOOT][package] web server is started.

  - See @TRANSCRIBING-WITH-EMACS for how to use the transcription
    features. There are no related variables to customize.

  Independently from the Common Lisp side, the Elisp functions
  `mgl-pax-hideshow-documentation` and `mgl-pax-hideshow-comments`
  help focus on the code only by folding or unfolding
  MGL-PAX:DEFSECTION, MGL-PAX:DEFINE-GLOSSARY-TERM forms and long
  strings, or comments.")

(defsection @emacs-quicklisp (:title "Installing from Quicklisp")
  """If you installed PAX with Quicklisp, the location of `mgl-pax.el`
  may change with updates, and you may want to copy the current
  version of `mgl-pax.el` to a stable location by evaluating this in
  CL:

      (mgl-pax:install-pax-elisp "~/quicklisp/")

  If working from, say, a git checkout, there is no need for this
  step."""
  (install-pax-elisp function))

(defsection @emacs-loading (:title "Loading PAX")
  """Assuming the Elisp file is in the `~/quicklisp/` directory, add
  something like this to your `.emacs`:

  ```elisp
  (add-to-list 'load-path "~/quicklisp/")
  (require 'mgl-pax)
  ```

  If the Elisp variable `mgl-pax-autoload` is true (the default), then
  PAX will be loaded in the connected Lisp on-demand via @SLIME.

  If loading fails, `mgl-pax` will be unloaded from Emacs and any
  [overridden Slime key bindings][@EMACS-KEYS] restored.
  """)

;;; This is not a DEFVAR so that it picks up version changes on
;;; reload. Set to NIL by the DONT-CHECK restart.
(defparameter *pax-version*
  (with-open-file (s (asdf:system-relative-pathname
                      "mgl-pax" "version.lisp-expr"))
    (read s)))

(defun install-pax-elisp (target-dir)
  "Install `mgl-pax.el` distributed with this package in TARGET-DIR."
  (let ((source (asdf:system-relative-pathname "mgl-pax" "src/mgl-pax.el"))
        (target (merge-pathnames "mgl-pax.el"
                                 (uiop:ensure-directory-pathname target-dir))))
    (with-open-file (s target :direction :output :if-does-not-exist :create
                              :if-exists :supersede)
      (dolist (line (uiop:read-file-lines source))
        (if (string= line "(setq mgl-pax-version (mgl-pax-read-version))")
            (format s "(setq mgl-pax-version '~S)~%" *pax-version*)
            (write-line line s))))
    target))

(defun check-pax-elisp-version (pax-elisp-version)
  (when *pax-version*
    ;; For upgrading from versions where the version was a list.
    (when (listp pax-elisp-version)
      (setq pax-elisp-version (uiop:unparse-version pax-elisp-version)))
    (unless (equal pax-elisp-version *pax-version*)
      (restart-case
          (cerror "Ignore version mismatch."
                  "~@<In Emacs, mgl-pax-version is ~S, ~
                  which is different from the CL version ~S. ~
                  You may need to ~S if this had been done before, ~
                  and M-x mgl-pax-reload in any case. ~
                  See ~S for more.~:@>"
                  pax-elisp-version *pax-version* 'install-pax-elisp
                  '@emacs-setup)
        (dont-check ()
          :report (lambda (stream)
                    (format stream "Turn off version checking."))
          ;; ... until this file is loaded again.
          (setq *pax-version* nil)))))
  t)


(defsection @emacs-keys (:title "Setting up Keys")
  """The recommended key bindings are this:

  ```
  (global-set-key (kbd "C-.") 'mgl-pax-document)
  (global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
  (global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
  ```

  The global key bindings above are global because their commands work
  in any mode. If that's not desired, one may bind `\\C-.` locally in
  all Slime related modes like this:

  ```elisp
  (slime-bind-keys slime-parent-map nil '(("C-." mgl-pax-document)))
  ```

  If the customizable variable `mgl-pax-hijack-slime-doc-keys` is
  true, then upon loading `mgl-pax`, the following changes are made to
  `slime-doc-map` (assuming it's bound to `C-c C-d`):

  - `C-c C-d a`: replaces `slime-apropos` with `mgl-pax-apropos`
  - `C-c C-d z`: replaces `slime-apropos-all` with `mgl-pax-apropos-all`
  - `C-c C-d p`: replaces `slime-apropos-package` with `mgl-pax-apropos-package`
  - `C-c C-d f`: replaces `slime-describe-function` with `mgl-pax-document`
  - `C-c C-d d`: replaces `slime-describe-symbol` with
     `mgl-pax-hideshow-documentation`
  - `C-c C-d c`: installs `mgl-pax-hideshow-comments`
  - `C-c C-d u`: installs `mgl-pax-edit-parent-section`

  Calling `mgl-pax-unhijack-slime-doc-keys` reverts these changes.
  """)

(defsection @background (:title "Background")
  """As a user, I frequently run into documentation that's incomplete
  and out of date, so I tend to stay in the editor and explore the
  code by jumping around with @SLIME's @M-. (`slime-edit-definition`).
  As a library author, I spend a great deal of time polishing code but
  precious little writing documentation.

  In fact, I rarely write anything more comprehensive than docstrings
  for exported stuff. Writing docstrings feels easier than writing a
  separate user manual, and they are always close at hand during
  development. The drawback of this style is that users of the library
  have to piece the big picture together themselves.

  That's easy to solve, I thought, let's just put all the narrative
  that holds docstrings together in the code and be a bit like a
  Literate Programmer turned inside out. The original prototype, which
  did almost everything I wanted, was this:

  ```
  (defmacro defsection (name docstring)
    `(defun ,name () ,docstring))
  ```

  Armed with this [DEFSECTION][dislocated], I soon found myself
  organizing code following the flow of user-level documentation and
  relegated comments to implementation details entirely. However, some
  parts of [`DEFSECTION`][dislocated] docstrings were just listings of
  all the functions, macros and variables related to the narrative,
  and this list was repeated in the DEFPACKAGE form complete with
  little comments that were like section names. A clear violation of
  @OAOO, one of them had to go, so [DEFSECTION][dislocated] got a list
  of symbols to export.

  That was great, but soon I found that the listing of symbols is
  ambiguous if, for example, a function, a compiler macro and a class
  were named by the same symbol. This did not concern exporting, of
  course, but it didn't help readability. Distractingly, on such
  symbols, `\\M-.` was popping up selection dialogs. There were two
  birds to kill, and the symbol got accompanied by a type, which was
  later generalized into the concept of locatives:

  ```
  (defsection @introduction ()
    "A single line for one man ..."
    (foo class)
    (bar function))
  ```

  After a bit of elisp hacking, `\\M-.` was smart enough to
  disambiguate based on the locative found in the vicinity of the
  symbol, and everything was good for a while.

  Then, I realized that sections could refer to other sections if
  there were a SECTION locative. Going down that path, I soon began to
  feel the urge to generate pretty documentation as all the necessary
  information was available in the DEFSECTION forms. The design
  constraint imposed on documentation generation was that following
  the typical style of upcasing symbols in docstrings, there should be
  no need to explicitly mark up links: if `\\M-.` works, then the
  documentation generator shall also be able figure out what's being
  referred to.

  I settled on @MARKDOWN as a reasonably non-intrusive format, and a
  few thousand lines later PAX was born. Since then, locatives and
  references were factored out into the [DRef][ DREF::@DREF-MANUAL]
  library to let PAX focus on `\\M-.` and documentation.""")


(defsection @basics (:title "Basics")
  "Now let's examine the most important pieces."
  (defsection macro)
  (*discard-documentation-p* variable)
  (define-package macro)
  (define-glossary-term macro)
  (note macro))


(defmacro note (&body args)
  """Define a note with an optional [NAME][argument] and an optional
  [DOCSTRING][argument]. The [DOCSTRING][function] of the note is its
  own docstring concatenated with docstrings of other notes in the
  lexical scope of BODY.

  ARGS has the form \[`NAME`\] \[`DOCSTRING`\] BODY, where the square
  brackets indicate optional arguments. See below for the details of
  parsing ARGS.

  __NOTE is experimental and as such subject to change.__

  NOTE can occur in an any evaluated position without changing its
  BODY's run-time behaviour or introducing any run-time overhead. [Top
  level forms][clhs] remain top level when whrapped in NOTE. The names
  of notes live in the same global namespace regardless of nesting or
  whether they are [top level form][clhs]s. _These properties come at
  the price of NOTE being weird: it defines named notes at
  macro-expansion time (or load time). But the definitions are
  idempotent, so it's fine to macroexpand NOTE any number of times._

  Notes are similar to Lisp comments, but they can be included in the
  documentation. In fact, notes are auto-included: a @SPECIFIC-LINK to
  a note is equivalent to including it with the DOCSTRING locative.

  Notes are intended to help reduce the distance between code and its
  documentation when there is no convenient definition docstring to
  use nearby.

  ```cl-transcript (:dynenv pax-std-env)
  (note @xxx "We change the package."
    (in-package :mgl-pax))
  ==> #<PACKAGE "MGL-PAX">
  (values (docstring (dref '@xxx 'note)))
  => "We change the package."
  ```

  Here is an example of how to overdo things:

  ```cl-transcript
  (note @1+*
    "This is a seriously overdone example."
    (defun 1+* (x)
      "[@1+* note][docstring]"
      (if (stringp x)
          (note (@1+*/1 :join #\Newline)
            "- If X is a STRING, then it is parsed as a REAL number."
            (let ((obj (read-from-string x)))
              (note "It is an error if X does not contain a REAL."
                (unless (realp obj)
                  (assert nil)))
              (1+ obj)))
          (note "- Else, X is assumed to be REAL number, and we simply
                   add 1 to it."
            (1+ x)))))

  (1+* "7")
  => 8

  (values (docstring (dref '@1+* 'note)))
  => "This is a seriously overdone example.

  - If X is a STRING, then it is parsed as a REAL number.
  It is an error if X does not contain a REAL.

  - Else, X is assumed to be REAL number, and we simply
  add 1 to it."
  ```

  The parsing of ARGS:

  - If the first element of ARGS is not a string, then it is a NAME (a
    non-NIL SYMBOL) or name with options, currently destructured
    as `(NAME &KEY JOIN)`. As in DEFSECTION and DEFINE-GLOSSARY-TERM,
    the convention is that NAME starts with a `@` character.

      JOIN is PRINCed before the docstring of a child note is output.
      Its default value is a string of two newline characters.

  - The next element of ARGS is a Markdown docstring. See
    @MARKDOWN-IN-DOCSTRINGS.

  - The rest of ARGS is the BODY. If BODY is empty, then NIL is
    returned.

  Note that named and nameless notes can contain other named or
  nameless notes without restriction, but nameless notes without a
  lexically enclosing named note are just an [implicit progn][clhs]
  with BODY, and their docstring is discarded.

  If NOTE occurs as a [top level form][clhs], then its SOURCE-LOCATION
  is reliably recorded. Else, the quality of the source location
  varies, but it is at least within the right top level form on all
  implementations. On SBCL, exact source location is supported."""
  (expand-nested-note nil args))

(defun expand-nested-note (parent-name args)
  (multiple-value-bind (name body) (handle-note parent-name args)
    `(,@(if name
            `(macrolet ((note (&body child-args)
                          (expand-nested-note ',name child-args)))
               (mark-this-source-location (xref ',name 'note)))
            '(progn))
      ;; When loading a compiled file, recreate the side-effects of
      ;; macro-expansion: build the NOTE tree (through
      ;; %NOTE-CHILDREN).
      ;;
      ;; KLUDGE: This relies on the evaluation order of the
      ;; LOAD-TIME-VALUEs corresponding to the nested notes, which is
      ;; unsepcified according to the CLHS. In practice,
      ;; implementations seem to follow the order of macroexpansion,
      ;; so we may be good.
      (dref::load-time-value* (handle-note ',parent-name ',args))
      (locally ,@body))))

(defmacro mark-this-source-location (xref)
  `(dref::load-time-value*
    (when (or *compile-file-truename* *load-truename*)
      (setf (definition-property ,xref 'source-location)
            (this-source-location)))))

(defstruct %note
  name
  ;; Child %NOTEs and docstrings in reverse order.
  children
  join
  package)

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

;;; This function is called at macro-expansion time and load time. In
;;; the base case, it adds the docstring of the note being
;;; macroexpanded to the parent note's list of children.
;;;
;;; PARENT-NAME is NIL if there is not named parent note. BODY is BODY
;;; argument of the NOTE macro.
(defun handle-note (parent-name body)
  ;; Since this is called at macro-expansion time and also at load
  ;; time, and macros may be expanded multiple times (even by the user
  ;; calling MACROEXPAND), we must be careful about side effects.
  ;; HANDLE-NOTE itself is not idempotent, but the process of
  ;; expanding a tree of nested notes is.
  (multiple-value-bind (name join docstring body) (parse-note-body body)
    (let ((note (when name
                  ;; If there is already a %NOTE, then reuse it
                  ;; because it might be a child of another note,
                  ;; which may not be macroexpanded right now. This
                  ;; happens for example, if the user MACROEXPANDs a
                  ;; named NOTE form that has a parent.
                  (let ((note (definition-property (xref name 'note) 'note)))
                    (cond (note
                            (assert (eq (%note-name note) name))
                            (setf (%note-children note) (ensure-list docstring)
                                  (%note-join note) join
                                  (%note-package note) *package*)
                            note)
                          (t
                           (make-%note :name name
                                       :children (ensure-list docstring)
                                       :join join
                                       :package *package*)))))))
      (when name
        (setf (definition-property (xref name 'note) 'note) note))
      (when parent-name
        (let* ((parent-xref (xref parent-name 'note))
               (parent-note (definition-property parent-xref 'note)))
          ;; The KLUDGE in EXPAND-NESTED-NOTE does not work on
          ;; LispWorks.
          ;; (https://github.com/melisgl/mgl-pax/issues/42).
          (when (progn #+lispworks parent-note
                       #-lispworks t)
            (assert parent-note)
            (assert (not (eq parent-name name)))
            ;; If NAMEd, it can acquire more docstrings itself, so
            ;; push NOTE and let the DOCSTRING method look up its
            ;; docstrings.
            ;;
            ;; Also, PUSHNEW is for the sake of NOTE. DOCSTRING cannot
            ;; be duplicated because expanding a nameless NOTE without
            ;; its lexical context makes it parentless and the docstring
            ;; is discarded.
            (pushnew (or note docstring) (%note-children parent-note))))))
    (values name body)))

(defun parse-note-body (body)
  (let ((name0 nil)
        (join0 nil)
        (docstring0 nil)
        (body0 body))
    (when (and body0 (not (stringp (first body0))))
      (let ((name (pop body0)))
        (if (listp name)
            (destructuring-bind (name &key join) name
              (setq name0 name)
              (setq join0 join))
            (setq name0 name))))
    (when (and body0 (stringp (first body0)))
      (setq docstring0 (pop body0)))
    (values name0 join0 docstring0 body0)))
