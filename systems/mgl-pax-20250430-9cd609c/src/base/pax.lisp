(in-package :mgl-pax)

;;;; USE-PACKAGEs that were not available in MGL-PAX-BOOTSTRAP.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package '#:named-readtables)
  (use-package '#:pythonic-string-reader))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; For backward compatibility with pre-DRef PAX, reexport these DRef
  ;; macros.
  (export 'define-locative-type)
  (export 'define-locative-alias)
  (export 'define-symbol-locative-type)
  (export 'define-definer-for-symbol-locative-type)
  (export 'define-restart))


(defun section-entries (section)
  "A list of markdown docstrings and [XREF][class]s in the order they
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
  (@emacs-setup section)
  (@links section)
  (@background section)
  (@basics section)
  (@parsing section)
  (@pax-locatives section)
  (@navigating-in-emacs section)
  (@generating-documentation section)
  (@transcripts section)
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
  => "Transfer control to a restart named ABORT, signalling a CONTROL-ERROR if
     none exists."
  ```

  Note how in the generated documentation, ABORT is set with a
  monospace font, while `\CONTROL-ERROR` is @AUTOLINKed:

  - \[function\] **\ABORT** *\\&OPTIONAL \CONDITION*

      Transfer control to a restart named `ABORT`, signalling a
      [`\CONTROL-ERROR`][6bc0] if none exists.

    [6bc0]: http://www.lispworks.com/documentation/HyperSpec/Body/e_contro.htm "CONTROL-ERROR CONDITION"

  The following [transcript][@transcripts] shows the raw Markdown for
  the previous example.

  ```
  (document #'abort :format :markdown)
  .. - [function] **ABORT** *&OPTIONAL CONDITION*
  ..
  ..     Transfer control to a restart named `ABORT`, signalling a [`CONTROL-ERROR`][7c2c] if
  ..     none exists.
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

(defsection @emacs-setup (:title "Emacs Setup")
  """Here is a quick recipe for setting up PAX for use via @SLIME to
  take advantage of the [conveniences on offer][@EMACS-FUNCTIONALITY].
  Conversely, there is no need to do any of this just to use
  DEFSECTION, write docstrings and for @GENERATING-DOCUMENTATION.

  If PAX was installed from @QUICKLISP, then evaluate this in CL to
  copy the Elisp code to a stable location:

      (mgl-pax:install-pax-elisp "~/quicklisp/")

  Assuming the Elisp file is in the `~/quicklisp/` directory, add
  something like this to your `.emacs`:

  ```elisp
  (add-to-list 'load-path "~/quicklisp/")
  (require 'mgl-pax)
  (global-set-key (kbd "C-.") 'mgl-pax-document)
  (global-set-key (kbd "s-x t") 'mgl-pax-transcribe-last-expression)
  (global-set-key (kbd "s-x r") 'mgl-pax-retranscribe-region)
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
     features. There are no related variables to customize.")

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

  If the Lisp variable `mgl-pax-autoload` is true (the default), then
  MGL-PAX will be loaded in the connected Lisp on-demand via @SLIME.

  If loading fails, `mgl-pax` will be unloaded from Emacs and any
  [overridden Slime key bindings][@EMACS-KEYS] restored.
  """)

(defun install-pax-elisp (target-dir)
  "Copy `mgl-pax.el` distributed with this package to TARGET-DIR."
  (uiop:copy-file (asdf:system-relative-pathname "mgl-pax" "src/mgl-pax.el")
                  (merge-pathnames "mgl-pax.el"
                                   (uiop:ensure-directory-pathname
                                    target-dir))))

(defun check-pax-elisp-version (pax-elisp-version)
  (let ((pax-cl-version '(0 4 1)))
    (unless (equal pax-elisp-version pax-cl-version)
      (cerror "Ignore version mismatch."
              "~@<In Emacs, mgl-pax-version is ~S, ~
              which is different from the CL version ~S. ~
              You may need to ~S and M-x mgl-pax-reload. ~
              See ~S for more.~:@>"
              pax-elisp-version pax-cl-version 'mgl-pax:install-pax-elisp
              '@emacs-setup)))
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
  true, then upon loading `mgl-pax', the following changes are made to
  `slime-doc-map` (assuming it's bound to `C-c C-d`):

  - `C-c C-d a`: replaces `slime-apropos` with `mgl-pax-apropos`
  - `C-c C-d z`: replaces `slime-apropos-all` with `mgl-pax-apropos-all`
  - `C-c C-d p`: replaces `slime-apropos-package` with `mgl-pax-apropos-package`
  - `C-c C-d d`: replaces `slime-describe-symbol` with `mgl-pax-document`
  - `C-c C-d f`: replaces `slime-describe-function` with `mgl-pax-document`
  - `C-c C-d c`: installs `mgl-pax-current-definition-toggle-view`
  - `C-c C-d u`: installs `mgl-pax-edit-parent-section`

  Calling `mgl-pax-unhijack-slime-doc-keys` reverts these changes.
  """)

(defsection @links (:title "Links and Systems")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/mgl-pax-manual.html)
  for the latest version.

  PAX is built on top of the [DRef
  library][DREF::@DREF-MANUAL] (bundled in the same repository). See
  [DRef's HTML
  documentation](http://melisgl.github.io/mgl-pax-world/dref-manual.html)"
  ("mgl-pax" asdf:system)
  (mgl-pax/full asdf:system))

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
  (define-glossary-term macro))
