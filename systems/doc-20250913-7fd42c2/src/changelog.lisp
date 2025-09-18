(uiop:define-package #:40ants-doc/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:defsection)
  (:import-from #:pythonic-string-reader)
  (:import-from #:named-readtables)
  (:export #:@changelog
           #:defchangelog))
(in-package #:40ants-doc/changelog)


(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defsection @index (:title "Changelog Generation")
  (defchangelog macro))


(defclass changelog (40ants-doc:section)
  ())

(defclass version (40ants-doc:section)
  ((date :initform nil
         :initarg :date
         :type (or null string)
         :documentation "This slot will contain a date in it's unparsed form, as a string.
                         Because we don't want to introduce dependency from LOCAL-TIME system for
                         changelog definition. This value will be parsed later, when we'll generate output."
         :accessor version-date)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun looks-like-date (item)
    (typecase item
      (symbol (looks-like-date (symbol-name item)))
      (string (and (= (length item) 10)
                   (digit-char-p (elt item 0))
                   (digit-char-p (elt item 1))
                   (digit-char-p (elt item 2))
                   (digit-char-p (elt item 3))
                   (char= (elt item 4) #\-)
                   (digit-char-p (elt item 5))
                   (digit-char-p (elt item 6))
                   (char= (elt item 7) #\-)
                   (digit-char-p (elt item 8))
                   (digit-char-p (elt item 9))))))
  
  (defun split-date-if-given (content)
    "Receives a list of entities and if first entity is a date in format 2021-09-24,
   then this date is removed from the content and returned as the first value.
   Rest entries are returned as the second value."
    (let ((first (first content)))
      (cond 
        ((looks-like-date first)
         (values (typecase first
                   (symbol (symbol-name first))
                   (t first))
                 (cdr content)))
        (t
         (values nil
                 content))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-version-section (version content external-docs external-links)
    (multiple-value-bind (date content)
        (split-date-if-given content)
      `(progn
         (defsection ,version (:title ,(symbol-name version)
                               :section-class version
                               :external-docs ,external-docs
                               :external-links ,external-links)
           ,@content)
         (setf (version-date ,version)
               ,date)))))


(defmacro defchangelog ((&key (title "ChangeLog")
                           ignore-words
                           external-docs
                           external-links)
                        &body versions)
  """
  This macro might be used to define a ChangeLog in a structured way.
  With DEFCHANGELOG you specify a body where each sublist starts with
  a version number and the rest is it's description in the markdown
  format. You can mention symbols from the rest of the documentation
  and they will be cross-linked automatically if you are using
  40ANTS-DOC-FULL/BUILDER:UPDATE-ASDF-SYSTEM-DOCS function.

  Here is an example:

  ```lisp
  (defchangelog ()
    (0.2.0
     "- Feature B implemented.
      - Bug was fixed in function FOO.")
    
    (0.1.0
     "- Project forked from [MGL-PAX](https://github.com/melisgl/mgl-pax).
      - Feature A implemented."))
  ```
  """
  (let ((section-name (intern "@CHANGELOG")))
    `(progn
       ;; Symbol should be exported, to allow DOCS-BUILDER
       ;; to discover a changelog.
       (export ',section-name)
      
       (defsection ,section-name (:title ,title
                                  :ignore-words (list ,@ignore-words)
                                  :section-class changelog
                                  :external-docs ,external-docs
                                  :external-links ,external-links)
         ,@(loop for (version) in versions
                 collect `(,version section)))
       ,@(loop for (version . content) in versions
               collect (make-version-section version
                                             content
                                             external-docs
                                             external-links)))))


(defchangelog (:ignore-words ("MGL-PAX"
                              "UPDATE-ASDF-SYSTEM-HTML-DOCS"
                              "UPDATE-ASDF-SYSTEM-README"
                              "README"
                              "HTML"
                              "ASDF"
                              "RSS"
                              "URL"
                              "JS"
                              "MGL-PAX:DEFINE-PACKAGE"
                              "UIOP:DEFINE-PACKAGE"
                              "*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*"
                              "*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*"
                              "*DOCUMENT-LINK-SECTIONS*"
                              "*DOCUMENT-TEXT-NAVIGATION*"
                              "*DOCUMENT-FANCY-HTML-NAVIGATION*"
                              "LOCATE-AND-DOCUMENT"
                              "TOC"
                              "SLIME"
                              "CSS"
                              "SLY"
                              "API"
                              "SBCL"
                              "PlantUML"
                              "COMMONDOC:SECTION"
                              "COLLECT-REACHABLE-OBJECTS"
                              "LOCATE-AND-COLLECT-REACHABLE-OBJECTS"
                              "COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*"
                              "*DOCUMENT-MIN-LINK-HASH-LENGTH*"
                              "*DOCUMENT-MARK-UP-SIGNATURES*"
                              "*DOCUMENT-NORMALIZE-PACKAGES*"
                              "*DOCUMENT-DOWNCASE-UPPERCASE-CODE*"
                              "CLEAN-URLS"
                              ;; These objects are not documented yet:
                              "40ANTS-DOC/COMMONDOC/XREF:XREF"))
  (0.25.1 2025-09-13
          "* New package 40ants-doc/mito now exports `fixed-dao-table-class` class which should be used instead of Mito's `dao-table-class`.")
  (0.25.0 2025-09-13
          "* Added a `40ants-doc/mito` package with a helper to solve problem of rendering documentation for projects which use Mito's table classes. You should load this helper as your project's documentation dependency.")
  (0.24.2 2025-05-26
          "* Fixed a warning \"These symbols are external, but not documented\" shown for symboles ignored by passing IGNORE-SYMBOL-P argument.")
  (0.24.1 2025-05-26
          "* Fixed a bug when class slot accessor or reader was not filtered using a custom function passed as IGNORE-SYMBOL-P argument.")
  (0.24.0 2025-05-12
          "* Autodoc macro now accepts IGNORE-SYMBOL-P argument and by default ignores symbols starting with `%` character.")
  (0.23.0 2025-01-06
          "* IGNORE-PACKAGES argument was added to DEFSECTION macro. Packages
             listed in this argument allows to muffle this warning:

             ```
             Object referenced as #<XREF FOO:BAR CLASS> in \"Some section\" is not documented.
             ```")
  (0.22.0 2025-01-05
          "* Exported 40ANTS-DOC/OBJECT-PACKAGE:OBJECT-PACKAGE generic-function.
             Define a method for it if you have created a new locative type
             and encounter a warning like this:

             ```
             WARNING:
                Unable to figure out *package* for object #<40ANTS-DOC/LOCATIVES::DIAGRAM {1012A10B53}>
             ```")
  (0.21.0 2025-01-05
          "* Changed a way how images are processed. New behaviour should be backward compatible,
             but now it is possible. But now 40ANTS-DOC-FULL/COMMONDOC/IMAGE:LOCAL-IMAGE function
             is exported. You can use this function to build a piece of documentation and include
             an image into this doc. See function's docstring for usage example.
            ")
  (0.20.1 2024-12-14
          "* Fixed dependency from swank-backend package for autodoc package.")
  (0.20.0 2024-12-14
          "* Type definitions now supported by 40ANTS-DOC/AUTODOC:DEFAUTODOC macro.
           * Type expansion is shown for type definitions when building doc on SBCL.")
  (0.19.0 2024-12-13
          "* Argument IGNORE-PACKAGES was added to 40ANTS-DOC/AUTODOC:DEFAUTODOC macro.
             It can be used to exclude some packages from autogenerated docs.
           * Image URLs now can be specified relative to some other ASDF system.
             For example `![](asdf:some-other-system:images/demo.gif) will look for the image
             inside the directory of `some-other-system` ASDF system.")
  (0.18.0 2024-08-29
          "* Now default theme looks good on mobile devices.")
  (0.17.4 2024-05-15
          "* Update 5")
  (0.17.3 2024-05-15
          "* Update 2")
  (0.17.2 2024-05-15
          "* Another update.")
  (0.17.1 2024-05-15
          "* Just a check if tag will be added automatically.")
  (0.17.0 2024-05-13
          "## Backward incompatible

           * A plugin system was introduced.
           * Highlight.js and MathJax support were refactored into plugins and MathJax is turned off by default,
             because it's assets too heavy and math is unnecessary in most Common Lisp libraries.
           * Generic-function 40ANTS-DOC-FULL/THEMES/API:HIGHLIGHT-LANGUAGES and generic-function 40ANTS-DOC-FULL/THEMES/API:HIGHLIGHT-THEME
             are now deprecated. Pass options :LANGUAGES and :THEME arguments to 40ANTS-DOC-FULL/PLUGINS/HIGHLIGHTJS:HIGHLIGHTJS function
             instead.
             
     ")
  (0.16.0 2024-05-04
          "* A generic-function 40ANTS-DOC/LOCATIVES/ASDF-SYSTEM:ASDF-SYSTEM-DOCUMENTATION-TITLE was added. It allows to override the text of a section showing an information about ASDF system.")
  (0.15.4 2024-01-26
          "* Another fix to URLs on the search page. Now if CLEAN-URLS argument is true, search page will link to the pages ended with a backslash. Otherwise, it will link to the html pages.")
  (0.15.3 2024-01-24
          "* Fixed URLs in the search index file when CLEAN-URLS argument is given. This should prevent redirection to index.html file from search page - now index.html will be stripped from the path.")
  (0.15.2 2023-11-28
          "* Fixed stack exhausting when trying to collect dependencies for some ASDF systems.")
  (0.15.1 2023-08-05
          "* Fixed issue with unpacking Highlight.js archive when it is having absolute pathnames.

             Also, a new download API is used now.")
  (0.15.0 2023-07-22
          "* Autodoc was fixed to not show packages without external symbols.
           * Also, now autodoc sorts packages alphabetically.")
  (0.14.0 2023-06-04
          "* Added 40ANTS-DOC:*SYMBOLS-WITH-IGNORED-MISSING-LOCATIONS* variable to ignore warnings on missing source location.

             This might be useful for autogenerated methods. For example, Mito does generate such reader methods
             [here](https://github.com/fukamachi/mito/blob/6835d2c8120454e93c69d4f22cccb10d9ee24526/src/core/dao/mixin.lisp#L71).")
  (0.13.0 2023-06-04
          "* Support for variables was added to autodoc subsystem.")
  (0.12.0 2023-04-22
          "* Autodoc subsystem was added. It provides a 40ANTS-DOC/AUTODOC:DEFAUTODOC macro which can build an API reference for a given ASDF system.
           * Now we output a list of dependencies for an ASDF system description.")
  (0.11.2 2022-12-05
          "* Script on the search page was fixed.")
  (0.11.1 2022-11-26
          "* Fixed 40ANTS-DOC/LOCATIVES/BASE:LOCATIVE-TYPE broken by previous refactoring. Now this function
             was transformed into generic-function as well as 40ANTS-DOC/LOCATIVES/BASE:LOCATIVE-ARGS.")
  (0.11.0 2022-11-16
          "* Large refactoring was introduced. All files related to the documentation builder were moved to `./full/` folder and corresponding packages was renamed to have `40ants-doc-full/` prefix.")
  (0.10.0 2022-11-09
          "* Default theme was reworked and now uses divs and flex for rendering \"bullets\".
             Also, bullets were made more structured and now it is more convenient to change their design using CSS.")
  (0.9.2 2022-10-26
         "* Fixed a few issues with ASDF and warnings from find-system generic-function:

    ```
    Computing just-done stamp  for action (ASDF/LISP-ACTION:COMPILE-OP
      \"40ants-asdf-system/changelog\"), but dependency (ASDF/LISP-ACTION:COMPILE-OP
      \"40ants-asdf-system/changelog\"
      \"file-type\") wasn't done yet!
    ```
")
  (0.9.1 2022-10-19
         "* Fixed 40ants-doc asdf system dependency from the 40ants-doc/ignored-words. Because this module should be in the core system.")
  (0.9.0 2022-02-21
         "* Now you can refer internal documentation section of other libraries.
            To do this, put the link to this library documentation to the
            EXTERNAL-DOCS argument of 40ANTS-DOC:DEFSECTION macro and then
            mention other section like this: `40ANTS-CI::@CRITIC section`.")
  (0.8.0 2022-02-14
         "* Now 40ANTS-DOC-FULL/COMMONDOC/MAPPER:MAP-NODES generic-function
            supports any node type which defines a method for
            generic-function 40ANTS-DOC-FULL/COMMONDOC/MAPPER:NODE-SUPPORTS-CHILDREN.
          * Default theme was fixed to work with latest Spinneret, which now
            escapes single quotes inside HTML nodes.")
  (0.7.0 2021-12-31
         "* 40ANTS/CHANGELOG:DEFCHANGELOG now supports EXTERNAL-LINKS argument.
          * Automatic symbol extraction now ignores dates like 2021-12-31. Now
            to make it work, the symbol should contain at least one alpha character.")
  (0.6.0 2021-12-05
         "* Fixed the issue, when we tried to find uppercased xrefs inside inline code and links.
          * Added EXTERNAL-LINKS argument to DEFSECTION macro. It can be useful, if you have a multiple
            text sections having the same external link.
          * Now it is possible to use 40ANTS-DOC/IGNORED-WORDS:IGNORE-WORDS-IN-PACKAGE
            to suppress warning on symbols which are exported but not documented.")
  (0.5.8 2021-10-23
         "* Fixed the way of how a TOC is built. Previosly it incorrectly shown nested COMMONDOC:SECTION objects.")
  (0.5.7 2021-10-21
         "* DEFSECTION macro now supports EXTERNAL-DOCS argument.")
  (0.5.6 2021-10-21
         "* Blockquotes markup is supported now.
          * Now external references are rendered as code and downcased according to the settings.")
  (0.5.5 2021-09-26
         "Warning messages like:

          ```
          Unable to find target for reference #<XREF 40ANTS-DOC/COMMONDOC/XREF:XREF> mentioned at ChangeLog / 0.5.3  (2021-09-08)
          ```

          were rewritten to be more actionable:

          ```
          Object referenced as #<XREF 40ANTS-DOC/COMMONDOC/XREF:XREF> in ChangeLog / 0.5.3  (2021-09-08) is not documented.
          ```
          ")
  (0.5.4 2021-09-11
         "- Included changelog.lisp into the 40ANTS-DOC system.")
  (0.5.3 2021-09-08
         "- Fixed locatives parsing in case if there are more than one 40ANTS-DOC/COMMONDOC/XREF:XREF in the text.")
  (0.5.2 2021-09-08
         "- Now default theme removes underline from images nested into the `<a>` HTML tag.
          - Fixed images collection for case when current directory is different from the
            ASDF system's directory.")
  (0.5.1 2021-09-07
         "- Fixed reference index generation and comparison of usual references with references where locative is a string.")
  (0.5.0 2021-09-06
         "- Now 40ANTS-DOC:DEFSECTION macro accepts EXTERNAL-DOCS argument.
            Together with HTML pages, `references.json` file is rendered, and you can
            provide a list of urls of external libraries' documentation to have an automatic
            cross-referencing between them.")
  (0.4.1 2021-09-05
         "- Function 40ANTS-DOC-FULL/BUILDER:GET-CURRENT-ASDF-SYSTEM was added. Now you can use it to do something interesting
            like showing \"Fork me on the GitHub\" stripe [as my own theme do][commit].
          - Markdown files now will have a footer saying that a file was generated by 40ANTS-DOC.
          - An RSS feed is generated for changelog. Also, changelog items can have a date now.

          [commit]: https://github.com/40ants/40ants-doc-theme-40ants/commit/917a4c1e72b0379f509bdee4864531e641c9ec4e#diff-47d16baea2d4ef710747f19c24df8cf7ef4f6bbbfd1dbb0ade55f47457b1e8feR155-R161")
  (0.4.0 2021-09-05
         "- *DOCUMENT-NORMALIZE-PACKAGES* variable was replaced with FULL-PACKAGE-NAMES argument
      of 40ANTS-DOC-FULL/BUILDER:RENDER-TO-FILES function.")
  (0.3.0 2021-09-04
         "- Now images are copied to target folder together with HTML documentation
      and links are adjusted accordingly.
    - Added a protocol to define new color themes and change page layout.
      Three new themes are available out of the box.
      Read more at 40ANTS-DOC-FULL/THEMES/DOCS::@DEFINING-A-THEME section.")
  (0.2.0 2021-09-01
         "- Now defsection does not exports symbols by default
    - You can render documents in multiple formats in a single run having cross links.
      For example shorter README.md could mention symbols and have correct
      links to the full documentation
    - \"Clean\" URLs are supported out of the box.
    - Now defsection does not export nor mentioned symbols nor the name of the section
      It is better to have explicit exports.
    - 40ANTS-DOC/LOCATIVES:INCLUDE locative now does not support :HEADER, :FOOTER and some other arguments. Use :LANG argument instead.
    - Added code highlighting using Highlight.js library.
    - Added search form which uses index in browser. JS code was taken from
      [Sphinx](https://www.sphinx-doc.org/) documentation builder.
    - Elisp code for transcriptions was fixed and now should word not
      only with SLIME, but also with SLY.
    - 40ANTS-DOC:DEFSECTION macro now does not generate export code
      if :EXPORT argument is NIL.
    - Functions UPDATE-ASDF-SYSTEM-HTML-DOCS and UPDATE-ASDF-SYSTEM-README
      were replaced with 40ANTS-DOC-FULL/BUILDER:UPDATE-ASDF-SYSTEM-DOCS, which also supports
      ChangeLog.md generation. Use 40ANTS-DOC/CHANGELOG:DEFCHANGELOG to define versions.
    - Variables *DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL* and
      *DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL* were removed. Probably we'll return this
      feature back in other form, to restrict TOC's size.
    - Removed LOCATE-AND-DOCUMENT generic function.
    - Links to the GitHub now are generated automatically,
      if 40ANTS-DOC-FULL/BUILDER:UPDATE-ASDF-SYSTEM-DOCS function is used
      and system definition has a :SOURCE-CONTROL slot.
    - Generic functions COLLECT-REACHABLE-OBJECTS and LOCATE-AND-COLLECT-REACHABLE-OBJECTS
      were removed.
    - Variables *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-TEXT-NAVIGATION* and *DOCUMENT-FANCY-HTML-NAVIGATION* were removed.
    - Variable *DOCUMENT-MIN-LINK-HASH-LENGTH* was removed. Use COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*
    - Functions 40ANTS-DOC-FULL/BUILDER:UPDATE-ASDF-SYSTEM-DOCS and 40ANTS-DOC-FULL/BUILDER:RENDER-TO-FILES
      now accept WARN-ON-UNDOCUMENTED-PACKAGES argument and CLEAN-URLS argument.
    - Variable *DOCUMENT-MARK-UP-SIGNATURES* was removed.
    - Added DOWNCASE-UPPERCASE-CODE argument instead of *DOCUMENT-DOWNCASE-UPPERCASE-CODE*.
      This argument is true by default.
    - Added warnings on symbols, referenced like internals, using `::`.
    - Added 40ANTS-DOC:DEFSECTION-COPY macro to define copy of the section but with a different name.")
  
  (0.1.0 2021-05-01
         "- Project forked from [MGL-PAX](https://github.com/melisgl/mgl-pax).
      Code refactored into the package inferred system and core is separated
      to have minimum dependencies.
    - Fixed displaying docstring for constant locative.
    - Include locative was fixed for files with unicode characters
      file-subseq function was rewritten.
    - Locatives can be specified without a package prefix inside the defsection
      because all locative symbols now live in [40ANTS-DOC/LOCATIVES][package] package.
    - Function update-asdf-system-readmes was renamed to update-asdf-system-readmes and now
      it generates only one README file.
    - Tests were rewritten to use Rove and to support `(asdf:test-system :40ants-doc)`.
    - Removed MGL-PAX:DEFINE-PACKAGE macro. An UIOP:DEFINE-PACKAGE can be used instead.
    - Now builder issues a warning if it wasn't able to find a symbol mentioned in the docstring.
    - Uppercase word should have at least two charaters to be resolved as a symbol.
    - Improved work with package inferred systems. For examples, when fixed the
      automatic symbol rendering for case when documentation section and
      referenced objects are in different packages.
    - Allowed to reference objects using keywords.
    - Fixed docstring extraction for compiler macro."))


