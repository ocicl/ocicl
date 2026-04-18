(uiop:define-package #:40ants-doc-full/builder
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc-full/builder/vars
                #:*current-page*)
  (:import-from #:40ants-doc-full/page
                #:page-base-dir
                #:page-format)
  (:import-from #:40ants-doc-full/builder/printer)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc-full/github)
  (:import-from #:40ants-doc-full/world)
  (:import-from #:40ants-doc-full/themes/default)
  (:import-from #:40ants-doc-full/commondoc/page)
  (:import-from #:40ants-doc-full/commondoc/toc)
  (:import-from #:40ants-doc-full/commondoc/format)
  (:import-from #:40ants-doc-full/search)
  (:import-from #:40ants-doc-full/commondoc/transcribe)
  (:import-from #:40ants-doc/changelog)
  (:import-from #:40ants-doc-full/commondoc/changelog)
  (:import-from #:40ants-doc-full/commondoc/section)
  (:import-from #:40ants-doc-full/commondoc/xref)
  (:import-from #:40ants-doc-full/commondoc/image)
  (:import-from #:40ants-doc-full/external-index
                #:write-references-index)
  (:import-from #:40ants-doc-full/commondoc/reference)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:to-commondoc)
  (:import-from #:40ants-doc-full/link)
  (:import-from #:40ants-doc-full/rewrite)
  (:import-from #:40ants-doc-full/themes/api)
  (:import-from #:common-doc)
  (:import-from #:common-doc.format)
  (:import-from #:common-html)
  (:import-from #:common-html.emitter)
  (:import-from #:commondoc-markdown)

  (:export
   #:*document-html-top-blocks-of-links*
   #:*document-html-bottom-blocks-of-links*
   #:render-to-string
   #:render-to-files
   #:update-asdf-system-docs
   #:get-current-asdf-system))
(in-package #:40ants-doc-full/builder)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(defsection @generating-documentation
    (:title "Generating Documentation"
     :ignore-words ("LABEL"
                    "UPDATE-ASDF-SYSTEM-HTML-DOCS"
                    "UPDATE-ASDF-SYSTEM-README"
                    ":UPDATE-CSS-P"))
  "To make documentation builder work, you need to load 40ANTS-DOC-FULL asdf system.

   There are two core functions which render documentation to a string or files:"
  
  (render-to-string function)
  (render-to-files function)
  
  "Besides RENDER-TO-STRING and RENDER-TO-FILES a convenience function is provided
   to serve the common case of having an ASDF system with a readme and a directory for the
  HTML documentation."
  (update-asdf-system-docs function)
  (get-current-asdf-system function)
  (*document-html-top-blocks-of-links* variable)
  (*document-html-bottom-blocks-of-links* variable)
  (@rendering-multiple-formats section)
  (40ants-doc/changelog::@index section)
  (40ants-doc-full/github::@github-workflow section)
  (40ants-doc-full/world::@world section))


(defsection @rendering-multiple-formats (:title "Multiple Formats")
  "With 40ANTS-DOC you can render HTML and Markdown documentation simultaneously.
   This way, you can cross-reference entities from the README.md or ChangeLog.md to HTML docs.

   To render documents in multiple formats, you have to pass to function RENDER-TO-FILES not
   40ANTS-DOC:SECTION objects, but PAGE objects. Page object consists of one or more sections and additional
   information such as document format. A section can belong to a multiple pages usually having different
   formats. This allows you to include \"tutorial\" section into both HTML docs and README.

   Here is an example of rendering the full documentation and a README with only introduction and tutorial:

   ```lisp
   (defsection @full-manual (:title \"Manual\")
     (@introduction)
     (@tutorial)
     (@api)
     (@changelog))

   (render-to-files
    (list @full-manual
          (40ants-doc-full/page:make-page (list @introduction
                                           @tutorial)
                                          :format :markdown
                                          :base-filename \"README\")
          (40ants-doc-full/page:make-page @changelog
                                          :format :markdown
                                          :base-filename \"ChangeLog\")))
   ```

   The same approach works with the UPDATE-ASDF-SYSTEM-DOCS function.
   ")   


(defparameter *default-output-options*
  '(:if-does-not-exist :create
    :if-exists :supersede
    :ensure-directories-exist t))


(defun update-asdf-system-docs (sections-or-pages
                                asdf-system
                                &key
                                  (readme-sections nil)
                                  (changelog-sections nil)
                                  (theme '40ants-doc-full/themes/default::default-theme)
                                  (warn-on-undocumented-packages 40ants-doc-full/commondoc/page::*warn-on-undocumented-packages*)
                                  (base-url nil)
                                  (docs-dir #P"docs/")
                                  (clean-urls 40ants-doc-full/rewrite::*clean-urls*)
                                  (downcase-uppercase-code 40ants-doc-full/builder/vars::*downcase-uppercase-code*)
                                  highlight-languages
                                  highlight-theme
                                  (full-package-names t))
  "Generate pretty HTML documentation for a single ASDF system,
  possibly linking to github. If you are migrating from MGL-PAX,
  then note, this function replaces UPDATE-ASDF-SYSTEM-HTML-DOCS
  and UPDATE-ASDF-SYSTEM-README while making it possible to generate
  a crosslinks between README.md and HTML docs. The same way you
  can generate a ChangeLog.md file using :CHANGELOG-SECTIONS argument.
  See 40ANTS-DOC/CHANGELOG::@INDEX section to learn about
  40ANTS-DOC/CHANGELOG:DEFCHANGELOG helper.

  Both :README-SECTIONS and :CHANGELOG-SECTIONS arguments may be a single
  item or a list.

  See docs on RENDER-TO-FILES function to learn about meaning of
  BASE-DIR, BASE-URL, SOURCE-URI-FN, WARN-ON-UNDOCUMENTED-PACKAGES, CLEAN-URLS,
  DOWNCASE-UPPERCASE-CODE, THEME, HIGHLIGHT-LANGUAGES and HIGHLIGHT-THEME arguments.

  Example usage:

  ```lisp
  (40ants-doc-full/builder:update-asdf-system-docs 40ants-doc-full/doc:@index
                                                   :40ants-doc
                                                   :readme-sections 40ants-doc-full/doc:@readme)
  ```

  This is just a shorthand to call RENDER-TO-FILES for ASDF system.

  All sections, listed in :README-SECTIONS argment will be concantenated into the README.md.
  Some symbols, referenced in the :README-SECTIONS but not documented there will be
  linked to the HTML documentation. To make this work for a hosted static sites,
  then provide :BASE-URL of the site, otherwise, links will be relative.

  In MGL-PAX this function supported such parameters as :UPDATE-CSS-P and :PAGES,
  but in 40ANTS-DOC javascript and CSS files are updated automatically. See documentation
  on RENDER-TO-FILES to learn how does page separation and other parameters work.

  If you want a more generic wrapper for building documentation for your projects,
  take a look at [DOCS-BUILDER](https://40ants.com/docs-builder/)."
  (let ((40ants-doc-full/builder/vars::*current-asdf-system*
          (asdf:registered-system asdf-system)))
    (render-to-files (append (uiop:ensure-list sections-or-pages)
                             (when readme-sections
                               (list
                                (40ants-doc-full/page:make-page readme-sections
                                                                :base-filename "README"
                                                                :base-dir (asdf:system-relative-pathname
                                                                           asdf-system
                                                                           "./")
                                                                :format :markdown)))
                             (when changelog-sections
                               (list
                                (40ants-doc-full/page:make-page changelog-sections
                                                                :base-filename "ChangeLog"
                                                                :base-dir (asdf:system-relative-pathname
                                                                           asdf-system
                                                                           "./")
                                                                :format :markdown)
                                (40ants-doc-full/page:make-page changelog-sections
                                                                :base-filename "changelog"
                                                                :format '40ants-doc-full/commondoc/changelog::rss-feed))))
                     :base-dir (asdf:system-relative-pathname
                                asdf-system
                                (uiop:ensure-directory-pathname docs-dir))
                     :base-url base-url
                     :source-uri-fn (40ants-doc-full/github:make-github-source-uri-fn asdf-system)
                     :warn-on-undocumented-packages warn-on-undocumented-packages
                     :clean-urls clean-urls
                     :downcase-uppercase-code downcase-uppercase-code
                     :theme theme
                     :highlight-languages highlight-languages
                     :highlight-theme highlight-theme
                     :full-package-names full-package-names
                     :format :html)))


(defun get-current-asdf-system ()
  "Returns an ASDF system currently documented by call to UPDATE-ASDF-SYSTEM-DOCS.

   This function can be used by your extensions to do add some additional features
   like github stripe \"Fork Me\"."
  40ants-doc-full/builder/vars::*current-asdf-system*)


;;; Generate with the default HTML look

(defun process-document (document &key base-url)
  (let* ((references (40ants-doc-full/commondoc/reference::collect-references document))
         (document (40ants-doc-full/commondoc/page:warn-on-missing-exports document))
         (document (40ants-doc-full/commondoc/page:warn-on-undocumented-exports document
                                                                           references))
         (document (40ants-doc-full/commondoc/transcribe::warn-on-differences-in-transcriptions document))
         (document (if 40ants-doc-full/builder/printer:*document-uppercase-is-code*
                       (40ants-doc-full/commondoc/xref::extract-symbols document)
                       document))
         (document (40ants-doc-full/commondoc/xref:fill-locatives document))
         (document (40ants-doc-full/commondoc/section::fill-html-fragments document))
         (document (40ants-doc-full/commondoc/page::warn-on-references-to-internals document))
         (document (if 40ants-doc-full/link:*document-link-code*
                       (40ants-doc-full/commondoc/page::replace-xrefs document references
                                                                 :base-url base-url)
                       document))
         (document (40ants-doc-full/commondoc/image::replace-images document)))
    (values document
            references)))


(defun render-to-string (object &key (format :html)
                                  (source-uri-fn 40ants-doc/reference-api:*source-uri-fn*)
                                  (full-package-names t))
  "Renders given CommonDoc node into the string using specified format.
   Supported formats are :HTML and :MARKDOWN.

   This function is useful for debugging 40ANTS-DOC itself."
  (let ((format
          (40ants-doc-full/commondoc/format::ensure-format-class-name format))
        (40ants-doc/reference-api:*source-uri-fn* source-uri-fn))
    
    (40ants-doc-full/commondoc/format:with-format (format)
      (let* ((document (to-commondoc object))
             (processed-document (process-document document))
             (40ants-doc-full/builder/printer::*full-package-names* full-package-names))
        (with-output-to-string (stream)
          (common-doc.format:emit-document (make-instance format)
                                           processed-document
                                           stream))))))

(defun write-footer-for-markdown (format stream)
  (when (typep format 'commondoc-markdown:markdown)
    (format stream "~2&* * *~&###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]~%")))


(defun render-to-files (sections &key (theme '40ants-doc-full/themes/default:default-theme)
                                      (base-dir #P"./")
                                      (base-url nil)
                                      (source-uri-fn 40ants-doc/reference-api:*source-uri-fn*)
                                      (warn-on-undocumented-packages 40ants-doc-full/commondoc/page::*warn-on-undocumented-packages*)
                                      (clean-urls 40ants-doc-full/rewrite::*clean-urls*)
                                      (downcase-uppercase-code 40ants-doc-full/builder/vars::*downcase-uppercase-code*)
                                      (format :html)
                                      highlight-languages
                                      highlight-theme
                                      (full-package-names t))
  "Renders given sections or pages into a files on disk.

   By default, it renders in to HTML, but you can specify FORMAT argument.
   Supported formats are :HTML and :MARKDOWN.

   Returns an absolute pathname to the output directory as the first value
   and pathnames corresponding to each of given sections.

   When WARN-ON-UNDOCUMENTED-PACKAGES is true, then builder will check if there
   are other packages of the package-inferred system with external but
   not documented symbols. Otherwise, external symbols are searched only
   in packages with at least one documented entity.

   If CLEAN-URLS is true, then builder rewrites filenames and urls to make
   it possible to host files on site without showing .html files inside. Also,
   you need to specify a BASE-URL, to make urls absolute if you are rendering
   markdown files together with HTML.

   If DOWNCASE-UPPERCASE-CODE is true, then all references to symbols will be
   downcased.

   THEME argument should be a theme class name. By default it is
   40ANTS-DOC-FULL/THEMES/DEFAULT:DEFAULT-THEME. See 40ANTS-DOC-FULL/THEMES/DOCS::@DEFINING-A-THEME
   to learn how to define themes.

   HIGHLIGHT-LANGUAGES and HIGHLIGHT-THEME arguments allow to redefine theme's
   settings for Highlight.js. Languages should be a list of strings where each
   item is a language name, [supported by Highlight.js][langs]. Theme should be a
   name of a supported theme. You can preview different highlighting themes [here][themes]

   When FULL-PACKAGE-NAMES is true (default), then all symbols in documentation headers
   are rendered in their fully qualified form. This helps a lot when you are documenting
   a package inferred ASDF system.

   When building HTML documentation, this function also renders and index file `references.json
   with references to all documented entities. You can give a list of urls to such reference files
   as EXTERNAL-DOCS argument of DEFSECTION macro if you want to reference entities from other libraries.

   [langs]: https://github.com/highlightjs/highlight.js/blob/main/SUPPORTED_LANGUAGES.md
   [themes]: https://highlightjs.org/static/demo/
"

  (setf format
        (40ants-doc-full/commondoc/format::ensure-format-class-name format))
  
  (let ((num-warnings 0)
        ;; By default it uses "~A.html/#~A" which is wrong because there shouldn't
        ;; be a slash after the .html
        (common-html.emitter:*document-section-format-control* "~A#~A")
        (40ants-doc-full/commondoc/page::*warn-on-undocumented-packages* warn-on-undocumented-packages)
        (40ants-doc-full/rewrite::*clean-urls* clean-urls)
        (40ants-doc/reference-api:*source-uri-fn* source-uri-fn)
        (40ants-doc-full/builder/vars::*downcase-uppercase-code* downcase-uppercase-code)
        (40ants-doc-full/builder/vars::*base-dir* base-dir)
        (40ants-doc-full/builder/vars::*base-url* base-url)
        (40ants-doc-full/builder/printer::*full-package-names* full-package-names))

    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (incf num-warnings))))
      (40ants-doc-full/commondoc/format:with-format (format)
        (40ants-doc-full/themes/api::with-theme (theme)
          (let* ((sections (uiop:ensure-list sections))
                 (pages (mapcar #'40ants-doc-full/page:ensure-page sections))
                 (page-documents (mapcar #'to-commondoc pages)))
            ;; Probably it is a good idea to bind references to a special variable
            ;; and provide some function to generate a URL pointing to a reference?
            (multiple-value-bind (full-document references)
                (process-document (common-doc:make-document "Documentation"
                                                            :children page-documents)
                                  :base-url base-url)
              (let* ((absolute-dir (uiop:ensure-directory-pathname
                                    (uiop:ensure-absolute-pathname base-dir
                                                                   (probe-file "."))))
                     (40ants-doc-full/commondoc/toc::*full-document* full-document)
                     (output-paths nil))
                
                (ensure-directories-exist absolute-dir)

                (when (eql 40ants-doc-full/commondoc/format::*current-format* 'common-html:html)
                  (write-references-index (uiop:merge-pathnames* "references.json" absolute-dir)
                                          references
                                          (or base-url
                                              absolute-dir)))

                (flet ((make-full-filename (page)
                         ;; PAGE argument could be either PAGE object or string denoting a relative path
                         ;; of HTML page.
                         (let* ((page-base-dir (or (when (typep page '40ants-doc-full/commondoc/page:page)
                                                     (page-base-dir page))
                                                   base-dir))
                                (absolute-dir (uiop:ensure-absolute-pathname page-base-dir
                                                                             (probe-file ".")))
                                (filename (etypecase page
                                            (40ants-doc-full/commondoc/page:page
                                             (40ants-doc-full/commondoc/page::full-filename page))
                                            (string
                                             page))))
                           (uiop:merge-pathnames* filename absolute-dir))))
                  (loop with global-format = format
                        for *current-page* in page-documents
                        for full-filename = (make-full-filename *current-page*)
                        for format-type = (or
                                           ;; Page may override global format setting
                                           (page-format *current-page*)
                                           global-format)
                        for format = (make-instance format-type)
                        do (ensure-directories-exist full-filename)
                           (uiop:with-output-file (stream full-filename
                                                          :if-exists :supersede)
                             (common-doc.format:emit-document format
                                                              *current-page*
                                                              stream)
                             (write-footer-for-markdown format stream)
                             (push full-filename output-paths)))
             
                  (when (eql format
                             'common-html:html)
                    (40ants-doc-full/themes/api::render-static absolute-dir
                                                               :highlight-languages highlight-languages
                                                               :highlight-theme highlight-theme)

                    (let* ((page (40ants-doc-full/commondoc/page:make-page nil "search/index"
                                                                           :title "Search Page"
                                                                           :format :html))
                           (filename (make-full-filename page)))
                      (ensure-directories-exist filename)
                      (uiop:with-output-file (common-html.emitter::*output-stream*
                                              filename
                                              :if-exists :supersede)
                        (40ants-doc-full/commondoc/page::emit-search-page page))

                      (uiop:with-output-file (stream (uiop:merge-pathnames* #P"searchindex.js" absolute-dir)
                                                     :if-exists :supersede)
                        (write-string (40ants-doc-full/search::generate-search-index full-document page)
                                      stream)
                        (terpri stream)))))

                (unless (zerop num-warnings)
                  (warn "~A warning~:P ~A caught"
                        num-warnings
                        (if (= num-warnings 1)
                            "was"
                            "were")))
                (apply #'values
                       absolute-dir
                       (nreverse output-paths))))))))))


(defvar *document-html-top-blocks-of-links* ()
  "A list of blocks of links to be display on the sidebar on the left,
  above the table of contents. A block is of the form
  `(&KEY TITLE ID LINKS)`, where `TITLE` will be displayed at the top of the block in a
  HTML `div` with `id`, followed by the links. LINKS is a list
  of `(URI LABEL)` elements.`

  **Is not supported yet.**")

(defvar *document-html-bottom-blocks-of-links* ()
  "Like *DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*, only it is displayed
  below the table of contents.

  **Is not supported yet.**")


(defun blocks-of-links-to-html-string (blocks-of-links)
  (format nil "~{~A~}" (mapcar #'block-of-links-to-html-string
                               blocks-of-links)))

(defun block-of-links-to-html-string (block-of-links)
  (destructuring-bind (&key title id links) block-of-links
    (with-output-to-string (stream)
      (format stream "<div class=\"menu-block\"")
      (when id
        (format stream " id=\"~A\"" id))
      (format stream ">")
      (when title
        (format stream "<span class=\"menu-block-title\">~A</span>" title))
      (format stream "<ul>")
      (dolist (link links)
        (format stream "<li><a href=\"~A\">~A</a></li>"
                (first link)
                (second link)))
      (princ "</ul></div>" stream))))

(defvar *google-analytics-id* nil)
