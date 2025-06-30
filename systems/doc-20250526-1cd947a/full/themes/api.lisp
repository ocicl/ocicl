(uiop:define-package #:40ants-doc-full/themes/api
  (:use #:cl)
  (:import-from #:alexandria
                #:once-only)
  (:export #:render-css
           #:render-page
           #:render-html-head
           #:render-toc
           #:render-search-form
           #:render-sidebar-footer
           #:render-sidebar-header
           #:render-sidebar
           #:render-sidebar-content
           #:render-content
           #:render-page-header
           #:render-page-footer
           #:highlight-languages
           #:highlight-theme
           #:inject-into-page-header
           #:theme-plugins
           #:inject-before-content
           #:inject-after-content
           #:copy-static))
(in-package #:40ants-doc-full/themes/api)

(defvar *theme*)

(defmacro with-theme ((theme) &body body)
  (once-only (theme)
    `(let ((*theme* (typecase ,theme
                      (symbol (make-instance ,theme))
                      (t ,theme))))
       ,@body)))


(defgeneric theme-plugins (theme)
  (:documentation "Returns a list of plugin objects which will be used to inject additional information into the pages.")
  (:method ((theme t))
    nil))


(defgeneric highlight-languages (theme)
  (:documentation "Returns a list of languages to highlight in snippets. Each language should be supported by Highlight.js.

                   **Deprecated!** will be removed after 2024-11-13.
                   Pass languages and highlight theme as arguments to highlighjs plugin.")
  (:method ((theme t))
    (list :lisp
          :bash)))

(defgeneric highlight-theme (theme)
  (:documentation "Returns a string with the name of the Highlight.js color theme for highlighted snippets.

                   To preview themes, use this site: <https://highlightjs.org/static/demo/>

                   **Deprecated!** Will be removed after 2024-11-13.
                   Pass languages and highlight theme as arguments to highlighjs plugin.")
  (:method ((theme t))
    "magula"))


(defgeneric copy-static (plugin target-dir)
  (:documentation "Define a method for this function if your plugin need to some static assets to work.

                   TARGET-DIR argument is an absolute directory pathname pointing to the root of the site.

                   By default does nothing.")
  (:method ((theme t) (target-dir t))
    (values)))


(defgeneric render-css (theme)
  (:documentation "Returns a string with CSS."))

(defgeneric render-page (theme uri title &key toc content)
  (:documentation "Renders whole page using theme and callable CONTENT-FUNC."))

(defgeneric render-page-header (theme uri title)
  (:documentation "Renders whole page header. Does nothing by default."))

(defgeneric inject-into-page-header (plugin uri)
  (:documentation "Plugins can define a method for this generic-function to add some code to the end of a page header.

                   Each method should return a string which will be inserted without \"escaping\" so
                   the plugin's responsibility to escape all user input's if necessary.

                   Does nothing by default.")
  (:method ((plugin t) uri)
    nil))

(defgeneric inject-before-content (plugin uri)
  (:documentation "Plugins can define a method for this generic-function to add some HTML before the main content of the page.

                   Each method should return a string which will be inserted without \"escaping\" so
                   the plugin's responsibility to escape all user input's if necessary.

                   Does nothing by default.")
  (:method ((plugin t) uri)
    nil))

(defgeneric inject-after-content (plugin uri)
  (:documentation "Plugins can define a method for this generic-function to add some HTML after the main content of the page.

                   Each method should return a string which will be inserted without \"escaping\" so
                   the plugin's responsibility to escape all user input's if necessary.

                   Does nothing by default.")
  (:method ((plugin t) uri)
    nil))

(defgeneric render-page-footer (theme uri)
  (:documentation "Renders whole page footer. Does nothing by default."))

(defgeneric render-html-head (theme uri title)
  (:documentation "Renders content of the HTML HEAD tag."))

(defgeneric render-content (theme uri toc content-func)
  (:documentation "Renders page's content. It can wrap content into HTML tags and should funcall CONTENT-FUNC without arguments."))

(defgeneric render-sidebar (theme uri toc)
  (:documentation "Renders page's sidebar"))

(defgeneric render-sidebar-header (theme uri toc)
  (:documentation "Renders sidebar's header. Usually it contains a search input."))

(defgeneric render-sidebar-footer (theme uri toc)
  (:documentation "Renders sidebar's header. By default it contains a link to the 40ANTS-DOC system."))

(defgeneric render-sidebar-content (theme uri toc)
  (:documentation "Renders sidebar's content. By default it calls RENDER-TOC generic-function."))

(defgeneric render-toc (theme uri toc)
  (:documentation "Renders documentation TOC."))

(defgeneric render-search-form (theme uri toc)
  (:documentation "Renders a search form."))

(defun check-theme ()
  (unless (boundp '*theme*)
    (error "Please, use WITH-THEME macro around the call")))

(defun render-static (absolute-dir &key highlight-languages highlight-theme)
  (check-theme)
  
  (let ((css-filename (uiop:merge-pathnames* #P"theme.css" absolute-dir)))
    (uiop:with-output-file (stream css-filename
                                   :if-exists :supersede)
      (write-string (render-css *theme*)
                    stream)
      (terpri stream))
    
    (when (or highlight-languages
              highlight-theme)
      (warn "Deprecated, will be removed after 2024-11-13.
Pass languages and highlight theme as arguments to highlighjs plugin of the theme ~A"
            *theme*))

    (loop with paths = '(("toc.js" "toc.js")
                         ("search/searchtools.js" "searchtools.js")
                         ("search/language_data.js" "language_data.js")
                         ("search/doctools.js" "doctools.js")
                         ("underscore.js" "underscore.js")
                         ("jquery.js" "jquery.js"))
          for (from to) in paths
          do (uiop:copy-file (asdf:system-relative-pathname :40ants-doc
                                                            (concatenate 'string
                                                                         "static/" from))
                             (uiop:merge-pathnames* to absolute-dir)))
    
    ;; Plugins can override or remove some of the files written above:
    (loop for plugin in (theme-plugins *theme*)
          do (copy-static plugin absolute-dir))))

(defun call-with-page-template (func uri title toc)
  (check-type uri string)
  (check-theme)
  (render-page *theme* uri title
               :toc toc
               :content func))

(defmacro with-page-template ((uri title &key toc) &body body)
  `(call-with-page-template
    (lambda ()
      ,@body)
    ,uri
    ,title
    ,toc))
