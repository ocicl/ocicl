(uiop:define-package #:40ants-doc-full/themes/docs
  (:use #:cl)
  (:import-from #:40ants-doc-full/themes/api
                #:copy-static
                #:inject-before-content
                #:inject-after-content
                #:inject-into-page-header
                #:theme-plugins
                #:render-css
                #:render-page
                #:render-html-head
                #:render-page-header
                #:render-page-footer
                #:render-content
                #:render-sidebar
                #:render-sidebar-header
                #:render-sidebar-footer
                #:render-sidebar-content
                #:render-toc
                #:render-search-form)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc-full/themes/default)
  (:import-from #:40ants-doc-full/themes/light)
  (:import-from #:40ants-doc-full/themes/dark)
  (:import-from #:40ants-doc-full/plugins/mathjax
                #:mathjax)
  (:import-from #:40ants-doc-full/plugins/highlightjs
                #:highlightjs))
(in-package #:40ants-doc-full/themes/docs)


(defsection @defining-a-theme (:title "Defining a Custom Theme"
                               :ignore-words ("HEAD"
                                              "40A"))
  "Out of the box, 40ANTS-DOC system supports three color themes:

   - 40ANTS-DOC-FULL/THEMES/DEFAULT:DEFAULT-THEME
   - 40ANTS-DOC-FULL/THEMES/LIGHT:LIGHT-THEME
   - 40ANTS-DOC-FULL/THEMES/DARK:DARK-THEME

   You can pass these names as THEME argument to the 40ANTS-DOC-FULL/BUILDER:RENDER-TO-FILES
   function. Or you can pass a contructed theme object instead of a symbolic class name.

   And of cause, you can define your own theme!

   Theme allows to control HTML page rendering, colors and code highlighting.

   ## Changing Colors

   The simplest way to customize theme is to redefine some colors using CSS.
   Here is how to set orange page background:

   ```lisp
   (defclass my-theme (default-theme)
     ())

   (defmethod 40ants-doc-full/themes/api:render-css ((theme my-theme))
     (concatenate
      'string
      (call-next-method)
     
      (lass:compile-and-write
       `(body
         :background orange))))
   ```

   Also you might want to redefine a color theme for code highlighter:

   ```
   (defmethod 40ants-doc-full/themes/api:highlight-theme ((theme my-theme))
     \"atom-one-light\")
   ```

   Talking about code highlighting, you can also redefine a list of
   languages to highlight:

   ```lisp
   (defmethod 40ants-doc-full/themes/api:highlight-languages ((theme my-theme))
     (list \"lisp\"
           \"python\"
           \"bash\"))
   ```

   ## Changing Page Layout

   The main entry-point for page rendering is
   RENDER-PAGE generic-function. It calls all other
   rendering functions.

   If you are inheriting your theme class from 40ANTS-DOC-FULL/THEMES/DEFAULT:DEFAULT-THEME,
   then rendering functions will be called in the following order:

   ![Page Rendering Flow](static/rendering.png{width=600})

   On this page stripes on the right demonstrate order in which different rendering functions will be called:

   - green is [RENDER-PAGE][generic-function];
   - blue is [RENDER-PAGE-HEADER][generic-function];
   - violet is [RENDER-SIDEBAR][generic-function]
   - red is [RENDER-SIDEBAR-HEADER][generic-function]
   - yellow is [RENDER-SIDEBAR-CONTENT][generic-function];
   - orange is [RENDER-SIDEBAR-FOOTER][generic-function];
   - salad green is [RENDER-CONTENT][generic-function];
   - pink is [RENDER-PAGE-FOOTER][generic-function].

   Some of these methods might call [RENDER-TOC][generic-function] and
   [RENDER-SEARCH-FORM][generic-function] to display a table of content
   and a table form. Also, you might want to redefine RENDER-HTML-HEAD generic-function
   to change html page metadata such as included stylesheets and js files, page title, etc.

   If you want to introduce changes, it is better to inherit from existing theme class
   and to define a few methods to change only needed properties. For example, here is
   a [theme I've made][my-theme] for all 40Ants projects. I've added header, footer and made colors match
   the main site.

   [my-theme]: https://github.com/40ants/40ants-doc-theme-40ants/blob/master/theme.lisp

   ## Available Themes"
  
  (40ants-doc-full/themes/default:default-theme class)
  (40ants-doc-full/themes/light:light-theme class)
  (40ants-doc-full/themes/dark:dark-theme class)
  
  "## Theme Definition Protocol"
  
  (40ants-doc-full/themes/api:highlight-languages generic-function)
  (40ants-doc-full/themes/api:highlight-theme generic-function)
  
  (40ants-doc-full/themes/api:render-css generic-function)
  
  (render-page generic-function)
  (render-html-head generic-function)
  (render-page-header generic-function)
  (render-page-footer generic-function)
  (render-content generic-function)
  (render-sidebar generic-function)
  (render-sidebar-header generic-function)
  (render-sidebar-footer generic-function)
  (render-sidebar-content generic-function)
  (render-toc generic-function)
  (render-search-form generic-function)

  "## Plugins API

   40ANTS-DOC themes support plugins. Plugins are small objects holding some configuration parameters and able to
   inject additional content into documentation pages or able to copy some static files to the results directory.

   By default, only HIGHLIGHTJS plugin is enabled, but you can pass a custom list of plugins when creating
   a theme object. For example, here is how to can enable both Hightlight.js and MathJax plugins:

   ```
   (defsection @index (:title \"Example\")
     \"MathJax example:

      1. $a \ne 0$
      2. $x = {-b \pm \sqrt{b^2-4ac} \over 2a}.$\")

   (40ants-doc-full/builder:render-to-files
    @index
    :base-dir \"/tmp/output/\"
    :theme (make-instance '40ants-doc-full/themes/light:light-theme
                          :plugins (list
                                    (highlightjs)
                                    (mathjax))))
   ```
   "

  (theme-plugins generic-function)
  (copy-static generic-function)
  (inject-into-page-header generic-function)
  (inject-before-content generic-function)
  (inject-after-content generic-function)

  "## Built-in Plugins"

  (highlightjs function)
  (mathjax function))

