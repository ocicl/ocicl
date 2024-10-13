(uiop:define-package #:40ants-doc-full/markdown
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:defsection)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader))
(in-package #:40ants-doc-full/markdown)


(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defsection @markdown-support (:title "Markdown Support")
  "The Markdown in docstrings is processed with the
  [3BMD][3bmd] library."
  (@markdown-indentation section)
  (@markdown-syntax-highlighting section)
  (@mathjax section))


(defsection @markdown-indentation (:title "Indentation")
  """
  Docstrings can be indented in any of the usual styles.
  40ANTS-DOC normalizes indentation by converting:

  ```
  (defun foo ()
    "This is
     indented
     differently")
  ```

  to

  ```
  (defun foo ()
    "This is
  indented
  differently")
  ```


  Docstrings in sources are indented in various ways which can easily
  mess up markdown. To handle the most common cases leave the first
  line alone, but from the rest of the lines strip the longest run of
  leading spaces that is common to all non-blank lines."
  """)

(defsection @markdown-syntax-highlighting (:title "Syntax highlighting")
  "For syntax highlighting, github's [fenced code
  blocks][fenced-code-blocks] markdown extension to mark up code
  blocks with triple backticks is enabled so all you need to do is
  write:

      ```elisp
      (defun foo ())
      ```

  to get syntactically marked up HTML output. The language tag, `elisp` in this example,
  is optional and defaults to `commonlisp`.

  Originally MGL-PAX used [colorize][colorize] for the syntax
  highlighting, but 40ANTS-DOC uses [Highlight.js][highlightjs] which is able to
  guess code block language if it is not specified. To minimize HTML document's
  static size, Hightlight.js is configured to support only these languages:

  * bash
  * css
  * json
  * yaml
  * plain-text
  * html
  * markdown
  * lisp
  
  There is a separate [README][readme] where you will find instructions on how to
  support other languages.

  Besides an automatic language detection, the other cool feature of Highlight.js
  is it's support for different color themes. Here you can view all available themes:
  [https://highlightjs.org/static/demo/](https://highlightjs.org/static/demo/).
  There is no easy way to choose color theme yet, but probably this will be a nice
  feature for 40ANTS-DOC.
  
  [readme]: https://github.com/40ants/doc/blob/master/static/README.md#how-to-update-highlightjs
  [highlightjs]: https://highlightjs.org/
  [3bmd]: https://github.com/3b/3bmd
  [colorize]: https://github.com/redline6561/colorize/
  [fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks")

(defsection @mathjax (:title "MathJax")
  """Displaying pretty mathematics in TeX format is supported via
  MathJax. It can be done inline with `$` like this:

      $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

  which is diplayed as $\int_0^\infty e^{-x^2}
  dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

      $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  to get: $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  MathJax will leave code blocks (including those inline with
  backticks) alone. Outside code blocks, escape `$` by prefixing it
  with a backslash to scare MathJax off.

  Escaping all those backslashes in TeX fragments embedded in Lisp
  strings can be a pain. [Pythonic String
  Reader](https://github.com/smithzvk/pythonic-string-reader) can help
  with that.""")
