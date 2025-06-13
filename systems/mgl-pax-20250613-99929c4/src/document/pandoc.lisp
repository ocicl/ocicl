(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

;;;; TeX

(defvar *tex-special-chars* "&%$#_{}~^\\")

;;; https://tex.stackexchange.com/a/34586
(defun/autoloaded escape-tex (string)
  "Construct a new string from STRING by adding a backslash before
  special TeX characters

      &%$#_{}~^\\"
  (backslash-escape string *tex-special-chars*))


(defsection @pdf-output (:title "PDF Output")
  """When invoked with :FORMAT :PDF, DOCUMENT generates
  @MARKDOWN-OUTPUT and converts it to PDF with @PANDOC, which in turn
  uses [LaTeX](https://www.latex-project.org/). Make sure that they
  are installed.

  ```
  (with-open-file (s "x.pdf" :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
    (pax:document "Hello, World!" :stream s :format :pdf))
  ```

  To see how the output looks like, visit
  [pax-manual-v0.4.1.pdf][pax-manual-sample] and
  [dref-manual-v0.4.1.pdf][dref-manual-sample].

    [pax-manual-sample]: http://quotenil.com/blog-files/pax-manual-v0.4.1.pdf
    [dref-manual-sample]: http://quotenil.com/blog-files/dref-manual-v0.4.1.pdf

  PDF output is similar to *DOCUMENT-HTML-DEFAULT-STYLE* :CHARTER
  without the off-white tint and with coloured instead of underlined
  links. The latter is because underlining interferes with hyphenation
  in LaTeX. As in HTML output, locative types link to the respective
  definition in the sources on GitHub (see MAKE-GIT-SOURCE-URI-FN).

  Note that linking from one PDF to another is currently not supported
  due to the lack of consistent support in PDF viewers. Therefore,
  such links are replaced by their label or the title if any (e.g. of
  a SECTION or GLOSSARY-TERM).

  The generation of Markdown is subject to the standard
  variables (again see [DOCUMENT][]). The Markdown to PDF conversion
  can be customized with the following variables."""
  (*document-pandoc-program* variable)
  (*document-pandoc-pdf-options* variable)
  (*document-pandoc-pdf-header-includes* (variable "<too messy to include>"))
  (*document-pandoc-pdf-metadata-block* variable))

(define-glossary-term @pandoc (:title "Pandoc"
                               :url "https://pandoc.org/"))

(defvar/autoloaded *document-pandoc-program* "pandoc"
  """The name of the Pandoc binary. It need not be an absolute pathname
  as `\\PATH` is searched.""")

(defvar/autoloaded *document-pandoc-pdf-options*
  `(("-V" "papersize=a4")
    ("-V" "margin-left=1.03in")
    ("-V" "margin-right=1.03in")
    ("-V" "margin-top=1.435in")
    ("-V" "margin-bottom=1.435in")
    ("-V" "fontfamily=XCharter")
    ("-V" "fontsize=11pt")
    ("-V" "colorlinks=true")
    ("-V" "linkcolor=blue")
    ("-V" "urlcolor=Maroon")
    ("-V" "toccolor=blue")
    "--verbose")
  "The command-line options to invoke *DOCUMENT-PANDOC-PROGRAM* with.
  For ease of manipulation, related options are grouped into sublists,
  but the entire nested list is flattened to get the list of options
  to pass to Pandoc. If `--verbose` is specified, then in addition to
  Pandoc logging LaTeX sources, PAX will log to *ERROR-OUTPUT* the
  Markdown that it converts to PDF via LaTeX. The Markdown includes
  *DOCUMENT-PANDOC-PDF-HEADER-INCLUDES* and
  *DOCUMENT-PANDOC-PDF-METADATA-BLOCK*.")

(defvar/autoloaded *document-pandoc-pdf-metadata-block* ""
  "A @PANDOC-YAML-METADATA-BLOCK as a string.

  Concatenate to this string to customize it.")

(define-glossary-term @pandoc-yaml-metadata-block
    (:title "Pandoc YAML metadata block"
     :url "https://pandoc.org/MANUAL.html#extension-yaml_metadata_block"))

(defun default-pandoc-pdf-header-includes ()
  """% Downscaled but still slightly larger than the main font to stand
% out a bit more.
\usepackage[scaled=0.88]{DejaVuSansMono}

%% EB Garamond main title
\usepackage[p,osf,scaled=1.0524843]{ebgaramond}
\newcommand{\titlefont}{\ebgaramond}
\makeatletter
\def\@maketitle{%
  \newpage
  \null
  {\centering \fontsize{20pt}{0pt} \titlefont \textls[100]{%
    \MakeUppercase{\@title}} \par}
  %{\vspace*{2\baselineskip}\centering\fontsize{11pt}{14pt}\titlefont \@author \par}
  \vspace*{4\baselineskip}}
\makeatother

\newcommand\paxlink[2]{\hyperref[#1]{#2}}

%%
\usepackage{fvextra}
\fvset{breaklines}
\fvset{breaknonspaceingroup}
\fvset{breakanywhere}
\DefineVerbatimEnvironment{Highlighting}{Verbatim}{
  commandchars=\\\{\},
  breaklines, breaknonspaceingroup, breakanywhere,
  fontsize=\small}
\usepackage[htt]{hyphenat}
\renewcommand\texttt[1]{{\ttfamily\color{.!80!yellow}#1}}
\usepackage{framed}
\definecolor{shadecolor}{RGB}{248,248,248}
\usepackage{etoolbox}
\ifcsmacro{Shaded}{
  \renewenvironment{Shaded}{\begin{snugshade}}{\end{snugshade}}
}{}
\NewEnvironmentCopy{verbatimOld}{verbatim}
\renewenvironment{verbatim}{%
  \VerbatimEnvironment
  \begin{snugshade}
  \begin{Verbatim}[fontsize=\small]%
}{%
  \end{Verbatim}%
  \end{snugshade}%
}
\renewcommand{\labelitemii}{$\circ$}
\newcommand\paxlocativetype[1]{\textbf{[#1]}}
\newcommand\paxlocativetypewithsource[2]{%
  \href{#1}{\textcolor{black}{\textbf{[#2]}}}}
\newcommand\paxname[1]{%
  \hspace{0.1em}\fcolorbox[HTML]{606060}{f5f5f5}{\textbf{#1}}}
""")

(defvar/autoloaded *document-pandoc-pdf-header-includes*
    (default-pandoc-pdf-header-includes)
    "LaTeX code (a string) to include in the preamble via
  [`header-includes`](https://pandoc.org/MANUAL.html#layout).

  The default includes have no configuration knobs. Look at the value
  to see how to customize it.")

(defun pandoc-metadata-string ()
  (format nil "---~%header-includes: |~%  ~
              ```{=latex}~%~A~&  ```~%~
              ~A~&~
              ---~%"
          (prefix-lines "  " *document-pandoc-pdf-header-includes*)
          *document-pandoc-pdf-metadata-block*))

;;; For testing
(defvar *pandoc-output-format* "pdf")

(defun print-pandoc-pdf (parse-tree stream)
  (let* ((pandoc-options (flatten *document-pandoc-pdf-options*))
         (verbosep (find "--verbose" pandoc-options :test #'equal))
         (program-and-args
           (append (list
                    *document-pandoc-program*
                    (format nil "--highlight-style=~A"
                            (asdf:system-relative-pathname
                             "mgl-pax" "src/document/pandoc-pdf.theme"))
                    "--shift-heading-level-by=-1"
                    "-f" "markdown-smart-raw_tex"
                    "-t" *pandoc-output-format*
                    "-o" "-")
                   ;; Later options take precedence.
                   pandoc-options)))
    (when verbosep
      (format *error-output* "~@<;; [PAX] Invoking Pandoc: ~S~:@>~&"
              program-and-args))
    ;; Working with temporary files is better than using
    ;; UIOP:LAUNCH-PROGRAM with :INPUT :STREAM and :OUTPUT STREAM
    ;; because
    ;;
    ;; - It avoids https://bugs.launchpad.net/sbcl/+bug/840190 on
    ;;  SBCL.
    ;;
    ;; - We do not need to add :SHARING :EXTERNAL for CCL when STREAM
    ;;   is created.
    ;;
    ;; - ECL does not support non-file STREAMs in RUN-PROGRAM.
    ;;
    ;; - We don't need to handle broken pipe (in a portable way).
    (uiop:with-temporary-file (:pathname input)
      (let ((metadata (pandoc-metadata-string)))
        (when verbosep
          (format *error-output* "~@<;; [PAX] Markdown output~:@>~&")
          (write-string metadata *error-output*)
          (print-markdown parse-tree *error-output*
                          :format :markdown)
          (format *error-output* "~@<;; [PAX] Latex output~:@>~&"))
        (with-open-file (input input :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
          (write-string metadata input)
          (print-markdown parse-tree input :format :markdown)))
      (uiop:with-temporary-file (:pathname output)
        (let ((exit-code (nth-value
                          2 (uiop:run-program program-and-args
                                              :input input
                                              :output output
                                              ;; KLUDGE: ECL fails
                                              ;; with this.
                                              #-ecl :error-output #-ecl t
                                              :ignore-error-status t))))
          (unless (zerop exit-code)
            (error "~@<Running ~S failed with exit code ~S. See ~
                   *ERROR-OUTPUT* for the error message.~:@>"
                   program-and-args exit-code)))
        (write-sequence (alexandria:read-file-into-byte-vector output)
                        stream)))))

(declaim (ftype function link-definition))
(declaim (ftype function link-page))
(declaim (ftype function anchor-id))
(declaim (ftype function page-p))
(declaim (special *id-to-link*))
(declaim (special *page*))

(defun prepare-parse-tree-for-printing-to-pandoc-pdf (parse-tree)
  (map-markdown-parse-tree '(:code 3bmd-code-blocks::code-block :reference-link)
                           '() nil #'translate-for-pandoc-pdf
                           parse-tree))

(defun translate-for-pandoc-pdf (parent tree)
  (declare (ignore parent))
  ;; There is also
  ;; https://github.com/jgm/pandoc/issues/10841#issuecomment-2876656645,
  ;; but it's not clear where the fix belongs.
  (ecase (first tree)
    (:code
     `(:code ,(remove-newlines-and-indent (second tree))))
    (3bmd-code-blocks::code-block
     (let ((lang (pt-get tree :lang)))
       (if (and lang (plusp (length lang))
                ;; Pandoc only knows "lisp" syntax.
                (not (find lang '("cl" "common-lisp" "commonlisp" "elisp")
                           :test #'equal)))
           tree
           (let ((tree (copy-list tree)))
             (setf (pt-get tree :lang) "lisp")
             tree))))
    (:reference-link
     (let* ((label (pt-get tree :label))
            (definition (first (pt-get tree :definition)))
            (link (gethash definition *id-to-link*))
            (target-page (and link (link-page link))))
       (assert (page-p *page*))
       (cond
         ((not (page-p target-page))
          tree)
         ;; Linking to PDF destinations in other PDFs with the URL
         ;; fragment syntax doesn't work reliably across PDF viewers
         ;; and browsers, so just drop the link and emit the label.
         ((not (eq target-page *page*))
          (values label nil t))
         ;; Intra-PDF links
         (t
          (multiple-value-bind (label label-last-code)
              (split-off-last-code label)
            (values `((:code
                       ,(format nil "\\paxlink{~A}{"
                                (anchor-id (link-definition link))))
                      "{=latex}"
                      ,@label
                      ;; If the last element of label is :CODE, then
                      ;; (:CODE "}") would be printed with two
                      ;; consecutive backticks, so let's concatenate
                      ;; the two.
                      (:code ,(format nil "\\texttt{~A}}"
                                      (escape-tex label-last-code)))
                      "{=latex}")
                    nil t))))))))

(defun split-off-last-code (tree)
  (let ((last (first (last tree))))
    (if (parse-tree-p last :code)
        (values (butlast tree) (second last))
        tree)))

(defun remove-newlines-and-indent (string)
  (with-output-to-string (out)
    (let ((prev-char #\Null))
      (loop for char across string
            do (cond ((char= char #\Newline)
                      (unless (whitespacep prev-char)
                        (write-char #\Space out)))
                     ((and (whitespacep char)
                           (char= prev-char #\Newline))
                      ;; Discard indent
                      (setq char #\Newline))
                     (t
                      (write-char char out)))
               (setq prev-char char)))))
