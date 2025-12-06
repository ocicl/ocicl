(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defun sanitize-docstring (docstring &key (first-line-special-p t) aggressivep)
  (let ((indentation
          (docstring-indentation docstring
                                 :first-line-special-p first-line-special-p)))
    (if (and aggressivep (not (almost-surely-markdown-p docstring)))
        (round-up-indentation (strip-docstring-indent docstring indentation
                                                      first-line-special-p)
                              #'escape-docstring-line)
        (strip-docstring-indent docstring indentation first-line-special-p))))

(defun almost-surely-markdown-p (docstring)
  (search "```" docstring))

(defun escape-docstring-line (line)
  (escape-html-in-docstring (escape-heading-in-docstring line)))


(defun strip-docstring-indent (docstring indentation first-line-special-p)
  """
  - Docstrings can be indented in any of the usual styles. PAX
    normalizes indentation by stripping the longest run of leading
    spaces common to all non-blank lines except the first. Thus, the
    following two docstrings are equivalent:

          (defun foo ()
            "This is
            indented
            differently")

          (defun foo ()
            "This is
          indented
          differently")"""
  (with-output-to-string (out)
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            do (multiple-value-bind (line missing-newline-p)
                   (read-line s nil nil)
                 (unless line
                   (return))
                 (write-string (if (and first-line-special-p
                                        (zerop i))
                                   line
                                   (subseq* line indentation))
                               out)
                 (unless missing-newline-p
                   (terpri out)))))))

(defun n-leading-spaces (line)
  (let ((n 0))
    (loop for i below (length line)
          while (char= (aref line i) #\Space)
          do (incf n))
    n))

;;; Return the minimum number of leading spaces in non-blank lines
;;; after the first.
(defun docstring-indentation (docstring &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (with-input-from-string (s docstring)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (and (or (not first-line-special-p) (plusp i))
                          (not (blankp line)))
                 (when (or (null n-min-indentation)
                           (< (n-leading-spaces line) n-min-indentation))
                   (setq n-min-indentation (n-leading-spaces line))))))
    (or n-min-indentation 0)))

(defun docstring-indentation* (lines &key (first-line-special-p t))
  (let ((n-min-indentation nil))
    (loop for i upfrom 0
          for line in lines
          while line
          do (when (and (or (not first-line-special-p) (plusp i))
                        (not (blankp line)))
               (when (or (null n-min-indentation)
                         (< (n-leading-spaces line) n-min-indentation))
                 (setq n-min-indentation (n-leading-spaces line)))))
    (or n-min-indentation 0)))


(defun round-up-indentation (docstring line-transform)
  "Indentation of what looks like blocks of Lisp code is rounded up to
  a multiple of 4. More precisely, non-zero indented lines between
  blank lines or the docstring boundaries are reindented if the first
  non-space character of the first line is an `(` or a `;` character."
  (declare (type function line-transform))
  (with-output-to-string (out)
    (with-input-from-string (in docstring)
      (let ((indented-lines ()))
        (labels
            ((write-line* (line)
               (write-string (funcall line-transform line) out))
             (flush-indented-lines ()
               (let* ((lines (reverse indented-lines))
                      (prefix (extra-indent-for-lisp-code lines)))
                 #+nil
                 (format t "FLUSH: ~S~%  ~S~%" last-blank-line indented-lines)
                 (dolist (line (reverse indented-lines))
                   (write-line* (concatenate 'string prefix line))))
               (setq indented-lines :none)))
          (loop for (line missing-newline-p) = (multiple-value-list
                                                (read-line in nil nil))
                while line
                do (let ((blankp (blankp line))
                         (indentedp (starts-with #\Space line))
                         (line (if missing-newline-p
                                   line
                                   (format nil "~A~%" line))))
                     #+nil
                     (format t "LINE: ~S ~S ~S~%  ~S~%"
                             blankp indentedp indented-lines line)
                     ;; End the indented block on blanks and
                     ;; non-indented lines.
                     (when (and (or blankp (not indentedp))
                                (not (eq indented-lines :none)))
                       (flush-indented-lines))
                     (if (eq indented-lines :none)
                         (write-line* line)
                         (push line indented-lines))
                     ;; Start an indented block.
                     (when blankp
                       (setq indented-lines ()))))
          (unless (eq indented-lines :none)
            (flush-indented-lines)))))))

(defun extra-indent-for-lisp-code (lines)
  (if (lines-looking-like-lisp-code-p lines)
      (let* ((indentation (docstring-indentation* lines
                                                  :first-line-special-p nil))
             (new-indentation (round-up-to-multiple-of indentation 4)))
        (make-string (- new-indentation indentation) :initial-element #\Space))
      ""))

(defun lines-looking-like-lisp-code-p (lines)
  (and lines
       (let* ((line (first lines))
              (n-spaces (n-leading-spaces line)))
         (and (<= n-spaces (length line))
              (member (aref line n-spaces) '(#\( #\;))))))

(defun round-up-to-multiple-of (n m)
  (* (ceiling n m) m))


;;;; The right way to avoid HTML and headings would be to transform
;;;; the Markdown parse tree. Currently, the transformed tree would
;;;; need to be printed back to Markdown only to be parsed back later,
;;;; which -- apart from being a performance issue -- would bring out
;;;; 3BMD's parse/print inconsistencies. So instead, we resort to
;;;; fragile heuristics, knowing that they are applied to non-PAX
;;;; stuff only (i.e. when SANITIZE-AGGRESSIVELY-P).

(defun escape-html-in-docstring (line)
  "Special HTML characters `<&` are escaped."
  (if (starts-with-subseq "    " line)
      line
      (escape-markdown line :escape-html t :escape-inline nil
                       :escape-mathjax nil :escape-block nil)))

(defun escape-heading-in-docstring (line)
  """Furthermore, to reduce the chance of inadvertently introducing a
  Markdown heading, if a line starts with a string of `#` characters,
  then the first one is automatically escaped. Thus, the following two
  docstrings are equivalent:

          The characters #\Space, #\Tab and
          #Return are in the whitespace group.

          The characters #\Space, #\Tab and
          \#Return are in the whitespace group."""
  (let ((n-spaces (n-leading-spaces line)))
    (if (and (< n-spaces (length line))
             (char= (aref line n-spaces) #\#))
        (concatenate 'string (subseq line 0 n-spaces)
                     "\\"
                     (subseq line n-spaces))
        line)))
