(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defun parse-markdown (string)
  (clean-up-parsed-parse-tree (parse-markdown-fast string)))

(defun parse-markdown-fast (string)
  (if (< (length string) 1000)
      (3bmd-grammar:parse-doc string)
      (let ((parses ()))
        (map-markdown-block-parses (lambda (parse)
                                     (push parse parses))
                                   string)
        (nreverse parses))))

(defun map-markdown-block-parses (fn string)
  (if (< (length string) 1000)
      (map nil fn (3bmd-grammar:parse-doc string))
      ;; This is 3BMD-GRAMMAR:PARSE-DOC's currently commented out
      ;; alternative implementation, which is much faster on long
      ;; strings but perhaps slower on short strings. This still has
      ;; a performance problem with large lists, which are processed
      ;; in one %BLOCK, hence the workaround in PAX-APROPOS*.
      (let ((block-rule (if (esrap:find-rule '3bmd-grammar::%block)
                            '3bmd-grammar::%block
                            ;; Make it work with old 3BMD.
                            '3bmd-grammar::block)))
        (progn
          (loop
            for start = 0 then pos
            for (%block pos) = (multiple-value-list
                                (esrap:parse block-rule string
                                             :start start :junk-allowed t))
            while %block
            do (funcall fn %block)
            while pos)))))

(defmacro with-colorize-silenced (() &body body)
  `(let ((*trace-output* (make-broadcast-stream)))
     ,@body))

(defun print-markdown (parse-tree stream &key (format :markdown))
  (with-colorize-silenced ()
    (3bmd::print-doc-to-stream-using-format
     (preprocess-parse-tree-for-printing parse-tree format) stream format)))


;;;; Text based Markdown fragments

(defun heading (level stream)
  (loop repeat (1+ level) do (write-char #\# stream)))

(defun code (string)
  (if (zerop (length string))
      ""
      (format nil "`~A`" string)))

(defun bold (string stream)
  (if (zerop (length string))
      ""
      (format stream "**~A**" string)))

(defun italic (string stream)
  (if (zerop (length string))
      ""
      (format stream "*~A*" string)))

(defun markdown-special-inline-char-p (char)
  (member char '(#\\ #\* #\_ #\` #\[ #\])))

(defun markdown-special-html-char-p (char)
  (member char '(#\< #\&)))

(defun markdown-special-block-char-p (char)
  (member char '(#\# #\Return #\Newline)))

(defun/autoloaded escape-markdown
    (string &key (escape-inline t) (escape-mathjax t) (escape-html t)
            (escape-block t))
  "Backslash escape Markdown constructs in STRING.

  - If ESCAPE-INLINE, then escape the following characters:

          *_`[]\\
  - If ESCAPE-MATHJAX, then escape `$` characters.

  - If ESCAPE-HTML, then escape the following characters:

          <&

  - If ESCAPE-BLOCK, then escape whatever is necessary to avoid
    starting a new Markdown block (e.g. a paragraph, heading, etc)."
  (flet ((blank-line-until-p (pos)
           (loop for i downfrom (1- pos) downto 0
                 for char = (aref string i)
                 do (when (char= char #\Newline)
                      (return t))
                 do (unless (whitespacep char)
                      (return nil))
                 finally (return t))))
    (with-output-to-string (stream)
      (dotimes (i (length string))
        (let ((char (aref string i)))
          (cond ((and escape-html (char= char #\&))
                 ;; If there is no semicolon or there is no whitespace
                 ;; between & and ;, then it's not parsed as an
                 ;; entity, so don't escape it to reduce clutter.
                 (let ((semicolon-pos (position #\; string :start (1+ i)))
                       (whitespace-pos (position-if #'whitespacep string
                                                    :start (1+ i))))
                   (if (and semicolon-pos (or (null whitespace-pos)
                                              (< semicolon-pos whitespace-pos)))
                       (write-string "\\&" stream)
                       (write-char char stream))))
                (t
                 (when (or (and escape-inline
                                (markdown-special-inline-char-p char))
                           (and escape-mathjax
                                (char= char #\$))
                           (and escape-html
                                (markdown-special-html-char-p char))
                           (and escape-block
                                (markdown-special-block-char-p char)
                                (blank-line-until-p i)))
                   (write-char #\\ stream))
                 (write-char char stream))))))))


;;;; Parse tree based Markdown fragments

(declaim (inline parse-tree-p))
(defun parse-tree-p (parse-tree tag)
  (and (listp parse-tree)
       (eq (first parse-tree) tag)))

(defmacro pt-get (parse-tree attr)
  `(getf (rest ,parse-tree) ,attr))

(defun code-fragment (string)
  `(:code ,(princ-to-string string)))

(defun indent-verbatim (tree)
  (assert (eq (first tree) :verbatim))
  `(:verbatim ,(prefix-lines "  " (second tree))))

(defun indent-code-block (tree)
  (assert (eq (first tree) '3bmd-code-blocks::code-block))
  (let ((tree (copy-list tree)))
    (setf (pt-get tree :content)
          (prefix-lines "  " (pt-get tree :content)))
    tree))

;;; This may return NIL (e.g for ((:EMPH "XXX")) :DEEMPH NIL).
(defun parse-tree-to-text (parse-tree &key deemph)
  (labels
      ((recurse (e)
         (cond ((stringp e)
                ;; "S" -> "S"
                e)
               ((and (listp e)
                     (or (stringp (first e))
                         (listp (first e))))
                ;; ("mgl-pax-test:" (:EMPH "test-variable")) =>
                ;; "mgl-pax-test:*test-variable*"
                (apply #'concatenate 'string (mapcar #'recurse e)))
               ;; Recurse into (:PLAIN ...)
               ((parse-tree-p e :plain)
                (format nil "~A" (recurse (rest e))))
               ;; (:EMPH "S") -> "*S*"
               ((and deemph (parse-tree-p e :emph))
                (format nil "*~A*" (recurse (rest e))))
               ;; (:CODE "S") -> "S"
               ((parse-tree-p e :code)
                (let ((string (second e)))
                  (cond ((starts-with-subseq "\\\\" string)
                         (recurse (subseq string 2)))
                        ((starts-with-subseq "\\" string)
                         (recurse (subseq string 1)))
                        (t
                         (recurse string)))))
               ((and (listp e) (null (cdr e)))
                (recurse (first e)))
               (t
                (return-from parse-tree-to-text nil)))))
    (recurse parse-tree)))


;;;; Markdown parse tree transformation

;;; Perform a depth-first traversal of TREE. Call FN with the parent
;;; of each node and the node itself. FN returns three values:
;;;
;;; 1. New tree: It's substituted for the node.
;;;
;;; 2. Recurse flag: If recurse and the new tree is not a leaf, then
;;;    traversal recurses into the new tree.
;;;
;;; 3. Slice flag: If slice, then instead of adding the new tree as an
;;;    element to the transformed output (of the parent), add all
;;;    elements of the new tree (which must be a LIST). No slice is
;;;    like MAPCAR, slice is is MAPCAN.
(defun transform-tree (fn tree)
  (declare (optimize speed))
  (let ((fn (coerce fn 'function)))
    (labels ((foo (parent tree)
               (multiple-value-bind (new-tree recurse slice)
                   (funcall fn parent tree)
                 (assert (or (not slice) (listp new-tree)))
                 (if (or (atom new-tree)
                         (not recurse))
                     (values new-tree slice)
                     (values (loop for sub-tree in new-tree
                                   nconc (multiple-value-bind
                                               (new-sub-tree slice)
                                             (foo new-tree sub-tree)
                                           (if slice
                                               (copy-list new-sub-tree)
                                               (list new-sub-tree))))
                             slice)))))
      (foo nil tree))))

;;; When used as the FN argument to TRANSFORM-TREE, leave the tree
;;; intact except for subtrees (lists) whose CAR is in TAGS, whose
;;; transformation is deferred to FN. FN must return the three values
;;; TRANSFORM-TREE expects. If HANDLE-STRINGS, then FN is called with
;;; STRING nodes, too.
;;;
;;; If the CAR of a subtree is in STOP-TAGS, then the entire subtree
;;; is included in the output without further processing.
(defun defer-tag-handling (tags stop-tags handle-strings fn parent tree)
  (cond ((or (and (consp tree)
                  (member (first tree) tags))
             (and handle-strings
                  (stringp tree)))
         (funcall fn parent tree))
        ((and (consp tree)
              (member (first tree) stop-tags))
         (values tree nil nil))
        (t
         (values tree t nil))))

(defun map-markdown-parse-tree (tags stop-tags handle-strings fn parse-tree)
  (transform-tree (lambda (parent tree)
                    (defer-tag-handling tags stop-tags
                      handle-strings fn parent tree))
                  parse-tree))


(defun clean-up-parsed-parse-tree (parse-tree)
  (transform-tree (lambda (parent tree)
                    (declare (ignore parent))
                    (if (and (listp tree)
                             (not (parse-tree-p tree :verbatim)))
                        (values (join-stuff-in-list tree) t nil)
                        tree))
                  parse-tree))

(defun join-stuff-in-list (tree)
  (let ((result ()))
    (dolist (element tree)
      (let ((prev (first result)))
        (cond
          ;; Join consecutive non-blank strings: "x" "y" -> "xy"
          ((and (stringp element) (not (blankp element))
                (stringp prev) (not (blankp prev)))
           (setf (first result) (concatenate 'string prev element)))
          ;; Join consecutive blank strings
          ((and (stringp element) (blankp element)
                (stringp prev) (blankp prev))
           (setf (first result) (concatenate 'string prev element)))
          ;; "CL:" (:EMPH "*FEATURES*") - > "CL:*FEATURES*"
          ((and (listp element) (eq (first element) :emph)
                (stringp prev) (ends-with #\: prev))
           (setf (first result)
                 (format nil "~A*~A*" prev (second element))))
          (t
           (push element result)))))
    (reverse result)))

(defun preprocess-parse-tree-for-printing (parse-tree format)
  (if (eq format :markdown)
      (map-markdown-parse-tree
       '() '(:code :verbatim 3bmd-code-blocks::code-block)
       t #'fudge-markdown-strings parse-tree)
      parse-tree))

(defun fudge-markdown-strings (parent string)
  (declare (ignore parent))
  ;; KLUDGE: 3BMD parse-print roundtrip loses backslash escapes:
  ;;
  ;; (with-output-to-string (out)
  ;;   (3bmd::print-doc-to-stream-using-format
  ;;    (3bmd-grammar:parse-doc "[\\\\][x]")
  ;;    out :markdown))
  ;; => "[\\][x]"
  ;;
  ;; (3bmd-grammar:parse-doc "[\\][x]")
  ;; => ((:PLAIN "[" "]" (:REFERENCE-LINK :LABEL ("x") :TAIL NIL)))
  (when (ends-with #\\ string)
    (setq string (concatenate 'string string "\\")))
  ;; While we are here, also add a workaround for \$ not escaping math
  ;; on GitHub. On the other hand, GitHub parses a$x$ as normal text,
  ;; so let's add a zero-width space before $ characters at the
  ;; beginning of a word.
  (when (and (null *subformat*)
             (starts-with #\$ string))
    (let ((escape #.(ignore-errors (string (code-char 8203)))))
      (when escape
        (setq string (concatenate 'string escape string)))))
  string)

;;; Post-process the Markdown parse tree to make it prettier on w3m
;;; and maybe make relative links absolute.
(defun prepare-parse-tree-for-printing-to-w3m (parse-tree)
  (flet ((translate (parent tree)
           (declare (ignore parent))
           (cond ((eq (first tree) :code)
                  `(:strong ,tree))
                 ((eq (first tree) :verbatim)
                  (values `((:raw-html #.(format nil "<i>~%"))
                            ,(indent-verbatim tree) (:raw-html "</i>"))
                          nil t))
                 (t
                  (values `((:raw-html #.(format nil "<i>~%"))
                            ,(indent-code-block tree)
                            (:raw-html "</i>"))
                          nil t)))))
    (map-markdown-parse-tree '(:code :verbatim 3bmd-code-blocks::code-block)
                             '() nil #'translate parse-tree)))

(defun prepare-parse-tree-for-printing-to-plain (parse-tree)
  (note @plain-strip-markup
    "
    - Markup for @MARKDOWN/EMPHASIS, @MARKDOWN/INLINE-CODE,
      @MARKDOWN/REFLINKs and @FENCED-CODE-BLOCKS is stripped from
      the output."
    (flet ((translate (parent tree)
             (declare (ignore parent))
             (ecase (first tree)
               ((:emph :strong :code)
                (values (rest tree) t t))
               ((:reference-link)
                (values (pt-get tree :label) t t))
               ((3bmd-code-blocks::code-block)
                (values `(:verbatim ,(pt-get tree :content)))))))
      (map-markdown-parse-tree '(:emph :strong :code :reference-link
                                 3bmd-code-blocks::code-block)
                               '() nil #'translate parse-tree))))


;;; Call FN with STRING and START, END indices of @WORDS.
;;;
;;; FN must return two values: a replacement Markdown parse tree
;;; fragment (or NIL, if the subseq shall not be replaced), whether
;;; the replacement shall be sliced into the result list. MAP-WORDS
;;; returns a parse tree fragment that's a list of non-replaced parts
;;; of STRING and replacements (maybe sliced). Consecutive strings are
;;; concatenated.
(defun map-words (string fn)
  (declare (type string string))
  (let ((translated ())
        (n (length string))
        (start 0))
    (flet ((add (a)
             (if (and (stringp a)
                      (stringp (first translated)))
                 (setf (first translated)
                       (concatenate 'string (first translated) a))
                 (push a translated))))
      (loop while (< start n)
            do (let* ((at-delimiter-p (delimiterp (aref string start)))
                      (end (or (if at-delimiter-p
                                   (position-if-not #'delimiterp string
                                                    :start start)
                                   (position-if #'delimiterp string
                                                :start start))
                               n)))
                 (if at-delimiter-p
                     (add (subseq string start end))
                     (multiple-value-bind (replacement slice)
                         (funcall fn string start end)
                       (cond ((null replacement)
                              (add (subseq string start end)))
                             (slice
                              (dolist (a replacement)
                                (add a)))
                             (t
                              (add replacement)))))
                 (setq start end))))
    (nreverse translated)))

(defun delimiterp (char)
  (or (whitespacep char) (find char "()'`\"")))
