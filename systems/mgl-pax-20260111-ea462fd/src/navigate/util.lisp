(in-package :mgl-pax)

;;;; I/O

(defmacro with-standard-io-syntax* (&body body)
  `(with-standard-io-syntax
     ;; With *PRINT-READABLY*, CLISP insists on printing FOO as |FOO|.
     (let (#+clisp (*print-readably* nil))
       ,@body)))

(defparameter *utf-8-external-format*
  #+abcl :utf-8
  #+allegro :utf-8
  #+clisp charset:utf-8
  #-(or abcl allegro clisp) :default)


;;;; Parsing of symbols, strings, numbers and their nested lists
;;;; without interning and recognizing some unreadable values

(defvar *on-unreadable* :error)
(defvar *truncating-on-unreadable* nil)
(defvar *on-read-eval-error* :parse-error)

;;; A non-interning parser like SWANK::PARSE-SYMBOL, but it supports
;;; nested lists of symbols, strings and numbers, which is currently
;;; enough for @NAMEs.
;;;
;;; May signal END-OF-FILE, READER-ERROR and PARSE-ERROR if ERRORP.
;;;
;;; *READTABLE* is ignored, and no macro dispatch character is handled
;;; except #. (this is allowed to INTERN) and #<, which behave as in
;;; the standard readtable.
;;
;;; If ON-UNREADABLE is :TRUNCATE, then the already read stuff is
;;; returned with the symbol PAX::UNREADABLE marking the location.
;;;
;;; If ON-UNREADABLE is a function of a STREAM argument, it is called
;;; when an unreadable marker #< is encountered, with FILE-POSITION at
;;; the # character. It should return the parsed unreadable object
;;; (with FILE-POSITION of STREAM updated) or signal PARSE-SEXP-ERROR.
(defun parse-sexp (string &key (start 0) junk-allowed (errorp t)
                            (on-unreadable :error) )
  (handler-bind (((or end-of-file reader-error parse-error)
                   (lambda (c)
                     (declare (ignore c))
                     (unless errorp
                       (return-from parse-sexp nil)))))
    (let ((string (subseq string start)))
      (with-input-from-string (stream string)
        (let* ((*on-unreadable* on-unreadable)
               (*truncating-on-unreadable* nil)
               (*on-read-eval-error* (if errorp :parse-error nil))
               (sexp (parse-sexp* stream string))
               (pos (file-position stream)))
          (when (and (not junk-allowed)
                     (not *truncating-on-unreadable*)
                     (peek-char t stream nil))
            (parse-sexp-error "Junk in string ~S." string))
          (values sexp (+ start pos)))))))

(define-condition parse-sexp-error (parse-error)
  ((format-control :initarg :format-control :reader format-control)
   (format-args :initarg :format-args :reader format-args))
  (:report (lambda (condition stream)
             (format stream "~@<~?~:@>" (format-control condition)
                     (format-args condition)))))

(defun parse-sexp-error (format-control &rest format-args)
  (error 'parse-sexp-error :format-control format-control
                           :format-args format-args))

(defun parse-sexp* (stream string)
  (let ((next (peek-char t stream nil)))
    (cond ((null next)
           (parse-sexp-error "Unexpected EOF"))
          ((eql next #\))
           (parse-sexp-error "Unmatched closing parenthesis"))
          ((eql next #\()
           (read-char stream)
           (parse-sexp*/list stream string))
          ((eql next #\#)
           (read-char stream)
           (let ((next (peek-char nil stream nil)))
             (cond ((null next)
                    (parse-sexp-error "Unexpected EOF after # character"))
                   ((eql next #\:)
                    (file-position stream (1- (file-position stream)))
                    (read stream))
                   ((eql next #\.)
                    (read-char stream)
                    (let ((object (read stream)))
                      (handler-bind
                          ((error
                             (lambda (e)
                               (when (eq *on-read-eval-error* :parse-error)
                                 (parse-sexp-error "#.~A failed with:~:@_  ~A"
                                                   object e)))))
                        (eval object))))
                   ((eql next #\<)
                    (cond ((eq *on-unreadable* :error)
                           (parse-sexp-error "Unreadable value found in ~S."
                                             string))
                          ((eq *on-unreadable* :truncate)
                           (setq *truncating-on-unreadable* t)
                           'unreadable)
                          (t
                           (file-position stream (1- (file-position stream)))
                           (funcall *on-unreadable* stream))))
                   (t
                    (parse-sexp-error "Unsupported sharp macro character ~S."
                                      next)))))
          (t
           (parse-sexp*/atom stream string)))))

(defun parse-sexp*/list (stream string)
  (loop
    do (when *truncating-on-unreadable*
         (return children))
    collect (let ((next (peek-char t stream nil)))
              (cond ((null next)
                     (parse-sexp-error "Unexpected EOF while parsing list"))
                    ((eql next #\))
                     (read-char stream)
                     (return children))
                    (t
                     (parse-sexp* stream string))))
      into children))

(defun parse-sexp*/atom (stream string)
  (let ((next (peek-char t stream nil)))
    (cond ((null next)
           (parse-sexp-error "Unexpected EOF"))
          ((or (eql next #\") (digit-char-p next))
           (read stream))
          (t
           (multiple-value-bind (symbol foundp end-pos)
               (parse-interned-symbol string :start (file-position stream)
                                             :junk-allowed t)
             (unless foundp
               (parse-sexp-error "~S does not name an interned symbol."
                                 (subseq string (file-position stream)
                                         end-pos)))
             (file-position stream end-pos)
             symbol)))))

;;; From START in STRING, skip over the next sexp, and return the
;;; index of the next character (preserving whitespace). Thus, (SUBSEQ
;;; STRING INDEX) is the sexp as a string. May signal READER-ERROR or
;;; END-OF-FILE.
(defun skip-sexp (string &key (start 0))
  (nth-value 1 (let ((*read-suppress* t))
                 (read-from-string string t nil :start start
                                                :preserve-whitespace t))))

(defun parse-interned-symbol (string &key (start 0) junk-allowed)
  (handler-case
      (let ((pos (skip-sexp string :start start)))
        (multiple-value-bind (symbol foundp)
            (swank::parse-symbol (trim-whitespace (subseq string start pos)))
          (if foundp
              (multiple-value-bind (symbol2 pos)
                  (read-from-string string t nil :start start)
                (cond ((not (eq symbol symbol2))
                       #+sbcl (assert (eq symbol symbol2))
                       #-sbcl (values nil nil pos))
                      ((or junk-allowed (= pos (length string)))
                       (values symbol2 t pos))
                      (t
                       (values symbol2 nil pos))))
              (values nil nil pos))))
    ((or reader-error end-of-file) ())))

(note (@unreadable-prints-to :join " ")
  "An object with an unreadable representation is said to print to
  some string `S`"
  ;; A utility for :ON-UNREADABLE of PARSE-SEXP.
  (defun read-unreadable (stream unreadables)
    (loop for unreadable in unreadables
          when (skip-string-ignoring-case-and-whitespace
                stream (unreadable-to-string unreadable))
            do (return unreadable)
          finally (parse-sexp-error "Unrecognized unreadable value in ~S."
                                    (uiop:slurp-stream-string stream))))

  (defun unreadable-to-string (object)
    (note "if its PRIN1 representation (under WITH-STANDARD-IO-SYNTAX
          but in the current package and with *PRINT-READABLY* NIL)"
      (let ((package *package*))
        (with-standard-io-syntax*
          (let ((*package* package)
                (*print-readably* nil))
            (prin1-to-string object))))))

  ;; Read characters from STREAM and STRING one-by-one as long as they
  ;; are the EQUALP with the twist that one WHITESPACEP character is
  ;; the same as multiple. Return T if the entire STRING matched.
  ;; Else, return NIL and restore the FILE-POSITION of STREAM.
  (defun skip-string-ignoring-case-and-whitespace (stream string)
    (let ((orig-pos (file-position stream)))
      (flet ((fail ()
               (file-position stream orig-pos)
               (return-from skip-string-ignoring-case-and-whitespace nil)))
        (with-input-from-string (stream2 string)
          (note "is the same as `S`,")
          (loop
            (let ((char1 (peek-char nil stream nil))
                  (char2 (peek-char nil stream2 nil)))
              (when (null char2)
                (return t))
              (when (or (null char1)
                        (not (eq (not (whitespacep char1))
                                 (not (whitespacep char2)))))
                (fail))
              (cond ((whitespacep char1)
                     (note "where consecutive whitepace characters are
                     replaced with a single space in both strings,"
                       (peek-char t stream nil)
                       (peek-char t stream2 nil)))
                    ((note "and the comparison is case-insensitive."
                       (char-not-equal char1 char2))
                     (fail))
                    (t
                     (read-char stream)
                     (read-char stream2))))))))))


;;;; Symbols

(defun external-symbol-p (symbol &optional (package (symbol-package symbol)))
  (and package
       (multiple-value-bind (symbol* status)
           (find-symbol (symbol-name symbol) package)
         (and (eq status :external)
              (eq symbol symbol*)))))

(defun external-symbol-in-any-package-p (symbol)
  (loop for package in (list-all-packages)
          thereis (external-symbol-p symbol package)))

(defun symbol-other-packages (symbol)
  (loop for package in (list-all-packages)
        when (and (external-symbol-p symbol package)
                  (not (eq package (symbol-package symbol))))
          collect package))


(defmacro with-debugger-hook (fn &body body)
  (with-gensyms (prev-debugger-hook condition this-hook)
    `(let* ((,prev-debugger-hook *debugger-hook*)
            (*debugger-hook* (lambda (,condition ,this-hook)
                               (declare (ignore ,this-hook))
                               (funcall ,fn ,condition)
                               (let ((*debugger-hook* ,prev-debugger-hook))
                                 (invoke-debugger ,condition)))))
       ,@body)))


;;;; Sequences

(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))


;;;; Trees

(defun find-if-in-tree (fn tree)
  (labels ((recurse (tree)
             (if (listp tree)
                 (dolist (subtree tree)
                   (recurse subtree))
                 (when (funcall fn tree)
                   (return-from find-if-in-tree tree)))))
    (recurse tree)))

(defun flatten (tree)
  (let ((r ()))
    (find-if-in-tree (lambda (leaf)
                       (push leaf r)
                       nil)
                     tree)
    (reverse r)))


;;;; Pathnames

(defun relativize-pathname (pathname reference-pathname)
  "Return a pathname that's equivalent to PATHNAME but relative to
  REFERENCE-PATHNAME if possible. Like ENOUGH-NAMESTRING, but inserts
  :UP components if necessary."
  (let ((pathname (merge-pathnames pathname *default-pathname-defaults*))
        (reference-pathname (merge-pathnames reference-pathname
                                             *default-pathname-defaults*)))
    (assert (equal (pathname-host pathname)
                   (pathname-host reference-pathname)))
    (assert (equal (pathname-device pathname)
                   (pathname-device reference-pathname)))
    (let* ((dir (remove :relative (pathname-directory pathname)))
           (ref-dir (remove :relative (pathname-directory reference-pathname)))
           (mismatch-index (or (mismatch dir ref-dir :test #'equal)
                               (length dir))))
      (normalize-pathname
       (make-pathname :directory (nconc (list :relative)
                                        (make-list (- (length ref-dir)
                                                      mismatch-index)
                                                   :initial-element :up)
                                        (subseq dir mismatch-index))
                      :defaults pathname)))))

(defun normalize-pathname (pathname)
  (if (equal '(:relative) (pathname-directory pathname))
      ;; Some implementations print (:RELATIVE) as "", some as "./",
      ;; no such troubles with the equivalent ().
      (make-pathname :directory () :defaults pathname)
      pathname))


;;;; Strings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *whitespace-chars*
    '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page)))

(defun whitespacep (char)
  (member char *whitespace-chars*))

(defun blankp (string-or-nil &key (start 0))
  (loop for i upfrom start below (length string-or-nil)
        always (whitespacep (aref string-or-nil i))))

(defun trim-whitespace (string-or-nil)
  (if string-or-nil
      (string-trim #.(format nil "~{~A~}" *whitespace-chars*) string-or-nil)
      nil))

(defun mixed-case-p (string)
  (and (some #'upper-case-p string)
       (some #'lower-case-p string)))

(defun prin1-to-string/case (object case)
  (let ((package *package*))
    (with-standard-io-syntax*
      (let ((*print-case* case)
            ;; Avoid mentions of BASE-CHAR and such.
            (*print-readably* nil)
            (*package* package))
        (prin1-to-string object)))))

;;; Add PREFIX to every line in STRING.
(defun prefix-lines (prefix string &key exclude-first-line-p)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (unless line
            (return))
          (if (and exclude-first-line-p (= i 0))
              (format out "~a" line)
              (format out "~a~a" prefix line))
          (unless missing-newline-p
            (terpri out)))))))

(defun shorten-string (string &key n-lines n-chars ellipsis)
  (let ((shortened string))
    (when n-lines
      (setq shortened (first-lines shortened n-lines)))
    (when (and n-chars (< n-chars (length shortened)))
      (setq shortened (subseq shortened 0 n-chars)))
    (if (and ellipsis (< (length shortened) (length string)))
        (concatenate 'string shortened ellipsis)
        shortened)))

(defun first-lines (string &optional (n-lines 1))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for i below n-lines do
        (multiple-value-bind (line missing-newline-p) (read-line in nil nil)
          (when line
            (if missing-newline-p
                (write-string line out)
                (write-line line out))))))))
