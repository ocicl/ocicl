
(defpackage :pythonic-string-reader
  (:use :cl :named-readtables)
  (:export
   #:enable-pythonic-string-syntax
   #:pythonic-string-syntax
   #:disable-pythonic-string-syntax))

;; Modified by code by Yury Sulsky

;; The original code stated that this was for reading multi-line strings.  This
;; isn't at all an issue in Common Lisp.  What this really does is allow us to
;; write strings without the need to escape special characters.

;; I am providing two modes of operation.  The first is to use three consecutive
;; quotation marks to mark the beginning and ending of your string.  Since there
;; is an odd number of quotes on each side of the string, this will naturally be
;; interpretted by your editor as a string.  The second mode is to use four
;; consecutive quotes to mark your string.  In this case, the editor will see an
;; even number of quotes before and after, hinting that it should treat it as
;; Lisp code.  This is particularly useful if you are writing Lisp code in a
;; string but would like to use the editors development tools while you do it.
;; This might seem like a rare case, but I find it comes up often enough and it
;; doesn't do any harm.

;; Also, as there are two syntactic markers for forming pythonic style strings,
;; escape free strings, we now have the ability to include a pythonic string
;; within our pythonic string.  This is a useful feature if you ever want to
;; read the string you are defining.

(in-package :pythonic-string-reader)

(let ((normal-string-reader
        ;; Grab the original reader from the original readtable (is this the
        ;; right thing to do?  Seems like the most sane thing.)
        (get-macro-character #\" (copy-readtable nil))))
  (defun read-multiline-string (stream c)
    (let ((buffer ()))
      (when (not (char= #\" (peek-char nil stream)))
        (return-from read-multiline-string
          (funcall normal-string-reader stream c)))
      (read-char stream)

      (when (not (char= #\" (peek-char nil stream)))
        (return-from read-multiline-string ""))
      (read-char stream)

      ;; Eat one last quote if given.  This means that we can use three or
      ;; four consequtive double quotes to designate a multi-line string
      (let ((four-quote-string
              (if (char= #\" (peek-char nil stream))
                  (progn (read-char stream) t)
                  nil)))
        (do ((chars (if four-quote-string
                        (list (read-char stream)
                              (read-char stream)
                              (read-char stream)
                              (read-char stream))
                        (list (read-char stream)
                              (read-char stream)
                              (read-char stream)))
                    (cdr (nconc chars (list (read-char stream))))))
            ((every #'(lambda (c) (eq c #\")) chars)
             (coerce (nreverse buffer) 'string))
          (push (car chars) buffer))))))

;; add python-style multi-line strings except also except four quote
;; delimitation so indentation works properly
(defreadtable pythonic-string-syntax
  (:merge :standard)
  (:macro-char #\" #'read-multiline-string t))

(defun enable-pythonic-string-syntax ()
  (set-macro-character #\" #'read-multiline-string))

(defun disable-pythonic-string-syntax ()
  (set-macro-character #\" (get-macro-character #\" (copy-readtable nil))))
