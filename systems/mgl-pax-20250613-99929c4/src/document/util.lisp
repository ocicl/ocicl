(in-package :mgl-pax)

(defun list-of-one-p (list)
  (and list (null (cdr list))))


(defun backslash-escape (string chars)
  (with-output-to-string (stream)
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (when (find char chars)
          (write-char #\\ stream))
        (write-char char stream)))))

(defun backslash-unescape (string)
  (if (find #\\ string)
      (read-from-string (format nil "\"~A\"" string))
      string))


;;;; Cached DREF:DEFINITIONS with all LOCATIVE-TYPES.

(defvar *definitions-cache*)

(defmacro with-definitions-cached (&body body)
  `(let ((*definitions-cache* (make-hash-table :test 'equal)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *local-dtype* '(or argument dislocated)))

(defparameter *non-local-dtype* `(not ,*local-dtype*))

(defun definitions* (name)
  (if (boundp '*definitions-cache*)
      (multiple-value-bind (drefs presentp) (gethash name *definitions-cache*)
        (unless presentp
          (setq drefs (definitions name :dtype *non-local-dtype*))
          (setf (gethash name *definitions-cache*) drefs))
        ;; @LOCAL-DEFINITIONs are not cacheable.
        (append drefs (definitions name :dtype *local-dtype*)))
      (definitions name :dtype 'top)))


;;;; Like the DREF function and DEFINITIONS*, but for references and
;;;; name given as strings, respectively.

;;; Parse "NAME LOCATIVE-TYPE" or "NAME (LOCATIVE-TYPE ...)", but do
;;; not intern stuff. Return:
;;;
;;; 1. the corresponding DREF if found,
;;  2. the locative if a valid one was found,
;;; 3. the invalid locative as a string or nil.
(defun parse-dref (string)
  (flet ((maybe-junk (start)
           (let ((locstring (trim-whitespace (subseq string start))))
             (if (zerop (length locstring))
                 nil
                 locstring))))
    (handler-case
        ;; Skip whatever NAME may be ...
        (let* ((name-end-pos (skip-sexp string))
               (raw (subseq string 0 name-end-pos)))
          ;; ... then just try to parse the locative.
          (multiple-value-bind (locative pos)
              (parse-locative (subseq string name-end-pos))
            (if locative
                (values (find-name (rcurry #'dref locative nil)
                                   (trim-whitespace raw))
                        locative
                        (maybe-junk (+ name-end-pos pos)))
                (values nil nil (maybe-junk name-end-pos)))))
      ((or reader-error end-of-file) ()
        nil))))

(defun parse-definitions* (string)
  (find-name #'definitions* (trim-whitespace string)))


;;;; Funny printing of @NAMEs

;;; Symbols in names (they can be lists) are printed almost as PRIN1
;;; would with *PACKAGE* were the CL package. Differences:
;;;
;;; - For symbols in other packages, a single #\: is printed even if
;;;   it is an internal symbol.
;;;
;;; - Package and symbol names are printed without the || syntax but
;;;   #\: and #\Space are escaped with backslashes.
(defun prin1-funny (name &optional (stream *standard-output*))
  (etypecase name
    (symbol
     (let* ((package (symbol-package name))
            (name (symbol-name name))
            (cl-package #.(find-package :common-lisp))
            (keyword-package #.(find-package :keyword)))
       (cond
         ((eq package cl-package)
          (prin1-funny* name stream))
         ((eq package keyword-package)
          (write-char #\: stream)
          (prin1-funny* name stream))
         (t
          (prin1-funny* (package-name package) stream)
          ;; Note the single : character.
          (write-char #\: stream)
          (prin1-funny* name stream)))))
    (string
     (prin1 name stream))
    (list
     (write-char #\( stream)
     (loop for el in name
           for firstp = t then nil
           do (unless firstp
                (write-char #\Space stream))
              (prin1-funny el stream))
     (write-char #\) stream))))

;;; Escape #\:, #\Space, #\(, #\), #\\ with a backslash.
(defun prin1-funny* (string &optional (stream *standard-output*))
  (loop for char across string
        do (when (member char '(#\: #\Space #\( #\) #\\))
             (write-char #\\ stream))
           (write-char char stream)))

;;; Like READ, but do not INTERN.
(defun read-funny (stream &optional (eof-error-p t) eof-value)
  (case (peek-char t stream eof-value eof-value)
    ((#\))
     (error "~@<Unpaired closing paren.~:@>"))
    ((#\()
     (assert nil () "FIXME: Unimplemented"))
    ((#\")
     (read stream eof-error-p eof-value))
    (t
     (let ((name-1 (read-funny* stream))
           (next-char (peek-char nil stream nil)))
       (cond ((eql next-char #\:)
              (read-char stream)
              (find-symbol (read-funny* stream)
                           (if (zerop (length name-1))
                               #.(find-package :keyword)
                               (find-package name-1))))
             (t
              (find-symbol name-1 #.(find-package :cl))))))))

(defun read-funny* (stream)
  (with-output-to-string (s)
    (loop for char = (read-char stream nil)
          while char
          do ;; These would be escaped if they were part of the name.
             (when (member char '(#\: #\Space #\( #\)))
               (unread-char char stream)
               (return))
             (when (eql char #\\)
               ;; EOF is invalid syntax.
               (setq char (read-char stream)))
             (write-char char s))))

(defun prin1-funny-to-string (name)
  (with-output-to-string (stream)
    (prin1-funny name stream)))

(defun prin1-funny*-to-string (string)
  (with-output-to-string (stream)
    (prin1-funny* string stream)))

(defun read-funny-from-string (string)
  (with-input-from-string (stream string)
    (values (read-funny stream)
            (file-position stream))))

(defun read-funny*-from-string (string)
  (with-input-from-string (stream string)
    (values (read-funny* stream)
            (file-position stream))))


;;;; Determine the ASDF system a definition belongs to.

(defvar *filename-to-asdf-system-name-map*)

;;; Amortize the cost of ASDF-SYSTEM-NAME-OF and ASDF-SYSTEM-NAME-OF*
;;; calls within the dynamic scope of BODY.
(defmacro with-filename-to-asdf-system-name-map (&body body)
  `(let ((*filename-to-asdf-system-name-map*
           (if (boundp '*filename-to-asdf-system-name-map*)
               *filename-to-asdf-system-name-map*
               (filename-to-asdf-system-name-map))))
     ,@body))

;;;; Return the ASDF:SYSTEM DREF is defined in or NIL.
(defun asdf-system-name-of (dref)
  (when-let (source-location (source-location dref))
    (when-let (file (source-location-file source-location))
      (asdf-system-name-of-filename file))))

;;; Like ASDF-SYSTEM-NAME-OF, but if DREF has no SOURCE-LOCATION then
;;; fall back to the source location of the home package of DREF-NAME
;;; (if it's a symbol).
(defun asdf-system-name-of* (dref)
  (if-let (file (file-of-dref dref))
    (asdf-system-name-of-filename file)
    (when (symbolp (dref-name dref))
      (when-let (package-dref (locate (symbol-package (dref-name dref))))
        (when-let (file (file-of-dref package-dref))
          (asdf-system-name-of-filename file))))))

(defun asdf-system-name-of-filename (filename)
  (let ((filename (namestring filename)))
    (if (boundp '*filename-to-asdf-system-name-map*)
        (gethash filename *filename-to-asdf-system-name-map*)
        (filename-to-asdf-system-name filename))))

(defmacro do-asdf-files ((system-name filename) &body body)
  (alexandria:with-unique-names (system component)
    `(dolist (,system-name (asdf:registered-systems))
       (let ((,system (dref::find-system* ,system-name)))
         (dolist (,component (asdf/component:sub-components ,system))
           (when (typep ,component 'asdf:cl-source-file)
             (when-let (,filename (namestring
                                   (slot-value
                                    ,component
                                    'asdf/component:absolute-pathname)))
               ,@body)))))))

(defun filename-to-asdf-system-name-map ()
  (let ((h (make-hash-table :test #'equal)))
    (do-asdf-files (system-name filename)
      (setf (gethash filename h) system-name))
    h))

(defun filename-to-asdf-system-name (filename)
  (let ((filename (namestring filename)))
    (do-asdf-files (system-name filename-1)
      ;; KLUDGE: Compare namestrings so that e.g. NIL vs :NEWEST in
      ;; PATHNAME-VERSION is hopefully not a diference.
      (when (equal filename-1 filename)
        (return-from filename-to-asdf-system-name system-name)))))

(defun file-of-dref (dref)
  (when-let (source-location (source-location dref))
    (source-location-file source-location)))
