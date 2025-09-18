(uiop:define-package #:40ants-doc-full/utils
  (:use #:cl)
  (:import-from #:40ants-doc-full/builder/vars)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:import-from #:str
                #:ensure-suffix
                #:replace-all)
  (:import-from #:40ants-doc/docstring
                #:whitespacep
                #:blankp)
  (:export
   #:is-external
   #:get-package-from-symbol-name
   #:parse-symbol-name
   #:get-symbol-from-string
   #:make-relative-path))
(in-package #:40ants-doc-full/utils)


(defun symbol-global-value (symbol)
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #-(or sbcl allegro)
  (ignore-errors (symbol-value symbol)))


(defun subseq* (seq start)
  (subseq seq (min (length seq) start)))


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
    (let* ((dir (pathname-directory pathname))
           (ref-dir (pathname-directory reference-pathname))
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

;;;; Stream specs

(defgeneric make-stream-spec (object &rest args))

(defgeneric unmake-stream-spec (stream-spec))

(defgeneric call-with-open-stream-spec (stream-spec direction fn))

(defgeneric delete-stream-spec (stream-spec))

(defmacro with-open-stream-spec ((stream stream-spec &key (direction :input))
                                 &body body)
  `(call-with-open-stream-spec ,stream-spec ,direction
                               (lambda (,stream) ,@body)))

;;;; STRING-STREAM-SPEC

(defclass string-stream-spec ()
  ((string :initform ""
           :initarg :string
           :accessor string-stream-spec-string)))


(defmethod print-object ((obj string-stream-spec) stream)
  (print-unreadable-object (obj stream :type t)))

(defmethod make-stream-spec ((object null) &rest args)
  (assert (endp args))
  (make-instance 'string-stream-spec))

(defmethod unmake-stream-spec ((spec string-stream-spec))
  (string-stream-spec-string spec))

(defmethod call-with-open-stream-spec ((spec string-stream-spec)
                                       (direction (eql :input)) fn)
  (funcall fn (make-string-input-stream (string-stream-spec-string spec))))

(defmethod call-with-open-stream-spec ((spec string-stream-spec)
                                       (direction (eql :output)) fn)
  (let ((output-stream (make-string-output-stream)))
    (unwind-protect
         (funcall fn output-stream)
      (setf (string-stream-spec-string spec)
            (concatenate 'string (string-stream-spec-string spec)
                         (get-output-stream-string output-stream))))))

(defmethod delete-stream-spec ((spec string-stream-spec))
  (setf (string-stream-spec-string spec) ""))

;;;; FILE-STREAM-SPEC

(defclass file-stream-spec ()
  ((pathname :initarg :pathname
             :reader file-stream-spec-pathname)
   (open-args :initform () :initarg :open-args
              :reader file-stream-spec-open-args)))

(defmethod print-object ((spec file-stream-spec) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream "~S" (file-stream-spec-pathname spec))))

(defmethod make-stream-spec ((object string) &rest args)
  (make-instance 'file-stream-spec :pathname object
                 ;; Copy ARGS, because we'll call REMF on it.
                 :open-args (copy-list args)))

(defmethod make-stream-spec ((object pathname) &rest args)
  (make-instance 'file-stream-spec :pathname object
                 ;; Copy ARGS, because we'll call REMF on it.
                 :open-args (copy-list args)))

(defmethod unmake-stream-spec ((spec file-stream-spec))
  (file-stream-spec-pathname spec))

(defmethod call-with-open-stream-spec ((spec file-stream-spec) direction fn)
  (let ((open-args (file-stream-spec-open-args spec))
        (pathname (file-stream-spec-pathname spec)))
    (when (getf open-args :ensure-directories-exist)
      (ensure-directories-exist pathname))
    (remf open-args :ensure-directories-exist)
    (unwind-protect
         (with-open-stream (stream (apply #'open pathname
                                          :direction direction
                                          open-args))
           (funcall fn stream))
      ;; Subsequent opens must append.
      (loop while (remf (slot-value spec 'open-args) :if-exists))
      (setf (slot-value spec 'open-args)
            (append (list :if-exists :append) (slot-value spec 'open-args))))))

(defmethod delete-stream-spec ((spec file-stream-spec))
  (delete-file (file-stream-spec-pathname spec)))

;;;; STREAM-STREAM-SPEC

(defmethod make-stream-spec ((stream stream) &rest args)
  (assert (endp args))
  stream)

(defmethod unmake-stream-spec ((stream stream))
  stream)

(defmethod call-with-open-stream-spec ((stream stream) direction fn)
  (ecase direction
    ((:input) (assert (input-stream-p stream)))
    ((:output) (assert (output-stream-p stream))))
  (funcall fn stream))

;;;; T

(defmethod make-stream-spec ((spec (eql t)) &rest args)
  (assert (endp args))
  *standard-output*)

;;;; Hotpatching

#+allegro
(progn
  swank-backend::
  (unless (get 'function-name 'implementation)
    (defimplementation function-name (f)
      (check-type f function)
      (cross-reference::object-to-function-name f))))

#+allegro
(progn
  swank-backend::
  (unless (get 'find-source-location 'implementation)
    (defimplementation find-source-location (obj)
      (first (rest (first (fspec-definition-locations obj)))))))


;;;; String utilities

;; TODO: probably replace it with strip-docstring-indentation?
(defun strip-longest-common-prefix (string chars &key (first-line-special-p t))
  (let ((prefix (longest-common-prefix
                 string chars :first-line-special-p first-line-special-p)))
    (values
     (with-output-to-string (output)
       (with-input-from-string (s string)
         (loop for i upfrom 0
               for line = (read-line s nil nil)
               while line
               do (if (and first-line-special-p (zerop i))
                      (write-line line output)
                      (write-line (subseq line (length prefix)) output)))))
     prefix)))

;;; Return the longest common prefix of lines of STRING, where the
;;; prefix is made of CHARS.
(defun longest-common-prefix (string chars &key (first-line-special-p t))
  (let ((longest-prefix nil))
    (with-input-from-string (s string)
      (loop for i upfrom 0
            for line = (read-line s nil nil)
            while line
            do (when (or (not first-line-special-p) (plusp i))
                 (let ((prefix (matching-prefix line chars)))
                   (setq longest-prefix
                         (if longest-prefix
                             (subseq longest-prefix
                                     0 (or (mismatch longest-prefix prefix)
                                           (length longest-prefix)))
                             prefix))))))
    longest-prefix))

(defun matching-prefix (string chars)
  (let ((position (position-if-not (lambda (char)
                                     (find char chars))
                                   string)))
    (if position
        (subseq string 0 position)
        string)))

;;; Read as many consucutive lines starting with PREFIX from STREAM as
;;; possible. From each mathing line, strip the prefix and join them
;;; into a non-prefixed string conserving the newlines. As the second
;;; value, return the number of lines read.
;;;
;;; As the third value, return the first non-matching line (without
;;; the newline) or NIL at eof. The fourth value is whether the first
;;; non-matching line returned as the thrid value had a missing
;;; newline. The fifth value is file position of the start of the line
;;; returned as the third value.
;;;
;;; Note that reading (with prefix "..")
;;;
;;;     .. 1
;;;     .. 2
;;;
;;; gives "1~%2". If you want to end with a newline, then:
;;;
;;;     .. 1
;;;     .. 2
;;;     ..
(defun read-prefixed-lines (stream prefix &key (first-line-prefix prefix)
                            (eat-one-space-p t))
  (with-output-to-string (output)
    (loop for n-lines-read upfrom 0 do
      (multiple-value-bind (line missing-newline-p file-position)
          (read-line* stream nil nil)
        (let ((prefix (if (zerop n-lines-read) first-line-prefix prefix)))
          (when (or (null line)
                    (not (alexandria:starts-with-subseq prefix line)))
            (return-from read-prefixed-lines
              (values (get-output-stream-string output) n-lines-read
                      line missing-newline-p file-position)))
          (unless (zerop n-lines-read)
            (terpri output))
          (let ((line (subseq line (length prefix))))
            (format output "~A" (if (and eat-one-space-p
                                         (plusp (length line))
                                         (char= (aref line 0) #\Space))
                                    (subseq line 1)
                                    line))))))))

(defun read-line* (stream &optional (eof-error-p t) eof-value)
  (let ((file-position (file-position stream)))
    (multiple-value-bind (line missing-newline-p)
        (read-line stream eof-error-p eof-value)
      (values line missing-newline-p file-position))))

;;; The inverse of READ-PREFIXED-LINES. If ADD-ONE-SPACE-P, a space
;;; character is printed after the prefix if the line is zero length.
(defun write-prefixed-lines (string prefix stream &key (add-one-space-p t)
                             (first-line-prefix prefix))
  (let ((last-newline-missing-p nil))
    (with-input-from-string (s string)
      (loop for n-lines-read upfrom 0 do
        (multiple-value-bind (line missing-newline-p) (read-line s nil nil)
          (unless line
            (return))
          (setq last-newline-missing-p missing-newline-p)
          (if (zerop (length line))
              (write-line prefix stream)
              (format stream "~A~A~A~%"
                      (if (zerop n-lines-read) first-line-prefix prefix)
                      (if add-one-space-p " " "")
                      line)))))
    (unless last-newline-missing-p
      (write-line prefix stream))))


;;;; Escaping of HTML ID and NAME

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun _mark-range (array start end)
    (loop for a from (char-code start) to (char-code end) do
      (setf (sbit array a) 1)))

  (defun _mark-one (array ch)
    (setf (sbit array (char-code ch)) 1)))

(defparameter +first-name-characters+ 
  (let ((array (make-array 255 :element-type 'bit :initial-element 0)))
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    array))

(defparameter +name-characters+ 
  (let ((array (copy-seq +first-name-characters+)))
    (_mark-range array #\0 #\9)
    (_mark-one array #\-)
    ;; Encode these as well to work around github markdown bug which
    ;; would otherwise break links.
    #+nil (_mark-one array #\_)
    #+nil (_mark-one array #\.)
    #+nil (_mark-one array #\:)
    array))

(defun html-safe-name (name)
  ;; Copied from HTML-Encode
  ;;?? this is very consy
  ;;?? crappy name
  (declare (type simple-string name))
  (let ((output (make-array (truncate (length name) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0))
	(first? t))
    (with-output-to-string (out output)
      (loop for char across name
            for code = (char-code char)
            for valid = +first-name-characters+ then +name-characters+
            do (cond ((and (< code 255)
                           (= (sbit valid code) 1))
                      (write-char char out))
                     (t
                      ;; See http://www.w3.org/TR/html4/types.html#h-6.2
                      ;; ID and NAME tokens must begin with a letter ([A-Za-z]) 
                      ;; and may be followed by any number of letters, 
                      ;; digits ([0-9]), hyphens ("-"), underscores ("_"), 
                      ;; colons (":"), and periods (".").
                      (when first?
                        (write-char #\x out)) 
                      (format out "-~:@(~16r~)" code)))
               (setf first? nil)))
    (coerce output 'simple-string)))


;;;; Text based HTML fragments

(defun anchor (anchor stream)
  (format stream "<a id='~A'></a>~%~%" (html-safe-name anchor)))


;;;; Text based markdown fragments

(defun heading (level stream)
  (loop repeat (1+ level) do (write-char #\# stream)))

(defun code (string)
  (if (zerop (length string))
      ""
      (format nil "`~A`" string)))

(defun markdown-special-char-p (char)
  (member char '(#\* #\_ #\` #\< #\> #\[ #\])))

(defun prin1-and-escape-markdown (object)
  (escape-markdown (prin1-to-string object)))

(defun escape-markdown (string)
  (with-output-to-string (stream)
    (dotimes (i (length string))
      (let ((char (aref string i)))
        (when (markdown-special-char-p char)
          (write-char #\\ stream))
        (write-char char stream)))))

(defun bold (string stream)
  (if (zerop (length string))
      ""
      (format stream "**~A**" string)))

(defun italic (string stream)
  (if (zerop (length string))
      ""
      (format stream "*~A*" string)))



;;;; Parse tree based markdown fragments

(defun code-fragment (name)
  `(:code ,(princ-to-string name)))


;;;; Markdown parse tree transformation

;;; Perform a depth first traversal of TREE. Call FN with the parent
;;; of each node and the node itself. FN returns three values: a new
;;; tree to be substituted for the node, a recurse and slice flag. If
;;; slice, then the new tree is sliced into parent. If recurse (and
;;; the new tree is not a leaf), then traversal goes recurses into the
;;; new tree.
(defun transform-tree (fn tree)
  (labels ((process (parent tree)
             (multiple-value-bind (new-tree recurse slice)
                 (funcall fn parent tree)
               (assert (or (not slice) (listp new-tree)))
               (if (or (atom new-tree)
                       (not recurse))
                   (values new-tree slice)
                   (values (loop for sub-tree in new-tree
                                 append (multiple-value-bind
                                              (new-sub-tree slice)
                                            (process new-tree sub-tree)
                                          (if slice
                                              new-sub-tree
                                              (list new-sub-tree))))
                           slice)))))
    (process nil tree)))

(defun defer-tag-handling (tags stop-tags handle-strings fn parent tree)
  (cond ((or (and (listp tree)
                  (member (first tree) tags))
             (and handle-strings
                  (stringp tree)))
         (funcall fn parent tree))
        ((and (listp tree)
              (member (first tree) stop-tags))
         (values tree nil nil))
        (t
         (values tree (and tree (listp tree)) nil))))

(defun join-consecutive-non-blank-strings-in-parse-tree (parse-tree)
  (transform-tree
   (lambda (parent tree)
     (declare (ignore parent))
     (if (listp tree)
         (values (join-consecutive-non-blank-strings-in-list tree) t nil)
         tree))
   parse-tree))

(defun join-consecutive-non-blank-strings-in-list (list)
  (let ((result ()))
    (dolist (element list)
      (if (and (stringp element)
               (stringp (first result))
               (not (blankp element))
               (not (blankp (first result))))
          (setf (first result)
                (concatenate 'string (first result) element))
          (push element result)))
    (reverse result)))


(defun no-lowercase-chars-p (string &key min-length)
  "If min-length is given, then string should contain at least this number of uppercase chars."
  (let ((string
          ;; Allows plurals as in "FRAMEs" and "FRAMEs."
          (string-right-trim 40ants-doc-full/builder/vars::*find-definitions-right-trim-2* string)))
    (and (notany (lambda (char)
                   (char/= char (char-upcase char)))
                 string)
         (or (null min-length)
             (>= (length string)
                 min-length)))))



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


(defun delimiterp (char)
  (or (whitespacep char)
      (find char "()'`\"#<")))


;;; Call FN with STRING and START, END indices. FN returns three
;;; values: a replacement parse tree fragment (or NIL, if the subseq
;;; shall not be replaced), whether the replacement shall be sliced
;;; into the result list, and the number of characters replaced (may
;;; be less than (- END START). MAP-NAMES returns a parse tree
;;; fragment that's a list of non-replaced parts of STRING and
;;; replacements (maybe sliced). Consecutive strings are concatenated.
(defun map-names (string fn)
  (let ((translated ())
        (i 0)
        (n (length string)))
    (flet ((add (a)
             (if (and (stringp a)

                      (stringp (first translated)))
                 (setf (first translated)
                       (concatenate 'string (first translated) a))
                 (push a translated ))))
      (loop while (< i n)
            for prev = nil then char
            for char = (aref string i)
            do (let ((replacement nil)
                     (n-chars-replaced nil)
                     (slice nil))
                 (when (and (not (delimiterp char))
                            (or (null prev) (delimiterp prev)))
                   (let ((end (or (position-if #'delimiterp string :start i)
                                  (length string))))
                     (multiple-value-setq (replacement slice n-chars-replaced)
                       (funcall fn string i end))
                     (when replacement
                       (if slice
                           (dolist (a replacement)
                             (add a))
                           (add replacement))
                       (if n-chars-replaced
                           (incf i n-chars-replaced)
                           (setq i end)))))
                 (unless replacement
                   (add (string char))
                   (incf i)))))
    (reverse translated)))


(defun external-dependencies (system-name) 
  (let ((primary-name (asdf:primary-system-name system-name))
        (processed nil))
    (labels ((rec (system-name &optional collected)
               (cond
                 ((member system-name processed
                          :test #'string-equal)
                  collected)
                 (t
                  (push system-name processed)
                  ;; (format t "Processing ~S system~%" system-name)
                  
                  (let* ((system (asdf:registered-system system-name))
                         (dependencies (asdf/system:system-depends-on system)))
                    (loop for dep in dependencies
                          for dep-primary = (asdf:primary-system-name dep)
                          unless (or (string-equal primary-name dep-primary)
                                     (member dep collected
                                             :test #'string-equal))
                          collect dep into new-deps
                          finally (setf collected
                                        (append new-deps
                                                collected)))
                    (loop for dep in dependencies
                          do (setf collected
                                   (rec dep collected)))
                    collected)))))
      (sort (rec system-name)
            #'string<))))


;; (defun file-package (filename)
;;   "Searches for (in-package ...) form and returns referred package object."
;;   (when (probe-file filename)
;;     (uiop:with-safe-io-syntax (:package :cl)
;;       (let ((forms (uiop:read-file-forms filename)))
;;         (loop for form in forms
;;               when (and (consp form)
;;                         (eql (first form)
;;                              'in-package))
;;               do (return (find-package
;;                           (second form))))))))

(defun file-package (filename)
  "Searches for (in-package ...) form and returns referred package object."
  (when (probe-file filename)
    (uiop:with-safe-io-syntax (:package :cl)
      ;; Here we'll read form one by one because UIOP:READ-FILE-FORMS
      ;; may be broken on files with PYTHONIC-STRING-SYNTAX because
      ;; it does not respect readtable changes.
      (loop for idx upfrom 0
            for form = (ignore-errors
                        (uiop:read-file-form filename :at idx))
            when (and (consp form)
                      (eql (first form)
                           'in-package))
            do (return (find-package
                        (second form)))))))


(defun symbol-name-p (string)
  "Checks if given string looks like a symbol's name.

   String should include uppercased characters, digits,
   a minus sign. Also, symbol name could be package-qualified.
 "
  (cl-ppcre:scan
   "^(?:[\\p{UppercaseLetter}\\/0-9-]+[:]{1,2})?[*+]?\\p{UppercaseLetter}[\\p{UppercaseLetter}0-9-]+[*+]?$"
   string))


(defun is-external (symbol &optional (package (symbol-package symbol)))
  "Checks if package is external in a package where it is defined."
  (when package
    (eql (nth-value 1
                    (find-symbol (symbol-name symbol)
                                 package))
         :external)))


(defun get-package-from-symbol-name (symbol-name)
  "Returns a package if symbol-name is package qualified."
  (when (find #\: symbol-name)
    (let ((package-name (first (str:split #\: symbol-name))))
      (find-package package-name))))


(defun parse-symbol-name (symbol-name)
  "Returns a symbol name and package object as a second value. Package might be nil if it is not found or not specified."
  (let ((delimiter-position (position #\: symbol-name)))
    (cond
      ;; No package specified
      ((null delimiter-position)
       (values symbol-name nil))
      
      ((zerop delimiter-position)
       (values (subseq symbol-name 1)
               (find-package "KEYWORD")))
      (t
       (let* ((splitted (str:split #\: symbol-name))
              (package-name (first splitted))
              (symbol-name (car (last splitted))))
         (values symbol-name
                 (find-package package-name)))))))


(defun get-symbol-from-string (symbol-name)
  (multiple-value-bind (symbol-name package)
      (parse-symbol-name symbol-name)
  
    (let ((package (or package
                       *package*)))
      (do-symbols (symbol package)
        (when (string= (symbol-name symbol)
                       symbol-name)
          (return-from get-symbol-from-string
            symbol))))))


(defun make-relative-path (from to)
  (check-type from string)
  (check-type to string)

  (let* ((from-parts (str:split "/" from))
         (to-parts (str:split "/" to)))
    (with-output-to-string (s)
      (loop for (from-part . rest-from-parts) on from-parts
            for (to-part . rest-to-parts) on to-parts
            while (string= from-part
                           to-part)
            finally
               (cond
                 ((and (null rest-from-parts)
                       (null rest-to-parts)
                       (equal from-part
                              to-part))
                  "")
                 (t
                  (loop repeat (length rest-from-parts)
                        do (write-string "../" s))
                  (push to-part rest-to-parts)
                  (format s "~{~A~^/~}" rest-to-parts)))))))


(defun make-clean-uri (path)
  (ensure-suffix "/"
                 (replace-all "/index.html"
                              "/"
                              path)))


(defgeneric maybe-downcase (obj)
  (:method ((string string))
    (if (and 40ants-doc-full/builder/vars::*downcase-uppercase-code*
             (no-lowercase-chars-p string))
        (string-downcase string)
        string))
  (:method ((list list))
    (mapcar #'maybe-downcase list))
  (:method ((obj (eql nil)))
    nil)
  (:method ((symbol symbol))
    (maybe-downcase (symbol-name symbol))))


(defun url-join (first rest)
  "Concatenates two pieces of URL while adding / to the end of the FIRST if necessary."
  (concatenate 'string
               first
               (unless (str:ends-with-p "/" first)
                 "/")
               rest))



(defun call-with-temp-package (thunk &key (use (list :cl)))
  (let* ((temp-package (make-package (format nil "tmp-package-~A"
                                             (get-internal-real-time))
                                     ;; Without this one, ASDF might
                                     ;; not read some files correctly
                                     :use use))
         (*package* temp-package))
    (unwind-protect (funcall thunk)
      (delete-package temp-package))))


(defmacro with-temp-package ((&key (use (list :cl))) &body body)
  `(flet ((thunk-with-temp-package ()
            ,@body))
     (declare (dynamic-extent (function thunk-with-temp-package)))
     (call-with-temp-package #'thunk-with-temp-package
                             :use ',use)))
