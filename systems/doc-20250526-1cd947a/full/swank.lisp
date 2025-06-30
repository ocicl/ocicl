(uiop:define-package #:40ants-doc-full/swank
  (:use #:cl)
  (:import-from #:swank)
  (:import-from #:alexandria)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc-full/utils)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc/docstring
                #:trim-whitespace)
  (:export
   #:locate-definition-for-emacs))
(in-package #:40ants-doc-full/swank)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defun read-marked-up-locative-from-string (string)
  (let ((*read-eval* nil)
        (string (if (or (alexandria:starts-with #\` string)
                        (alexandria:starts-with #\' string))
                    (subseq string 1)
                    string)))
    (read-locative-from-string string)))

;;; Return one source location for the thing that can be located with
;;; NAME (a string) and LOCATIVE-STRING. Called from the elisp
;;; function slime-locate-definition. It's like LOCATE but takes
;;; string arguments and returns a location suitable for
;;; make-slime-xref.
(defun locate-definition-for-emacs (name locative-string)
  (let ((locative-string (trim-whitespace locative-string)))
    (swank-backend::converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (or
         ;; SECTION class and class SECTION
         ;; SECTION `class` and `class` SECTION
         ;; `SECTION` class and class `SECTION`
         ;; `SECTION` `class` and `class` `SECTION`
         (ignore-errors
          (locate-definition-for-emacs-1 name locative-string))
         ;; [SECTION][(class)] gets here as NAME="[SECTION][",
         ;; LOCATIVE-STRING="(class)".
         (ignore-errors
          (locate-definition-for-emacs-1 (string-trim "[]" name)
                                         locative-string))
         ;; [SECTION][class] gets here as NAME="[SECTION][class]",
         ;; LOCATIVE-STRING=garbage.
         (ignore-errors
          (locate-reference-link-definition-for-emacs name))
         ;; [DEFSECTION][]
         (let* ((swank:*find-definitions-left-trim* "[#:<")
                (swank:*find-definitions-right-trim* "][,:.>sS")
                (locations (swank:find-definitions-for-emacs name)))
           (if (= (length locations) 1)
               (first (rest (first locations)))
               nil)))))))

;;; Handle references with quoted or non-quoted symbols and locatives.
;;; Since SECTION is both a class and and a documented symbol it
;;; serves as a good example.
(defun locate-definition-for-emacs-1 (name locative-string)
  (multiple-value-bind (symbol found)
      (swank::find-definitions-find-symbol-or-package name)
    (when found
      (let ((locative (read-marked-up-locative-from-string locative-string)))
        (when locative
          (let ((thing (uiop:symbol-call :40ants-doc/locatives/base :locate symbol locative :errorp nil)))
            (when thing
              (40ants-doc/source-api:find-source thing))))))))

;;; Ensure that some Swank internal facilities (such as
;;; SWANK::FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE,
;;; SWANK::WITH-BUFFER-SYNTAX, SWANK::PARSE-SYMBOL) are operational
;;; even when not running under Slime.
(defmacro with-swank (() &body body)
  `(let* ((swank::*buffer-package* (if (boundp 'swank::*buffer-package*)
                                       swank::*buffer-package*
                                       *package*))
          (swank::*buffer-readtable*
            (if (boundp 'swank::*buffer-readtable*)
                swank::*buffer-readtable*
                (swank::guess-buffer-readtable swank::*buffer-package*))))
     ,@body))



;;; Like READ-FROM-STRING, but try to avoid interning symbols.
(defun read-locative-from-string (string &key (package *package*))
  (let ((swank::*buffer-package* package))
    (multiple-value-bind (symbol found)
        (with-swank ()
          (swank::find-definitions-find-symbol-or-package string))
      (if found
          symbol
          (let ((first-char-pos (position-if-not #'40ants-doc/docstring::whitespacep string)))
            (when (and first-char-pos
                       (char= #\())
              ;; Looks like a list. The first element must be an
              ;; interned symbol naming a locative.
              (let ((delimiter-pos (position-if #'40ants-doc-full/utils::delimiterp string
                                                :start (1+ first-char-pos))))
                (multiple-value-bind (symbol found)
                    (swank::parse-symbol
                     (subseq string (1+ first-char-pos) delimiter-pos))
                  (declare (ignore symbol))
                  
                  (when found
                    ;; The rest of the symbols in the string need not be
                    ;; already interned, so let's just read it.
                    (ignore-errors
                     (let* ((*read-eval* t)
                            (result (read-from-string string)))
                       ;; Some string may be read as cons,
                       ;; for example, reading "'foo"
                       ;; will result in (cons 'QUOTE 'FOO)
                       ;; but we only want this function to return symbols.
                       result)))))))))))

(defun locate-reference-link-definition-for-emacs (string)
  (when (and (= 2 (count #\[ string))
             (= 2 (count #\] string)))
    (let ((first-open (position #\[ string))
          (first-close (position #\] string))
          (second-open (position #\[ string :from-end t))
          (second-close (position #\] string :from-end t)))
      (when (< first-open first-close second-open second-close)
        (locate-definition-for-emacs-1
         (string-trim "`" (subseq string (1+ first-open) first-close))
         (subseq string (1+ second-open) second-close))))))

