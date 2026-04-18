(uiop:define-package #:40ants-doc/ignored-words
  (:use #:cl)
  (:export
   #:ignored-words
   #:supports-ignored-words-p
   #:ignore-words-in-package
   #:ignore-in-package))
(in-package #:40ants-doc/ignored-words)


(defvar *package-ignore-words*
  (make-hash-table))


(defgeneric supports-ignored-words-p (obj)
  (:documentation "Should return `T` if objects implements a method for IGNORED-WORDS generic-function.")
  (:method ((obj t))
    nil)
  (:method ((obj package))
    (gethash obj *package-ignore-words*)))


(defgeneric ignored-words (obj)
  (:documentation "Returns a list of words or symbols to ignore in OBJ's documentation.")
  (:method ((obj package))
    (gethash obj *package-ignore-words*)))


(defun ignore-in-package (symbol-or-string &key (package *package*))
  "Adds SYMBOL-OR-STRING to the list of symbols ignored in the given PACKAGE."
  (pushnew symbol-or-string
           (gethash package *package-ignore-words*)
           :test #'equal))


(defmacro ignore-words-in-package (&rest symbols-or-strings)
  "Adds given symbols or string to ignore list bound to the current package.

   You will not be warned when one of these symbols is not documented
   or documented and not exported from the package."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash *package* *package-ignore-words*)
           (union
            (gethash *package* *package-ignore-words*)
            (list ,@symbols-or-strings)
            :test 'equal))))


(defun ignored-in-package (symbol-or-string package)
  "Checks if given symbol or string is ignored in a package."
  (member symbol-or-string
          (gethash package *package-ignore-words*)
          :test #'equal))

