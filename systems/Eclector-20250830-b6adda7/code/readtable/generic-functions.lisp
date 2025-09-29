(cl:in-package #:eclector.readtable)

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric readtablep (object)
  (:method ((object t)) nil))

;;; This function makes a fresh copy of READTABLE and returns the
;;; fresh copy.  READTABLE must be a readtable.
(defgeneric copy-readtable (readtable))

;;; This function copies all the information from the readtable FROM-READTABLE
;;; to the readtable TO-READTABLE.  Finally, it returns TO-READTABLE.
;;; Both FROM-READTABLE and TO-READTABLE must be readtables.
(defgeneric copy-readtable-into (from-readtable to-readtable))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.  Notice that the readtable is the
;;; first parameter to this function.
(defgeneric make-dispatch-macro-character
    (readtable char &optional non-terminating-p))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric readtable-case (readtable))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric (setf readtable-case) (mode readtable))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric get-macro-character (readtable char))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric set-macro-character
    (readtable char function &optional non-terminating-p))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric get-dispatch-macro-character (readtable disp-char sub-char))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric set-dispatch-macro-character
    (readtable disp-char sub-char function))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric (setf syntax-from-char)
    (from-char to-char to-readtable from-readtable)
  (:method (from-char to-char to-readtable from-readtable)
    (setf (syntax-type to-readtable to-char)
          (syntax-type from-readtable from-char))
    (multiple-value-bind (macro-function non-terminating-p)
        (get-macro-character from-readtable from-char)
      (when (not (null macro-function))
        (set-macro-character
         to-readtable to-char macro-function non-terminating-p)))
    from-char))

(defun set-syntax-from-char (to-char from-char &optional
                                               (to-readtable *readtable*)
                                               (from-readtable *readtable*))
  (setf (syntax-from-char to-char to-readtable from-readtable) from-char)
  t)

;;; This function returns the syntax type of the character CHAR in
;;; READTABLE.  The syntax type is one of :WHITESPACE,
;;; :TERMINATING-MACRO, :NON-TERMINATING-MACRO, :CONSTITUENT,
;;; :SINGLE-ESCAPE, :MULTIPLE-ESCAPE, or :INVALID.
(defgeneric syntax-type (readtable char))

;;; This function sets the syntax type of the character CHAR in
;;; READTABLE to SYNTAX-TYPE.  It return SYNTAX-TYPE.
(defgeneric (setf syntax-type) (syntax-type readtable char))
