(cl:in-package #:eclector.reader)

;;; As recommended by the HyperSpec, the backquote and comma reader
;;; macros generate a data structure that corresponds to the one used
;;; in Scheme, i.e. containing the forms (QUASIQUOTE <form>),
;;; (UNQUOTE <form>), and (UNQUOTE-SPLICING <form>).
;;;
;;; These forms are converted to standard Common Lisp forms
;;; containing APPEND, LIST, QUOTE, APPLY, and VECTOR.
;;;
;;; Our implementation is an almost immediate translation of section
;;; 2.4.6 of the Hyperspec.  Where the HyperSpec uses brackets [FORM],
;;; we use (TRANSFORM FORM).  The other difference is that the
;;; HyperSpec converts a form such as `(x1 x2 ... xn . atom) directly
;;; to (append [x1] [x2] ... [xn] (quote atom)), whereas we do it
;;; indirectly by calling TRANSFORM-COMPOUND on the (potentially
;;; dottet) list.

(defun transform (form &optional (wrap-in-list t))
  (flet ((maybe-wrap (thing)
           (if wrap-in-list
               `(list ,thing)
               thing)))
    (typecase form
      ((cons (eql unquote))
       (maybe-wrap (second form)))
      ((cons (eql unquote-splicing))
       (second form))
      (t
       (maybe-wrap (transform-quasiquote-argument form))))))

(defun transform-compound (compound)
  (labels ((rec (object)
             (typecase object
               ((cons t (or (not cons) (cons (eql unquote))))
                (list (transform (car object)) (transform (cdr object) nil)))
               ((cons t (cons (eql unquote-splicing)))
                (error 'unquote-splicing-in-dotted-list
                       :argument (second object)))
               (t
                (list* (transform (car object)) (rec (cdr object)))))))
    (rec compound)))

(defun transform-quasiquote-argument (argument)
  (typecase argument
    ((cons (eql unquote))
     (second argument))
    ((cons (eql unquote-splicing))
     (error 'unquote-splicing-at-top :argument (second argument)))
    (cons
     `(append ,@(transform-compound argument)))
    ((and vector (not string))
     `(apply #'vector
             ,(transform-quasiquote-argument
               (coerce argument 'list))))
    (t
     `(quote ,argument))))

(defun expand (form)
  (if (atom form)
      form
      (let ((expanded (cons (expand (car form)) (expand (cdr form)))))
        (if (eq (first expanded) 'quasiquote)
            (transform-quasiquote-argument (second expanded))
            expanded))))

(defmacro quasiquote (&whole form argument)
  (declare (ignore argument))
  (expand form))

(defmacro unquote (argument)
  (error 'unquote-not-inside-backquote-during-macroexpansion
         :splicing-p nil :argument argument))

(defmacro unquote-splicing (argument)
  (error 'unquote-not-inside-backquote-during-macroexpansion
         :splicing-p t :argument argument))
