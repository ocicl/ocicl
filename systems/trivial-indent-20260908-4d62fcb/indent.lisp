(defpackage #:trivial-indent
  (:use #:cl)
  (:nicknames #:org.tymoonnext.radiance.lib.trivial-indent #:indent)
  (:export
   #:indentation
   #:list-indentation-rules
   #:define-indentation
   #:remove-indentation
   #:initialize-slime
   #:initialize-sly))

(in-package #:org.tymoonnext.radiance.lib.trivial-indent)

(defvar *indentation-hints* (make-hash-table :test #'eq))
(defvar *foreign-indentation-hints* (make-hash-table :test #'equal))

(defmacro with-symbol ((name symbol package module) &body body)
  "Check if SYMBOL exists in PACKAGE when MODULE is loaded and bind it to NAME."
  (let ((pkg (gensym)))
    `(when (member ,module *modules* :test #'string=)
       (let* ((,pkg (find-package ,package))
              (,name (when ,pkg (find-symbol (string ,symbol) ,pkg))))
         (when ,name
           ,@body)))))

(defun list-indentation-rules ()
  "Returns an ALIST of NAME and RULE.

The NAME can either be a CL:SYMBOL, or a cons of PACKAGE-NAME
and SYMBOL-NAME strings, denoting a \"foreign\" symbol that is not
interned in the current implementation system.

See INDENTATION"
  (let ((rules ()))
    (loop for package-name being the hash-keys of *foreign-indentation-hints*
          using (hash-value table)
          for package = (find-package package-name)
          do (loop for symbol-name being the hash-keys of table
                   using (hash-value rule)
                   for symbol = (when package (find-symbol symbol-name package))
                   do (push (list (or symbol (cons package-name symbol-name)) rule) rules)))
    rules))

(defun indentation (name)
  "Returns the custom defined indentation of a name if there is any.

SETF-able.

The NAME can either be a CL:SYMBOL, or a cons of PACKAGE-NAME
and SYMBOL-NAME strings, denoting a \"foreign\" symbol that is not
interned in the current implementation system."
  (etypecase name
    (symbol
     (or (gethash name *indentation-hints*)
         (let ((tab (gethash (package-name (symbol-package name))
                             *foreign-indentation-hints*
                             (load-time-value (make-hash-table)))))
           (gethash (symbol-name name) tab))))
    (cons
     (let ((tab (gethash (car name) *foreign-indentation-hints*
                         (load-time-value (make-hash-table)))))
       (gethash (cdr name) tab)))))

(defun (setf indentation) (rule-form name)
  "Sets the indentation hint for a name.

The NAME can either be a CL:SYMBOL, or a cons of PACKAGE-NAME
and SYMBOL-NAME strings, denoting a \"foreign\" symbol that is not
interned in the current implementation system.

Looking up indentation will result in the proper rule form even if the
symbol should become resident in the future."
  (initialize-slime)
  (initialize-sly)
  (etypecase name
    (symbol
     (setf (gethash name *indentation-hints*) rule-form)
     (let* ((pkg (package-name (symbol-package name)))
            (tab (or (gethash pkg *foreign-indentation-hints*)
                     (setf (gethash pkg *foreign-indentation-hints*) (make-hash-table :test #'equal)))))
       (setf (gethash (symbol-name name) tab) rule-form)))
    (cons
     (let* ((pkg (car name))
            (tab (or (gethash pkg *foreign-indentation-hints*)
                     (setf (gethash pkg *foreign-indentation-hints*) (make-hash-table :test #'equal)))))
       (setf (gethash (cdr name) tab) rule-form))))
  (with-symbol (update '#:update-indentation-information :swank "SWANK-INDENTATION")
    (funcall (symbol-function update)))
  (with-symbol (update '#:update-indentation-information :slynk "SLYNK/INDENTATION")
    (funcall (symbol-function update)))
  (with-symbol (set '#:indentation-rule :org.shirakumo.commander.server :commander/server)
    (funcall (fdefinition `(setf ,set)) rule-form name))
  rule-form)

(defmacro define-indentation (name rule-form)
  "Define an indentation hint for a symbol.

Example: (define-indentation defmacro (4 &lambda &body))

Each element in the rule specifies how the corresponding element in
the source form is to be indented. If there's more elements in the
source form than there are in the rule, the last element of the rule
applies to those elements.

Each element in the rule can be one of the following:

- NIL
  Use the default indentation behaviour for this element.
- an integer
  Use this number of spaces to indent the element.
- &lambda
  Treat this element as a lambda list and use the default indentation
  depth of 4 for itself.
- &rest
  Must be followed by another element that specifies the behaviour for
  all remaining elements explicitly.
- &body
  Use standard Lisp function body indentation rules for this and all
  remaining elements.
- &whole
  Must be followed by another element that specifies the behaviour for
  the current element. This indentation is then used as an additional
  offset for all remaining elements.
- a cons
  Specify the indentation rule recursively for elements of the
  corresponding source list.

See INDENTATION"
  (check-type name (or symbol (cons string string)))
  (assert (listp rule-form))
  `(setf (indentation ',name) ',rule-form))

(defun remove-indentation (name)
  "Remove the indentation hint for a symbol.

The NAME can either be a CL:SYMBOL, or a cons of PACKAGE-NAME
and SYMBOL-NAME strings, denoting a \"foreign\" symbol that is not
interned in the current implementation system.

See INDENTATION"
  (initialize-slime)
  (initialize-sly)
  (etypecase name
    (symbol
     (remhash name *indentation-hints*)
     (let ((tab (gethash (package-name (symbol-package name))
                         *foreign-indentation-hints*
                         (load-time-value (make-hash-table)))))
       (remhash (symbol-name name) tab)))
    (cons
     (let ((tab (gethash (car name)
                         *foreign-indentation-hints*
                         (load-time-value (make-hash-table)))))
       (remhash (cdr name) tab))))
  (with-symbol (update '#:update-indentation-information :swank "SWANK-INDENTATION")
    (funcall (symbol-function update)))
  (with-symbol (update '#:update-indentation-information :slynk "SLYNK/INDENTATION")
    (funcall (symbol-function update)))
  (with-symbol (set '#:indentation-rule :org.shirakumo.commander.server :commander/server)
    (funcall (fdefinition `(setf ,set)) NIL name))
  name)

(defun initialize-slime ()
  "Attempts to initialize slime with our indentation table.
If SWANK-INDENTATION is not loaded, this does nothing.
It should be safe to call this function regardless of whether
SWANK is loaded at all or not.

This is automatically called when TRIVIAL-INDENT is loaded."
  (with-symbol (tables '#:*application-hints-tables* :swank "SWANK-INDENTATION")
    (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
    t))

(defun initialize-sly ()
  "Attempts to initialize sly with our indentation table.
If SLYNK/INDENTATION is not loaded, this does nothing.
It should be safe to call this function regardless of whether
SLYNK is loaded at all or not.

This is automatically called when TRIVIAL-INDENT is loaded."
  (with-symbol (tables '#:*application-hints-tables* :slynk "SLYNK/INDENTATION")
    (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
    t))

(defparameter *default-indentation-hints*
  '((block                    1)
    (case                     (4 &rest (&whole 2 &rest 1)))
    (ccase                    case)
    (ecase                    case)
    (typecase                 case)
    (etypecase                case)
    (ctypecase                case)
    (catch                    1)
    (cond                     (&rest (&whole 2 &rest nil)))
    (defvar                   (4 2 2))
    (defclass                 (6 (&whole 4 &rest 1) (&whole 2 &rest 1) (&whole 2 &rest 1)))
    (defconstant              defvar)
    (define-modify-macro      (4 &lambda &body))
    (defun                    (4 &lambda &body))
    (defgeneric               (4 &lambda &body))
    (define-compiler-macro    defun)
    (defparameter             defvar)
    (define-condition         defclass)
    (define-setf-expander     defun)
    (defmacro                 defun)
    (deftype                  defun)
    (defpackage               (4 2))
    (defstruct                ((&whole 4 &rest (&whole 2 &rest 1)) &rest (&whole 2 &rest 1)))
    (destructuring-bind       (&lambda 4 &body))
    (dolist                   ((&whole 4 2 1) &body))
    (dotimes                  dolist)
    (eval-when                1)
    (flet                     ((&whole 4 &rest (&whole 1 4 &lambda &body)) &body))
    (labels                   flet)
    (macrolet                 flet)
    (handler-case             (4 &rest (&whole 2 2 4 &body)))
    (restart-case             handler-case)
    (if                       (&rest nil))
    (let                      ((&whole 4 &rest (&whole 1 1 2)) &body))
    (let*                     let)
    (handler-bind             let)
    (restart-bind             let)
    (locally                  1)
    (multiple-value-bind      ((&whole 6 &rest 1) 4 &body))
    (multiple-value-call      (4 &body))
    (multiple-value-prog1     1)
    (multiple-value-setq      (4 2))
    (pprint-logical-block     (4 2))
    (print-unreadable-object  ((&whole 4 1 &rest 1) &body))
    (prog1                    1)
    (prog2                    2)
    (progn                    0)
    (progv                    (4 4 &body))
    (return                   0)
    (return-from              (nil &body))
    (symbol-macrolet          let)
    (throw                    1)
    (unless                   1)
    (unwind-protect           (5 &body))
    (when                     1)
    (with-accessors           multiple-value-bind)
    (with-compilation-unit    ((&whole 4 &rest 1) &body))
    (with-condition-restarts  multiple-value-bind)
    (with-output-to-string    (4 2))
    (with-slots               multiple-value-bind)
    (with-standard-io-syntax  (2))))

(defun initialize-defaults ()
  (loop for (name rule) in *default-indentation-hints*
        do (if (symbolp rule)
               (setf (indentation name) (or (indentation rule)
                                            (error "Cannot copy rule from ~s as it does not exist." rule)))
               (setf (indentation name) rule))))

(initialize-slime)
(initialize-sly)
(initialize-defaults)
