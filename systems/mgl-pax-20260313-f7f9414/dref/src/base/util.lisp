(in-package :dref)

(defun ensure-list* (obj)
  (if (listp obj)
      obj
      (list obj)))

;;; https://gitlab.common-lisp.net/alexandria/alexandria/-/issues/46#note_16734
(defmacro nth-value-or* (nth-value &body forms)
  (case (length forms)
    ((0) nil)
    ((1) (first forms))
    (t
     (let ((%nth-value (gensym "NTH-VALUE"))
           (%values (gensym "VALUES")))
       `(let ((,%nth-value ,nth-value)
              (,%values (multiple-value-list ,(first forms))))
          (if (nth ,%nth-value ,%values)
              (values-list ,%values)
              (nth-value-or* ,%nth-value ,@(rest forms))))))))

(defun parse-body-docstring (body)
  (if (and (stringp (first body))
           (<= 2 (length body)))
      (values (first body) (rest body))
      (values () body)))

(defun parse-body-declare (body)
  (loop for form in body
        for rest on body
        while (and (listp form)
                   (eq (first form) 'declare))
        collect form into declarations
        finally (return (values declarations rest))))

;;; This is effectively what ARGLIST-PARAMETERS* (METHOD T (EQL
;;; :MACRO)) will be, but we need it for early.lisp.
(defun macro-arglist-parameters (arglist)
  (let ((names ()))
    (labels ((foo (arglist)
               (let ((seen-special-p nil))
                 (loop for rest on arglist
                       do (let ((arg (car rest)))
                            (cond ((member arg '(&key &optional &rest &body
                                                 &allow-other-keys))
                                   (setq seen-special-p t))
                                  ((symbolp arg)
                                   (push arg names))
                                  (seen-special-p
                                   (when (symbolp (first arg))
                                     (push (first arg) names)))
                                  (t
                                   (foo arg))))
                          (unless (listp (cdr rest))
                            (push (cdr rest) names))))))
      (foo arglist))
    (reverse names)))

(defmacro succeedsp (&body body)
  `(null (nth-value 1 (ignore-errors (values ,@body)))))

#+(or allegro clisp)
(defvar *used*)


(defmacro on-unknown-type-warning ((&optional (value-form nil)) &body body)
  #-(or sbcl cmucl) (declare (ignore value-form))
  `(handler-case
       (progn ,@body)
     ;; Avoid "WARNING: * is not permitted as a type specifier" on
     ;; SBCL.
     #+sbcl
     (warning (c) (ignore-errors (muffle-warning c))
       ,value-form)
     ;; Silence compiler notes on SBCL when run via ASDF:TEST-SYSTEM.
     #+sbcl
     (sb-kernel:parse-unknown-type ()
       ,value-form)
     #+cmucl
     (sys::parse-unknown-type ()
       ,value-form)))

(defun valid-type-specifier-p (type-specifier)
  (cond ((member-type-specifier-p type-specifier))
        ((satisfies-type-specifier-p type-specifier)
         (valid-satisfies-type-specifier-args-p (rest type-specifier)))
        (t
         ;; TYPEP does not signal errors on ABCL
         #+abcl
         (or (and (atom type-specifier)
                  (or (gethash type-specifier system::*known-types*)
                      (find-class type-specifier nil)
                      (eq type-specifier nil)))
             (let ((name (type-specifier-name type-specifier)))
               (if (and (not (atom type-specifier))
                        (member name '(and or not)))
                   (every #'valid-type-specifier-p (rest type-specifier))
                   (let ((expander (get name 'system::deftype-definition)))
                     (or (and expander
                              (succeedsp (funcall expander
                                                  (if (atom type-specifier)
                                                      ()
                                                      (rest type-specifier)))))
                         ;; (INTEGER 3 5) and co have no expanders.
                         (subtypep name 'number))))))
         #-abcl
         (on-unknown-type-warning (nil)
           (succeedsp (typep nil type-specifier))))))

(defun type-specifier-name (type-specifier)
  (if (listp type-specifier)
      (first type-specifier)
      type-specifier))

(defun member-type-specifier-p (type-specifier)
  (and (not (atom type-specifier))
       (eq (first type-specifier) 'member)))

(defun satisfies-type-specifier-p (type-specifier)
  (and (not (atom type-specifier))
       (eq (first type-specifier) 'satisfies)))

(defun valid-satisfies-type-specifier-args-p (args)
  (and (= (length args) 1)
       (symbolp (first args))))

;;; A wrapper around ASDF:FIND-SYSTEM to make it play nicer with
;;; package-inferred systems and warn on unexpected errors.
(defun find-system* (name &key (errorp t) (warnp t))
  ;; To have a better chance of READing DEFPACKAGE forms in
  ;; package-inferred systems. Also, see @SOURCE-FILE-READ-EVAL.
  (let ((*read-eval* t)
        (*package* #.(find-package :cl-user))
        (*readtable* (named-readtables:find-readtable :standard)))
    (handler-bind
        ((error
           (lambda (e)
             (when (and warnp
                        (not (typep e 'asdf/find-component:missing-component)))
               (warn "~@<Loading ASDF system definition ~S failed with: ~A~:@>"
                     name e))
             (when (not errorp)
               (return-from find-system* nil)))))
      (asdf:find-system name t))))


(define-condition condition-context-mixin ()
  ((context-message
    :initform ""
    :initarg :context-message
    :accessor condition-context-message)
   (context-message-args
    :initform nil
    :initarg :context-message-args
    :accessor condition-context-message-args)))

(defun append-condition-context (condition format-control &rest format-args)
  (setf (condition-context-message condition)
        (concatenate 'string (condition-context-message condition)
                     format-control))
  (setf (condition-context-message-args condition)
        (append (condition-context-message-args condition) format-args)))

(defun format-condition-context (destination condition)
  (apply #'format destination (condition-context-message condition)
         (condition-context-message-args condition))  )

(defmethod print-object :after ((condition condition-context-mixin) stream)
  (format-condition-context stream condition))
