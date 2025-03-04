(in-package #:trivial-cltl2)

(defun valid-type-specifier-p (spec &optional environment)
  (ignore-errors (subtypep spec spec environment)))

(defun allegro-information-alist-type-kludge (alist &optional environment)
  "Allegro 10.1 sometimes returns a type specifier IN A LIST, like
'((ARRAY (INTEGER 0 1) (*))))'.  This function tries to unwrap it..."
  ;; See also: https://github.com/ruricolist/serapeum/pull/47
  (let ((type-spec (cdr (assoc 'type alist))))
    (unless (valid-type-specifier-p type-spec environment)
      (assert (and (consp type-spec)
                   (= (list-length type-spec) 1)
                   (valid-type-specifier-p (first type-spec)))
              ()
              "Unexpected format type-specifier: ~A" type-spec)
      (push (cons 'type (first type-spec))
            alist)))
  alist)

(defun variable-information (symbol &optional environment all-declarations)
  "Compatibility layer for Allegro's `system:variable-information',
which takes extra optional parameters and returns different values
from the CLtL2's function."
  ;; See https://franz.com/support/documentation/current/doc/operators/system/variable-information.htm
  (multiple-value-bind (type locative-cons alist local-p)
      (system:variable-information symbol environment all-declarations)
    (values type
            local-p
            (allegro-information-alist-type-kludge alist)
            locative-cons)))

(defun function-information (fspec &optional environment all-declarations special-operators)
  "Compatibility layer for Allegro's `system:function-information',
which takes extra optional parameters returns different values from
the CLtL2's function."
  ;; See https://franz.com/support/documentation/current/doc/operators/system/function-information.htm
  (multiple-value-bind (type locative-cons alist local-p)
      (system:function-information fspec environment all-declarations special-operators)
    (values (case type
              (:special-operator :special-form)
              (otherwise type))
            local-p
            alist
            locative-cons)))

;;; For `define-declaration', I referred 'cl-environments' by Alexander Gutev.
;;; https://github.com/alex-gutev/cl-environments/blob/master/src/other/allegro.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun guess-declaration-kind-from-body (body)
    "Guess the kind parameter of `system:define-declaration' from the
 BODY and return the kind or NIL if failed to guess.

 Currently this works only for the simplest case, like:
 > (define-declaration foo (spec env)
 >   (values :variable (...))) "
    (let ((last-form (car (last body))))
      (if (eq (first last-form) 'values)
          (second last-form)
          nil)))

  (defun convert-allegro-define-declaration-result (kind second-value)
    (ecase kind
      ((:variable :function :both)      ; :BOTH is allegro-specific.
       (values kind second-value))
      (:declare
       ;; Cited from https://franz.com/support/documentation/current/doc/operators/system/define-declaration.htm
       ;; > if the kind is :declare, the list returned is a list of
       ;; > (list key value) instead of the cons that is specified
       ;; > there, to ease the consistency of the implementation).
       (values kind
               (list (list (car second-value)
                           (cdr second-value))))))))

(defmacro define-declaration (decl-name lambda-list &body body)
  "Compatibility layer for Allegro's `system:define-declaration',
which has very different arguments and return values from the CLtL2's one."
  ;; See https://franz.com/support/documentation/current/doc/operators/system/define-declaration.htm
  (let ((guessed-kind (guess-declaration-kind-from-body body))
        (decl-spec-arg (gensym "decl-spec-arg"))
        (env-arg (gensym "env-arg")))
    (unless guessed-kind
      (warn "TRIVIAL-CLTL2:DEFINE-DECLARATION failed to guess the kind of declaration '~A'.
  Please use SYSTEM:DEFINE-DECLARATION for specifying exactly."
            decl-name))
    `(system:define-declaration ,decl-name
         (&rest ,(gensym "declaration-syntax-lambda-list-args")) ; 'lambda-list' argument.
       ,decl-name                       ; 'prop' argument.
       ,(or guessed-kind :both)         ; 'kind' argument.
       (lambda (,decl-spec-arg ,env-arg) ; 'def' argument.
         (multiple-value-call #'convert-allegro-define-declaration-result
           ;; I use `lambda' for accepting a docstring and/or
           ;; declarations in BODY.
           ((lambda ,lambda-list ,@body)
            ,decl-spec-arg
            ,env-arg))))))

;;; This code is derived from 'clweb' by Alex Plotnick,
;;; https://github.com/plotnick/clweb/blob/4c736b4c8b4c0afbdd939eefbcb986c16c24c1e3/clweb.lisp#L1853
(defun parse-macro (name lambda-list body &optional env)
  (declare (ignorable name lambda-list body env))
  (excl::defmacro-expander `(,name ,lambda-list ,@body) env))

(defun enclose (lambda-expression &optional environment)
  (excl:compile-lambda-expr-in-env lambda-expression environment))
