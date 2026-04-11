(in-package :autoload)

;;; Define a function with NAME that loads ASDF-SYSTEM-NAME (neither
;;; evaluated) that calls the function of the same name, which is
;;; expected to have been redefined by the loaded system. If not
;;; redefined, then an error will be signalled and all subsequent
;;; calls to the function will produce the same error without
;;; attempting to load the system again.
(defmacro autoload (name asdf-system-name &key (export t) macro)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,(if (not macro)
          `(unless (fboundp ',name)
             (declaim (notinline ,name))
             ;; Workaround for the CMUCL bug that results in "Function
             ;; with declared result type NIL returned" errors when
             ;; the real function definition returns.
             #+cmucl
             (declaim (ftype function ,name))
             (defun ,name (&rest args)
               ,(format nil "Autoloaded function in system [~A][asdf:system]."
                        ;; ESCAPE-MARKDOWN, which should be used here,
                        ;; is itself autoloaded. This should be fine
                        ;; because asdf system names are rarely funny.
                        asdf-system-name)
               ;; Prevent infinite recursion which would happen if the
               ;; loaded system doesn't redefine the function.
               (setf (symbol-function ',name)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "~@<Autoloaded function ~S was not redefined ~
                        by system [~A][asdf:system].~:@>"
                              ',name ,asdf-system-name)))
               (asdf:load-system ,asdf-system-name)
               ;; Make sure that the function redefined by LOAD-SYSTEM
               ;; is invoked and not this stub, which could be the
               ;; case without the SYMBOL-FUNCTION call.
               (apply (symbol-function ',name) args)))
          ;; Autoloading macros like this should work, but what's the
          ;; point? Macro-expansion happens (most often) at compile
          ;; time, and autoloading should be about run time. Actually,
          ;; when the fasls are loaded, this won't usually trigger.
          `(unless (macro-function ',name)
             (defmacro ,name (&rest args)
               ,(format nil "Autoloaded macro in system [~A][asdf:system]."
                        asdf-system-name)
               (setf (macro-function ',name)
                     (lambda (&rest args)
                       (declare (ignore args))
                       (error "~@<Autoloaded macro ~S was not redefined ~
                        by system [~A][asdf:system].~:@>"
                              ',name ,asdf-system-name)))
               (asdf:load-system ,asdf-system-name)
               (apply (macro-function ',name) args))))
     ,@(when export
         `((export ',name)))))

(defmacro without-redefinition-warnings (&body body)
  #+sbcl
  `(locally
       (declare (sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
       ,@body))
  #-sbcl
  `(progn ,@body))

;;; Like DEFUN, but silences redefinition warnings. We could also
;;; remember autoloaded functions (in an :AROUND-COMPILE in the ASDF
;;; system definition) and generate autoload definitions.
(defmacro defun/autoloaded (name lambda-list &body body)
  (unless (ignore-errors (fdefinition name))
    (warn "~S function ~S not defined." 'defun/autoloaded name))
  `(without-redefinition-warnings
     (defun ,name ,lambda-list
       ,@body)))

;;; Like DEFVAR, but works with the global binding. This is for (LET
;;; ((*X* 1)) (DEFVAR/AUTOLOADED *X* 2)) to work like (PROGN (DEFVAR
;;; *X* 2) (LET ((*X* 1)) ...)).
(defmacro defvar/autoloaded (var &optional (val nil valp) (doc nil docp))
  (assert (special-variable-name-p var))
  `(progn
     (defvar ,var)
     ,@(when valp
         `((unless (symbol-globally-boundp ',var)
             (setf (symbol-global-value ',var) ,val))))
     ,@(when docp
         `((setf (documentation ',var 'variable) ,doc)))))

(defun special-variable-name-p (obj)
  (and (symbolp obj)
       #+ccl (member (ccl::variable-information obj) '(:special :constant))
       #+sbcl (member (sb-int:info :variable :kind obj)
                      '(:special :constant))))


;;;; Global bindings of specials
;;;;
;;;; On Lisps that don't support access to global bindings, we fall
;;;; back to the current binding.

(defun symbol-globally-boundp (symbol)
  #-ecl (null (nth-value 1 (symbol-global-value symbol)))
  #+ecl (ffi:c-inline (symbol) (:object) :object
                      "(#0->symbol.value == OBJNULL) ? ECL_NIL : ECL_T"
                      :one-liner t))

(defun symbol-global-value (symbol)
  (check-type symbol symbol)
  #+allegro
  (multiple-value-bind (value bound) (sys:global-symbol-value symbol)
    (values value (eq bound :unbound)))
  #+ccl
  (let ((value (ccl::%sym-global-value symbol)))
    (values value (eq value (ccl::%unbound-marker))))
  #+ecl
  (if (symbol-globally-boundp symbol)
      (values (ffi:c-inline (symbol) (:object) :object
                            "#0->symbol.value" :one-liner t)
              nil)
      (values nil t))
  #+sbcl
  (ignore-errors (sb-ext:symbol-global-value symbol))
  #-(or allegro ccl ecl sbcl)
  (ignore-errors (symbol-value symbol)))

(defun set-symbol-global-value (symbol value)
  #+allegro
  (setf (sys:global-symbol-value) value)
  #+ccl
  (ccl::%set-sym-global-value symbol value)
  #+ecl
  (progn (ffi:c-inline (symbol value) (:object :object) :void
                       "#0->symbol.value = #1"
                       :one-liner t)
         value)
  #+sbcl
  (setf (sb-ext:symbol-global-value symbol) value)
  #-(or allegro ccl ecl sbcl)
  (setf (symbol-value symbol) value))

(defsetf symbol-global-value set-symbol-global-value)
