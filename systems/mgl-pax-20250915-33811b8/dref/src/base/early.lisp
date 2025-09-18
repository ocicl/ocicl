;;;; Early definitions for DREF/FULL.

(in-package :dref)

(define-symbol-locative-type restart ()
  "A locative to refer to the definition of a restart defined by
  DEFINE-RESTART.")

(define-definer-for-symbol-locative-type define-restart restart
  "Associate a definition with the name of a restart, which must be a symbol.
  LAMBDA-LIST should be what calls like `(INVOKE-RESTART '<SYMBOL>
  ...)` must conform to, but this not enforced.

  PAX \"defines\" standard CL restarts such as USE-VALUE with
  DEFINE-RESTART:

  ```cl-transcript (:dynenv dref-std-env)
  (first-line (source-location-snippet
               (source-location (dref 'use-value 'restart))))
  => \"(define-restart use-value (value)\"
  ```

  Note that while there is a CL:RESTART class, its instances have no
  docstring or source location.")


;;;; Early DTYPEs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *dtype-expanders* (make-hash-table)))

(defmacro define-dtype (name lambda-list &body body)
  "Like DEFTYPE, but it may expand into other DTYPEs.

  The following example defines `METHOD*` as the locative METHOD
  without its direct locative subtypes.

  ```common-lisp
  (define-dtype method* () '(and method (not reader) (not writer)))
  ```

  See DTYPEP for the semantics and also the locative DTYPE."
  (multiple-value-bind (docstring body) (parse-body-docstring body)
    (let ((%xref (gensym "XREF")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *dtype-expanders*)
               (compile nil '(lambda ,lambda-list
                              ,@body)))
         (let ((,%xref (xref ',name 'dtype)))
           (setf (definition-property ,%xref 'arglist)
                 (list ',lambda-list :macro))
           ,@(when docstring
               `((setf (definition-property ,%xref 'docstring)
                       (list ,docstring ,*package*))))
           (setf (definition-property ,%xref 'source-location)
                 (this-source-location)))))))

(autoload dtypep "dref/full")


(autoload definitions "dref/full")
(autoload dref-apropos "dref/full")


(autoload make-source-location "dref/full" :export nil)
(autoload source-location-p "dref/full" :export nil)
(autoload source-location-file "dref/full" :export nil)
(autoload source-location-file-position "dref/full" :export nil)
(autoload source-location-buffer "dref/full" :export nil)
(autoload source-location-buffer-position "dref/full" :export nil)
(autoload source-location-snippet "dref/full" :export nil)
(autoload source-location-adjusted-file-position "dref/full" :export nil)


(autoload swank-source-location* "dref/full" :export nil)
#+sbcl
(autoload translate-sb-source-location "dref/full" :export nil)
