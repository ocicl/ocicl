;;;; These DEFPACKAGE forms are wrapped in what will eventually be
;;;; defined as MGL-PAX:DEFINE-PACKAGE.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-kernel::package-at-variance))
    (handler-bind
        (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
      ;; Foreshadow some symbols that MGL-PAX-BOOTSTRAP interns, which
      ;; would otherwise lead to conflicts when DREF-EXT is finally
      ;; :USEd. The alternative of SHADOWING-IMPORTing these just
      ;; before (USE-PACKAGE :DREF-EXT) would be annoying because in
      ;; this case recompiling MGL-PAX-BOOTSTRAP dumps a fasl with
      ;; references to DREF-EXT:LOCATIVE-TYPE in it, which then fails
      ;; to load into a fresh lisp because DREF-EXT is not yet
      ;; defined.
      (defpackage :dref-ext
        (:use #:common-lisp)
        (:export #:locative-type #:locative-args))
      (defpackage :dref
        (:use #:common-lisp)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (locally
      (declare #+sbcl
               (sb-ext:muffle-conditions sb-kernel::package-at-variance))
    (handler-bind
        (#+sbcl (sb-kernel::package-at-variance #'muffle-warning))
      (defpackage :mgl-pax
        (:documentation "See MGL-PAX::@PAX-MANUAL.")
        (:use #:common-lisp #:autoload #:dref #:dref-ext)
        (:nicknames #:pax)
        ;; These are the exports from MGL-PAX-BOOTSTRAP.
        (:export #:define-package
         #:defsection #:section
         #:exportable-reference-p
         #:exportable-locative-type-p
         #:define-glossary-term #:glossary-term
         #:make-github-source-uri-fn
         #:make-git-source-uri-fn
         #:register-doc-in-pax-world)))))
