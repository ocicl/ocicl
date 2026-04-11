;;;; SBCL source location support
;;;;
;;;; SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME returns
;;;; SB-INTROSPECT:DEFINITION-SOURCES objects. We call
;;;; SWANK/SBCL::DEFINITION-SOURCE-FOR-EMACS to convert them into a
;;;; DRef @SOURCE-LOCATIONS, which is DRef's only substantial
;;;; dependency on Swank on SBCL. There is no way around this short of
;;;; duplicating several hundered lines of Swank, which includes
;;;; source-file-cache.lisp, source-path-parser.lisp.
;;;;
;;;; On the other hand, on SBCL we avoid the fragile
;;;; DSPEC-TO-DEFINITION conversion in swank-util.lisp.

(in-package :dref)

;;; Like SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME, but:
;;;
;;; - If DESCRIPTION is specified, then find the definition source in
;;;   the returned list whose description is equal to it. This is
;;;   useful when there are e.g. multiple :METHODs for NAME.
;;;
;;; - If DESCRIPTION is not specified, then find the single matching
;;;   definition source.
;;;
;;; - Convert the definition source to a source location
;;;   (see @SOURCE-LOCATIONS).
(defun sb-one-source-location (name type &key (description nil descriptionp))
  (let ((defsrcs (sb-introspect:find-definition-sources-by-name name type)))
    (when defsrcs
      (cond (descriptionp
             (loop for defsrc in defsrcs
                   when (equal (sb-introspect::definition-source-description
                                defsrc)
                               description)
                     return (ignore-errors
                             (swank/sbcl::definition-source-for-emacs
                              defsrc type name))))
            (t
             (assert (= (length defsrcs) 1))
             (ignore-errors
              (swank/sbcl::definition-source-for-emacs (first defsrcs)
                                                       type name)))))))


(defparameter *unknown-definition-types*
  '(:transform :deftransform
    :optimizer :defoptimizer
    :vop :define-vop
    :source-transform :define-source-transform
    :ir1-convert :def-ir1-translator
    :declaration declaim
    :alien-type :define-alien-type))

;;; This may return dspecs that correspond to known definition types
;;; if a user defined a locative type for any of
;;; *UNKNOWN-DEFINITION-TYPES*.
(defun at-least-unknown-dspecs (name &key include-location)
  (multiple-value-bind (name foundp) (sb-introspect-definition-name name)
    (when foundp
      (loop
        for type in *unknown-definition-types* by #'cddr
        for defsrcs = (sb-introspect:find-definition-sources-by-name name type)
        append (loop
                 for defsrc in defsrcs
                 collect
                 (let ((dspec (swank/sbcl::make-dspec type name defsrc)))
                   (if include-location
                       (list dspec
                             (swank/sbcl::definition-source-for-emacs
                              defsrc type name))
                       dspec)))))))

;;; Turn OBJECT into a symbol suitable as an argument to
;;; SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME.
(defun sb-introspect-definition-name (object)
  (cond ((stringp object)
         (values (make-symbol object) t))
        ((or (symbolp object)
             (and (listp object)
                  (extended-function-name-p object)))
         (values object t))))

(defun unknown-definitions (name)
  (when (and (or (symbolp name) (stringp name) (listp name)))
    (loop for dspec in (at-least-unknown-dspecs name)
          collect (make-instance 'unknown-dref :name name
                                 :locative `(unknown ,dspec)))))


(defun/autoloaded translate-sb-source-location (sb-source-location)
  (swank/sbcl::definition-source-for-emacs
   (sb-introspect::translate-source-location sb-source-location)
   nil nil))
