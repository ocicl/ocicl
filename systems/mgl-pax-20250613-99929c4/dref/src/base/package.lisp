;;;; Normally, the DEFSECTIONs would do all the exporting. However,
;;;; there are some self-inflicted design decisions that get in the
;;;; way:
;;;;
;;;; - We want to export the normal stuff from the DREF package and
;;;;   the extension api from the DREF-EXT package.
;;;;
;;;; - We want export to each symbol from only one package, its home
;;;;   package. No reexporting.
;;;;
;;;; - Some of the definitions for both packages are autoloaded via
;;;;   the DREF/FULL ASDF:SYSTEM.
;;;;
;;;; There is no clear interface between DREF and DREF-EXT, so all
;;;; files are under (IN-PACKAGE :DREF).

(mgl-pax:define-package #:dref-ext
    (:documentation "See DREF-EXT::@EXTENDING-DREF.")
  (:use #:common-lisp #:editor-hints.named-readtables
        #:mgl-pax #:pythonic-string-reader)
  ;; So, we export the extension api manually and import DREF-EXT into
  ;; DREF below.
  (:export #:*check-locate* #:accessor-dref #:arglist*
           #:asdf-system-dref #:call-cast #:call-locator
           #:call-lookup #:check-locative-args #:class-dref
           #:compiler-macro-dref #:condition-dref #:constant-dref
           #:declaration-dref #:define-cast
           #:define-definer-for-symbol-locative-type
           #:define-locative-alias #:define-locative-type
           #:define-locator #:define-lookup
           #:define-pseudo-locative-type
           #:define-symbol-locative-type #:definition-properties
           #:definition-property #:delete-definition-properties
           #:delete-definition-property #:docstring* #:dref-class
           #:dtype-dref #:function-dref #:generic-function-dref
           #:lambda-dref #:locate-error #:locative-args
           #:locative-dref #:locative-type
           #:locative-type-direct-subs
           #:locative-type-direct-supers #:macro-dref
           #:make-source-location #:map-definitions-of-name
           #:map-definitions-of-type #:method-combination-dref
           #:method-dref #:move-definition-properties
           #:package-dref #:reader-dref #:readtable-dref
           #:resolve* #:resolve-error #:restart-dref
           #:setf-compiler-macro-dref #:setf-dref
           #:setf-function-dref #:setf-generic-function-dref
           #:setf-method-dref #:source-location*
           #:source-location-adjusted-file-position
           #:source-location-buffer
           #:source-location-buffer-position
           #:source-location-file #:source-location-file-position
           #:source-location-p #:source-location-snippet
           #:structure-accessor-dref #:structure-dref #:symbol-locative-dref
           #:symbol-macro-dref #:this-source-location #:type-dref
           #:unknown-dref #:variable-dref #:writer-dref))

(mgl-pax:define-package #:dref
    (:documentation "See DREF::@DREF-MANUAL.")
  (:use #:common-lisp #:autoload #:dref-ext
        #:editor-hints.named-readtables #:mgl-pax
        #:pythonic-string-reader)
  ;; Some of these are in autoloaded sections, so we export everything
  ;; manually.
  (:export #:accessor #:arglist #:class #:compiler-macro
           #:condition #:constant #:declaration #:define-dtype
           #:define-restart #:definitions #:docstring #:dref
           #:dref-apropos #:dref-locative #:dref-locative-args
           #:dref-locative-type #:dref-name #:dref-origin #:dtype
           #:dtypep #:function #:generic-function #:lambda
           #:lisp-locative-types #:locate #:locate-error
           #:locative #:locative-aliases #:locative-args
           #:locative-type #:locative-types #:macro
           #:make-source-location #:method #:method-combination
           #:package #:pseudo #:pseudo-locative-types #:reader
           #:readtable #:resolve #:resolve-error #:restart #:setf
           #:setf-compiler-macro #:setf-function
           #:setf-generic-function #:setf-method
           #:source-location #:structure #:structure-accessor
           #:symbol-macro #:top #:type #:unknown
           #:variable #:writer #:xref
           #:xref-locative #:xref-locative-args
           #:xref-locative-type #:xref-name #:xref=))

;;; This sets the home package of the extension documentation symbols
;;; to DREF-EXT and makes them available in DREF.
(import
 '(dref-ext::@canonicalization dref-ext::@cast-name-change
   dref-ext::@default-downcast dref-ext::@defining-locative-types
   dref-ext::@defining-lookups-locators-and-casts
   dref-ext::@definition-properties dref-ext::@dref-classes
   dref-ext::@extending-dref dref-ext::@extending-everything-else
   dref-ext::@extending-locate dref-ext::@extension-tutorial
   dref-ext::@initial-definition dref-ext::@locative-type-hierarchy
   dref-ext::@source-locations dref-ext::@symbol-locatives)
 '#:dref)

(in-package :dref)

;;; Since we export everything manually, switch it off in DEFSECTION.
(defmethod exportable-reference-p ((package (eql (find-package 'dref-ext)))
                                   symbol locative-type locative-args)
  nil)
(defmethod exportable-reference-p ((package (eql (find-package 'dref)))
                                   symbol locative-type locative-args)
  nil)

;;; For MGL-PAX::@TRANSCRIPT-DYNENV
(defun dref-std-env (fn)
  (let ((*package* (find-package :dref)))
    ;; FIXME: Add all others too.
    (progv '(pax::*document-downcase-uppercase-code*
             pax::*transcribe-check-consistency*)
        '(nil #+sbcl t #-sbcl nil)
      (handler-bind ((warning #'muffle-warning))
        (unwind-protect
             (funcall fn)
          (unintern '*my-var* (find-package :dref)))))))

(import '(dref-std-env) '#:dref-ext)
