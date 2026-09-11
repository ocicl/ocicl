;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :iolib/common-lisp-user)

(defpackage :iolib/base
  (:nicknames :iolib.base)
  (:extend/excluding :iolib/common-lisp
                     #:defun #:defmethod #:defmacro #:define-compiler-macro
                     #:constantp)
  (:extend :alexandria :split-sequence)
  (:export
   ;; Conditions
   #:bug #:iolib-bug
   #:subtype-error #:subtype-error-datum #:subtype-error-expected-supertype
   ;; Debugging
   #:*safety-checks*
   #:debug-only #:debug-only*
   #:production-only #:production-only*
   ;; Types
   #:function-designator
   #:character-designator
   #:sb8 #:sb16 #:sb32 #:sb64
   #:ub8 #:ub16 #:ub32 #:ub64
   #:ub8-sarray #:ub16-sarray #:ub32-sarray #:ub64-sarray
   #:ub8-vector #:ub16-vector #:ub32-vector #:ub64-vector
   ;; RETURN*
   #:return* #:lambda* #:defun #:defmethod
   #:defmacro #:define-compiler-macro
   ;; DEFALIAS
   #:constantp
   #:defnamespace
   #:make-alias
   #:defalias
   ;; #:function is already in CL
   ;; #:compiler-macro is already in CL
   #:macro
   #:constant
   ;; #:special is already in CL
   ;; #:class is already in CL
   ;; DEFFOLDABLE
   #:deffoldable
   #:constant-form-value
   ;; DEFOBSOLETE
   #:defobsolete
   #:signal-obsolete
   #:deprecation-warning
   #:deprecation-warning-function-name
   #:deprecation-warning-type
   #:deprecation-warning-reason
   ;; Reader utils
   #:define-syntax
   #:define-literal-reader
   #:enable-literal-reader #:enable-literal-reader*
   #:unknown-literal-syntax #:unknown-literal-syntax-name
   #:fcase
   ;; Misc
   #:function-name #:function-name-p
   #:check-bounds #:join #:join* #:shrink-vector #:full-string
   ;; Matching
   #:multiple-value-case #:flags-case
   ;; Time
   #:timeout-designator #:positive-timeout-designator
   #:decode-timeout #:normalize-timeout #:clamp-timeout
   ;; Dynamic-buffer
   #:dynamic-buffer
   #:sequence-of
   #:read-cursor-of
   #:write-cursor-of
   #:growth-size-of
   #:write-ub8
   #:write-ub16
   #:write-ub32
   #:write-vector
   #:read-ub8
   #:read-ub16
   #:read-ub32
   #:read-vector
   #:read-ub16-from-vector
   #:read-ub32-from-vector
   #:ub16-to-vector
   #:seek-read-cursor
   #:dynamic-buffer-input-error
   #:dynamic-buffer-eof
   #:dynamic-buffer-index-out-of-bounds
   ))
