(in-package :common-lisp-user)

(defpackage :editor-hints.named-readtables
  (:use :common-lisp)
  (:nicknames :named-readtables)
  (:export
   #:defreadtable
   #:in-readtable
   #:make-readtable
   #:merge-readtables-into
   #:find-readtable
   #:ensure-readtable
   #:rename-readtable
   #:readtable-name
   #:register-readtable
   #:unregister-readtable
   #:copy-named-readtable
   #:list-all-named-readtables
   ;; Types
   #:named-readtable-designator
   ;; Conditions
   #:readtable-error
   #:reader-macro-conflict
   #:readtable-does-already-exist
   #:readtable-does-not-exist)
  (:documentation "See NAMED-READTABLES::@NAMED-READTABLES-MANUAL."))

(pushnew :named-readtables *features*)
