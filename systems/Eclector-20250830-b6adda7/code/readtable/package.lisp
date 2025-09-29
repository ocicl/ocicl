(cl:defpackage #:eclector.readtable
  (:use
   #:common-lisp)

  ;; Shadowed standard symbols
  (:shadow
   . #1=(#:*readtable*
         #:readtablep
         #:copy-readtable
         #:make-dispatch-macro-character
         #:readtable-case
         #:get-macro-character
         #:set-macro-character
         #:get-dispatch-macro-character
         #:set-dispatch-macro-character
         #:set-syntax-from-char))

  (:export
   . #1#)

  ;; Other exported symbols
  (:export
   #:copy-readtable-into

   #:unterminated-dispatch-macro
   #:sub-char-must-not-be-a-decimal-digit
   #:char-must-be-a-dispatching-character
   #:unknown-macro-sub-character

   #:syntax-type
   #:syntax-from-char)) ; SETF
