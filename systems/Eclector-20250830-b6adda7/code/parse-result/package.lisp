(cl:defpackage #:eclector.parse-result
  (:use
   #:common-lisp
   #:alexandria)

  (:shadow
   #:read
   #:read-preserving-whitespace
   #:read-from-string)

  ;; Parse result protocol
  (:export
   #:make-expression-result
   #:make-skipped-input-result

   #:labeled-object  ; reader for DEFINITION and REFERENCE instances
   #:definition      ; class
   #:make-definition ; constructor
   #:reference       ; class
   #:make-reference  ; constructor
   )

  ;; Read protocol
  (:export
   #:read
   #:read-preserving-whitespace
   #:read-from-string)

  ;; Client protocol class (can be used as a superclass)
  (:export
   #:parse-result-client))
