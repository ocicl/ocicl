(cl:defpackage #:eclector.base
  (:use
   #:common-lisp)

  (:shadow
   . #1=(#:end-of-file

         #:read-char))

  (:export
   . #1#)

  ;;Exported for eclector.*, not public use.
  #+sbcl
  (:export
   #:&optional-and-&key-style-warning)

  ;; Conditions (with accessors)
  (:export
   #:stream-position-reader-error
   #:stream-position
   #:position-offset
   #:range-length

   #:end-of-file

   #:incomplete-construct

   #:missing-delimiter
   #:delimiter)

  ;; Recover restart
  (:export
   #:recover) ; function and restart name

  ;; Source location protocol
  (:export
   #:source-position
   #:make-source-range)

  ;; Exported for eclector.reader, not public use.
  (:export
   #:*client*

   #:%reader-error
   #:%recoverable-reader-error

   #:recovery-description
   #:recovery-description-using-language
   #:format-recovery-report

   #:read-char-or-error
   #:read-char-or-recoverable-error))
