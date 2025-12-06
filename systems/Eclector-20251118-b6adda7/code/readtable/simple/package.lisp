(cl:defpackage #:eclector.readtable.simple
  (:use
   #:common-lisp)

  (:shadow
   . #1=(#:readtable))

  (:import-from #:eclector.base
   #:recovery-description-using-language)

  (:export
   . #1#))
