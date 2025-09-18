(uiop:define-package #:40ants-doc/source-api
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:export
   #:find-source))
(in-package #:40ants-doc/source-api)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defgeneric find-source (object)
  (:documentation """Like SWANK:FIND-DEFINITION-FOR-THING, but this
  one is a generic function to be extensible. In fact, the default
  implementation simply defers to SWANK:FIND-DEFINITION-FOR-THING.
  This function is called by 40ANTS-DOC-FULL/SWANK:LOCATE-DEFINITION-FOR-EMACS which lies
  behind the `M-.` extension (see 40ANTS-DOC-FULL/DOC:@EMACS-INTEGRATION).

  If successful, the return value looks like this:

  ```lisp
  (:location (:file "/home/mega/own/mgl/pax/test/test.lisp")
             (:position 24) nil)
  ```

  The NIL is the source snippet which is optional. Note that position
  1 is the first character. If unsuccessful, the return values is
  like:

  ```lisp
  (:error "Unknown source location for SOMETHING")
  ```"""))
