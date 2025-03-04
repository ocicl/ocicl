#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "sb-cltl2"))

(defpackage #:trivial-cltl2
  (:nicknames #:cltl2)
  (:use #:cl
        #+sbcl #:sb-cltl2
        #+openmcl #:ccl
        #+cmu #:ext
        #+clasp #:clasp-cltl2
        #+ecl #:si
        #+abcl #:lisp
        #+lispworks #:hcl)
  #+allegro (:import-from #:excl #:compiler-let)
  #+allegro (:import-from #:system
                          #:declaration-information
                          #:augment-environment)
  #+lispworks (:import-from #:lw #:compiler-let)
  (:export #:compiler-let
           #:variable-information
           #:function-information
           #:declaration-information
           #:augment-environment
           #:define-declaration
           #:parse-macro
           #:enclose))
