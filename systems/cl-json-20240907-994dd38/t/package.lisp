(defpackage :json-test
  (:use :json :json-rpc :common-lisp :5am )
  #+cl-json-clos
  (:import-from #+(or mcl openmcl) #:ccl
                #+cmu #:clos-mop
                #+sbcl #:sb-mop
                #+(or clisp ecl scl lispworks) #:clos
                #+(or allegro abcl) #:mop
		#+genera #:clos-internals
    #:finalize-inheritance))

(in-package :json-test)
(def-suite json)
