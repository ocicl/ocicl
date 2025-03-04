(defpackage #:introspect-environment
  (:use #:cl)
  (:export #:compiler-macroexpand-1 #:compiler-macroexpand)
  (:export #:specialp #:variable-type #:function-type
	   #:parse-compiler-macro)
  (:export #:constant-form-value)
  (:export #:policy #:policy-quality)
  (:export #:typexpand #:typexpand-1)
  (:export #:function-information #:variable-information
	   #:declaration-information #:parse-macro)
  #+(or sbcl ccl cmucl clasp) ; implementations with cltl2
  (:shadowing-import-from #+sbcl "SB-CLTL2" #+cmucl "EXT"
                          #+ccl "CCL" #+clasp "CLASP-CLTL2"
			  #:function-information #:variable-information
			  #:declaration-information #:parse-macro)
  #+sbcl
  (:shadowing-import-from "SB-EXT"
			  #:typexpand #:typexpand-1)
  #+sbcl
  ;; c-f-v is in sb-int and therefore not good to export.
  ;;  as in, it's unstable. would be nice to mention this in docs.
  ;;  would be nice to convince devs to move it.
  (:shadowing-import-from "SB-INT" #:constant-form-value)
  #+clasp
  (:shadowing-import-from "CLASP-CLTL2" #:parse-compiler-macro)
  #+clasp
  (:shadowing-import-from "EXT"
                          #:constant-form-value
                          #:typexpand #:typexpand-1))
