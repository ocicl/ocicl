(uiop:define-package #:40ants-doc/restart
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/define-definer
                #:define-definer-for-symbol-locative-type)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:export
   #:define-restart))
(in-package #:40ants-doc/restart)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(define-definer-for-symbol-locative-type define-restart restart
  """A definer macro to hang the documentation of a restart on a
  symbol.

  ```
  (define-restart my-ignore-error ()
    "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
  ```

  Note that while there is a CL:RESTART class, there is no
  corresponding source location or docstring like for CONDITIONs.
  """)
