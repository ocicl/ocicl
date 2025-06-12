(defpackage #:cl-template
  (:use #:cl)
  (:nicknames #:clt)
  (:export #:compile-template #:*add-progn-to-if*))

;; Symbols that can be used in templates.
(defpackage #:cl-template-template-symbols
  (:use #:cl))
