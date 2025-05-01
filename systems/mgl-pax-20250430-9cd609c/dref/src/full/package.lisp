(in-package :dref)

;;; The DREF package is created in the DREF asdf:system, which does
;;; not depend on ALEXANDRIA. The DREF/FULL system does depend on
;;; ALEXANDRIA, and we add the imports belatedly here. To avoid
;;; package conflicts if stuff is added to ALEXANDRIA, we import only
;;; specific symbols.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((imports '(alexandria:if-let alexandria:when-let
                   alexandria:ensure-list
                   alexandria:first-elt alexandria:last-elt
                   alexandria:starts-with alexandria:starts-with-subseq
                   alexandria:ends-with alexandria:ends-with-subseq
                   alexandria:once-only alexandria:with-gensyms
                   alexandria:read-stream-content-into-string
                   alexandria:read-file-into-string
                   alexandria:hash-table-keys
                   alexandria:plist-hash-table alexandria:hash-table-plist
                   alexandria:compose
                   alexandria:rcurry
                   alexandria:featurep)))
    (dolist (symbol imports)
      (import symbol))))
