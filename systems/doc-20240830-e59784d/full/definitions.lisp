(uiop:define-package #:40ants-doc-full/definitions
  (:use #:cl)
  (:import-from #:swank)
  (:import-from #:40ants-doc-full/swank)
  (:import-from #:40ants-doc-full/builder/vars))
(in-package #:40ants-doc-full/definitions)


(defun find-definitions-find-symbol-or-package (name)
  (flet ((do-find (original-name n)
           (multiple-value-bind (symbol found name)
               (40ants-doc-full/swank::with-swank ()
                 (swank::with-buffer-syntax (*package*)
                   ;; TODO: Replace with custom parsing
                   (swank::parse-symbol original-name)))
             (cond (found
                    (return-from find-definitions-find-symbol-or-package
                      (values symbol n)))
                   ;; Some locative types may be refered using keywords
                   ;; TODO: Probably we should rethink the way of finding
                   ;;       references by name to not rely on symbols only?
                   ((find-symbol original-name :keyword)
                    (return-from find-definitions-find-symbol-or-package
                      (values (find-symbol original-name :keyword)
                              n)))
                   ;; Packages are not named by symbols, so
                   ;; not-interned symbols can refer to packages
                   ((find-package name)
                    (return-from find-definitions-find-symbol-or-package
                      (values (make-symbol name) n)))))))
    (do-find name (length name))
    (let* ((right-trimmed
             (string-right-trim 40ants-doc-full/builder/vars::*find-definitions-right-trim* name))
           (right-trimmed-length (length right-trimmed)))
      (do-find right-trimmed right-trimmed-length))
    (let* ((right-trimmed-2
             (string-right-trim 40ants-doc-full/builder/vars::*find-definitions-right-trim-2* name))
           (right-trimmed-2-length (length right-trimmed-2)))
      (do-find right-trimmed-2 right-trimmed-2-length))))
