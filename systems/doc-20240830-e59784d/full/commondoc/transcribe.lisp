(uiop:define-package #:40ants-doc-full/commondoc/transcribe
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-doc.ops)
  (:import-from #:40ants-doc-full/transcribe)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:with-node-package))
(in-package #:40ants-doc-full/commondoc/transcribe)


(defun warn-on-differences-in-transcriptions (document)
  (flet ((check (node)
           (when (typep node 'common-doc:code-block)
             (let ((code (common-doc.ops:collect-all-text node))
                   (lang (common-doc:language node)))

               (when (string-equal lang "cl-transcript")
                 (with-output-to-string (out)
                   (with-input-from-string (s code)
                     (handler-case (40ants-doc-full/transcribe::transcribe s out :check-consistency t)
                       (40ants-doc-full/transcribe:transcription-error (c)
                         (warn "~A" c))))))))
           node))
    (with-node-package
      (40ants-doc-full/commondoc/mapper:map-nodes document #'check))))
