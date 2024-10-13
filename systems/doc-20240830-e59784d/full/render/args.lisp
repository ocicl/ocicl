(uiop:define-package #:40ants-doc-full/render/args
  (:use #:cl)
  (:import-from #:40ants-doc-full/utils))
(in-package #:40ants-doc-full/render/args)


(defun arglist-to-string (arglist)
  (cond ((stringp arglist)
         ;; must be escaped markdown
         arglist)
        ((eq arglist :not-available)
         "")
        (t (%arglist-to-string arglist))))


;;; Print arg names without the package prefix to a string. The
;;; default value with prefix. Works for macro arglists too.
(defun %arglist-to-string (arglist)
  (with-output-to-string (out)
    (let ((seen-special-p nil)
          (*print-pretty* t)
          (*print-right-margin* nil))
      (labels ((resolve* (object)
                 (40ants-doc-full/utils::prin1-and-escape-markdown object))
               (foo (arglist level)
                 (unless (= level 0)
                   (format out "("))
                 (loop for i upfrom 0
                       for arg in arglist
                       do (unless (zerop i)
                            (format out " "))
                          (cond ((member arg '(&key &optional &rest &body))
                                 (setq seen-special-p t)
                                 (format out "~A"
                                         (40ants-doc-full/utils::prin1-and-escape-markdown arg)))
                                ((symbolp arg)
                                 (format out "~A"
                                         (40ants-doc-full/utils::escape-markdown
                                          (symbol-name arg))))
                                ((atom arg)
                                 (format out "~A"
                                         (40ants-doc-full/utils::prin1-and-escape-markdown arg)))
                                (seen-special-p
                                 (if (symbolp (first arg))
                                     (format out "(~A~{ ~A~})"
                                             (40ants-doc-full/utils::escape-markdown
                                              (symbol-name (first arg)))
                                             (mapcar #'resolve* (rest arg)))
                                     (format out "~A"
                                             (40ants-doc-full/utils::prin1-and-escape-markdown arg))))
                                (t
                                 (foo arg (1+ level)))))
                 (unless (= level 0)
                   (format out ")"))))
        (foo arglist 0)))))

