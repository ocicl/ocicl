(uiop:define-package #:40ants-doc-full/args
  (:use #:cl))
(in-package #:40ants-doc-full/args)


;;; Return the names of the function arguments in ARGLIST that's a
;;; lambda list. Handles &KEY, &OPTIONAL, &REST.
(defun function-arg-names (arglist)
  (unless (eq arglist :not-available)
    (mapcar (lambda (arg)
              (if (and (listp arg)
                       (symbolp (first arg)))
                  (first arg)
                  arg))
            arglist)))

;;; Return the names of the arguments in ARGLIST that's a macro lambda
;;; list.
(defun macro-arg-names (arglist)
  (unless (eq arglist :not-available)
    (let ((names ()))
      (labels ((foo (arglist)
                 (let ((seen-special-p nil))
                   (loop for arg in arglist
                         do (cond ((member arg '(&key &optional &rest &body))
                                   (setq seen-special-p t))
                                  ((symbolp arg)
                                   (push arg names))
                                  (seen-special-p
                                   (when (symbolp (first arg))
                                     (push (first arg) names)))
                                  (t
                                   (foo arg)))))))
        (foo arglist))
      (reverse names))))
