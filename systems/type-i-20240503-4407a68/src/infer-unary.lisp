(in-package :type-i)

;;; unary function

(define-inference-rule unary-function (test)
  (match test
    ((list function '?)
     (let ((name (symbol-name function)))
       (match (subseq name (- (length name) 2) (length name))
         ((string #\- #\P)
          (when-let ((typesym (find-symbol (subseq name 0 (- (length name) 2))
                                           (symbol-package function))))
            (typep-form typesym)))
         ((string _ #\P)
          (when-let ((typesym (find-symbol (subseq name 0 (- (length name) 1))
                                           (symbol-package function))))
            (typep-form typesym))))))))

(defun typep-form (typesym)
  (when-let ((compounds (all-compound-types typesym)))
    ;; array, (array), (array *), ...
    (mapcar (lambda (x) `(typep ? ',x)) compounds)))

