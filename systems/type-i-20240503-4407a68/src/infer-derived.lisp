(in-package :type-i)

;;; user-defined types with deftype
;; requires typexpand

(define-inference-rule derived-type (test)
  (match test
    ((list 'typep '? (list 'quote type))
     (list (list 'typep '? (list 'quote (typexpand-1 type))))))) ;; ensure all intermediate types are added
