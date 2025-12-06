(cl:in-package #:eclector.reader)

(defmacro with-forbidden-quasiquotation
    ((context &optional (quasiquote-forbidden-p t)
                        (unquote-forbidden-p t))
     &body body)
  `(with-quasiquotation-state (eclector.base:*client*
                               ,context
                               ,quasiquote-forbidden-p
                               ,unquote-forbidden-p)
     ,@body))
#+sbcl (declaim (sb-ext:deprecated
                 :early ("Eclector" "0.11")
                 (function with-forbidden-quasiquotation
                           :replacement with-quasiquotation-state)))
