(cl:defpackage #:eclector.syntax-extensions.extended-package-prefix
  (:use
   #:cl)

  (:export
   #:extended-package-prefix-syntax-mixin))

(cl:in-package #:eclector.syntax-extensions.extended-package-prefix)

(defclass extended-package-prefix-syntax-mixin ()
  ())

(defun ends-with-double-package-marker-p
    (token position-marker-1 position-marker-2)
  (let ((length (length token)))
    (and (= position-marker-2 (1- length))
         (= position-marker-1 (1- position-marker-2)))))

(defmethod eclector.reader:check-symbol-token
    ((client                    extended-package-prefix-syntax-mixin)
     (input-stream              t)
     (token                     t)
     (escape-ranges             t)
     (position-package-marker-1 integer)
     (position-package-marker-2 integer))
  (if (ends-with-double-package-marker-p
       token position-package-marker-1 position-package-marker-2)
      (values token position-package-marker-1 position-package-marker-2)
      (call-next-method)))

(defmethod eclector.reader:interpret-symbol-token
    ((client                    extended-package-prefix-syntax-mixin)
     (input-stream              t)
     (token                     t)
     (position-package-marker-1 integer)
     (position-package-marker-2 integer))
  (if (ends-with-double-package-marker-p
       token position-package-marker-1 position-package-marker-2)
      (let ((package-name (subseq token 0 position-package-marker-1)))
        (eclector.reader:call-with-state-value
         client
         (lambda ()
           (eclector.reader:read input-stream t nil t))
         '*package* package-name))
      (call-next-method)))
