(cl:defpackage #:eclector.syntax-extensions.extended-package-prefix.test
  (:use
   #:cl
   #:fiveam))

(cl:in-package #:eclector.syntax-extensions.extended-package-prefix.test)

(def-suite* :eclector.syntax-extensions.extended-package-prefix
  :in :eclector.syntax-extensions)

(test smoke
  "Smoke test for the extended-package-prefix syntax extension."
  (let ((eclector.base:*client*
          (make-instance 'eclector.syntax-extensions.extended-package-prefix:extended-package-prefix-syntax-mixin)))
    (multiple-value-bind (value position)
        (let ((*package* (find-package '#:keyword)))
          (eclector.reader:read-from-string "cl-user::(foo bar)"))
      (is (equal '(cl-user::foo cl-user::bar) value))
      (is (eql 18 position)))

    (multiple-value-bind (value position)
        (let ((*package* (find-package '#:keyword)))
          (eclector.reader:read-from-string "cl-user::;foo
bar"))
      (is (equal 'cl-user::bar value))
      (is (eql 17 position)))))
