(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.utilities
  :in :eclector.reader)

(test convert-according-to-readtable-case/smoke
  "Smoke test for the CONVERT-ACCORDING-TO-READTABLE-CASE function."

  (mapc (lambda (foo)
          (destructuring-bind (token escape-ranges case expected) foo
            (let ((token (copy-seq token))
                  (readtable (eclector.readtable:copy-readtable
                              eclector.reader:*readtable*)))
              (setf (eclector.readtable:readtable-case readtable) case)
              (is (string= expected (convert-according-to-readtable-case
                                     readtable token escape-ranges))))))
        '((""    ()        :upcase   "")
          ("!"   ()        :upcase   "!")
          ("foo" ()        :upcase   "FOO")
          ("foo" ((1 . 2)) :upcase   "FoO")
          ("FOO" ()        :upcase   "FOO")
          ("FOO" ((1 . 2)) :upcase   "FOO")
          ("Foo" ()        :upcase   "FOO")
          ("Foo" ((1 . 2)) :upcase   "FoO")

          (""    ()        :downcase "")
          ("!"   ()        :downcase "!")
          ("foo" ()        :downcase "foo")
          ("foo" ((1 . 2)) :downcase "foo")
          ("FOO" ()        :downcase "foo")
          ("FOO" ((1 . 2)) :downcase "fOo")
          ("Foo" ()        :downcase "foo")
          ("Foo" ((1 . 2)) :downcase "foo")

          (""    ()        :preserve "")
          ("!"   ()        :preserve "!")
          ("foo" ()        :preserve "foo")
          ("foo" ((1 . 2)) :preserve "foo")
          ("FOO" ()        :preserve "FOO")
          ("FOO" ((1 . 2)) :preserve "FOO")
          ("Foo" ()        :preserve "Foo")
          ("Foo" ((1 . 2)) :preserve "Foo")

          (""    ()        :invert   "")
          ("!"   ()        :invert   "!")
          ("!!!" ((1 . 2)) :invert   "!!!")
          ("foo" ()        :invert   "FOO")
          ("foo" ((1 . 2)) :invert   "FoO")
          ("FOO" ()        :invert   "foo")
          ("FOO" ((1 . 2)) :invert   "fOo")
          ("Foo" ()        :invert   "Foo")
          ("Foo" ((1 . 2)) :invert   "Foo")
          ("foO" ()        :invert   "foO")
          ("foO" ((1 . 2)) :invert   "foO"))))

(test skip-whitespace/smoke
  "Smoke test for the SKIP-WHITESPACE function."
  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected-value expected-position)
              input-and-expected
            (let* ((stream (make-string-input-stream input))
                   (value (skip-whitespace stream eclector.reader:*readtable*))
                   (position (file-position stream)))
              (is (equal expected-value     value))
              (is (eql   expected-position  position)))))
        '((""      nil 0)
          (" "     t   1)
          ("foo"   t   0)
          (" foo"  t   1)
          ("  foo" t   1))))

(test skip-whitespace*/smoke
  "Smoke test for the SKIP-WHITESPACE* function."
  (mapc (lambda (input-and-expected)
          (destructuring-bind (input expected-value expected-position)
              input-and-expected
            (let* ((stream (make-string-input-stream input))
                   (value (skip-whitespace* stream eclector.reader:*readtable*))
                   (position (file-position stream)))
              (is (equal expected-value     value))
              (is (eql   expected-position  position)))))
        '((""      nil 0)
          (" "     nil 1)
          ("foo"   t   0)
          (" foo"  t   1)
          ("  foo" t   2))))
