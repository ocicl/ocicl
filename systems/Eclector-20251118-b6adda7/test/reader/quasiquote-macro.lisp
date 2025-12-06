(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.quasiquote
  :in :eclector.reader)

(test read-and-expand-quasiquote/smoke
  "Smoke test for `quasiquote' expansion. This covers some of the error
cases the random test cannot."
  (do-stream-input-cases ((input) expected)
    (let ((form (with-stream (stream)
                  (eclector.reader:read stream))))
      (flet ((do-it ()
               (macroexpand-1 form)))
        (error-case (input expected)
          (error (do-it))
          (t (expect "evaluated result" (relaxed-equalp
                                         expected (eval (do-it))))))))
    '(("`,1"           1)
      ("`,@1"          eclector.reader:unquote-splicing-at-top)
      ("`,.1"          eclector.reader:unquote-splicing-at-top)

      ("`(1 ,2)"       (1 2))
      ("`(1 ,@'(2))"   (1 2))
      ("`(1 ,.'(2))"   (1 2))

      ("`(1 . ,2)"     (1 . 2))
      ("`(1 . ,@'(2))" eclector.reader:unquote-splicing-in-dotted-list)
      ("`(1 . ,.'(2))" eclector.reader:unquote-splicing-in-dotted-list)

      ("`#(,1)"        #(1))
      ("`#(,@'(1))"    #(1))
      ("`#(,.'(1))"    #(1))

      ("`\"foo\""      "foo")

      ("#.(second '`,1)"    eclector.reader::unquote-not-inside-backquote-during-macroexpansion)
      ("#.(second '`,@(1))" eclector.reader::unquote-not-inside-backquote-during-macroexpansion))))

(test expand-unquote
  "Make sure that expanding the macros `eclector.reader:unquote' and
`eclector.reader:unquote-splicing' in invalid contexts
`eclector.reader:quasiquote' signals appropriate errors."
  (flet ((test-case (expected-condition-type expected-splicing-p form)
           (handler-case (macroexpand-1 form)
             (error (condition)
               (is (alexandria:type= expected-condition-type (type-of condition))
                   "~@<When macroexpanding ~S, expected a condition of ~S, ~
                    but got ~S~@:>"
                   form expected-condition-type condition)
               (is (eq expected-splicing-p
                       (eclector.reader::splicing-p condition)))
               ;; Make sure CONDITION prints properly.
               (is (not (string= "" (princ-to-string condition)))
                   "~@<When printing the signaled condition ~S expected a ~
                    non-empty string, but got an empty string.~@:>"
                   condition))
             (:no-error (&rest values)
               (declare (ignore values))
               (fail "~@<When macroexpanding ~S, expected a condition of ~
                      type ~S to be signaled but no condition was signaled.~@:>"
                     form expected-condition-type)))))
    (test-case
     'eclector.reader::unquote-not-inside-backquote-during-macroexpansion nil
     `(eclector.reader:unquote 1))
    (test-case
     'eclector.reader::unquote-not-inside-backquote-during-macroexpansion t
     `(eclector.reader:unquote-splicing '(1 2)))
    (test-case
     'eclector.reader:unquote-splicing-at-top t
     `(eclector.reader:quasiquote (eclector.reader:unquote-splicing '(1 2))))
    (test-case
     'eclector.reader:unquote-splicing-in-dotted-list t
     `(eclector.reader:quasiquote
       (1 . (eclector.reader:unquote-splicing '(2 3)))))))

(test expand-quasiquote.host-equivalence/random
  "Check between equivalence to our result and the result of the host for
expanding and evaluating random `quasiquote' expressions."
  (let () #+no ((*num-trials* 100000)
        (*max-trials* 100000))
    (for-all ((expression (gen-quasiquote-expression)))
      (let* ((host-expression (hostify expression))
             (host-result     (eval host-expression))
             (eclector-result (hostify (eval expression))))
        (typecase host-result
          (string
           (is (equal host-result eclector-result)))
          (t ; not ideal since this may compare nested strings using EQUALP
           (is (equalp host-result eclector-result))))))))
