(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.macro-functions
  :in :eclector.reader)

(test semicolon/smoke
  "Smoke test for the SEMICOLON reader macro function."
  (do-stream-input-cases ((input length) expected
                          &optional (expected-position length))
    (multiple-value-bind (result position)
        (with-stream (stream)
          (multiple-value-list
           (eclector.reader::semicolon stream #\;)))
      (expect "values"   (equal expected          result))
      (expect "position" (eql   expected-position position)))
    '((""    ())
      ("~%"  () 1)
      (";"   ())
      (";~%" () 2))))

(test single-quote/smoke
  "Smoke test for the SINGLE-QUOTE reader macro function."
  (do-stream-input-cases ((input length)
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::single-quote stream #\'))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    '((""  eclector.reader:end-of-input-after-quote 0 0)
      (")" eclector.reader:object-must-follow-quote 0)

      ("1" (quote 1)))))

(test double-quote/smoke
  "Smoke test for the DOUBLE-QUOTE reader macro function."
  (do-stream-input-cases ((input length)
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::double-quote stream #\"))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    '((""       eclector.reader:unterminated-string 0 0)
      ("\\"     eclector.reader:unterminated-single-escape-in-string 0)

      ("\""     "")

      ("a"      eclector.reader:unterminated-string 1 0)

      ("\\a"    eclector.reader:unterminated-string 2 0)
      ("\\a\""  "a")
      ("\\\\"   eclector.reader:unterminated-string 2 0)
      ("\\\\\"" "\\")
      ("\\\""   eclector.reader:unterminated-string 2 0)
      ("\\\"\"" "\""))))

(test backquote/smoke
  "Smoke test for the BACKQUOTE reader macro function."
  (do-stream-input-cases ((input length) quasiquote-forbidden
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::with-state-values
                   (eclector.base:*client*
                    'eclector.reader::*quasiquotation-state*
                    (cons (if quasiquote-forbidden
                              'eclector.reader::sharpsign-a
                              nil)
                          'eclector.reader::sharpsign-a)
                    'eclector.reader::*quasiquotation-depth* 0)
                 (eclector.reader::backquote stream #\`)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    '(;; Errors
      (""   nil eclector.reader:end-of-input-after-backquote 0 0)
      (")"  nil eclector.reader:object-must-follow-backquote 0)
      ("1"  t   eclector.reader:backquote-in-invalid-context -1)
      ;; Valid
      ("1"  nil (eclector.reader:quasiquote 1))
      (",1" nil (eclector.reader:quasiquote (eclector.reader:unquote 1))))))

(test comma/smoke
  "Smoke test for the COMMA reader macro function."
  (do-stream-input-cases ((input length) unquote-forbidden backquote-depth
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::with-state-values
                   (eclector.base:*client*
                    'eclector.reader::*quasiquotation-state*
                    (cons nil
                          (if unquote-forbidden
                              'eclector.reader::sharpsign-a
                              nil))
                    'eclector.reader::*quasiquotation-depth*
                    backquote-depth)
                 (eclector.reader::comma stream #\,)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    '(;; Errors
      (""       nil 1 eclector.reader:end-of-input-after-unquote   0 0)
      ("@"      nil 1 eclector.reader:end-of-input-after-unquote   1 0)
      ("."      nil 1 eclector.reader:end-of-input-after-unquote   1 0)
      (")"      nil 1 eclector.reader:object-must-follow-unquote   0)
      ("@)"     nil 1 eclector.reader:object-must-follow-unquote   1)
      (".)"     nil 1 eclector.reader:object-must-follow-unquote   1)
      ("1"      nil 0 eclector.reader:unquote-not-inside-backquote -1)
      ("1"      t   1 eclector.reader:unquote-in-invalid-context   -1)
      (","      t   1 eclector.reader:unquote-in-invalid-context   -1)
      ;; Valid
      ("1"      nil 1 (eclector.reader:unquote 1))
      ("@1"     nil 1 (eclector.reader:unquote-splicing 1))
      (".1"     nil 1 (eclector.reader:unquote-splicing 1)))))

(test left-parenthesis/smoke
  "Smoke test for the LEFT-PARENTHESIS reader macro function."
  (do-stream-input-cases ((input length)
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::left-parenthesis stream #\())))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    '(;; Errors
      (""        eclector.reader:unterminated-list                       0 0)
      ("."       eclector.reader:invalid-context-for-consing-dot         0)
      ("1 ."     eclector.reader:end-of-input-after-consing-dot          3 0)
      ("1 . )"   eclector.reader:object-must-follow-consing-dot          4)
      ("1 . 2 3" eclector.reader:multiple-objects-following-consing-dot  6)
      ("1 . ."   eclector.reader:invalid-context-for-consing-dot         4)
      ;; Valid
      (")"       ())
      ("1)"      (1))
      ("1 2)"    (1 2))
      ;; Trailing whitespace. Not consuming it seems to be
      ;; permitted irregardless of whether we were asked to
      ;; preserve whitespace.
      (") "      ()    1))))

(test right-parenthesis/smoke
  "Smoke test for the RIGHT-PARENTHESIS reader macro function."
  (eclector.test:check-signals-error
   "" 'eclector.reader:invalid-context-for-right-parenthesis -1 1
   (lambda ()
     (with-input-from-string (stream "")
       (eclector.reader::right-parenthesis stream #\))))))

(test sharpsign-single-quote/smoke
  "Smoke test for the SHARPSIGN-SINGLE-QUOTE reader macro function.

Tests the \"relaxed\" variant, that is SHARPSIGN-SINGLE-QUOTE, and the
\"strict\" variant, that is STRICT-SHARPSIGN-SINGLE-QUOTE."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected-relaxed
                          &optional (expected-position-relaxed length)
                                    (expected-length-relaxed 1)
                                    (expected-strict expected-relaxed)
                                    (expected-position-strict expected-position-relaxed)
                                    (expected-length-strict expected-length-relaxed))
    (labels ((do-call (function)
               (with-stream (stream)
                 (eclector.reader::with-state-values
                     (eclector.base:*client*
                      '*read-suppress* read-suppress
                      'eclector.reader::*quasiquotation-depth* 1)
                     (funcall function stream #\' parameter))))
             (do-variant (function expected expected-position expected-length)
               (error-case (input expected expected-position expected-length)
                 (error (do-call function))
                 (t (multiple-value-bind (result position) (do-call function)
                      (expect "result"   (equal expected          result))
                      (expect "position" (eql   expected-position position)))))))
      (do-variant 'eclector.reader::sharpsign-single-quote
                  expected-relaxed expected-position-relaxed expected-length-relaxed)
      (do-variant 'eclector.reader:strict-sharpsign-single-quote
                  expected-strict expected-position-strict expected-length-strict))
    '(;; Errors
      (""                nil nil eclector.reader:end-of-input-after-sharpsign-single-quote 0 0)
      (")"               nil nil eclector.reader:object-must-follow-sharpsign-single-quote 0)
      ("5"               nil nil (function 5))
      ("X"               1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ("X"               10  nil eclector.reader:numeric-parameter-supplied-but-ignored -3 2)
      (",foo"            nil nil (function (eclector.reader:unquote foo))   4 1
       eclector.reader:unquote-in-invalid-context 0)
      ("(lambda () ,1)"  nil nil (function (lambda () (eclector.reader:unquote 1))) 14 1
                                 eclector.reader:unquote-in-invalid-context         11)
      ;; Valid
      ("X"               nil nil (function X))
      ("CL-USER::X"      nil nil (function cl-user::x))
      ("(lambda ())"     nil nil (function (lambda ())))
      ("(lambda () `,1)" nil nil (function (lambda ()
                                   (eclector.reader:quasiquote
                                    (eclector.reader:unquote 1)))))
      ("X "              nil nil (function x) 1 nil
                                 (function x) 1)
      ;; With *READ-SUPPRESS* bound to T
      ("X"               nil t   nil)
      ("5"               nil t   nil)
      ("X"               1   t   nil))))

(test sharpsign-single-quote/switch-to-strict
  "Test switching to the \"strict\" variant of the
SHARPSIGN-SINGLE-QUOTE reader macro function."
  (let ((eclector.reader:*readtable* (eclector.readtable:copy-readtable
                                      eclector.reader:*readtable*)))
    (eclector.readtable:set-dispatch-macro-character
     eclector.reader:*readtable* #\# #\'
     'eclector.reader:strict-sharpsign-single-quote)
    (signals eclector.reader:unquote-in-invalid-context
      (eclector.reader:read-from-string "`#',(foo)"))))

(test sharpsign-left-parenthesis/smoke
  "Smoke test for the SHARPSIGN-LEFT-PARENTHESIS reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.reader::sharpsign-left-parenthesis
                  stream #\( parameter)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equalp expected          result))
             (expect "position" (eql    expected-position position))))))
    '(;; Errors
      (""       nil nil eclector.reader:unterminated-vector 0 0)
      ("1"      nil nil eclector.reader:unterminated-vector 1 0)
      (")"      2   nil eclector.reader:no-elements-found 0)
      ("1 1 1)" 1   nil eclector.reader:too-many-elements 4)
      ;; Valid
      (")"      nil nil #())
      (") "     nil nil #()    1)
      ("))"     nil nil #()    1)
      ("1 1)"   nil nil #(1 1))
      ("1 1) "  nil nil #(1 1) 4)
      (")"      0   nil #0())
      ("1)"     1   nil #1(1))
      ("1)"     2   nil #2(1))
      ;; With *READ-SUPPRESS* bound to T
      (")"      nil t   nil)
      ("1)"     nil t   nil)
      ("1)"     2   t   nil)
      ("1 1)"   2   t   nil)
      ("1 2 3)" 2   t   nil))))

(test sharpsign-dot/smoke
  "Smoke test for the SHARPSIGN-DOT reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress read-eval
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress)
                     (*read-eval* read-eval))
                 (eclector.reader::sharpsign-dot stream #\. parameter)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected result))
             (expect "position" (eql   length   position))))))
    '(;; Error cases
      (""                nil nil t   eclector.reader:end-of-input-after-sharpsign-dot 0 0)
      (")"               nil nil t   eclector.reader:object-must-follow-sharpsign-dot 0)
      ("1"               1   nil t   eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ("1"               nil nil nil eclector.reader:read-time-evaluation-inhibited 0 0)
      ("(error \"foo\")" nil nil t   eclector.reader:read-time-evaluation-error 12)
      ;; Good inputs
      ("1"               nil nil t   1)
      ("(+ 1 2)"         nil nil t   3)
      ("`1"              nil nil t   1)
      ;; With *READ-SUPPRESS* bound to T
      ("(error \"foo\")" nil t   t   nil)
      ("1"               1   t   t   nil))))

(test sharpsign-backslash/smoke
  "Smoke test for the SHARPSIGN-BACKSLASH reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.reader::sharpsign-backslash
                  stream #\\ parameter)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (result position) (do-it)
             (expect "result"   (equal expected          result))
             (expect "position" (eql   expected-position position))))))
    '(;; Error cases
      (""                  nil nil eclector.reader:end-of-input-after-backslash                    0  0)
      ("x\\"               nil nil eclector.reader:unterminated-single-escape-in-character-name    2  0)
      ("x|"                nil nil eclector.reader:unterminated-multiple-escape-in-character-name  2  0)
      ("\\no-such-char"    nil nil eclector.reader:unknown-character-name                          0 13)
      ("|no-such-char"     nil nil eclector.reader:unknown-character-name                          0 13)
      ("no-such-char"      nil nil eclector.reader:unknown-character-name                          0 12)
      ("no-such:char"      nil nil eclector.reader:unknown-character-name                          0 12)
      ("no-such::char"     nil nil eclector.reader:unknown-character-name                          0 13)
      ("no-such:::char"    nil nil eclector.reader:unknown-character-name                          0 14)
      ("a"                 1   nil eclector.reader:numeric-parameter-supplied-but-ignored         -2)
      ;; Single-character case
      ("a"                 nil nil #\a)
      ("A"                 nil nil #\A)
      ("("                 nil nil #\()
      ("\\"                nil nil #\\)
      ("|"                 nil nil #\|)
      ("a "                nil nil #\a 1)
      ("a("                nil nil #\a 1)
      ;; Multi-character case
      ("space"             nil nil #\Space)
      ("Space"             nil nil #\Space)
      ("SPACE"             nil nil #\Space)
      ("Ret\\urn"          nil nil #\Return)
      ("Re|tur|n"          nil nil #\Return)
      ("Re|t\\ur|n"        nil nil #\Return)
      ;; With *READ-SUPPRESS* bound to T
      (""                  nil t   eclector.reader:end-of-input-after-backslash                   0 0)
      ("x\\"               nil t   eclector.reader:unterminated-single-escape-in-character-name   2 0)
      ("x|"                nil t   eclector.reader:unterminated-multiple-escape-in-character-name 2 0)
      ("no-such-char"      nil t   nil)
      ("1"                 1   t   nil))))

(test read-rational/smoke
  "Smoke test for the READ-RATIONAL reader macro function."
  (do-stream-input-cases ((input length) base
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (let ((readtable eclector.reader:*readtable*))
               (with-stream (stream)
                 (eclector.reader::read-rational stream readtable base nil)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (value position) (do-it)
             (expect "value"    (eql expected          value))
             (expect "position" (eql expected-position position))))))
    '(;; Error cases
      (""       10 eclector.reader:end-of-input-before-digit 0 0)
      (" "      10 eclector.reader:digit-expected 0)
      ("x"      10 eclector.reader:digit-expected 0)
      ("|"      10 eclector.reader:digit-expected 0)
      ("- "     10 eclector.reader:digit-expected 1)
      ("-x"     10 eclector.reader:digit-expected 1)
      ("1x"     10 eclector.reader:digit-expected 1)
      ("1#"     10 eclector.reader:digit-expected 1)
      ("1/"     10 eclector.reader:end-of-input-before-digit 2 0)
      ("1/ "    10 eclector.reader:digit-expected 2)
      ("1/x"    10 eclector.reader:digit-expected 2)
      ("1/1x"   10 eclector.reader:digit-expected 3)
      ("1/1#"   10 eclector.reader:digit-expected 3)
      ("1/0"    10 eclector.reader:zero-denominator 2)
      ;; Good inputs
      ("1"      10 1)
      ("-1"     10 -1)
      ("+1"     10 +1)
      ("1 "     10 1     1)
      ("-1 "    10 -1    2)
      ("+1 "    10 1     2)
      ("12"     10 12)
      ("-12"    10 -12)
      ("12 "    10 12    2)
      ("-12 "   10 -12   3)
      ("12("    10 12    2)
      ("-12("   10 -12   3)
      ("1/2"    10 1/2)
      ("-1/2"   10 -1/2)
      ("1/2 "   10 1/2   3)
      ("-1/2 "  10 -1/2  4)
      ("1/2("   10 1/2   3)
      ("-1/2("  10 -1/2  4)
      ("1/23"   10 1/23)
      ("-1/23"  10 -1/23)
      ("1/23 "  10 1/23  4)
      ("-1/23 " 10 -1/23 5)
      ("+1/23 " 10 1/23  5)
      ("1 2"    10 1     1)
      ;; Base 10
      ("1"      10 1)
      ("10"     10 10)
      ("a"      10 eclector.reader:digit-expected 0)
      ("z"      10 eclector.reader:digit-expected 0)
      ;; Base 16
      ("1"      16 1)
      ("10"     16 16)
      ("a"      16 10)
      ("z"      16 eclector.reader:digit-expected 0)
      ;; Base 36
      ("1"      36 1)
      ("10"     36 36)
      ("a"      36 10)
      ("z"      36 35))))

(macrolet
    ((define-rational-reader-macro-test (char cases)
       (let* ((function-name (let ((*package* (find-package '#:eclector.reader)))
                               (alexandria:symbolicate '#:sharpsign- char)))
              (test-name (alexandria:symbolicate function-name '#:/smoke)))
         `(test ,test-name
            ,(format nil "Smoke test for the ~A function." function-name)
            (do-stream-input-cases ((input length) parameter read-suppress
                                    expected &optional (expected-position length)
                                                       (expected-length 1))
              (flet ((do-it ()
                       (with-stream (stream)
                         (let ((*read-suppress* read-suppress))
                           (,function-name stream ,char parameter)))))
                (error-case (input expected expected-position expected-length)
                  (error (do-it))
                  (t (multiple-value-bind (value position) (do-it)
                       (expect "value"    (equalp expected value))
                       (expect "position" (equal expected-position position))))))
              ,cases)))))

  (define-rational-reader-macro-test #\B
    '(;; Errors
      (""     nil nil eclector.reader:end-of-input-before-digit 0 0)
      ("1/"   nil nil eclector.reader:end-of-input-before-digit 2 0)
      (" "    nil nil eclector.reader:digit-expected 0)
      ("2"    nil nil eclector.reader:digit-expected 0)
      ("x"    nil nil eclector.reader:digit-expected 0)
      ("1."   nil nil eclector.reader:digit-expected 1)
      ("#b1"  nil nil eclector.reader:digit-expected 0)
      ("1/0"  nil nil eclector.reader:zero-denominator 2)
      ("1"    1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ;; Valid binary rationals
      ("1"    nil nil 1)
      ("-1"   nil nil -1)
      ("+1"   nil nil 1)
      ("1/10" nil nil 1/2)
      ;; With *READ-SUPPRESS* bound to T
      ("1"    1   t   nil)
      (""     nil t   nil)
      ("-"    nil t   nil)
      ("/#!"  nil t   nil)
      ("-/#!" nil t   nil)
      ("2"    nil t   nil)
      ("1."   nil t   nil)
      ("\\1"  nil t   nil)
      ("1\\2" nil t   nil)))

  (define-rational-reader-macro-test #\O
    '(;; Errors
      (""     nil nil eclector.reader:end-of-input-before-digit 0 0)
      ("1/"   nil nil eclector.reader:end-of-input-before-digit 2 0)
      (" "    nil nil eclector.reader:digit-expected 0)
      ("8"    nil nil eclector.reader:digit-expected 0)
      ("x"    nil nil eclector.reader:digit-expected 0)
      ("1."   nil nil eclector.reader:digit-expected 1)
      ("#o1"  nil nil eclector.reader:digit-expected 0)
      ("1/0"  nil nil eclector.reader:zero-denominator 2)
      ("1"    1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ;; Valid octal rationals
      ("1"    nil nil 1)
      ("-1"   nil nil -1)
      ("+1"   nil nil 1)
      ("1/10" nil nil 1/8)
      ;; With *READ-SUPPRESS* bound to T
      ("1"    1   t   nil)
      (""     nil t   nil)
      ("-"    nil t   nil)
      ("/#!"  nil t   nil)
      ("-/#!" nil t   nil)
      ("8"    nil t   nil)
      ("1."   nil t   nil)
      ("\\1"  nil t   nil)
      ("1\\2" nil t   nil)))

  (define-rational-reader-macro-test #\X
    '(;; Errors
      (""     nil nil eclector.reader:end-of-input-before-digit 0 0)
      ("1/"   nil nil eclector.reader:end-of-input-before-digit 2 0)
      (" "    nil nil eclector.reader:digit-expected 0)
      ("g"    nil nil eclector.reader:digit-expected 0)
      ("x"    nil nil eclector.reader:digit-expected 0)
      ("1."   nil nil eclector.reader:digit-expected 1)
      ("#x1"  nil nil eclector.reader:digit-expected 0)
      ("1/0"  nil nil eclector.reader:zero-denominator 2)
      ("1"    1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ;; Valid hexadecimal rationals
      ("1"    nil nil 1)
      ("-1"   nil nil -1)
      ("+1"   nil nil 1)
      ("1/10" nil nil 1/16)
      ;; With *READ-SUPPRESS* bound to T
      ("1"    1   t   nil)
      (""     nil t   nil)
      ("-"    nil t   nil)
      ("/#!"  nil t   nil)
      ("-/#!" nil t   nil)
      ("g"    nil t   nil)
      ("1."   nil t   nil)
      ("\\1"  nil t   nil)
      ("1\\2" nil t   nil)))

  (define-rational-reader-macro-test #\R
    '(;; Errors
      (""     17  nil eclector.reader:end-of-input-before-digit 0 0)
      ("1/"   17  nil eclector.reader:end-of-input-before-digit 2 0)
      (" "    17  nil eclector.reader:digit-expected 0)
      ("h"    17  nil eclector.reader:digit-expected 0)
      ("x"    17  nil eclector.reader:digit-expected 0)
      ("1."   17  nil eclector.reader:digit-expected 1)
      ("#2R1" 17  nil eclector.reader:digit-expected 0)
      ("1/0"  17  nil eclector.reader:zero-denominator 2)
      ("1"    nil nil eclector.reader:numeric-parameter-not-supplied-but-required -1)
      ("1"    0   nil eclector.reader:invalid-radix -2)
      ("1"    1   nil eclector.reader:invalid-radix -2)
      ("1"    37  nil eclector.reader:invalid-radix -3 2)
      ;; Valid base-17 rationals
      ("1"    17  nil 1)
      ("-1"   17  nil -1)
      ("+1"   17  nil 1)
      ("1/10" 17  nil 1/17)
      ("g"    17  nil 16)
      ("1 2"  16  nil 1 1)
      ;; With *READ-SUPPRESS* bound to T
      ("h"    17  t   nil)
      ("x"    17  t   nil)
      ("1."   17  t   nil)
      (""     nil t   nil)
      ("-"    nil t   nil)
      ("/#!"  nil t   nil)
      ("-/#!" nil t   nil)
      ("1"    nil t   nil)
      ("1"    0   t   nil)
      ("1"    1   t   nil)
      ("1"    37  t   nil)
      ("\\1"  16  t   nil)
      ("1\\2" 16  t   nil))))

(test rational-reader-macro+digit-is-macro-character
  "Ensure that non-terminating macro characters work as digits in
rational numbers."
  (let ((eclector.reader:*readtable* (eclector.readtable:copy-readtable
                                      eclector.reader:*readtable*)))
    (eclector.readtable:set-macro-character
     eclector.reader:*readtable*
     #\1
     (lambda (stream char)
       (declare (ignore stream char))
       1)
     t) ; non-terminating
    (is (equal (values 16 4) (eclector.reader:read-from-string "#x10")))
    (is (equal (values  1 4) (eclector.reader:read-from-string "#x01")))))

(test sharpsign-asterisk/smoke
  "Smoke test for the SHARPSIGN-ASTERISK function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected &optional (expected-position length)
                                             (expected-input 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.reader::sharpsign-asterisk stream #\* parameter)))))
      (error-case (input expected expected-position expected-input)
        (error (do-it))
        (t (multiple-value-bind (value position) (do-it)
             (expect "value"    (equalp expected value))
             (expect "position" (equal expected-position position))))))
    '(;; Errors
      ("a"   nil nil eclector.reader:digit-expected 0)
      ("2"   nil nil eclector.reader:digit-expected 0)
      (""    2   nil eclector.reader:no-elements-found 0 0)
      ("a"   2   nil eclector.reader:digit-expected 0)
      ("111" 1   nil eclector.reader:too-many-elements 1 2)
      ;; Valid
      (""    nil nil #*)
      (" "   nil nil #*   0)
      (")"   nil nil #*   0)
      ("11"  nil nil #*11)
      ("11 " nil nil #*11 2)
      ("11)" nil nil #*11 2)
      (""    0   nil #0*)
      ("1"   1   nil #1*1)
      ("1"   2   nil #2*1)
      ("1 "  2   nil #2*1 1)
      ("1)"  2   nil #2*1 1)
      ;; With *READ-SUPPRESS* bound to T
      (""    nil t   nil)
      ("1"   nil t   nil)
      (""    2   t   nil)
      ("1"   2   t   nil)
      ("11"  2   t   nil)
      ("111" 2   t   nil)
      ("abc" 2   t   nil)
      ("abc" nil t   nil))))

(test sharpsign-vertical-bar/smoke
  "Smoke test for the SHARPSIGN-VERTICAL-BAR function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (multiple-value-list
                  (eclector.reader::sharpsign-vertical-bar
                   stream #\| parameter))))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (values position) (do-it)
             (expect "values"   (equalp expected values))
             (expect "position" (equal expected-position position))))))
    '(;; Errors
      (""         nil nil eclector.reader:unterminated-block-comment 0 0)
      ("a"        nil nil eclector.reader:unterminated-block-comment 1 0)
      ("a|"       nil nil eclector.reader:unterminated-block-comment 2 0)
      ("a#||#"    nil nil eclector.reader:unterminated-block-comment 5 0)
      ("a|#"      1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ;; Valid
      ("a|#"      nil nil ())
      ("a# |#"    nil nil ())
      ("a| |#"    nil nil ())
      ("a#|b|#|#" nil nil ())
      ;; With *READ-SUPPRESS* bound to T
      ("a|#"      nil t   ())
      ("a# |#"    nil t   ())
      ("a| |#"    nil t   ())
      ("a#|b|#|#" nil t   ()))))

(test sharpsign-a/smoke
  "Smoke test for the SHARPSIGN-A reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          &optional expected (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::with-state-values
                   (eclector.base:*client*
                    'eclector.reader::*quasiquotation-depth* 1
                    '*read-suppress* read-suppress)
                 (eclector.reader::sharpsign-a stream #\A parameter)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (value position) (do-it)
             (expect "value"    (equalp expected value))
             (expect "position" (equal  length   position))))))
    '(;; Errors
      (""          1   nil eclector.reader:end-of-input-after-sharpsign-a 0 0)
      (")"         1   nil eclector.reader:object-must-follow-sharpsign-a 0)

      ("(,1)"      1   nil eclector.reader:unquote-in-invalid-context 1)
      ("(1)"       2   nil eclector.reader:read-object-type-error 2)
      ("(1 (1))"   2   nil eclector.reader:read-object-type-error 6)
      ("((1) 1)"   2   nil eclector.reader:read-object-type-error 6)
      ("(() (1))"  2   nil eclector.reader:incorrect-initialization-length 7)
      ("1"         1   nil eclector.reader:read-object-type-error 0)
      ("(1)"       nil nil eclector.reader:numeric-parameter-not-supplied-but-required -1)
      ;; Valid
      ("()"        0   nil #0A())
      ("(`,1)"     0   nil #0A((eclector.reader:quasiquote
                                (eclector.reader:unquote 1))))
      ("(1 2)"     0   nil #0A(1 2))
      ("(((1)))"   0   nil #0A(((1))))
      ("()"        1   nil #1A())
      ("(0 0)"     1   nil #1A(0 0))
      ("((1) (2))" 1   nil #1A((1) (2)))
      ("(((1)))"   1   nil #1A(((1))))
      ("((0) (0))" 2   nil #2A((0) (0)))
      ;; With *READ-SUPPRESS* bound to T
      (""          1   t  eclector.reader:end-of-file 0 0)
      ("(1)"       2   t  nil)
      ("(() (1))"  2   t  nil)
      ("1"         1   t  nil)
      ("(1)"       nil t  nil)
      ("()"        0   t  nil)
      ("(1 2)"     0   t  nil)
      ("(((1)))"   0   t  nil)
      ("()"        1   t  nil)
      ("(0 0)"     1   t  nil)
      ("((1) (2))" 1   t  nil)
      ("(((1)))"   1   t  nil)
      ("((0) (0))" 2   t  nil))))

(test sharpsign-colon/smoke
  "Smoke test for the SHARPSIGN-COLON reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.reader::sharpsign-colon stream #\. parameter)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (value position) (do-it)
             (expect "name"     (string= (symbol-name expected) (symbol-name value)))
             (expect "position" (equal expected-position position))))))
    '(;; Errors
      ("\\"      nil nil eclector.reader:unterminated-single-escape-in-symbol 1 0)
      ("|"       nil nil eclector.reader:unterminated-multiple-escape-in-symbol 1 0)
      ("|\\"     nil nil eclector.reader:unterminated-single-escape-in-symbol 2 0)
      ("a:b"     nil nil eclector.reader:uninterned-symbol-must-not-contain-package-marker 1)
      ("a:b:c"   nil nil eclector.reader:uninterned-symbol-must-not-contain-package-marker 1)
      ("a"       1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ;; Valid
      (""        nil nil #:||)
      (" "       nil nil #:|| 0)
      ("("       nil nil #:|| 0)
      ("\\a"     nil nil #:|a|)
      ("\\:"     nil nil #:|:|)
      ("|\\a|"   nil nil #:|a|)
      ("|a|"     nil nil #:|a|)
      ("|a:|"    nil nil #:|a:|)
      ("a "      nil nil #:a 1)
      ;; With *READ-SUPPRESS* bound to T
      ("\\"      nil t   eclector.reader:unterminated-single-escape-in-symbol 1 0)
      ("|"       nil t   eclector.reader:unterminated-multiple-escape-in-symbol 1 0)
      ("a"       1   t   nil))))

(test sharpsign-c/smoke
  "Smoke test for the SHARPSIGN-C reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected-relaxed
                          &optional (expected-position-relaxed length)
                                    (expected-length-relaxed 1)
                                    (expected-strict expected-relaxed)
                                    (expected-position-strict expected-position-relaxed)
                                    (expected-length-strict expected-length-relaxed))
    (labels ((do-call (function)
               (with-stream (stream)
                 (eclector.reader::with-state-values
                     (eclector.base:*client*
                      '*read-suppress* read-suppress
                      'eclector.reader::*quasiquotation-depth* 1)
                   (funcall function stream #\C parameter))))
             (do-variant (function expected expected-position expected-length)
               (error-case (input expected expected-position expected-length)
                 (error (do-call function))
                 (t (multiple-value-bind (value position) (do-call function)
                      (expect "value"    (equal expected value))
                      (expect "position" (equal length   position)))))))
      (do-variant 'eclector.reader::sharpsign-c
                  expected-relaxed expected-position-relaxed expected-length-relaxed)
      (do-variant 'eclector.reader:strict-sharpsign-c
                  expected-strict expected-position-strict expected-length-strict))
    '(;; Errors
      (""               nil nil eclector.reader:end-of-input-after-sharpsign-c 0 0)
      (")"              nil nil eclector.reader:complex-parts-must-follow-sharpsign-c 0)
      ("\"foo\""        nil nil eclector.reader:non-list-following-sharpsign-c 4)
      ("("              nil nil eclector.reader:end-of-input-before-complex-part 1 0)
      ("()"             nil nil eclector.reader:complex-part-expected 1)
      ("(0"             nil nil eclector.reader:end-of-input-before-complex-part 2 0)
      ("(0)"            nil nil eclector.reader:complex-part-expected 2)
      ("(0 0"           nil nil eclector.reader:unterminated-list 4 0)
      ("(0 0 0)"        nil nil eclector.reader:too-many-complex-parts 5)
      ("#(0 0)"         nil nil eclector.reader:non-list-following-sharpsign-c 5)
      ("(:a 0)"         nil nil eclector.reader:read-object-type-error 2)
      ("(0 :a)"         nil nil eclector.reader:read-object-type-error 4)
      ("(0 0)"          1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)
      ("(`,0 0)"        nil nil eclector.reader:backquote-in-invalid-context 1)
      ("(,0 0)"         nil nil eclector.reader:unquote-in-invalid-context 1)
      ;; Valid
      ("(0 1)"          nil nil #C(0 1))
      ("(-1 1)"         nil nil #C(-1 1))
      ("(0 1/2)"        nil nil #C(0 1/2))
      ("(#.1 2)"        nil nil #C(1 2))
      ("(0 #.(+ 1 2))"  nil nil #C(0 3)) ; since we bind *LIST-READER*
      ;; Invalid for relaxed and strict versions for different
      ;; reasons
      ("#.(list 1 2 3)" nil nil eclector.reader:read-object-type-error         13 1
                                eclector.reader:non-list-following-sharpsign-c 13)
      ;; Valid only for relaxed version
      (" (1 2)"         nil nil #C(1 2)                                        0 1
                                eclector.reader:non-list-following-sharpsign-c)
      ("#||# (1 2)"     nil nil #C(1 2)                                        3 1
                                eclector.reader:non-list-following-sharpsign-c)
      ("#.(list 1 2)"   nil nil #C(1 2)                                        11 1
                                eclector.reader:non-list-following-sharpsign-c)
      ;; With *READ-SUPPRESS* bound to T
      ("(0)"            nil t   nil)
      ("(0 0 0)"        nil t   nil)
      ("#(0 0)"         nil t   nil)
      ("(:a 0)"         nil t   nil)
      ("(0 :a)"         nil t   nil)
      ("(0 0)"          nil t   nil)
      ("#.(list 1 2 3)" nil t   nil))))

(test sharpsign-s/smoke
  "Smoke test for the SHARPSIGN-S reader macro function.

Tests the \"relaxed\" variant, that is SHARPSIGN-S, and the \"strict\"
variant, that is STRICT-SHARPSIGN-S."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected-relaxed
                          &optional (expected-position-relaxed length)
                                    (expected-length-relaxed   1)
                                    (expected-strict           expected-relaxed)
                                    (expected-position-strict  expected-position-relaxed)
                                    (expected-length-strict    expected-length-relaxed))
    (labels ((do-call (function)
               (with-stream (stream)
                 (let* ((client (make-instance 'sharpsign-s-client))
                        (eclector.base:*client* client))
                  (eclector.reader::with-state-values
                      (client
                       '*read-suppress* read-suppress
                       'eclector.reader::*quasiquotation-depth* 1)
                    (funcall function stream #\S parameter)))))
             (do-variant (function expected expected-position expected-length)
               (error-case (input expected expected-position expected-length)
                 (error (do-call function))
                 (t (multiple-value-bind (value position) (do-call function)
                      (expect "value"    (relaxed-equalp expected value))
                      (expect "position" (equal          length   position)))))))
      (do-variant 'eclector.reader::sharpsign-s
                  expected-relaxed expected-position-relaxed expected-length-relaxed)
      (do-variant 'eclector.reader:strict-sharpsign-s
                  expected-strict expected-position-strict expected-length-strict))
    '(;; Errors
      ("(foo)"           1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)

      (""                nil nil eclector.reader:end-of-input-after-sharpsign-s 0 0)
      ("1"               nil nil eclector.reader:non-list-following-sharpsign-s 0)
      (")"               nil nil eclector.reader:structure-constructor-must-follow-sharpsign-s 0)
      ("`"               nil nil eclector.reader:backquote-in-invalid-context 0)
      ("(foo . 1)"       nil nil eclector.reader:invalid-context-for-consing-dot 5)

      ("("               nil nil eclector.reader:end-of-input-before-structure-type-name 1 0)
      ("()"              nil nil eclector.reader:no-structure-type-name-found 1)
      ("(1)"             nil nil eclector.reader:structure-type-name-is-not-a-symbol 1)

      ("(foo"            nil nil eclector.reader:end-of-input-before-slot-name 4 0)
      ("(foo 1 2)"       nil nil eclector.reader:slot-name-is-not-a-string-designator 5)
      ("(foo :bar"       nil nil eclector.reader:end-of-input-before-slot-value 9 0)
      ("(foo :bar)"      nil nil eclector.reader:no-slot-value-found 9)
      ("(foo :bar 1"     nil nil eclector.reader:end-of-input-before-slot-name 11 0)

      ("(`,foo :bar 1)"  nil nil eclector.reader:backquote-in-invalid-context 1)
      ("(,foo :bar 1)"   nil nil eclector.reader:unquote-in-invalid-context 1)
      ("(foo `,:bar 1)"  nil nil eclector.reader:backquote-in-invalid-context 5)
      ("(foo ,:bar 1)"   nil nil eclector.reader:unquote-in-invalid-context 5)
      ("(foo :bar ,1)"   nil nil eclector.reader:unquote-in-invalid-context 10)
      ;; Valid
      ("(foo)"           nil nil (foo))
      ("(foo #:bar 1)"   nil nil (foo #:bar 1))
      ("(foo :bar 1)"    nil nil (foo :bar 1))
      ("(foo bar 1)"     nil nil (foo bar 1))
      ("(foo \"bar\" 1)" nil nil (foo "bar" 1))
      ("(foo #\\b 1)"    nil nil (foo #\b 1))
      ("(foo :bar `,1)"  nil nil (foo :bar (eclector.reader:quasiquote
                                            (eclector.reader:unquote 1))))
      ;; With *READ-SUPPRESS* bound to T
      ("(foo)"           nil t   nil)
      ("(foo 1 2)"       nil t   nil)
      ("(foo :bar)"      nil t   nil)
      ("(foo)"           1   t   nil)
      ;; Differences between relaxed and strict
      (" (foo)"          nil nil (foo)                                                0  nil
                                 eclector.reader:non-list-following-sharpsign-s       0  1)  ; unclear in the spec. we do not allow it
      ("(#.(quote foo))" nil nil (foo)
                                 eclector.reader:non-list-following-sharpsign-s) ; since we bind *LIST-READER*
      ("#.1"             nil nil eclector.reader:non-list-following-sharpsign-s       2  1
                                 eclector.reader:non-list-following-sharpsign-s)
      ("#.(list 'foo)"   nil nil (foo)                                                12 1
                                 eclector.reader:non-list-following-sharpsign-s) ; unclear in the spec. we do not allow it
      ("#.(list 1)"      nil nil eclector.reader:structure-type-name-is-not-a-symbol  9  1
                                 eclector.reader:non-list-following-sharpsign-s)
      ("#.(list 'foo 1)" nil nil eclector.reader:slot-name-is-not-a-string-designator 14 1
                                 eclector.reader:non-list-following-sharpsign-s)
      ("#.'(a b 1 c 2)"  nil nil (a b 1 c 2)                                          nil nil
                                 eclector.reader:non-list-following-sharpsign-s       13  1))))

(test sharpsign-p/smoke
  "Smoke test for the SHARPSIGN-P reader macro function."
  (do-stream-input-cases ((input length) parameter read-suppress
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (eclector.reader::with-state-values
                   (eclector.base:*client*
                    '*read-suppress* read-suppress
                    'eclector.reader::*quasiquotation-depth* 1)
                 (eclector.reader::sharpsign-p stream #\P parameter)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (value position) (do-it)
             (expect "value"    (equal expected value))
             (expect "position" (equal length   position))))))
    '(;; Errors
      ("\"foo\""   1   nil eclector.reader:numeric-parameter-supplied-but-ignored -2)

      (""          nil nil eclector.reader:end-of-input-after-sharpsign-p 0 0)
      ("1"         nil nil eclector.reader:non-string-following-sharpsign-p 0)
      (")"         nil nil eclector.reader:namestring-must-follow-sharpsign-p 0)
      ("`,\"foo\"" nil nil eclector.reader:backquote-in-invalid-context 0)
      (",\"foo\""  nil nil eclector.reader:unquote-in-invalid-context 0)
      ;; Valid
      ("\"foo\""   nil nil #P"foo")
      ;; With *READ-SUPPRESS* bound to T
      ("\"foo\""   nil t   nil)
      ("\"foo\""   1   t   nil))))

(test sharpsign-plus-minus/smoke
  "Smoke test for the SHARPSIGN-{PLUS,MINUS} functions."
  (do-stream-input-cases ((input length) parameter *read-suppress*
                          plus-expected &optional (minus-expected plus-expected)
                                                  (expected-position length)
                                                  (expected-length 1))
    (labels ((do-it (which)
               (with-stream (stream)
                 (eclector.reader::with-state-values
                     (eclector.base:*client*
                      'eclector.reader::*quasiquotation-depth* 1)
                   (multiple-value-list
                    (ecase which
                      (:plus  (eclector.reader::sharpsign-plus
                               stream #\+ parameter))
                      (:minus (eclector.reader::sharpsign-minus
                               stream #\- parameter)))))))
             (do-one (which expected)
               (error-case (input expected expected-position expected-length)
                 (error (do-it which))
                 (t (multiple-value-bind (values position) (do-it which)
                      (expect "values"   (equal expected values))
                      (expect "position" (equal length   position)))))))
      (do-one :plus  plus-expected)
      (do-one :minus minus-expected))
    '(;; Errors
      (""                      nil nil eclector.reader:end-of-input-after-sharpsign-plus-minus
                                       eclector.reader:end-of-input-after-sharpsign-plus-minus
                                       0 0)
      (")"                     nil nil eclector.reader:feature-expression-must-follow-sharpsign-plus-minus
                                       eclector.reader:feature-expression-must-follow-sharpsign-plus-minus
                                       0)
      ("(and)"                 nil nil eclector.reader:end-of-input-after-feature-expression
                                       eclector.reader:end-of-input-after-feature-expression
                                       5 0)
      ("(and))"                nil nil eclector.reader:object-must-follow-feature-expression
                                       eclector.reader:object-must-follow-feature-expression
                                       5)

      ("1"                     nil nil eclector.reader:feature-expression-type-error
                                       eclector.reader:feature-expression-type-error
                                       0)
      ("(1)"                   nil nil eclector.reader:feature-expression-type-error
                                       eclector.reader:feature-expression-type-error
                                       2)
      ("(not :foo :bar)"       nil nil eclector.reader:single-feature-expected
                                       eclector.reader:single-feature-expected
                                       14)
      ("(not 1)"               nil nil eclector.reader:feature-expression-type-error
                                       eclector.reader:feature-expression-type-error
                                       6)
      ("(and 1)"               nil nil eclector.reader:feature-expression-type-error
                                       eclector.reader:feature-expression-type-error
                                       6)
      ("(or 1)"                nil nil eclector.reader:feature-expression-type-error
                                       eclector.reader:feature-expression-type-error
                                       5)

      ("(and) 1"               1   nil eclector.reader:numeric-parameter-supplied-but-ignored
                                       eclector.reader:numeric-parameter-supplied-but-ignored
                                       -2)

      ("`,(and)"               nil nil eclector.reader:backquote-in-invalid-context
                                       eclector.reader:backquote-in-invalid-context
                                       0)
      (",(and)"                nil nil eclector.reader:unquote-in-invalid-context
                                       eclector.reader:unquote-in-invalid-context
                                       0)
      ;; Valid
      ("common-lisp 1"         nil nil (1)   ())
      ("(not common-lisp) 1"   nil nil ()    (1))
      ("(and) 1"               nil nil (1)   ())
      ("(or) 1"                nil nil ()    (1))
      ("(not (not (and))) 1"   nil nil (1)   ())
      ;; With *READ-SUPPRESS* bound to T
      ("(and) 1"               nil t   (nil) ())
      ("(and) 1"               1   t   (nil) ())
      ;; In which package is the guarded expression read?
      ("(and) foo"             nil nil (foo) ())
      ;; Vendor extension shouldn't break other implementations
      ;; when guarded properly
      ("(and some-lisp
             (version> \"1\"))
        1"                     nil nil ()    (1)))))

(test sharpsign-invalid/smoke
  "Smoke test for the SHARPSIGN-INVALID function."
  (eclector.test:check-signals-error
   "" 'eclector.reader:sharpsign-invalid -1 1
   (lambda ()
     (with-input-from-string (stream "")
       (eclector.reader::sharpsign-invalid stream #\< nil)))))

;;; Reader macros for sharpsign equals and sharpsign sharpsign.

(test sharpsign-{equal\,sharpsign}/smoke
  "Smoke test for the SHARPSIGN-{EQUAL,SHARPSIGN} functions."
  (do-stream-input-cases ((input length) read-suppress
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*read-suppress* read-suppress))
                 (eclector.reader:read stream)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (recursive-cons
         (let ((result (do-it)))
           (is-true (consp result))
           (expect "result" (eq result (car result)))))
        (recursive-cons2
         (let ((result (do-it)))
           (is-true (consp result))
           (expect "result" (eq result (cadadr result)))
           (expect "result" (eq (cadr result) (caadr result)))))
        (recursive-vector
         (let ((result (do-it)))
           (is-true (vectorp result))
           (expect "result" (eq result (aref result 0)))))
        (t
         (expect "result" (equalp expected (do-it))))))
    '(;; sharpsign equals errors
      ("#1="                     nil eclector.reader:end-of-input-after-sharpsign-equals 3 0)
      ("(#1=)"                   nil eclector.reader:object-must-follow-sharpsign-equals 4)
      ("#="                      nil eclector.reader:numeric-parameter-not-supplied-but-required 1)
      ("(#10=1 #10=2)"           nil eclector.reader:sharpsign-equals-label-defined-more-than-once 8 2)
      ("#1=#1#"                  nil eclector.reader:sharpsign-equals-only-refers-to-self 5)
      ;; sharpsign sharpsign errors
      ("##"                      nil eclector.reader:numeric-parameter-not-supplied-but-required 1)
      ("#1#"                     nil eclector.reader:sharpsign-sharpsign-undefined-label 1)
      ("(#1=1 #200#)"            nil eclector.reader:sharpsign-sharpsign-undefined-label 7 3)
      ;; Labels and references in conses and vectors
      ("(#1=1)"                  nil (1))
      ("(#1=1 #1#)"              nil (1 1))
      ("(#1=1 #1# #1#)"          nil (1 1 1))
      ("#1=(#1#)"                nil recursive-cons)
      ("#1=(1 #2=(#2# #1# #2#))" nil recursive-cons2)
      ("#1=#(#1#)"               nil recursive-vector)
      ;; There was a problem leading to unrelated expressions of the
      ;; forms (nil) and (t) being replaced by the fixup processor.
      ("(#1=1 (nil))"            nil (1 (nil)))
      ("(#1=((nil)))"            nil (((nil))))
      ("(#1=1 (t))"              nil (1 (t)))
      ("(#1=((t)))"              nil (((t))))
      ;; With *READ-SUPPRESS* bound to t
      ("#=1"                     t   nil)
      ("(#1=1 #1=2)"             t   nil)
      ("##"                      t   nil)
      ("#1#"                     t   nil))))
