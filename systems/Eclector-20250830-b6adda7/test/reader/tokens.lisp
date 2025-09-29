(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.tokens
  :in :eclector.reader)

(test read-token/smoke
  "Smoke test for the default method on READ-TOKEN."
  (do-stream-input-cases ((input length) eof-error-p eof-value
                          expected &optional (expected-position length)
                                             (expected-length 1))
    (flet ((do-it ()
             (with-stream (stream)
               (let ((*package* (find-package '#:eclector.reader.test))) ; TODO use a client that does not intern in INTERPRET-TOKEN
                 (eclector.reader:read-token
                  t stream eof-error-p eof-value)))))
      (error-case (input expected expected-position expected-length)
        (error (do-it))
        (t (multiple-value-bind (value position) (do-it)
             (expect "value"    (equalp expected          value))
             (expect "position" (eql    expected-position position))))))
    `((,(format nil "~C" #\Backspace)            t   nil eclector.reader:invalid-constituent-character 0)
      (,(format nil "cl-user::~C" #\Backspace)   t   nil eclector.reader:invalid-constituent-character 9)
      (,(format nil "~C" #\Rubout)               t   nil eclector.reader:invalid-constituent-character 0)
      (,(format nil "cl-user::~C" #\Rubout)      t   nil eclector.reader:invalid-constituent-character 9)
      ("a"                                       t   nil |A|)
      ("cl-user::a"                              t   nil cl-user::|A|)
      ("\\"                                      t   nil eclector.reader:unterminated-single-escape-in-symbol 1 0)
      ("cl-user::\\"                             t   nil eclector.reader:unterminated-single-escape-in-symbol 10 0)
      ("\\"                                      nil nil eclector.reader:unterminated-single-escape-in-symbol 1 0)
      ("cl-user::\\"                             nil nil eclector.reader:unterminated-single-escape-in-symbol 10 0)
      ("\\a"                                     t   nil |a|)
      ("cl-user::\\a"                            t   nil cl-user::|a|)
      (,(format nil "\\~C" #\Backspace)          t   nil ,(intern (format nil "~C" #\Backspace)))
      (,(format nil "cl-user::\\~C" #\Backspace) t   nil ,(intern (format nil "~C" #\Backspace)
                                                                  '#:cl-user))

      (,(format nil "a~C" #\Backspace)           t   nil eclector.reader:invalid-constituent-character 1)
      (,(format nil "cl-user::a~C" #\Backspace)  t   nil eclector.reader:invalid-constituent-character 10)
      (,(format nil "a~C" #\Rubout)              t   nil eclector.reader:invalid-constituent-character 1)
      (,(format nil "cl-user::a~C" #\Rubout)     t   nil eclector.reader:invalid-constituent-character 10)
      ("aa"                                      t   nil |AA|)
      ("cl-user::aa"                             t   nil cl-user::|AA|)
      ("a#"                                      t   nil |A#|)
      ("cl-user::a#"                             t   nil cl-user::|A#|)
      ("a\\"                                     t   nil eclector.reader:unterminated-single-escape-in-symbol 2 0)
      ("cl-user::a\\"                            t   nil eclector.reader:unterminated-single-escape-in-symbol 11 0)
      ("a\\"                                     nil nil eclector.reader:unterminated-single-escape-in-symbol 2 0)
      ("cl-user::a\\"                            nil nil eclector.reader:unterminated-single-escape-in-symbol 11 0)
      ("a\\a"                                    t   nil |Aa|)
      ("cl-user::a\\a"                           t   nil cl-user::|Aa|)
      ("a|a|"                                    t   nil |Aa|)
      ("cl-user::a|a|"                           t   nil cl-user::|Aa|)
      ("a,"                                      t   nil |A|           1)
      ("cl-user::a,"                             t   nil cl-user::|A| 10)
      ("a "                                      t   nil |A|           1)
      ("cl-user::a "                             t   nil cl-user::|A| 10)

      ("|"                                       t   nil eclector.reader:unterminated-multiple-escape-in-symbol 1 0)
      ("cl-user::|"                              t   nil eclector.reader:unterminated-multiple-escape-in-symbol 10 0)
      ("|"                                       nil nil eclector.reader:unterminated-multiple-escape-in-symbol 1 0)
      ("cl-user::|"                              nil nil eclector.reader:unterminated-multiple-escape-in-symbol 10 0)
      ("||"                                      t   nil ||)
      ("cl-user::||"                             t   nil cl-user::||)
      ("||a"                                     t   nil |A|)
      ("cl-user::||a"                            t   nil cl-user::|A|)
      ("|a|"                                     t   nil |a|)
      ("cl-user::|a|"                            t   nil cl-user::|a|)
      ("|#|"                                     t   nil |#|)
      ("cl-user::|#|"                            t   nil cl-user::|#|)
      ("|,|"                                     t   nil |,|)
      ("cl-user::|,|"                            t   nil cl-user::|,|)
      ("| |"                                     t   nil | |)
      ("cl-user::| |"                            t   nil cl-user::| |)
      (,(format nil "cl-user::|~C|" #\Backspace) t   nil ,(intern (format nil "~C" #\Backspace)
                                                                  '#:cl-user))
      ("|\\"                                     t   nil eclector.reader:unterminated-single-escape-in-symbol 2 0)
      ("cl-user::|\\"                            t   nil eclector.reader:unterminated-single-escape-in-symbol 11 0)
      ("|\\"                                     nil nil eclector.reader:unterminated-single-escape-in-symbol 2 0)
      ("cl-user::|\\"                            nil nil eclector.reader:unterminated-single-escape-in-symbol 11 0)
      ("|\\|"                                    t   nil eclector.reader:unterminated-multiple-escape-in-symbol 3 0)
      ("cl-user::|\\|"                           t   nil eclector.reader:unterminated-multiple-escape-in-symbol 12 0)
      ("|\\|"                                    nil nil eclector.reader:unterminated-multiple-escape-in-symbol 3 0)
      ("cl-user::|\\|"                           nil nil eclector.reader:unterminated-multiple-escape-in-symbol 12 0)
      ("|\\||"                                   t   nil |\||)
      ("cl-user::|\\||"                          t   nil cl-user::|\||)

      (".\\."                                    t   nil |..|)
      ("cl-user::.\\."                           t   nil cl-user::|..|)
      (".||."                                    t   nil |..|)
      ("cl-user::.||."                           t   nil cl-user::|..|)
      (".||"                                     t   nil |.|)
      ("cl-user::.||"                            t   nil cl-user::|.|)
      ("..a"                                     t   nil |..A|)
      ("cl-user::..a"                            t   nil cl-user::|..A|)

      ("\\a||"                                   t   nil |a|)
      ("cl-user::\\a||"                          t   nil cl-user::|a|)
      ("\\a||b"                                  t   nil |aB|)
      ("cl-user::\\a||b"                         t   nil cl-user::|aB|))))

(test check-symbol-token/smoke
  "Smoke test for the default method on CHECK-SYMBOL-TOKEN."
  (mapc (lambda (arguments-package-expected)
          (destructuring-bind (token escape-ranges marker1 marker2 package
                               &optional signals
                                         (expected-position (length token))
                                         (expected-length 1))
              arguments-package-expected
            (let ((*package* (or package *package*)))
              (flet ((do-it ()
                       (with-input-from-string (stream token)
                         (loop :repeat (length token) :do (read-char stream))
                         (eclector.reader:check-symbol-token
                          nil stream token escape-ranges marker1 marker2))))
                (error-case (token signals expected-position expected-length)
                  (error (do-it))
                  (t (multiple-value-bind (new-token new-marker1 new-marker2)
                         (do-it)
                       (is (equal token new-token))
                       (is (eql   marker1 new-marker1))
                       (is (eql   marker2 new-marker2)))))))))
        '((""                               ()        nil nil nil)
          ("a"                              ()        nil nil nil)
          ("A"                              ()        nil nil nil)
          ("A:"                             ()        1   nil nil eclector.reader:symbol-name-must-not-end-with-package-marker 1)
          ("A::"                            ()        2   nil nil eclector.reader:symbol-name-must-not-end-with-package-marker 2)

          (":"                              ()        nil nil nil)
          (":"                              ()        0   nil nil eclector.reader:symbol-name-must-not-be-only-package-markers 0)
          (":"                              ((1 . 1)) 0   nil nil)
          (":a"                             ()        0   nil nil)
          (":A"                             ()        0   nil nil)
          ("CL:NIL"                         ()        2   nil nil)
          ("ECLECTOR.READER.TEST:INTERNAL"  ()        20  nil nil)

          ("::"                             ()        0   1   nil eclector.reader:symbol-name-must-not-be-only-package-markers 0 2)
          ("::"                             ((2 . 2)) 0   1   nil eclector.reader:two-package-markers-must-not-be-first 0 2)
          ("::A"                            ()        0   1   nil eclector.reader:two-package-markers-must-not-be-first 0 2)
          ("CL::NIL"                        ()        2   3   nil)
          ("ECLECTOR.READER.TEST::INTERNAL" ()        20  21  nil))))

(test interpret-symbol-token/smoke
  "Smoke test for the default method on INTERPRET-SYMBOL-TOKEN."
  (mapc (lambda (arguments-package-expected)
          (destructuring-bind (token marker1 marker2 package expected
                               &optional (expected-position (length token))
                                         (expected-length 1))
              arguments-package-expected
            (let ((*package* (or package *package*)))
              (flet ((do-it ()
                       (with-input-from-string (stream token)
                         (loop :repeat (length token) :do (read-char stream))
                         (eclector.reader:interpret-symbol-token
                          nil stream token marker1 marker2))))
                (error-case (token expected expected-position expected-length)
                  (error (do-it))
                  (t (is (equal expected (do-it)))))))))
        '((""                               nil nil nil ||)
          ("a"                              nil nil nil |a|)
          ("A"                              nil nil nil a)

          (":"                              nil nil nil |:|)
          (":"                              0   nil nil :||)
          (":a"                             0   nil nil :|a|)
          (":A"                             0   nil nil :a)
          ("NP:NIX"                         2   nil nil eclector.reader:package-does-not-exist 0 2)
          ("CL:NIX"                         2   nil nil eclector.reader:symbol-does-not-exist 3 3)
          ("ECLECTOR.READER.TEST:INTERNAL"  20  nil nil eclector.reader:symbol-is-not-external 21 8)
          ("CL:NIL"                         2   nil nil nil)
          ("CL:ABS"                         2   nil nil abs)

          ("NP::NIX"                        2   3   nil eclector.reader:package-does-not-exist 0 2)
          ("ECLECTOR.READER.TEST::INTERNAL" 20  21  nil internal)
          ("CL::NIL"                        2   3   nil nil)
          ("CL::ABS"                        2   3   nil abs))))

(defun do-interpret-token-test-case (arguments-context-expected)
  (destructuring-bind (token token-escapes *read-base* case expected
                       &optional (expected-position (length token))
                                 (expected-length 1))
      arguments-context-expected
    (let ((table (eclector.readtable:copy-readtable
                  eclector.reader:*readtable*)))
      (setf (eclector.readtable:readtable-case table) case)
      (flet ((do-it ()
               (with-input-from-string (stream token)
                 (loop :repeat (length token) :do (read-char stream))
                 (let ((token (copy-seq token))
                       (eclector.reader:*readtable* table))
                   (eclector.reader:interpret-token
                    eclector.base:*client* stream token token-escapes)))))
        (error-case (token expected expected-position expected-length)
          (error (do-it))
          (t
           (unless (or token-escapes
                       (zerop (length token))
                       (eq expected eclector.reader::*consing-dot*))
             (assert (equal expected
                            (let ((*readtable* (copy-readtable))
                                  (*read-default-float-format* 'single-float))
                              (setf (readtable-case *readtable*) case)
                              (read-from-string token)))))
           (is (equal expected (do-it)))))))))

(test interpret-token.default/smoke
  "Smoke test for the default method on INTERPRET-TOKEN."
  (let ((*read-default-float-format* 'single-float))
    (mapc #'do-interpret-token-test-case
          `(;; empty
            (""           ()                10 :upcase   ||)
            ;; Empty escape ranges
            ("a"          ((0 . 0))         10 :upcase   |A|)
            ("a"          ((1 . 1))         10 :upcase   |A|)

            ("abc"        ((0 . 0) (1 . 2)) 10 :upcase   |AbC|)
            ("abc"        ((0 . 0) (1 . 2)) 10 :downcase |abc|)
            ("abc"        ((0 . 0) (1 . 2)) 10 :preserve |abc|)
            ("abc"        ((0 . 0) (1 . 2)) 10 :invert   |AbC|)
            ;; "consing dot"
            ("."          ()                10 :upcase   ,eclector.reader::*consing-dot*)
            (".."         ((1 . 2))         10 :upcase   |..|) ; .\.
            (".."         ((1 . 1))         10 :upcase   |..|) ; .||.
            ("."          ((1 . 1))         10 :upcase   |.|)  ; .||
            ("..a"        ()                10 :upcase   |..A|)
            (".."         ()                10 :upcase   eclector.reader:too-many-dots 0 2)
            ("..."        ()                10 :upcase   eclector.reader:too-many-dots 0 3)
            ;; readtable case
            ("abc"        ()                10 :upcase   |ABC|)
            ("ABC"        ()                10 :upcase   |ABC|)
            ("aBc"        ()                10 :upcase   |ABC|)

            ("abc"        ()                10 :downcase |abc|)
            ("ABC"        ()                10 :downcase |abc|)
            ("aBc"        ()                10 :downcase |abc|)

            ("abc"        ()                10 :preserve |abc|)
            ("ABC"        ()                10 :preserve |ABC|)
            ("aBc"        ()                10 :preserve |aBc|)

            ("abc"        ()                10 :invert   |ABC|)
            ("ABC"        ()                10 :invert   |abc|)
            ("aBc"        ()                10 :invert   |aBc|)
            ;; symbol
            ("a"          ((0 . 1))         10 :upcase   |a|)
            ("-"          ()                10 :upcase   -)
            ("+"          ()                10 :upcase   +)
            ("+1"         ((2 . 2))         10 :upcase   |+1|)

            ("-a"         ((1 . 2))         10 :upcase   |-a|)
            ("-."         ()                10 :upcase   -.)
            ("-:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 1)
            ("-a"         ()                10 :upcase   -a)

            (".a"         ((1 . 2))         10 :upcase   |.a|)
            (".:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 1)
            (".a"         ()                10 :upcase   .a)

            ("-.a"        ((2 . 3))         10 :upcase   |-.a|)
            ("-.:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 2)
            ("-.a"        ()                10 :upcase   -.a)

            ("1"          ((1 . 1))         10 :upcase   |1|)
            ("1a"         ()                10 :upcase   1a)
            ("1a"         ((1 . 2))         10 :upcase   |1a|)
            ("1:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 1)
            ("1.a"        ((2 . 3))         10 :upcase   |1.a|)
            ("1.:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 2)
            ("1.a"        ()                10 :upcase   1.a)

            ("1/"         ()                10 :upcase   1/)
            ("1/1"        ((3 . 3))         10 :upcase   |1/1|)
            ("1/a"        ((2 . 3))         10 :upcase   |1/a|)
            ("1/:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 2)
            ("1/a"        ()                10 :upcase   1/a)
            ("1/0"        ()                10 :upcase   eclector.reader:zero-denominator 2 1)
            ("1/2a"       ((3 . 4))         10 :upcase   |1/2a|)
            ("1/2:"       ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 3)
            ("1/2a"       ()                10 :upcase   1/2a)

            (".1"         ((2 . 2))         10 :upcase   |.1|)
            (".1a"        ((2 . 3))         10 :upcase   |.1a|)
            (".1:"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 2)
            (".1a"        ()                10 :upcase   .1a)

            (".1e"        ()                10 :upcase   .1e)
            (".1e1"       ((4 . 4))         10 :upcase   |.1E1|)
            (".1ea"       ((3 . 4))         10 :upcase   |.1Ea|)
            (".1e:"       ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 3)
            (".1ea"       ()                10 :upcase   .1ea)
            (".1e-"       ()                10 :upcase   .1e-)
            (".1e+"       ()                10 :upcase   .1e+)
            (".1e-a"      ((4 . 5))         10 :upcase   |.1E-a|)
            (".1e-:"      ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 4)
            (".1e-a"      ()                10 :upcase   .1e-a)
            (".1e2a"      ((4 . 5))         10 :upcase   |.1E2a|)
            (".1e2:"      ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 4)
            (".1e2a"      ()                10 :upcase   .1e2a)

            ("1aa"        ((2 . 3))         16 :upcase   |1Aa|)
            ("1a/"        ()                16 :upcase   1a/)
            ("1a:"        ()                16 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 2)
            ("1ax"        ()                16 :upcase   1ax)

            ("aa"         ((1 . 2))         10 :upcase   |Aa|)
            (":"          ()                10 :upcase   eclector.reader:symbol-name-must-not-be-only-package-markers 0)
            (":"          ((1 . 1))         10 :upcase   :||)
            (":"          ((0 . 1))         10 :upcase   |:|)
            ("::"         ()                10 :upcase   eclector.reader:symbol-name-must-not-be-only-package-markers 0 2)
            ("a:"         ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 1)
            ("a:"         ((1 . 2))         10 :upcase   |A:|)
            ("a::"        ()                10 :upcase   eclector.reader:symbol-name-must-not-end-with-package-marker 2)
            ("a:::"       ()                10 :upcase   eclector.reader:symbol-can-have-at-most-two-package-markers 3)
            ("a::bb:cc"   ()                10 :upcase   eclector.reader:symbol-can-have-at-most-two-package-markers 5)
            ("a:a:"       ()                10 :upcase   eclector.reader:two-package-markers-must-be-adjacent 1 3)
            ("a:bb:cc"    ()                10 :upcase   eclector.reader:two-package-markers-must-be-adjacent 1 4)
            ("::a"        ()                10 :upcase   eclector.reader:two-package-markers-must-not-be-first 0 2)
            ("keyword:b"  ()                10 :upcase   :b)
            ("keyword::b" ()                10 :upcase   :b)
            ;; decimal-integer
            ("1"          ()                10 :upcase      1)
            ("-1"         ()                10 :upcase     -1)
            ("-12"        ()                10 :upcase    -12)

            ("1."         ()                10 :upcase      1)
            ;; ratio
            ("+1/2"       ()                10 :upcase      1/2)
            ("-1/2"       ()                10 :upcase     -1/2)
            ("1/2"        ()                10 :upcase      1/2)
            ("1/23"       ()                10 :upcase      1/23)
            ;; float-no-exponent
            ("-.0"        ()                10 :upcase      -.0f0)
            ("+.0"        ()                10 :upcase      +.0f0)
            ("+.234"      ()                10 :upcase       .234f0)
            ("-.234"      ()                10 :upcase      -.234f0)
            (".234"       ()                10 :upcase       .234f0)
            ("-0.0"       ()                10 :upcase      -.0f0)
            ("+0.0"       ()                10 :upcase      +.0f0)
            ("+1.234"     ()                10 :upcase      1.234f0)
            ("-1.234"     ()                10 :upcase     -1.234f0)
            ("1.234"      ()                10 :upcase      1.234f0)
            ;; float-exponent
            ("-.0e1"      ()                10 :upcase      -.0f0)
            ("+.0e1"      ()                10 :upcase      +.0f0)
            ("-0.0e1"     ()                10 :upcase      -.0f0)
            ("+0.0e1"     ()                10 :upcase      +.0f0)
            ("+1.0e2"     ()                10 :upcase    100.0f0)
            ("+1.0e-2"    ()                10 :upcase      0.01f0)
            ("-1.0e2"     ()                10 :upcase   -100.0f0)
            ("-1.0e-2"    ()                10 :upcase     -0.01f0)
            ("1.0e2"      ()                10 :upcase    100.0f0)
            ("1.0e-2"     ()                10 :upcase      0.01f0)
            ("1.e2"       ()                10 :upcase    100.0f0)
            ("1e2"        ()                10 :upcase    100.0f0)
            ("1e01"       ()                10 :upcase     10.0f0)
            ;; Nondefault *READ-BASE*
            ("2"          ()                2  :upcase    |2|)
            ("2/1"        ()                2  :upcase    |2/1|)
            ("2."         ()                2  :upcase    2)

            ("a"          ()                16 :upcase     10)
            ("-a"         ()                16 :upcase    -10)
            ("1a"         ()                16 :upcase     26)
            ("1aa"        ()                16 :upcase    426)

            ("10."        ()                16 :upcase    10)

            ("1111.1111"  ()                4  :upcase    1111.1111f0)
            ("1111.1234"  ()                4  :upcase    1111.1234f0)
            ("1111.4321"  ()                4  :upcase    1111.4321f0)
            ("1111.4444"  ()                4  :upcase    1111.4444f0)

            ("1234.1111"  ()                4  :upcase    1234.1111f0)
            ("1234.1234"  ()                4  :upcase    1234.1234f0)
            ("1234.4321"  ()                4  :upcase    1234.4321f0)
            ("1234.4444"  ()                4  :upcase    1234.4444f0)

            ("4321.1111"  ()                4  :upcase    4321.1111f0)
            #-ccl ("4321.1234"  ()                4  :upcase    4321.1234f0)
            ("4321.4321"  ()                4  :upcase    4321.4321f0)
            ("4321.4444"  ()                4  :upcase    4321.4444f0)

            ("4444.1111"  ()                4  :upcase    4444.1111f0)
            ;; TODO CCL's FLOAT is different:
            ;;
            ;;   (float (/ (+ 44440000 1234) 10000) 1.0f0) => 4444.123 (not 4444.1235)
            ;;
            ;; Maybe implement a different algorithm (SCALE-FLOAT?) for
            ;; building float results.
            ;;
            ;; Note that CCL's reader produces the "right" result
            ;; (i.e. 4444.1235) so there has to be a trick to do it in
            ;; CCL.
            #-ccl ("4444.1234"  ()                4  :upcase    4444.1234f0)
            ("4444.4321"  ()                4  :upcase    4444.4321f0)
            ("4444.4444"  ()                4  :upcase    4444.4444f0)))))

(test interpret-token.default/default-float-format
  "Test default float format handling in the default method on INTERPRET-TOKEN."
  (let ((eclector.base:*client* (make-instance 'invalid-float-format-client)))
    (mapc #'do-interpret-token-test-case
          '(("1.0" () 10 :upcase eclector.reader:invalid-default-float-format 3 0)
            ("1e0" () 10 :upcase eclector.reader:invalid-default-float-format 1 1)))))
