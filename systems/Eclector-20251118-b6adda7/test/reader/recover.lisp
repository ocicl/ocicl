(cl:in-package #:eclector.reader.test)

(def-suite* :eclector.reader.recover
  :in :eclector.reader)

(defun read-with-context-and-client (stream &key (client (make-instance
                                                          'sharpsign-s-client)))
  (let ((eclector.base:*client* client))
    (eclector.reader:read stream nil)))

(test recover/smoke
  "Test recovering from various syntax errors."
  (mapc (alexandria:rcurry #'do-recover-test-case
                           #'read-with-context-and-client)
        `(;; Recover from invalid syntax in symbols.
          (":"                                (eclector.reader:symbol-name-must-not-be-only-package-markers) |:|)
          ("::"                               (eclector.reader:symbol-name-must-not-be-only-package-markers) |::|)
          (,(format nil ":foo~C" #\Backspace) (eclector.reader:invalid-constituent-character)                :foo_)
          (":fo\\"                            (eclector.reader:unterminated-single-escape-in-symbol)         :fo)
          (":fo|o"                            (eclector.reader:unterminated-multiple-escape-in-symbol)       :fo|o|)
          ("foo:"                             (eclector.reader:symbol-name-must-not-end-with-package-marker) foo|:|)
          (":foo:bar"                         (eclector.reader:two-package-markers-must-be-adjacent)         :foo|:|bar)
          ("::foo"                            (eclector.reader:two-package-markers-must-not-be-first)        :foo)
          ("eclector.reader.test:::foo"       (eclector.reader:symbol-can-have-at-most-two-package-markers)  |:|foo)
          (".."                               (eclector.reader:too-many-dots)                                |..|)
          ("..."                              (eclector.reader:too-many-dots)                                |...|)

          ;; Recover from problems with syntactically valid symbols.
          ("no-such-package::symbol"             (eclector.reader:package-does-not-exist) #:symbol)
          ("eclector.reader.test:internal"       (eclector.reader:symbol-is-not-external) internal)
          ("eclector.reader.test:no-such-symbol" (eclector.reader:symbol-does-not-exist)  nil)

          ;; Recover from invalid number tokens.
          ("3/0" (eclector.reader:zero-denominator) 3)

          ;; Single quote
          ("'"         (eclector.reader:end-of-input-after-quote) 'nil)
          ("(')"       (eclector.reader:object-must-follow-quote) ('nil))

          ;; Double quote
          ("\""    (eclector.reader:unterminated-string)                  "")
          ("\"ab"  (eclector.reader:unterminated-string)                  "ab")
          ("\"a\\" (eclector.reader:unterminated-single-escape-in-string
                    eclector.reader:unterminated-string)                  "a")

          ;; Recover from quasiquotation-related errors.
          ("`"         (eclector.reader:end-of-input-after-backquote) (eclector.reader:quasiquote nil))
          ("(`)"       (eclector.reader:object-must-follow-backquote) ((eclector.reader:quasiquote nil)))

          ("`(1 ,)"    (eclector.reader:object-must-follow-unquote)   (eclector.reader:quasiquote
                                                                       (1 (eclector.reader:unquote nil))))
          ("`,"        (eclector.reader:end-of-input-after-unquote)   (eclector.reader:quasiquote
                                                                       (eclector.reader:unquote nil)))

          ("`#C(,1 2)" (eclector.reader:unquote-in-invalid-context)   (eclector.reader:quasiquote
                                                                       #C(1 2)))
          ("#C(`,1 2)" (eclector.reader:backquote-in-invalid-context
                        eclector.reader:unquote-not-inside-backquote)
                                                                      #C(1 2))

          ;; Recover from list-related errors
          ("("         (eclector.reader:unterminated-list)                      ())
          ("(1 2"      (eclector.reader:unterminated-list)                      (1 2))
          ("(1 ."      (eclector.reader:end-of-input-after-consing-dot
                        eclector.reader:unterminated-list)
                                                                                (1 . 1))
          ("(.)"       (eclector.reader:invalid-context-for-consing-dot)        ())
          ("(1 .)"     (eclector.reader:object-must-follow-consing-dot)         (1 . 1))
          ("(1 . 2 3)" (eclector.reader:multiple-objects-following-consing-dot) (1 . 2))
          (")(1)"      (eclector.reader:invalid-context-for-right-parenthesis)  (1))

          ;; Recover from errors related to read-time evaluation.
          ("#."             (eclector.reader:end-of-input-after-sharpsign-dot)       nil)
          ("(#.)"           (eclector.reader:object-must-follow-sharpsign-dot)       (nil))
          ("#.(error \"\")" (eclector.reader:read-time-evaluation-error)             nil)

          ("#1.1"           (eclector.reader:numeric-parameter-supplied-but-ignored) 1)

          ;; Recover from vector-related errors
          ("#("        (eclector.reader:unterminated-vector)             #())
          ("#(1 2"     (eclector.reader:unterminated-vector)             #(1 2))
          ("#(.)"      (eclector.reader:invalid-context-for-consing-dot) #())
          ("#(1 .)"    (eclector.reader:invalid-context-for-consing-dot) #(1))
          ("#(1 . 2)"  (eclector.reader:invalid-context-for-consing-dot) #(1 2))
          ("#1()"      (eclector.reader:no-elements-found)               #())
          ("#1(1 2)"   (eclector.reader:too-many-elements)               #(1))

          ;; Recover from errors in SHARPSIGN-BACKSLASH
          ("#\\"         (eclector.reader:end-of-input-after-backslash)                   #\?)
          ("#\\a\\"      (eclector.reader:unterminated-single-escape-in-character-name)   #\a)
          ("#\\a|"       (eclector.reader:unterminated-multiple-escape-in-character-name) #\a)
          ("#\\Return\\" (eclector.reader:unterminated-single-escape-in-character-name)   #\Return)
          ("#\\Return|"  (eclector.reader:unterminated-multiple-escape-in-character-name) #\Return)
          ("#\\Nosuch"   (eclector.reader:unknown-character-name)                         #\?)
          ("#\\Nosuch\\" (eclector.reader:unterminated-single-escape-in-character-name
                          eclector.reader:unknown-character-name)
                                                                                          #\?)

          ("#1\\a"       (eclector.reader:numeric-parameter-supplied-but-ignored)         #\a)

          ;; Recover from errors in READ-RATIONAL.
          ("#b"      (eclector.reader:end-of-input-before-digit)                   1)
          ("#b)"     (eclector.reader:digit-expected)                              #b0     2)
          ("#b|"     (eclector.reader:digit-expected)                              #b0     2)
          ("#b121"   (eclector.reader:digit-expected)                              #b111)
          ("#b1/"    (eclector.reader:end-of-input-before-digit)                   #b1/1)
          ("#b1/)"   (eclector.reader:digit-expected)                              #b1     4)
          ("#b1/|"   (eclector.reader:digit-expected)                              #b1     4)
          ("#b1/1|"  (eclector.reader:digit-expected)                              #b1     5)
          ("#b1/121" (eclector.reader:digit-expected)                              #b1/111 7)
          ("#b1/0"   (eclector.reader:zero-denominator)                            #b1/1)

          ("#37rz"   (eclector.reader:invalid-radix)                               35)
          ("#1rz"    (eclector.reader:invalid-radix)                               35)

          ("#1b10"   (eclector.reader:numeric-parameter-supplied-but-ignored)      #b10)

          ("#rz"     (eclector.reader:numeric-parameter-not-supplied-but-required) 35)

          ;; Recover from errors related to bit-vector literals
          ("#1*"       (eclector.reader:no-elements-found) #*)
          ("#1*11"     (eclector.reader:too-many-elements) #*1)
          ("#*021"     (eclector.reader:digit-expected)    #*001)

          ;; Recover from block-comment-related errors
          ("#|"    (eclector.reader:unterminated-block-comment)             nil)
          ("#|foo" (eclector.reader:unterminated-block-comment)             nil)

          ("#1||#" (eclector.reader:numeric-parameter-supplied-but-ignored) nil)

          ;; Recover from errors related to SHARPSIGN-SINGLE-QUOTE
          ("#'"   (eclector.reader:end-of-input-after-sharpsign-single-quote) nil)
          ("(#')" (eclector.reader:object-must-follow-sharpsign-single-quote) (nil))

          ("#1'+" (eclector.reader:numeric-parameter-supplied-but-ignored)    (function +))

          ;; Recover from general array-related errors
          ("#2A"            (eclector.reader:end-of-input-after-sharpsign-a)              #2A())
          ("(#2A)"          (eclector.reader:object-must-follow-sharpsign-a)              (#2A()))
          ("#2A("           (eclector.reader:unterminated-list)                           #2A())
          ("#2A(1)"         (eclector.reader:read-object-type-error)                      #2A())
          ("#2A((1) (1 2))" (eclector.reader:incorrect-initialization-length)             #2A())

          ("#A(1 2)"        (eclector.reader:numeric-parameter-not-supplied-but-required) #0A(1 2))

          ;; Recover from errors related to uninterned symbols
          ("#::foo" (eclector.reader:uninterned-symbol-must-not-contain-package-marker) #:|:|foo)
          ("#:foo:" (eclector.reader:uninterned-symbol-must-not-contain-package-marker) #:foo|:|)
          ("#:fo\\" (eclector.reader:unterminated-single-escape-in-symbol)              #:fo)
          ("#:fo|o" (eclector.reader:unterminated-multiple-escape-in-symbol)            #:fo|o|)

          ("#1:foo" (eclector.reader:numeric-parameter-supplied-but-ignored)            #:foo)

          ;; Recover from complex-related errors
          ("#C"          (eclector.reader:end-of-input-after-sharpsign-c)         #C(1 1))
          ("#C1"         (eclector.reader:non-list-following-sharpsign-c)         #C(1 1))
          ("#C||"        (eclector.reader:non-list-following-sharpsign-c)         #C(1 1))
          ("#C)"         (eclector.reader:complex-parts-must-follow-sharpsign-c)  #C(1 1) 2)
          ("#C("         (eclector.reader:end-of-input-before-complex-part)       #C(1 1))
          ("#C()"        (eclector.reader:complex-part-expected)                  #C(1 1))
          ("#C(2"        (eclector.reader:end-of-input-before-complex-part)       #C(2 1))
          ("#C(2)"       (eclector.reader:complex-part-expected)                  #C(2 1))
          ("#C(2 3"      (eclector.reader:unterminated-list)                      #C(2 3))
          ("#C(2 3 4)"   (eclector.reader:too-many-complex-parts)                 #C(2 3))
          ("#C(2 3 4 5)" (eclector.reader:too-many-complex-parts)                 #C(2 3))
          ("#C(. 2 3)"   (eclector.reader:invalid-context-for-consing-dot)        #C(2 3))
          ("#C(2 . 3)"   (eclector.reader:invalid-context-for-consing-dot)        #C(2 3))
          ("#C(2 3 .)"   (eclector.reader:invalid-context-for-consing-dot)        #C(2 3))
          ("#C(2 3 . 4)" (eclector.reader:invalid-context-for-consing-dot
                          eclector.reader:too-many-complex-parts)
                                                                                  #C(2 3))
          ("#C(#\\a 2)"  (eclector.reader:read-object-type-error)                 #C(1 2))

          ("#1C(2 3)"    (eclector.reader:numeric-parameter-supplied-but-ignored) #C(2 3))

          ;; Recover from structure-literal-related errors
          ("#S"            (eclector.reader:end-of-input-after-sharpsign-s)                nil)
          ("#S1"           (eclector.reader:non-list-following-sharpsign-s)                nil)
          ("#S1"           (eclector.reader:non-list-following-sharpsign-s)                nil)
          ("#S)"           (eclector.reader:structure-constructor-must-follow-sharpsign-s) nil 2)
          ("#S("           (eclector.reader:end-of-input-before-structure-type-name)       nil)
          ("#S()"          (eclector.reader:no-structure-type-name-found)                  nil)
          ("#S(.)"         (eclector.reader:invalid-context-for-consing-dot
                            eclector.reader:no-structure-type-name-found)
                                                                                           nil)
          ("#S(foo .)"     (eclector.reader:invalid-context-for-consing-dot)               (foo))
          ("#S(1)"         (eclector.reader:structure-type-name-is-not-a-symbol)           nil)
          ("#S(foo"        (eclector.reader:end-of-input-before-slot-name)                 (foo))
          ("#S(foo 1)"     (eclector.reader:slot-name-is-not-a-string-designator
                            eclector.reader:no-slot-value-found)
                                                                                           (foo))
          ("#S(foo 1 2)"   (eclector.reader:slot-name-is-not-a-string-designator)          (foo))
          ("#S(foo :bar"   (eclector.reader:end-of-input-before-slot-value)                (foo))
          ("#S(foo :bar)"  (eclector.reader:no-slot-value-found)                           (foo))
          ("#S(foo :bar 1" (eclector.reader:end-of-input-before-slot-name)                 (foo :bar 1))

          ("#1S(foo)"      (eclector.reader:numeric-parameter-supplied-but-ignored)        (foo))

          ("#S#.'(1)"      (eclector.reader:structure-type-name-is-not-a-symbol)           nil 8)

          ;; Recover from errors related to pathname literals
          ("#P"       (eclector.reader:end-of-input-after-sharpsign-p)         #P".")
          ("(#P)"     (eclector.reader:namestring-must-follow-sharpsign-p)     (#P"."))
          ("#P1"      (eclector.reader:non-string-following-sharpsign-p)       #P".")

          ("#1P\".\"" (eclector.reader:numeric-parameter-supplied-but-ignored) #P".")

          ;; Recover from errors related to feature-expressions
          ("#+"               (eclector.reader:end-of-input-after-sharpsign-plus-minus
                               eclector.reader:end-of-input-after-feature-expression)
                                                                                                   nil)
          ("(#+)"             (eclector.reader:feature-expression-must-follow-sharpsign-plus-minus
                               eclector.reader:object-must-follow-feature-expression)
                                                                                                   (nil))
          ("#+(and)"          (eclector.reader:end-of-input-after-feature-expression)              nil)
          ("(#+(and))"        (eclector.reader:object-must-follow-feature-expression)              (nil))

          ("#+1 :foo"         (eclector.reader:feature-expression-type-error)                      nil)
          ("#+(not a b) :foo" (eclector.reader:single-feature-expected)                            nil)

          ("#1+(and) :foo"    (eclector.reader:numeric-parameter-supplied-but-ignored)             :foo)

          ;; Recover from reference-related errors
          ("#1="         (eclector.reader:end-of-input-after-sharpsign-equals)           nil)
          ("(#1=)"       (eclector.reader:object-must-follow-sharpsign-equals)           (nil))
          ("(#1=1 #1=2)" (eclector.reader:sharpsign-equals-label-defined-more-than-once) (1 2))
          ("#1=#1#"      (eclector.reader:sharpsign-equals-only-refers-to-self)          nil)
          ("#=1"         (eclector.reader:numeric-parameter-not-supplied-but-required)   1)

          ("#1#"         (eclector.reader:sharpsign-sharpsign-undefined-label)           nil)
          ("##"          (eclector.reader:numeric-parameter-not-supplied-but-required)   nil)

          ;; Recover from missing, undefined and invalid # sub-characters
          ("#"   (eclector.reader:unterminated-dispatch-macro)    nil)
          ("#1"  (eclector.reader:unterminated-dispatch-macro)    nil)
          ("#!"  (eclector.readtable:unknown-macro-sub-character) nil)
          ("#1!" (eclector.readtable:unknown-macro-sub-character) nil)
          ("#<"  (eclector.reader:sharpsign-invalid)              nil)
          ("#1<" (eclector.reader:sharpsign-invalid)              nil)

          ;; Multiple subsequent recoveries needed.
          ("(1 (2"     (eclector.reader:unterminated-list
                        eclector.reader:unterminated-list)
                                                            (1 (2)))
          ("(1 \"a"    (eclector.reader:unterminated-string
                        eclector.reader:unterminated-list)
                                                            (1 "a")))))

(test recover-from-invalid-float-format
  "Test recovering from invalid value of *READ-DEFAULT-FLOAT-FORMAT*."
  (let ((client (make-instance 'invalid-float-format-client)))
    (mapc (alexandria:rcurry
           #'do-recover-test-case
           (alexandria:rcurry #'read-with-context-and-client :client client))
          '(("1.0" (eclector.reader:invalid-default-float-format) 1.0f0)
            ("1e0" (eclector.reader:invalid-default-float-format) 1.0f0)))))

(test recover/package-problems
  "Test recovering form non-existing packages, symbols and similar problems."
  (labels ((check-restart (name)
             (let* ((restart (find-restart name))
                    (report (princ-to-string restart)))
               (is-false (alexandria:emptyp report))))
           (invoke-restart-interactively* (restart-name)
             ;; CCL prints a message about invoking the restart to
             ;; *ERROR-OUTPUT*.
             (let ((*error-output* (make-broadcast-stream)))
               (invoke-restart-interactively restart-name)))
           (invoke-with-input (restart-name input)
             (let* ((output (make-string-output-stream))
                    (*query-io* (make-two-way-stream
                                 (make-string-input-stream input) output)))
               (invoke-restart-interactively* restart-name)
               (is-false (alexandria:emptyp (get-output-stream-string output)))))
           (test-restart1 (input condition-type expected handler)
             (let ((result
                     (handler-bind
                         ((error (lambda (condition)
                                   (is-true (typep condition condition-type))
                                   (funcall handler))))
                       (eclector.reader:read-from-string input))))
               (etypecase expected
                 (symbol
                  (is (eq expected result)))
                 (cons
                  (destructuring-bind (expected-package expected-name) expected
                    (is (string= expected-package
                                 (package-name (symbol-package result))))
                    (is (string= expected-name
                                 (symbol-name result))))))))
           (test-restart (input condition-type expected handler1 handler2)
             (test-restart1 input condition-type expected handler1)
             (test-restart1 input condition-type expected handler2)))
    ;; For PACKAGE-DOES-NOT-EXIST, invoke USE-VALUE restart.
    (test-restart
     "no-such-package::symbol" 'eclector.reader:package-does-not-exist 'cl:symbol
     (lambda ()
       (check-restart 'use-value)
       (invoke-restart 'use-value 'cl:symbol))
     (lambda ()
       (invoke-with-input 'use-value "cl:symbol")))
    ;; For PACKAGE-DOES-NOT-EXIST, invoke USE-PACKAGE restart.
    (test-restart
     "no-such-package::symbol" 'eclector.reader:package-does-not-exist 'cl:symbol
     (lambda ()
       (check-restart 'use-package)
       (invoke-restart 'use-package (find-package '#:cl)))
     (lambda ()
       (invoke-with-input 'use-package "CL")))
    ;; For SYMBOL-DOES-NOT-EXIST, invoke USE-VALUE restart.
    (test-restart
     "eclector.reader.test:no-such-symbol" 'eclector.reader:symbol-does-not-exist
     'cl:symbol
     (lambda ()
       (check-restart 'use-value)
       (invoke-restart 'use-value 'cl:symbol))
     (lambda ()
       (invoke-with-input 'use-value "cl:symbol")))
    (test-restart1
     "eclector.reader.test:no-such-symbol" 'eclector.reader:symbol-does-not-exist
     '("ECLECTOR.READER.TEST" "NO-SUCH-SYMBOL")
     (lambda ()
       (check-restart 'intern)
       (invoke-restart 'intern)))
    (multiple-value-bind (symbol status)
        (find-symbol "NO-SUCH-SYMBOL" "ECLECTOR.READER.TEST")
      (is (eq :internal status))
      (unintern symbol (symbol-package symbol)))
    ;; For SYMBOL-IS-NOT-EXTERNAL, invoke USE-VALUE restart.
    (test-restart
     "eclector.reader.test:test-restart" 'eclector.reader:symbol-is-not-external
     'cl:symbol
     (lambda ()
       (check-restart 'use-value)
       (invoke-restart 'use-value 'cl:symbol))
     (lambda ()
       (invoke-with-input 'use-value "cl:symbol")))
    ;; For SYMBOL-IS-NOT-EXTERNAL, invoke USE-ANYWAY restart.
    (test-restart
     "eclector.reader.test:test-restart" 'eclector.reader:symbol-is-not-external
     'test-restart
     (lambda ()
       (check-restart 'eclector.reader::use-anyway)
       (invoke-restart 'eclector.reader::use-anyway))
     (lambda ()
       (invoke-restart-interactively* 'eclector.reader::use-anyway)))))
