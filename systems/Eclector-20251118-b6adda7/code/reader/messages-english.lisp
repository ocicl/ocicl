(cl:in-package #:eclector.reader)

;;; Recovery strategy descriptions

(macrolet ((define-description (strategy description)
             `(defmethod recovery-description-using-language
                  ((strategy (eql ',strategy)) (language acclimation:english))
                ,description)))
  (define-description skip-token                   "Skip the invalid token.")

  (define-description use-partial-symbol           "Return a symbol named by the already read characters.")
  (define-description replace-invalid-character    "Replace the invalid character with a valid one.")
  (define-description treat-as-escaped             "Treat the character as if it had been escaped.")
  (define-description use-uninterned-symbol        (lambda (stream package-name symbol-name)
                                                     (format stream "Use an uninterned symbol named \"~A\" and ~
                                                                     ignore the non-existent \"~A\" package."
                                                             symbol-name package-name)))
  (define-description use-replacement-package      (lambda (stream package-name)
                                                     (format stream "Specify a package to use in place of the ~
                                                                     non-existent \"~A\" package."
                                                             package-name)))
  (define-description use-replacement-symbol       (lambda (stream package symbol-name)
                                                     (format stream "Specify a symbol to use in place of the ~
                                                                     non-existent \"~A\" in package ~A."
                                                             symbol-name package)))
  (define-description intern                       (lambda (stream package symbol-name)
                                                     (format stream "Intern a symbol named \"~A\" in package ~A."
                                                             symbol-name package)))
  (define-description use-anyway                   (lambda (stream package symbol-name)
                                                     (format stream "Use the unexported symbol named \"~A\" in ~
                                                                     package ~A anyway."
                                                             symbol-name package)))

  (define-description replace-invalid-digit        "Use a suitable digit in place of the invalid digit.")
  (define-description use-replacement-radix        "Use a suitable radix in place of the invalid radix.")
  (define-description use-replacement-float-format "Use a suitable float format in place of the invalid one.")

  (define-description ignore-quasiquote            "Read the following form as if it were not quasiquoted.")
  (define-description ignore-unquote               "Read the following form as if it were not unquoted.")
  (define-description ignore-missing-delimiter     "Ignore the missing delimiter.")
  (define-description use-partial-string           "Return a string of the already read characters.")
  (define-description inject-nil                   "Use NIL in place of the missing object.")
  (define-description use-replacement-object       (lambda (stream replacement-object)
                                                     (format stream "Use the object ~S in place of the missing object."
                                                             replacement-object)))
  (define-description ignore-object                "Ignore the object.")
  (define-description use-partial-list             "Return a list of the already read elements.")
  (define-description ignore-trailing-right-paren  "Ignore the trailing right parenthesis.")

  (define-description ignore-parameter             "Ignore the invalid numeric parameter.")
  (define-description use-replacement-parameter    "Use a valid numeric parameter in place of the missing one.")

  (define-description use-replacement-character    (lambda (stream replacement-character)
                                                     (format stream "Use the character ~:C in place of the invalid one."
                                                             replacement-character)))
  (define-description use-partial-character-name   "Use the already read part of the character name.")

  (define-description use-empty-vector             "Return an empty vector.")
  (define-description use-partial-vector           "Return a vector of the already read elements.")
  (define-description ignore-excess-elements       "Use the already read elements and ignore the excess elements.")

  (define-description use-empty-array              "Use an empty array in place of the invalid one.")

  (define-description use-replacement-part         "Use a replacement part in place of the invalid part.")
  (define-description use-partial-complex          "Complete the complex number using default values for missing parts.")
  (define-description ignore-excess-parts          "Use the already read parts and ignore the excess parts.")

  (define-description use-partial-initargs         "Use already read structure type name and initargs.")
  (define-description skip-slot                    "Skip the invalid slot.")

  (define-description replace-namestring           "Use a suitable namestring in place of the invalid one.")

  (define-description treat-as-false               "Treat the feature expression as false.")

  (define-description ignore-label                 "Read the following object as is if there was no label."))

;;; Contexts and condition reporters

(macrolet
    ((define-reporter (((condition-var condition-specializer) stream-var
                        &optional (language-var 'language))
                       &body body)
       `(defmethod acclimation:report-condition
            ((,condition-var ,condition-specializer)
             ,stream-var
             (,language-var acclimation:english))
          ,@body))
     (define-context (context name)
       `(defmethod context-name ((context  (eql ',context))
                                 (language acclimation:english))
          ,name)))

;;; Conditions related to reader state

  (define-reporter ((condition state-value-type-error) stream)
    (format stream "~@<The supplied value ~S is not of the type~@[ ~S~] ~
                    required for the ~A reader state aspect.~@:>"
            (type-error-datum condition)
            (type-error-expected-type condition)
            (aspect condition)))

;;; Type error

  (define-reporter ((condition read-object-type-error) stream)
    (format stream "~@<The read object ~S is not of the required type ~S.~@:>"
            (type-error-datum condition)
            (type-error-expected-type condition)))

;;; Conditions related to symbols

  (define-reporter ((condition package-does-not-exist) stream)
    (format stream "~@<Package named ~S does not exist.~@:>"
            (desired-package-name condition)))

;;; Conditions related to symbols

  (flet ((package-name* (package)
           ;; PACKAGE may be a `cl:package' but could also be a
           ;; client-defined representation of a package.
           (typecase package
             (package (package-name package))
             (t package))))

    (define-reporter ((condition symbol-does-not-exist) stream)
      (format stream "~@<Symbol named ~S not found in the ~A package.~@:>"
              (desired-symbol-name condition)
              (package-name* (desired-symbol-package condition))))

    (define-reporter ((condition symbol-is-not-external) stream)
      (format stream "~@<Symbol named ~S is not external in the ~A package.~@:>"
              (desired-symbol-name condition)
              (package-name* (desired-symbol-package condition)))))

  (define-reporter ((condition invalid-constituent-character) stream)
    (format stream "~@<The ~/eclector.base::describe-character-english/ must ~
                    not occur in a symbol as it is an invalid ~
                    constituent.~@:>"
            (aref (token condition) 0)))

  (define-reporter ((condition unterminated-single-escape-in-symbol) stream)
    (format stream "~@<While reading symbol, expected character after ~
                    the ~/eclector.base::describe-character-english/ ~
                    when input ended.~@:>"
            (escape-char condition)))

  (define-reporter ((condition unterminated-multiple-escape-in-symbol) stream)
    (format stream "~@<While reading symbol, expected the ~
                    ~/eclector.base::describe-character-english/ when ~
                    input ended.~@:>"
            (delimiter condition)))

  (define-reporter ((condition symbol-name-must-not-be-only-package-markers) stream)
    (format stream "~@<Symbol name without any escapes must not consist ~
                    solely of package markers (: characters).~@:>"))

  (define-reporter ((condition symbol-name-must-not-end-with-package-marker) stream)
    (format stream "~@<Symbol name must not end with a package ~
                    marker (the : character).~@:>"))

  (define-reporter ((condition two-package-markers-must-be-adjacent) stream)
    (format stream "~@<If a symbol token contains two package markers, ~
                    they must be adjacent as in package::symbol.~@:>"))

  (define-reporter ((condition two-package-markers-must-not-be-first) stream)
    (format stream "~@<A symbol token must not start with two package ~
                    markers as in ::name.~@:>"))

  (define-reporter ((condition symbol-can-have-at-most-two-package-markers) stream)
    (format stream "~@<A symbol token must not contain more than two ~
                    package markers as in package:::symbol or ~
                    package::first:rest.~@:>"))

  (define-reporter ((condition uninterned-symbol-must-not-contain-package-marker) stream)
    (format stream "~@<A symbol token following #: must not contain a ~
                    package marker.~@:>"))

;;; General reader macro conditions

  (define-reporter ((condition sharpsign-invalid) stream)
    (format stream "~@<The ~/eclector.base::describe-character-english/ is not ~
                    a valid sub-character for the # dispatch macro.~@:>"
            (character-found condition)))

  (define-reporter ((condition numeric-parameter-supplied-but-ignored) stream)
    (format stream "~@<Dispatch reader macro ~A was supplied with a ~
                    numeric parameter it does not accept.~@:>"
            (macro-name condition)))

  (define-reporter ((condition numeric-parameter-not-supplied-but-required) stream)
    (format stream "~@<Dispatch reader macro ~A requires a numeric ~
                    parameter, but none was supplied.~@:>"
            (macro-name condition)))

;;; Conditions related to quotation

  (define-context sharpsign-single-quote "the function reader macro")

  (define-reporter ((condition end-of-input-after-quote) stream)
    (format stream "~@<While reading quote, expected quoted material when ~
                    input ended.~@:>"))

  (define-reporter ((condition object-must-follow-quote) stream)
    (format stream "~@<An object must follow quote.~@:>"))

;;; Conditions related to strings

  (define-reporter ((condition unterminated-string) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on non-default character.
    (format stream "~@<While reading string, expected the ~
                    ~/eclector.base::describe-character-english/ when input ~
                    ended.~@:>"
            (delimiter condition)))

  (define-reporter ((condition unterminated-single-escape-in-string) stream)
    (format stream "~@<While reading string, expected character after ~
                    the escape ~
                    ~/eclector.base::describe-character-english/ ~
                    when input ended.~@:>"
            (escape-char condition)))

;;; Conditions related to quasiquotation

  (define-reporter ((condition backquote-in-invalid-context) stream)
    (format stream "~@<Backquote is illegal in ~A.~@:>"
            (context-name (context condition) language)))

  (define-reporter ((condition object-must-follow-backquote) stream)
    (format stream "~@<An object must follow backquote.~@:>"))

  (define-reporter ((condition end-of-input-after-backquote) stream)
    (format stream "~@<While reading backquote, expected an object when ~
                    input ended.~@:>"))

  (define-reporter ((condition unquote-not-inside-backquote) stream)
    (format stream "~@<~:[Unquote~;Splicing unquote~] not inside backquote.~@:>"
            (splicing-p condition)))

  (define-reporter ((condition unquote-in-invalid-context) stream)
    (format stream "~@<~:[Unquote~;Splicing unquote~] is illegal in ~A.~@:>"
            (splicing-p condition)
            (context-name (context condition) language)))

  (define-reporter ((condition end-of-input-after-unquote) stream)
    (format stream "~@<While reading ~:[~;splicing ~]unquote, expected an ~
                    object when input ended.~@:>"
            (splicing-p condition)))

  (define-reporter ((condition object-must-follow-unquote) stream)
    (format stream "~@<An object must follow a~:[~; splicing~] unquote.~@:>"
            (splicing-p condition)))

  (define-reporter ((condition unquote-splicing-in-dotted-list) stream)
    (format stream "~@<Splicing unquote at end of list (like a . ,@b).~@:>"))

  (define-reporter ((condition unquote-splicing-at-top) stream)
    (format stream "~@<Splicing unquote as backquote form (like `,@foo).~@:>"))

  (define-reporter
      ((condition unquote-not-inside-backquote-during-macroexpansion) stream)
    (let ((argument (argument condition)))
      (multiple-value-bind (operator description reader-macro)
          (if (splicing-p condition)
              (values 'unquote-splicing "A splicing unquote" '(",@" ",."))
              (values 'unquote          "An unquote"         '(",")))
        (format stream "~@<~A expression (indicated by a call of the ~S ~
                        macro or the ~{\"~A\"~^ or ~} reader macro) with inner ~
                        expression ~S occurred outside a quasiquote template ~
                        (established by a call of the ~S macro or the \"`\" ~
                        reader macro). This can happen when the unquoted ~
                        expression is produced in one context and then ~
                        \"injected\" into some other context, for example via ~
                        an object reference #N# or read-time evaluation ~
                        #.(...).~:@>"
                description operator reader-macro argument 'quasiquote))))

;;; Conditions related to lists

  (define-reporter ((condition unterminated-list) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on a non-default
    ;; character.
    (format stream "~@<While reading list, expected the ~
                    ~/eclector.base::describe-character-english/ when input ~
                    ended.~@:>"
            (delimiter condition)))

  (define-reporter ((condition too-many-dots) stream)
      (format stream "~@<A token consisting solely of multiple dots is ~
                    illegal.~@:>"))

  (define-reporter ((condition invalid-context-for-consing-dot) stream)
    (format stream "~@<A consing dot appeared in an illegal position.~@:>"))

  (define-reporter ((condition end-of-input-after-consing-dot) stream)
    (format stream "~@<While reading consing dot, expected an object when ~
                    input ended.~@:>"))

  (define-reporter ((condition object-must-follow-consing-dot) stream)
    (format stream "~@<An object must follow a consing dot.~@:>"))

  (define-reporter ((condition multiple-objects-following-consing-dot) stream)
    (format stream "~@<Only a single object can follow a consing dot.~@:>"))

  (define-reporter ((condition invalid-context-for-right-parenthesis) stream)
    (format stream "~@<Unmatched closing parenthesis ~
                    ~/eclector.base::describe-character-english/~@[ ~
                    when ~/eclector.base::describe-character-english/ ~
                    was expected~].~@:>"
            (found-character condition)
            (expected-character condition)))

;;; Conditions related SHARPSIGN-SINGLE-QUOTE

  (define-reporter ((condition end-of-input-after-sharpsign-single-quote) stream)
    (format stream "~@<While reading function literal, expected a ~
                    function name when input ended.~@:>"))

  (define-reporter ((condition object-must-follow-sharpsign-single-quote) stream)
    (format stream "~@<An object must follow #'.~@:>"))

;;; Conditions related to read-time evaluation

  (define-reporter ((condition end-of-input-after-sharpsign-dot) stream)
    (format stream "~@<While reading an expression to be evaluated at ~
                    read-time, expected a form when input ended.~@:>"))

  (define-reporter ((condition object-must-follow-sharpsign-dot) stream)
    (format stream "~@<A form must follow #..~@:>"))

  (define-reporter ((condition read-time-evaluation-inhibited) stream)
    (format stream "~@<Cannot evaluate expression at read-time because ~S ~
                    is false.~@:>"
            '*read-eval*))

  (define-reporter ((condition read-time-evaluation-error) stream)
    (let ((expression (expression condition))
          (original-condition (original-condition condition)))
      (format stream "~@<Read-time evaluation of expression ~S signaled ~
                      ~S: ~A~@:>"
              expression (type-of original-condition) original-condition)))

;;; Conditions related to characters

  (define-reporter ((condition end-of-input-after-backslash) stream)
    (format stream "~@<While reading character name, expected a name when ~
                    input ended.~@:>"))

  (define-reporter ((condition unterminated-single-escape-in-character-name) stream)
    (format stream "~@<While reading character name, expected character ~
                    after the ~/eclector.base::describe-character-english/ ~
                    when input ended.~@:>"
              (escape-char condition)))

  (define-reporter ((condition unterminated-multiple-escape-in-character-name) stream)
    (format stream "~@<While reading character name, expected ~
                    the ~/eclector.base::describe-character-english/ when ~
                    input ended.~@:>"
              (delimiter condition)))

  (define-reporter ((condition unknown-character-name) stream)
    (format stream "~@<Unrecognized character name: ~S~@:>" (name condition)))

;;; Conditions related to rational numbers

  (define-reporter ((condition end-of-input-before-digit) stream)
    (format stream "~@<While reading number, expected digit in base ~D ~
                    when input ended.~@:>"
            (base condition)))

  (define-reporter ((condition digit-expected) stream)
    (format stream "~@<The ~/eclector.base::describe-character-english/ is ~
                    not a digit in base ~D.~@:>"
            (character-found condition) (base condition)))

  (define-reporter ((condition zero-denominator) stream)
    (format stream "~@<The denominator of a rational number literal is 0.~@:>"))

  (define-reporter ((condition invalid-radix) stream)
    (format stream "~@<~D is too ~:[big~;small~] to be a radix.~@:>"
            (radix condition) (< (radix condition) 2)))

  (define-reporter ((condition invalid-default-float-format) stream)
    (format stream "~@<~:[A floating-point number without exponent ~
                    marker~;The exponent marker ~:*~A~] cannot be used ~
                    since the value of ~A is ~A which is not valid.~@:>"
            (exponent-marker condition)
            'cl:*read-default-float-format*
            (float-format condition)))

;;; Conditions related to block comments

  (define-reporter ((condition unterminated-block-comment) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on non-default (sub-)
    ;; character.
    (let ((delimiter-1 (delimiter condition))
          (delimiter-2 #\#))
      (format stream "~@<While reading block comment, expected ~
                      \"~A~A\" (that is the ~
                      ~/eclector.base::describe-character-english/ ~
                      followed by the ~
                      ~/eclector.base::describe-character-english/) when ~
                      input ended.~@:>"
              (string delimiter-1) (string delimiter-2)
              delimiter-1 delimiter-2)))

;;; Conditions related to arrays

  (define-context sharpsign-a "the general array reader macro")

  (define-reporter ((condition end-of-input-after-sharpsign-a) stream)
    (format stream "~@<While reading general array, expected initial ~
                    contents when input ended.~@:>"))

  (define-reporter ((condition object-must-follow-sharpsign-a) stream)
    (format stream "~@<Initial contents must follow #A.~@:>"))

  (define-reporter ((condition unterminated-vector) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on a non-default
    ;; character.
    (format stream "~@<While reading vector, expected ~:C when input ~
                    ended.~@:>"
            (delimiter condition)))

  (define-reporter ((condition too-many-elements) stream)
    (format stream "~@<~A was specified to have length ~D, but ~D ~
                    element~:P ~:*~[were~;was~:;were~] found.~@:>"
            (array-type condition)
            (expected-number condition)
            (number-found condition)))

  (define-reporter ((condition no-elements-found) stream)
    (format stream "~@<~A was specified to have length ~D, but no ~
                    elements were found.~@:>"
            (array-type condition) (expected-number condition)))

  (define-reporter ((condition incorrect-initialization-length) stream)
    (format stream "~@<~A was specified to have length ~D along the ~:R ~
                    axis, but provided initial-contents don't ~
                    match:~%~A~@:>"
            (array-type condition)
            (expected-length condition)
            (1+ (axis condition))
            (datum condition)))

;;; Sharpsign C conditions

  (define-context sharpsign-c "the complex reader macro")

  (define-reporter ((condition end-of-input-after-sharpsign-c) stream)
    (format stream "~@<While reading complex number literal, expected ~
                    list of two complex parts when input ended.~@:>"))

  (define-reporter ((condition complex-parts-must-follow-sharpsign-c) stream)
    (format stream "~@<A list of two complex parts must follow #C.~@:>"))

  (define-reporter ((condition non-list-following-sharpsign-c) stream)
    (format stream "~@<A proper list must immediately follow #C.~@:>"))

  (define-reporter ((condition end-of-input-before-complex-part) stream)
    (format stream "~@<While reading ~(~A~) part of a complex number ~
                    literal, expected real number when input ended.~@:>"
            (which condition)))

  (define-reporter ((condition complex-part-expected) stream)
    (format stream "~@<The ~(~A~) part of a complex number literal must ~
                    be a real number but is missing.~@:>"
            (which condition)))

  (define-reporter ((condition too-many-complex-parts) stream)
    (format stream "~@<A complex number literal must contain exactly one ~
                    real part and one imaginary part.~@:>"))

;;; Sharpsign S conditions

  (define-context sharpsign-s            "the structure literal reader macro")
  (define-context sharpsign-s-type       "the structure type name in the structure literal reader macro")
  (define-context sharpsign-s-slot-name  "a structure slot name in the structure literal reader macro")
  (define-context sharpsign-s-slot-value "a structure slot value in the structure literal reader macro")

  (define-reporter ((condition end-of-input-after-sharpsign-s) stream)
    (format stream "~@<While reading structure literal, expected list of ~
                    structure type name and initargs when input ~
                    ended.~@:>"))

  (define-reporter ((condition structure-constructor-must-follow-sharpsign-s) stream)
    (format stream "~@<A list of a structure name and initargs must ~
                    follow #S. ~@:>"))

  (define-reporter ((condition non-list-following-sharpsign-s) stream)
    (format stream "~@<A proper list of a structure type name and ~
                    initargs must follow #S.~@:>"))

  (define-reporter ((condition end-of-input-before-structure-type-name) stream)
    (format stream "~@<While reading structure literal, expected structure ~
                    type name when input ended.~@:>"))

  (define-reporter ((condition no-structure-type-name-found) stream)
    (format stream "~@<A symbol naming a structure type must be the first ~
                    element of the list following #S.~@:>"))

  (define-reporter ((condition structure-type-name-is-not-a-symbol) stream)
    (format stream "~@<~S should designate a structure type but is not a ~
                    symbol.~@:>"
            (type-error-datum condition)))

  (define-reporter ((condition end-of-input-before-slot-name) stream)
    (format stream "~@<While reading structure literal, expected slot ~
                    name designator when input ended.~@:>"))

  (define-reporter ((condition slot-name-is-not-a-string-designator) stream)
    (format stream "~@<~S should designate a structure slot but is ~
                    neither a symbol, nor a string nor a character.~@:>"
            (type-error-datum condition)))

  (define-reporter ((condition end-of-input-before-slot-value) stream)
    (format stream "~@<While reading structure literal, expected value ~
                    for slot ~S when input ended.~@:>"
            (slot-name condition)))

  (define-reporter ((condition no-slot-value-found) stream)
    (format stream "~@<A slot value form must follow the slot name ~S.~@:>"
            (slot-name condition)))

;;; Conditions related to pathnames

  (define-context sharpsign-p "the pathname reader macro")

  (define-reporter ((condition end-of-input-after-sharpsign-p) stream)
    (format stream "~@<While reading pathname literal, expected namestring ~
                    when input ended.~@:>"))

  (define-reporter ((condition namestring-must-follow-sharpsign-p) stream)
    (format stream "~@<A namestring must follow #P.~@:>"))

  (define-reporter ((condition non-string-following-sharpsign-p) stream)
    (format stream "~@<~S should be a namestring but is not a ~S.~@:>"
            (type-error-datum condition)
            (type-error-expected-type condition)))

;;; Conditions related to feature expressions

  (define-context :sharpsign-plus  "the #+ conditionalization reader macro")
  (define-context :sharpsign-minus "the #- conditionalization reader macro")

  (define-reporter ((condition end-of-input-after-sharpsign-plus-minus)
                    stream language)
    (format stream "~@<While reading ~A, expected a feature expression ~
                    when input ended.~@:>"
            (context-name (context condition) language)))

  (define-reporter ((condition feature-expression-must-follow-sharpsign-plus-minus)
                    stream language)
    (format stream "~@<A feature expression must follow ~A.~@:>"
            (context-name (context condition) language)))

  (define-reporter ((condition feature-expression-type-error) stream language)
    (format stream "~@<The feature expression ~S is not of type ~A.~@:>"
            (type-error-datum condition)
            (type-error-expected-type condition)))

  (define-reporter ((condition single-feature-expected) stream language)
    (let ((features (features condition)))
      (format stream "~@<Found ~[no features~*~:;the features ~{~S~^ ~}~] ~
                      when exactly one feature was expected,~@:>"
              (length features) features)))

  (define-reporter ((condition end-of-input-after-feature-expression)
                    stream language)
    (format stream "~@<While reading ~A, expected an expression following ~
                    the feature expression when input ended.~@:>"
              (context-name (context condition) language)))

  (define-reporter ((condition object-must-follow-feature-expression)
                    stream language)
    (format stream "~@<An expression must follow the feature expression ~
                    in ~A.~@:>"
            (context-name (context condition) language)))

;;; SHARPSIGN-{EQUALS,SHARPSIGN} conditions

  (define-reporter ((condition end-of-input-after-sharpsign-equals) stream)
    (format stream "~@<While reading label definition, expected ~
                    expression when input ended.~@:>"))

  (define-reporter ((condition object-must-follow-sharpsign-equals) stream)
    (format stream "~@<An expression must follow #=.~@:>"))

  (define-reporter ((condition sharpsign-equals-label-defined-more-than-once) stream)
    (format stream "~@<Label ~D defined more than once.~@:>"
            (label condition)))

  (define-reporter ((condition sharpsign-equals-only-refers-to-self) stream)
    (format stream "~@<Label ~D is defined as a reference to itself.~@:>"
            (label condition)))

  (define-reporter ((condition sharpsign-sharpsign-undefined-label) stream)
    (format stream "~@<Reference to undefined label #~D#.~@:>"
            (label condition)))

  ) ; MACROLET DEFINE-REPORTER
