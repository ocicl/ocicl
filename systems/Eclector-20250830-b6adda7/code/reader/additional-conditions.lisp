(cl:in-package #:eclector.reader)

;;; Conditions related to reader state

(define-condition state-value-type-error (acclimation:condition
                                          reader-error
                                          type-error)
  ((%aspect :initarg :aspect :reader aspect)))

;;; Type error

(define-condition read-object-type-error (stream-position-reader-error
                                          type-error)
  ())

;;; General escape errors

(define-condition unterminated-single-escape (end-of-file
                                              incomplete-construct)
  ((%escape-char :initarg :escape-char :reader escape-char)))

(define-condition unterminated-multiple-escape (missing-delimiter)
  ())

;;; Conditions related to symbols
;;;
;;; See HyperSpec section 2.3.5 (Valid Patterns for Tokens).

(define-condition package-does-not-exist (stream-position-reader-error)
  ((%package-name :initarg :package-name :reader desired-package-name)))

(define-condition symbol-access-error (stream-position-reader-error)
  ((%symbol-name :initarg :symbol-name :reader desired-symbol-name)
   (%package :initarg :package :reader desired-symbol-package)))

(define-condition symbol-does-not-exist (symbol-access-error)
  ())

(define-condition symbol-is-not-external (symbol-access-error)
  ())

(define-condition symbol-syntax-error (stream-position-reader-error)
  ((%token :initarg :token :reader token)))

(define-condition invalid-constituent-character (symbol-syntax-error)
  ())

(define-condition unterminated-single-escape-in-symbol (symbol-syntax-error
                                                        unterminated-single-escape)
  ())

(define-condition unterminated-multiple-escape-in-symbol (symbol-syntax-error
                                                          unterminated-multiple-escape)
  ())

(define-condition symbol-name-must-not-be-only-package-markers (symbol-syntax-error)
  ())

(define-condition symbol-name-must-not-end-with-package-marker (symbol-syntax-error)
  ())

(define-condition two-package-markers-must-be-adjacent (symbol-syntax-error)
  ())

(define-condition two-package-markers-must-not-be-first (symbol-syntax-error)
  ())

(define-condition symbol-can-have-at-most-two-package-markers (symbol-syntax-error)
  ())

(define-condition uninterned-symbol-must-not-contain-package-marker (symbol-syntax-error)
  ())

;;; General reader macro conditions

(define-condition sharpsign-invalid (stream-position-reader-error)
  ((%character-found :initarg :character-found :reader character-found)))

(define-condition numeric-parameter-supplied-but-ignored (stream-position-reader-error)
  ((%parameter :initarg :parameter :reader parameter)
   (%macro-name :initarg :macro-name :reader macro-name)))

(defun numeric-parameter-ignored (stream macro-name parameter suppress)
  (unless suppress
    (let ((length (numeric-token-length parameter)))
      (%recoverable-reader-error
       stream 'numeric-parameter-supplied-but-ignored
       :position-offset (- (1+ length)) :length length
       :parameter parameter :macro-name macro-name :report 'ignore-parameter))))

(define-condition numeric-parameter-not-supplied-but-required (stream-position-reader-error)
  ((%macro-name :initarg :macro-name :reader macro-name)))

(defun numeric-parameter-not-supplied (stream macro-name suppress)
  (unless suppress
    (%recoverable-reader-error
     stream 'numeric-parameter-not-supplied-but-required
     :position-offset -1
     :macro-name macro-name :report 'use-replacement-parameter)))

;;; Conditions related to single quote

(define-condition end-of-input-after-quote (end-of-file incomplete-construct)
  ())

(define-condition object-must-follow-quote (incomplete-construct)
  ())

;;; Conditions related to strings

(define-condition unterminated-string (missing-delimiter)
  ())

(define-condition unterminated-single-escape-in-string (unterminated-single-escape)
  ())

;;; Conditions related to quasiquotation

(defgeneric context-name (context language))

(define-condition backquote-error (stream-position-reader-error)
  ())

(define-condition backquote-context-error (backquote-error)
  ((%context :initarg :context :reader context)))

(define-condition backquote-in-invalid-context (backquote-context-error)
  ())

(define-condition object-must-follow-backquote (incomplete-construct
                                                backquote-error)
  ())

(define-condition end-of-input-after-backquote (end-of-file
                                                incomplete-construct
                                                backquote-error)
  ())

(define-condition unquote-condition ()
  ((%splicing-p :initarg :splicing-p :reader splicing-p)))

(define-condition unquote-error (backquote-error unquote-condition)
  ())

(define-condition invalid-context-for-unquote (unquote-error)
  ())

(define-condition unquote-not-inside-backquote (invalid-context-for-unquote)
  ())

(define-condition unquote-in-invalid-context (invalid-context-for-unquote
                                              backquote-context-error)
  ())

(define-condition end-of-input-after-unquote (end-of-file
                                              incomplete-construct
                                              unquote-condition)
  ())

(define-condition object-must-follow-unquote (unquote-error
                                              incomplete-construct)
  ())

(define-condition unquote-macroexpansion-error (acclimation:condition
                                                unquote-condition
                                                error)
  ((%argument :initarg :argument :reader argument)))

(define-condition unquote-splicing-in-dotted-list (unquote-macroexpansion-error)
  ()
  (:default-initargs
   :splicing-p t))

(define-condition unquote-splicing-at-top (unquote-macroexpansion-error)
  ()
  (:default-initargs
   :splicing-p t))

;;; Not intended for use by clients for now.  A similar condition type
;;; will be provided by a dedicated module for quasiquote macros and a
;;; quasiquote expansion protocol.
(define-condition unquote-not-inside-backquote-during-macroexpansion
    (unquote-macroexpansion-error)
  ())

;;; Conditions related to lists

(define-condition unterminated-list (missing-delimiter)
  ())

(define-condition too-many-dots (stream-position-reader-error)
  ())

(define-condition invalid-context-for-consing-dot (stream-position-reader-error)
  ())

(define-condition end-of-input-after-consing-dot (end-of-file
                                                  incomplete-construct)
  ())

(define-condition object-must-follow-consing-dot (incomplete-construct)
  ())

(define-condition multiple-objects-following-consing-dot (stream-position-reader-error)
  ())

(define-condition invalid-context-for-right-parenthesis (stream-position-reader-error)
  ((%expected-character :initarg :expected-character
                        :reader expected-character
                        :initform nil)
   (%found-character :initarg :found-character
                     :reader found-character)))

;;; Conditions related to SHARPSIGN-DOT

(define-condition end-of-input-after-sharpsign-single-quote (end-of-file
                                                             incomplete-construct)
  ())

(define-condition object-must-follow-sharpsign-single-quote (incomplete-construct)
  ())

;;; Conditions related to read-time evaluation

(define-condition end-of-input-after-sharpsign-dot (end-of-file
                                                    incomplete-construct)
  ())

(define-condition object-must-follow-sharpsign-dot (incomplete-construct)
  ())

(define-condition read-time-evaluation-inhibited (stream-position-reader-error)
  ())

(define-condition read-time-evaluation-error (stream-position-reader-error)
  ((%expression :initarg :expression :reader expression)
   (%original-condition :initarg :original-condition :reader original-condition)))

;;; Conditions related to characters

(define-condition end-of-input-after-backslash (end-of-file
                                                incomplete-construct)
  ())

(define-condition unterminated-single-escape-in-character-name
    (unterminated-single-escape)
  ())

(define-condition unterminated-multiple-escape-in-character-name
    (unterminated-multiple-escape)
  ())

(define-condition unknown-character-name (stream-position-reader-error)
  ((%name :initarg :name :reader name)))

;;; Conditions related to rational numbers

(define-condition digit-condition (condition)
  ((%base :initarg :base :reader base)))

(define-condition end-of-input-before-digit (end-of-file
                                             incomplete-construct
                                             digit-condition)
  ())

(define-condition digit-expected (stream-position-reader-error
                                  digit-condition)
  ((%character-found :initarg :character-found :reader character-found)))

(define-condition zero-denominator (stream-position-reader-error)
  ())

(define-condition invalid-radix (stream-position-reader-error)
  ((%radix :initarg :radix :reader radix)))

(define-condition invalid-default-float-format (stream-position-reader-error)
  ((%exponent-marker :initarg :exponent-marker :reader exponent-marker)
   (%float-format :initarg :float-format :reader float-format)))

;;; Conditions related to block comments

(define-condition unterminated-block-comment (missing-delimiter)
  ())

;;; Conditions related to arrays

(define-condition end-of-input-after-sharpsign-a (end-of-file
                                                  incomplete-construct)
  ())

(define-condition object-must-follow-sharpsign-a (incomplete-construct)
  ())

(define-condition unterminated-vector (missing-delimiter)
  ())

(define-condition array-initialization-error (stream-position-reader-error)
  ((%array-type :initarg :array-type :reader array-type)))

(define-condition too-many-elements (array-initialization-error)
  ((%expected-number :initarg :expected-number :reader expected-number)
   (%number-found :initarg :number-found :reader number-found)))

(define-condition no-elements-found (array-initialization-error)
  ((%expected-number :initarg :expected-number :reader expected-number)))

(define-condition incorrect-initialization-length (array-initialization-error)
  ((%axis :initarg :axis :reader axis)
   (%expected-length :initarg :expected-length :reader expected-length)
   (%datum :initarg :datum :reader datum)))

;;; Sharpsign C conditions

(define-condition end-of-input-after-sharpsign-c (end-of-file
                                                  incomplete-construct)
  ())

(define-condition complex-parts-must-follow-sharpsign-c (incomplete-construct)
  ())

(define-condition non-list-following-sharpsign-c (stream-position-reader-error)
  ())

(define-condition complex-part-condition ()
  ((%which :initarg :which :reader which)))

(define-condition end-of-input-before-complex-part (end-of-file
                                                    incomplete-construct
                                                    complex-part-condition)
  ())

(define-condition complex-part-expected (stream-position-reader-error
                                         complex-part-condition)
  ())

(define-condition too-many-complex-parts (stream-position-reader-error)
  ())

;;; Sharpsign S conditions

(define-condition end-of-input-after-sharpsign-s (stream-position-reader-error)
  ())

(define-condition structure-constructor-must-follow-sharpsign-s (stream-position-reader-error)
  ())

(define-condition non-list-following-sharpsign-s (stream-position-reader-error)
  ())

(define-condition end-of-input-before-structure-type-name (end-of-file
                                                           incomplete-construct)
  ())

(define-condition no-structure-type-name-found (incomplete-construct)
  ())

(define-condition structure-type-name-is-not-a-symbol (read-object-type-error)
  ()
  (:default-initargs
   :expected-type 'symbol))

(define-condition end-of-input-before-slot-name (end-of-file
                                                 incomplete-construct)
  ())

(define-condition slot-name-is-not-a-string-designator (read-object-type-error)
  ()
  (:default-initargs
   :expected-type 'symbol))

(define-condition slot-value-condition (condition)
  ((%slot-name :initarg :slot-name
               :reader slot-name)))

(define-condition end-of-input-before-slot-value (slot-value-condition
                                                  end-of-file
                                                  incomplete-construct)
  ())

(define-condition no-slot-value-found (slot-value-condition
                                       incomplete-construct)
  ())

;;; Conditions related to pathname literals

(define-condition end-of-input-after-sharpsign-p (end-of-file
                                                  incomplete-construct)
  ())

(define-condition namestring-must-follow-sharpsign-p (incomplete-construct)
  ())

(define-condition non-string-following-sharpsign-p (read-object-type-error)
  ())

;;; Conditions related to feature expressions
;;;
;;; Can be evaluated without a stream context. Therefore each
;;; condition has a stream- and a non-stream-variant.

(define-condition reader-conditional-condition (condition)
  ((%context :initarg :context :reader context)))

(define-condition end-of-input-after-sharpsign-plus-minus
    (reader-conditional-condition
     end-of-file
     incomplete-construct)
  ())

(define-condition feature-expression-must-follow-sharpsign-plus-minus
    (reader-conditional-condition
     incomplete-construct)
  ())

(define-condition feature-expression-type-error (acclimation:condition type-error)
  ())

(define-condition feature-expression-type-error/reader
    (feature-expression-type-error stream-position-reader-error)
  ())

(define-condition single-feature-expected (acclimation:condition error)
  ((%features :initarg :features :reader features)))

(define-condition single-feature-expected/reader
    (single-feature-expected stream-position-reader-error)
  ())

(define-condition end-of-input-after-feature-expression
    (reader-conditional-condition
     end-of-file
     incomplete-construct)
  ())

(define-condition object-must-follow-feature-expression
    (reader-conditional-condition
     incomplete-construct)
  ())

;;; SHARPSIGN-{EQUALS,SHARPSIGN} conditions

(define-condition end-of-input-after-sharpsign-equals (end-of-file
                                                       incomplete-construct)
  ())

(define-condition object-must-follow-sharpsign-equals (incomplete-construct)
  ())

(define-condition reference-error (stream-position-reader-error)
  ((%label :initarg :label :reader label)))

(define-condition sharpsign-equals-label-defined-more-than-once (reference-error)
  ())

(define-condition sharpsign-equals-only-refers-to-self (reference-error)
  ())

(define-condition sharpsign-sharpsign-undefined-label (reference-error)
  ())
