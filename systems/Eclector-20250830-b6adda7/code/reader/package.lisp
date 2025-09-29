(cl:defpackage #:eclector.reader
  (:use
   #:common-lisp)

  ;; When the reader is compiled for the purpose of cross compilation,
  ;; we must shadow a certain number of symbols that would otherwise
  ;; clash with the corresponding symbols in the host package
  ;; COMMON-LISP.
  (:shadow
   #:peek-char
   #:read
   #:read-preserving-whitespace
   #:read-from-string
   #:read-delimited-list)

  (:import-from #:alexandria
   #:array-index

   #:once-only)

  (:shadowing-import-from #:eclector.base
   #:end-of-file

   #:read-char

   #:recover) ; Function and restart name

  (:import-from #:eclector.base
   #:*client*

   #:%reader-error

   #:%recoverable-reader-error
   #:recovery-description
   #:recovery-description-using-language
   #:format-recovery-report

   #:stream-position-reader-error
   #:stream-position

   #:incomplete-construct

   #:missing-delimiter
   #:delimiter

   #:read-char-or-error
   #:read-char-or-recoverable-error)

  ;; Contrary to other variables affecting the reader, we cannot use
  ;; the host version of *READTABLE* because we do not necessarily
  ;; use the same representation of readtables as the host does, and
  ;; Common Lisp does not have a standardized API for manipulating
  ;; readtables.  Perhaps we should write a CDR (Common Lisp Document
  ;; Repository) document suggesting such an API.
  (:shadowing-import-from #:eclector.readtable
   #:*readtable*)

  (:import-from #:eclector.readtable
   #:unterminated-dispatch-macro
   #:unknown-macro-sub-character)

  (:export
   #:*client*

   #:*readtable*

   #:*skip-reason*

   #:read-char
   #:peek-char
   #:read
   #:read-preserving-whitespace
   #:read-from-string
   #:read-delimited-list

   #:recover) ; Function and restart name

  ;; Client protocol
  (:export
   #:call-as-top-level-read
   #:read-common
   #:read-maybe-nothing
   #:note-skipped-input)

  ;; Reader state protocol
  (:export
   #:valid-state-value-p
   #:state-value
   #:call-with-state-value)

  ;; Reader behavior protocol
  (:export
   #:read-token
   #:interpret-token
   #:check-symbol-token
   #:interpret-symbol-token
   #:interpret-symbol

   #:call-reader-macro
   #:find-character
   #:make-structure-instance

   #:evaluate-expression
   #:check-feature-expression
   #:evaluate-feature-expression)

  ;; Labeled object protocol
  (:export
   #:call-with-label-tracking
   #:note-labeled-object
   #:forget-labeled-object
   #:find-labeled-object
   #:make-labeled-object
   #:labeled-object-state
   #:finalize-labeled-object
   #:reference-labeled-object

   #:fixup-graph-p
   #:fixup-graph
   #:walk-fixup-tree
   #:fixup
   #:new-value-for-fixup

   #:fixup-case
   #:fixup-place-using-value
   #:fixup-place)

  ;; S-expression creation protocol
  (:export
   #:wrap-in-quote

   #:wrap-in-quasiquote
   #:wrap-in-unquote
   #:wrap-in-unquote-splicing

   #:wrap-in-function)

  ;; Conditions related to symbol and end-of-input.
  (:export
   #:end-of-file

   #:incomplete-construct

   #:missing-delimiter
   #:delimiter

   #:state-value-type-error
   #:aspect

   #:read-object-type-error

   #:unterminated-single-escape
   #:unterminated-multiple-escape

   #:package-does-not-exist
   #:symbol-does-not-exist
   #:symbol-is-not-external

   #:invalid-constituent-character
   #:unterminated-single-escape-in-symbol
   #:unterminated-multiple-escape-in-symbol
   #:symbol-name-must-not-be-only-package-markers
   #:symbol-name-must-not-end-with-package-marker
   #:two-package-markers-must-be-adjacent
   #:two-package-markers-must-not-be-first
   #:symbol-can-have-at-most-two-package-markers
   #:uninterned-symbol-must-not-contain-package-marker)

  ;; Conditions related to reader macros.
  (:export
   #:sharpsign-invalid

   #:unterminated-dispatch-macro
   #:unknown-macro-sub-character

   #:numeric-parameter-supplied-but-ignored
   #:numeric-parameter-not-supplied-but-required

   #:end-of-input-after-quote
   #:object-must-follow-quote

   #:unterminated-string
   #:unterminated-single-escape-in-string

   #:backquote-error
   #:backquote-context-error
   #:backquote-in-invalid-context
   #:object-must-follow-backquote
   #:end-of-input-after-backquote
   #:unquote-error
   #:invalid-context-for-unquote
   #:unquote-not-inside-backquote
   #:unquote-in-invalid-context
   #:end-of-input-after-unquote
   #:object-must-follow-unquote
   #:unquote-splicing-in-dotted-list
   #:unquote-splicing-at-top

   #:unterminated-list
   #:too-many-dots
   #:invalid-context-for-consing-dot
   #:end-of-input-after-consing-dot
   #:object-must-follow-consing-dot
   #:multiple-objects-following-consing-dot
   #:invalid-context-for-right-parenthesis

   #:end-of-input-after-sharpsign-single-quote
   #:object-must-follow-sharpsign-single-quote

   #:end-of-input-after-sharpsign-dot
   #:object-must-follow-sharpsign-dot
   #:read-time-evaluation-inhibited
   #:read-time-evaluation-error

   #:end-of-input-after-backslash
   #:unterminated-single-escape-in-character-name
   #:unterminated-multiple-escape-in-character-name
   #:unknown-character-name

   #:end-of-input-before-digit
   #:digit-expected
   #:zero-denominator
   #:invalid-radix
   #:invalid-default-float-format

   #:unterminated-block-comment

   #:end-of-input-after-sharpsign-a
   #:object-must-follow-sharpsign-a
   #:unterminated-vector
   #:too-many-elements
   #:no-elements-found
   #:incorrect-initialization-length

   #:end-of-input-after-sharpsign-c
   #:complex-parts-must-follow-sharpsign-c
   #:non-list-following-sharpsign-c
   #:end-of-input-before-complex-part
   #:complex-part-expected
   #:too-many-complex-parts

   #:end-of-input-after-sharpsign-s
   #:structure-constructor-must-follow-sharpsign-s
   #:non-list-following-sharpsign-s
   #:end-of-input-before-structure-type-name
   #:no-structure-type-name-found
   #:structure-type-name-is-not-a-symbol
   #:end-of-input-before-slot-name
   #:slot-name-is-not-a-string-designator
   #:end-of-input-before-slot-value
   #:no-slot-value-found

   #:end-of-input-after-sharpsign-p
   #:namestring-must-follow-sharpsign-p
   #:non-string-following-sharpsign-p

   #:end-of-input-after-sharpsign-plus-minus
   #:feature-expression-must-follow-sharpsign-plus-minus
   #:feature-expression-type-error
   #:single-feature-expected
   #:end-of-input-after-feature-expression
   #:object-must-follow-feature-expression

   #:end-of-input-after-sharpsign-equals
   #:object-must-follow-sharpsign-equals
   #:sharpsign-equals-label-defined-more-than-once
   #:sharpsign-equals-only-refers-to-self
   #:sharpsign-sharpsign-undefined-label)

  ;; Names of macros related to backquote.  We export them so that the
  ;; pretty printer can use them properly.
  (:export
   #:quasiquote
   #:unquote
   #:unquote-splicing)

  ;; Readtable initialization
  (:export
   #:set-standard-syntax-types
   #:set-standard-macro-characters
   #:set-standard-dispatch-macro-characters
   #:set-standard-syntax-and-macros)

  ;; Reader macro functions
  (:export
   #:strict-sharpsign-single-quote
   #:strict-sharpsign-c
   #:strict-sharpsign-s))
