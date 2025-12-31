;;;; package.lisp - Package definitions for rewrite-cl
;;;; SPDX-License-Identifier: MIT

(defpackage #:rewrite-cl.node
  (:documentation "AST node types for representing Common Lisp source code.")
  (:use #:cl #:alexandria)
  (:export
   ;; Protocols (generic functions)
   #:node-tag
   #:node-string
   #:node-sexpr
   #:node-sexpr-able-p
   #:node-printable-only-p
   #:node-length
   #:node-children
   #:node-inner-p
   #:node-replace-children
   #:node-position
   #:node-set-position

   ;; Position
   #:source-position
   #:make-source-position
   #:copy-source-position
   #:pos-line
   #:pos-column
   #:pos-end-line
   #:pos-end-column

   ;; Node types
   #:whitespace-node
   #:whitespace-node-p
   #:make-whitespace-node
   #:whitespace-node-text

   #:newline-node
   #:newline-node-p
   #:make-newline-node
   #:newline-node-text

   #:comment-node
   #:comment-node-p
   #:make-comment-node
   #:comment-node-text

   #:block-comment-node
   #:block-comment-node-p
   #:make-block-comment-node
   #:block-comment-node-text

   #:token-node
   #:token-node-p
   #:make-token-node
   #:token-node-value
   #:token-node-string-value

   #:string-node
   #:string-node-p
   #:make-string-node
   #:string-node-value
   #:string-node-source-text

   #:character-node
   #:character-node-p
   #:make-character-node
   #:character-node-value
   #:character-node-source-text

   #:seq-node
   #:seq-node-p
   #:make-list-node
   #:make-vector-node
   #:seq-node-tag
   #:seq-node-open
   #:seq-node-close
   #:seq-node-children

   #:quote-node
   #:quote-node-p
   #:make-quote-node
   #:make-syntax-quote-node
   #:make-unquote-node
   #:make-unquote-splicing-node
   #:quote-node-tag
   #:quote-node-prefix
   #:quote-node-child

   #:reader-macro-node
   #:reader-macro-node-p
   #:make-function-node
   #:make-read-eval-node
   #:make-feature-node
   #:make-uninterned-symbol-node
   #:make-pathname-node
   #:make-bit-vector-node
   #:make-array-node
   #:make-radix-node
   #:make-complex-node
   #:make-unknown-macro-node
   #:reader-macro-node-tag
   #:reader-macro-node-prefix
   #:reader-macro-node-children

   ;; Coercion
   #:coerce-to-node

   ;; Utilities
   #:nodes-string
   #:child-sexprs
   #:spaces
   #:newlines
   #:whitespace-or-comment-p

   ;; Character classification (used by parser)
   #:whitespace-char-p
   #:newline-char-p
   #:whitespace-or-newline-p))

(defpackage #:rewrite-cl.parser
  (:documentation "Parser for reading Common Lisp source into AST nodes.")
  (:use #:cl #:alexandria #:rewrite-cl.node)
  (:export
   #:source-reader
   #:make-source-reader
   #:make-string-reader
   #:reader-peek
   #:reader-read
   #:reader-unread
   #:reader-position
   #:reader-eof-p

   #:parse-next
   #:parse-all
   #:parse-string
   #:parse-string-all
   #:parse-file
   #:parse-file-all))

(defpackage #:rewrite-cl.zip
  (:documentation "Zipper-based navigation and editing of AST nodes.")
  (:use #:cl #:alexandria #:rewrite-cl.node)
  (:export
   ;; Zipper structure
   #:zipper
   #:zipper-p
   #:make-zipper
   #:zip-node
   #:zip-root

   ;; Creation
   #:of-string
   #:of-file
   #:of-node

   ;; Navigation
   #:zip-up
   #:zip-down
   #:zip-left
   #:zip-right
   #:zip-leftmost
   #:zip-rightmost
   #:zip-next
   #:zip-prev
   #:zip-end-p

   ;; Whitespace-skipping navigation
   #:zip-up*
   #:zip-down*
   #:zip-left*
   #:zip-right*
   #:zip-next*
   #:zip-prev*

   ;; Editing
   #:zip-replace
   #:zip-edit
   #:zip-insert-left
   #:zip-insert-right
   #:zip-insert-child
   #:zip-append-child
   #:zip-remove
   #:zip-splice

   ;; Find
   #:zip-find
   #:zip-find-tag
   #:zip-find-value
   #:zip-find-token
   #:zip-find-depth-first
   #:zip-find-all
   #:zip-find-by-position
   #:zip-find-next-token
   #:zip-find-next-list
   #:zip-find-next-string
   #:zip-find-in-children
   #:zip-find-child-value

   ;; Walk
   #:zip-prewalk
   #:zip-postwalk
   #:zip-walk
   #:zip-prewalk-while
   #:zip-collect
   #:zip-collect-sexprs
   #:zip-transform
   #:zip-transform-if

   ;; Subedit
   #:zip-subedit
   #:zip-subzip

   ;; Sequence operations
   #:zip-map
   #:zip-map-children
   #:zip-filter-children
   #:zip-child-sexprs
   #:zip-length
   #:zip-nth-child
   #:zip-first-child
   #:zip-second-child
   #:zip-last-child

   ;; Whitespace predicates
   #:zip-whitespace-p
   #:zip-newline-p
   #:zip-comment-p
   #:zip-whitespace-or-comment-p

   ;; Accessors
   #:zip-string
   #:zip-root-string
   #:zip-sexpr
   #:zip-tag
   #:zip-position
   #:zip-children))

(defpackage #:rewrite-cl
  (:documentation "Read, modify, and write Common Lisp source preserving formatting.")
  (:use #:cl)
  (:import-from #:rewrite-cl.node
   ;; Re-export commonly used node functions
   #:node-tag
   #:node-string
   #:node-sexpr
   #:node-children
   #:node-inner-p
   #:node-position
   #:coerce-to-node
   #:make-whitespace-node
   #:make-newline-node
   #:make-comment-node
   #:make-token-node
   #:make-string-node
   #:make-list-node
   #:make-vector-node
   #:make-quote-node
   #:spaces
   #:newlines)
  (:import-from #:rewrite-cl.parser
   #:parse-string
   #:parse-string-all
   #:parse-file
   #:parse-file-all)
  (:import-from #:rewrite-cl.zip
   ;; Re-export all zipper functions
   #:of-string
   #:of-file
   #:of-node
   #:zip-node
   #:zip-root
   #:zip-up
   #:zip-down
   #:zip-left
   #:zip-right
   #:zip-leftmost
   #:zip-rightmost
   #:zip-next
   #:zip-prev
   #:zip-end-p
   #:zip-down*
   #:zip-left*
   #:zip-right*
   #:zip-next*
   #:zip-prev*
   #:zip-replace
   #:zip-edit
   #:zip-insert-left
   #:zip-insert-right
   #:zip-insert-child
   #:zip-append-child
   #:zip-remove
   #:zip-splice
   #:zip-find
   #:zip-find-tag
   #:zip-find-value
   #:zip-find-all
   #:zip-find-in-children
   #:zip-find-child-value
   #:zip-prewalk
   #:zip-postwalk
   #:zip-walk
   #:zip-collect
   #:zip-collect-sexprs
   #:zip-subedit
   #:zip-subzip
   #:zip-map-children
   #:zip-nth-child
   #:zip-string
   #:zip-root-string
   #:zip-sexpr
   #:zip-tag
   #:zip-position
   #:zip-children)
  (:export
   ;; Node functions
   #:node-tag
   #:node-string
   #:node-sexpr
   #:node-children
   #:node-inner-p
   #:node-position
   #:coerce-to-node
   #:make-whitespace-node
   #:make-newline-node
   #:make-comment-node
   #:make-token-node
   #:make-string-node
   #:make-list-node
   #:make-vector-node
   #:make-quote-node
   #:spaces
   #:newlines
   ;; Parser functions
   #:parse-string
   #:parse-string-all
   #:parse-file
   #:parse-file-all
   ;; Zipper functions
   #:of-string
   #:of-file
   #:of-node
   #:zip-node
   #:zip-root
   #:zip-up
   #:zip-down
   #:zip-left
   #:zip-right
   #:zip-leftmost
   #:zip-rightmost
   #:zip-next
   #:zip-prev
   #:zip-end-p
   #:zip-down*
   #:zip-left*
   #:zip-right*
   #:zip-next*
   #:zip-prev*
   #:zip-replace
   #:zip-edit
   #:zip-insert-left
   #:zip-insert-right
   #:zip-insert-child
   #:zip-append-child
   #:zip-remove
   #:zip-splice
   #:zip-find
   #:zip-find-tag
   #:zip-find-value
   #:zip-find-all
   #:zip-find-in-children
   #:zip-find-child-value
   #:zip-prewalk
   #:zip-postwalk
   #:zip-walk
   #:zip-collect
   #:zip-collect-sexprs
   #:zip-subedit
   #:zip-subzip
   #:zip-map-children
   #:zip-nth-child
   #:zip-string
   #:zip-root-string
   #:zip-sexpr
   #:zip-tag
   #:zip-position
   #:zip-children))
