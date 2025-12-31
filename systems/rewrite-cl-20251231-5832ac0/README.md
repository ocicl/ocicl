# rewrite-cl

A Common Lisp library for reading, modifying, and writing Lisp source code while preserving whitespace, comments, and formatting.

## Overview

rewrite-cl parses Common Lisp source into an AST that retains all formatting information, allowing you to programmatically transform code and write it back without losing the original style. This is useful for:

- Code refactoring tools
- Automated code modifications
- Linters and formatters
- Source-to-source transformations

## Installation

Clone this repository and load via ASDF:

```lisp
(asdf:load-system "rewrite-cl")
```

## Quick Start

```lisp
(asdf:load-system "rewrite-cl")
(use-package :rewrite-cl)

;; Parse a string and get back the exact same string
(node-string (parse-string "(defun foo (x) (+ x 1))"))
;; => "(defun foo (x) (+ x 1))"

;; Parse preserves whitespace and comments
(node-string (parse-string "(foo  bar  ; comment
  baz)"))
;; => "(foo  bar  ; comment
;;   baz)"

;; Use zippers for navigation and editing
(let ((z (of-string "(defun foo (x) x)")))
  (setf z (zip-down z))           ; enter the list
  (setf z (zip-right* z))         ; skip to 'foo' (skipping whitespace)
  (setf z (zip-replace z (make-token-node 'bar "bar")))
  (zip-root-string z))
;; => "(defun bar (x) x)"
```

## Core Concepts

### Nodes

The AST is composed of nodes. Each node has:
- `node-tag` - A keyword identifying the type (`:symbol`, `:list`, `:whitespace`, etc.)
- `node-string` - The exact source text representation
- `node-sexpr` - The evaluated Lisp value (when applicable)
- `node-children` - Child nodes (for compound nodes)

Node types include: `:symbol`, `:keyword`, `:integer`, `:float`, `:ratio`, `:string`, `:character`, `:list`, `:vector`, `:quote`, `:syntax-quote`, `:unquote`, `:unquote-splicing`, `:whitespace`, `:newline`, `:comment`, `:block-comment`, and various reader macro types.

### Parsing

```lisp
;; Parse a single form
(parse-string "(+ 1 2)")

;; Parse all forms (returns a list)
(parse-string-all "x y z")

;; Parse from a file
(parse-file "mycode.lisp")
(parse-file-all "mycode.lisp")
```

### Zippers

Zippers provide a functional way to navigate and edit the AST:

```lisp
(let ((z (of-string "(a (b c) d)")))
  ;; Navigation
  (zip-down z)      ; Move to first child
  (zip-up z)        ; Move to parent
  (zip-left z)      ; Move to left sibling
  (zip-right z)     ; Move to right sibling
  (zip-next z)      ; Depth-first traversal

  ;; Whitespace-skipping variants (skip whitespace/comments)
  (zip-down* z)
  (zip-right* z)

  ;; Editing (returns new zipper)
  (zip-replace z new-node)
  (zip-insert-left z node)
  (zip-insert-right z node)
  (zip-remove z)

  ;; Get results
  (zip-root-string z)  ; Get modified source string
  (zip-sexpr z))       ; Get s-expression at point
```

### Creating Nodes

```lisp
;; From Lisp values (automatic conversion)
(coerce-to-node 'foo)
(coerce-to-node '(a b c))

;; Explicit constructors
(make-token-node 'symbol "symbol")
(make-string-node "hello" "\"hello\"")
(make-list-node (list child1 child2))
(make-comment-node "; my comment")
(spaces 4)      ; 4 spaces
(newlines 2)    ; 2 newlines
```

## API Reference

### Parsing Functions
- `parse-string` - Parse first form from string
- `parse-string-all` - Parse all forms from string
- `parse-file` - Parse first form from file
- `parse-file-all` - Parse all forms from file

### Node Functions
- `node-tag` - Get node type keyword
- `node-string` - Get source text
- `node-sexpr` - Get Lisp value
- `node-children` - Get child nodes
- `node-inner-p` - Check if node has children
- `coerce-to-node` - Convert Lisp value to node

### Zipper Creation
- `of-string` - Create zipper from string
- `of-file` - Create zipper from file
- `of-node` - Create zipper from node

### Zipper Navigation
- `zip-up`, `zip-down`, `zip-left`, `zip-right` - Basic movement
- `zip-down*`, `zip-left*`, `zip-right*` - Skip whitespace/comments
- `zip-next`, `zip-prev` - Depth-first traversal
- `zip-next*`, `zip-prev*` - Depth-first, skipping whitespace

### Zipper Editing
- `zip-replace` - Replace current node
- `zip-edit` - Apply function to current node
- `zip-insert-left`, `zip-insert-right` - Insert siblings
- `zip-insert-child`, `zip-append-child` - Insert children
- `zip-remove` - Remove current node

### Zipper Queries
- `zip-node` - Get current node
- `zip-root` - Get root node
- `zip-root-string` - Get full source string
- `zip-sexpr` - Get s-expression at point
- `zip-tag` - Get current node's tag

### Zipper Traversal
- `zip-find`, `zip-find-tag`, `zip-find-value` - Search
- `zip-find-all` - Find all matching nodes
- `zip-find-in-children`, `zip-find-child-value` - Search in children
- `zip-prewalk`, `zip-postwalk` - Tree walking with transformation
- `zip-collect`, `zip-collect-sexprs` - Collect nodes during traversal
- `zip-transform`, `zip-transform-if` - Transform matching nodes
- `zip-map-children` - Map over children
- `zip-nth-child` - Access nth child directly

## Acknowledgments

rewrite-cl is directly inspired by and draws heavily from [rewrite-clj](https://github.com/clj-commons/rewrite-clj), the excellent Clojure library for rewriting Clojure code. The zipper-based approach, API design, and overall architecture owe much to that project.

## License

MIT License

## Author

Anthony Green <green@moxielogic.com>
