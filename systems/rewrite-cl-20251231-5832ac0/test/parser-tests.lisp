;;;; parser-tests.lisp - Tests for parser
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl/test)

(def-suite parser-tests
  :description "Tests for parser"
  :in rewrite-cl-tests)

(in-suite parser-tests)

;;; Basic parsing

(test parse-symbol
  "Test parsing a symbol."
  (let ((node (parse-string "foo")))
    (is (eql :symbol (node-tag node)))
    (is (eq 'foo (node-sexpr node)))
    (is (string= "foo" (node-string node)))))

(test parse-keyword
  "Test parsing a keyword."
  (let ((node (parse-string ":foo")))
    (is (eql :keyword (node-tag node)))
    (is (eql :foo (node-sexpr node)))))

(test parse-number
  "Test parsing a number."
  (let ((node (parse-string "42")))
    (is (member (node-tag node) '(:integer :number)))
    (is (= 42 (node-sexpr node)))))

(test parse-float
  "Test parsing a float."
  (let ((node (parse-string "3.14")))
    (is (typep (node-sexpr node) *read-default-float-format*))
    (is (= 3.14 (node-sexpr node)))))

(test parse-negative
  "Test parsing negative number."
  (let ((node (parse-string "-5")))
    (is (= -5 (node-sexpr node)))))

;;; String parsing

(test parse-string-simple
  "Test parsing a simple string."
  (let ((node (parse-string "\"hello\"")))
    (is (eql :string (node-tag node)))
    (is (string= "hello" (node-sexpr node)))
    (is (string= "\"hello\"" (node-string node)))))

(test parse-string-with-escape
  "Test parsing string with escaped quote."
  (let ((node (parse-string "\"hello \\\"world\\\"\"")))
    (is (string= "hello \"world\"" (node-sexpr node)))))

;;; List parsing

(test parse-empty-list
  "Test parsing empty list."
  (let ((node (parse-string "()")))
    (is (eql :list (node-tag node)))
    (is (null (node-sexpr node)))))

(test parse-simple-list
  "Test parsing simple list."
  (let ((node (parse-string "(a b c)")))
    (is (eql :list (node-tag node)))
    (is (equal '(a b c) (node-sexpr node)))))

(test parse-nested-list
  "Test parsing nested list."
  (let ((node (parse-string "(a (b c) d)")))
    (is (equal '(a (b c) d) (node-sexpr node)))))

;;; Whitespace and comments

(test parse-preserves-whitespace
  "Test that parser preserves whitespace."
  (let ((nodes (parse-string-all "(a   b)")))
    (is (= 1 (length nodes)))
    (let ((children (node-children (first nodes))))
      ;; Should have: a, spaces, b
      (is (>= (length children) 3)))))

(test parse-line-comment
  "Test parsing line comment."
  (let ((nodes (parse-string-all (format nil "; comment~%foo"))))
    (is (= 3 (length nodes)))  ; comment, newline, foo
    (is (eql :comment (node-tag (first nodes))))))

(test parse-block-comment
  "Test parsing block comment."
  (let ((node (parse-string "#| comment |#")))
    (is (eql :block-comment (node-tag node)))))

(test parse-nested-block-comment
  "Test parsing nested block comment."
  (let ((node (parse-string "#| outer #| inner |# outer |#")))
    (is (eql :block-comment (node-tag node)))
    (is (string= "#| outer #| inner |# outer |#" (node-string node)))))

;;; Quote syntax

(test parse-quote
  "Test parsing quote."
  (let ((node (parse-string "'foo")))
    (is (eql :quote (node-tag node)))
    (is (string= "'foo" (node-string node)))))

(test parse-backquote
  "Test parsing backquote."
  (let ((node (parse-string "`foo")))
    (is (eql :syntax-quote (node-tag node)))))

(test parse-unquote
  "Test parsing unquote."
  (let ((node (parse-string ",foo")))
    (is (eql :unquote (node-tag node)))))

(test parse-unquote-splicing
  "Test parsing unquote-splicing."
  (let ((node (parse-string ",@foo")))
    (is (eql :unquote-splicing (node-tag node)))))

;;; Reader macros

(test parse-function
  "Test parsing #'function."
  (let ((node (parse-string "#'car")))
    (is (eql :function (node-tag node)))
    (is (string= "#'car" (node-string node)))))

(test parse-vector
  "Test parsing #(vector)."
  (let ((node (parse-string "#(1 2 3)")))
    (is (eql :vector (node-tag node)))
    (is (equalp #(1 2 3) (node-sexpr node)))))

(test parse-character
  "Test parsing #\\char."
  (let ((node (parse-string "#\\x")))
    (is (eql :character (node-tag node)))
    (is (char= #\x (node-sexpr node)))))

(test parse-character-name
  "Test parsing #\\Space."
  (let ((node (parse-string "#\\Space")))
    (is (char= #\Space (node-sexpr node)))))

(test parse-feature-positive
  "Test parsing #+feature."
  (let ((node (parse-string "#+sbcl foo")))
    (is (eql :feature-positive (node-tag node)))))

(test parse-feature-negative
  "Test parsing #-feature."
  (let ((node (parse-string "#-sbcl foo")))
    (is (eql :feature-negative (node-tag node)))))

(test parse-radix-binary
  "Test parsing #b binary."
  (let ((node (parse-string "#b101")))
    (is (= 5 (node-sexpr node)))))

(test parse-radix-hex
  "Test parsing #x hex."
  (let ((node (parse-string "#xFF")))
    (is (= 255 (node-sexpr node)))))

;;; Package-qualified symbols

(test parse-external-symbol
  "Test parsing pkg:sym."
  (let ((node (parse-string "cl:car")))
    (is (eq 'cl:car (node-sexpr node)))))

;;; Complex expressions

(test parse-defun
  "Test parsing defun form."
  (let ((node (parse-string "(defun foo (x) (+ x 1))")))
    (is (eql :list (node-tag node)))
    (let ((sexp (node-sexpr node)))
      (is (eq 'defun (first sexp)))
      (is (eq 'foo (second sexp))))))
