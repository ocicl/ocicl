;;;; node-tests.lisp - Tests for node types
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl/test)

(def-suite node-tests
  :description "Tests for node types"
  :in rewrite-cl-tests)

(in-suite node-tests)

;;; Whitespace nodes

(test whitespace-node-creation
  "Test whitespace node creation and accessors."
  (let ((node (make-whitespace-node "  ")))
    (is (eql :whitespace (node-tag node)))
    (is (string= "  " (node-string node)))
    (is (= 2 (node-length node)))
    (is (null (node-children node)))
    (is (not (node-inner-p node)))
    (is (node-printable-only-p node))
    (is (not (node-sexpr-able-p node)))))

(test newline-node-creation
  "Test newline node creation."
  (let ((node (make-newline-node)))
    (is (eql :newline (node-tag node)))
    (is (string= (string #\Newline) (node-string node)))
    (is (node-printable-only-p node))))

(test spaces-helper
  "Test spaces helper function."
  (let ((node (spaces 4)))
    (is (string= "    " (node-string node)))))

;;; Token nodes

(test token-node-symbol
  "Test token node with symbol."
  (let ((node (make-token-node 'foo)))
    (is (eql :symbol (node-tag node)))
    (is (eq 'foo (node-sexpr node)))
    (is (node-sexpr-able-p node))
    (is (not (node-printable-only-p node)))))

(test token-node-keyword
  "Test token node with keyword."
  (let ((node (make-token-node :foo)))
    (is (eql :keyword (node-tag node)))
    (is (eql :foo (node-sexpr node)))))

(test token-node-number
  "Test token node with number."
  (let ((node (make-token-node 42)))
    (is (eql :integer (node-tag node)))
    (is (= 42 (node-sexpr node)))))

(test token-node-preserves-string
  "Test that token node preserves original string."
  (let ((node (make-token-node 'foo "FOO")))
    (is (string= "FOO" (node-string node)))
    (is (eq 'foo (node-sexpr node)))))

;;; String nodes

(test string-node-creation
  "Test string node creation."
  (let ((node (make-string-node "hello" "\"hello\"")))
    (is (eql :string (node-tag node)))
    (is (string= "hello" (node-sexpr node)))
    (is (string= "\"hello\"" (node-string node)))))

;;; Comment nodes

(test comment-node-creation
  "Test comment node creation."
  (let ((node (make-comment-node "; this is a comment")))
    (is (eql :comment (node-tag node)))
    (is (string= "; this is a comment" (node-string node)))
    (is (node-printable-only-p node))))

;;; Sequence nodes

(test list-node-creation
  "Test list node creation."
  (let ((node (make-list-node (list (make-token-node 'a)
                                    (spaces 1)
                                    (make-token-node 'b)))))
    (is (eql :list (node-tag node)))
    (is (node-inner-p node))
    (is (= 3 (length (node-children node))))
    (is (string-equal "(a b)" (node-string node)))  ; case-insensitive
    (is (equal '(a b) (node-sexpr node)))))

(test list-node-empty
  "Test empty list node."
  (let ((node (make-list-node nil)))
    (is (string= "()" (node-string node)))
    (is (null (node-sexpr node)))))

(test vector-node-creation
  "Test vector node creation."
  (let ((node (make-vector-node (list (make-token-node 1)
                                      (spaces 1)
                                      (make-token-node 2)))))
    (is (eql :vector (node-tag node)))
    (is (string= "#(1 2)" (node-string node)))
    (is (equalp #(1 2) (node-sexpr node)))))

(test node-replace-children
  "Test replacing children in sequence node."
  (let* ((orig (make-list-node (list (make-token-node 'a))))
         (new (node-replace-children orig (list (make-token-node 'b)))))
    (is (string-equal "(a)" (node-string orig)))  ; case-insensitive
    (is (string-equal "(b)" (node-string new)))))

;;; Quote nodes

(test quote-node-creation
  "Test quote node creation."
  (let ((node (make-quote-node (make-token-node 'foo))))
    (is (eql :quote (node-tag node)))
    (is (string-equal "'foo" (node-string node)))  ; case-insensitive
    (is (node-inner-p node))
    (is (= 1 (length (node-children node))))))

(test syntax-quote-node-creation
  "Test syntax quote node creation."
  (let ((node (make-syntax-quote-node (make-token-node 'foo))))
    (is (eql :syntax-quote (node-tag node)))
    (is (string-equal "`foo" (node-string node)))))  ; case-insensitive

(test unquote-node-creation
  "Test unquote node creation."
  (let ((node (make-unquote-node (make-token-node 'foo))))
    (is (eql :unquote (node-tag node)))
    (is (string-equal ",foo" (node-string node)))))  ; case-insensitive

(test unquote-splicing-node-creation
  "Test unquote-splicing node creation."
  (let ((node (make-unquote-splicing-node (make-token-node 'foo))))
    (is (eql :unquote-splicing (node-tag node)))
    (is (string-equal ",@foo" (node-string node)))))  ; case-insensitive

;;; Coercion

(test coerce-symbol
  "Test coercing symbol to node."
  (let ((node (coerce-to-node 'foo)))
    (is (token-node-p node))
    (is (eq 'foo (node-sexpr node)))))

(test coerce-list
  "Test coercing list to node."
  (let ((node (coerce-to-node '(a b c))))
    (is (seq-node-p node))
    (is (equal '(a b c) (node-sexpr node)))))

(test coerce-nested
  "Test coercing nested structure."
  (let ((node (coerce-to-node '(a (b c) d))))
    (is (string-equal "(a (b c) d)" (node-string node)))))  ; case-insensitive
