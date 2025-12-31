;;;; zipper-tests.lisp - Tests for zipper operations
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl/test)

(def-suite zipper-tests
  :description "Tests for zipper operations"
  :in rewrite-cl-tests)

(in-suite zipper-tests)

;;; Basic creation

(test zipper-of-string
  "Test creating zipper from string."
  (let ((z (of-string "(+ 1 2)")))
    (is (not (null z)))
    (is (eql :list (zip-tag z)))
    (is (equal '(+ 1 2) (zip-sexpr z)))))

(test zipper-of-node
  "Test creating zipper from node."
  (let* ((node (make-list-node (list (make-token-node 'a))))
         (z (of-node node)))
    (is (eq node (zip-node z)))))

;;; Navigation

(test zip-down
  "Test moving down."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down z)))
    (is (not (null z2)))
    (is (eq 'a (zip-sexpr z2)))))

(test zip-down-star
  "Test moving down with whitespace skip."
  (let* ((z (of-string "( a b c)"))  ; leading space
         (z2 (zip-down* z)))
    (is (eq 'a (zip-sexpr z2)))))

(test zip-right
  "Test moving right."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down z))
         (z3 (zip-right z2)))
    (is (not (null z3)))))

(test zip-right-star
  "Test moving right with whitespace skip."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2)))
    (is (eq 'b (zip-sexpr z3)))))

(test zip-left
  "Test moving left."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))
         (z4 (zip-left* z3)))
    (is (eq 'a (zip-sexpr z4)))))

(test zip-up
  "Test moving up."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-up z2)))
    (is (eql :list (zip-tag z3)))))

(test zip-next
  "Test depth-first next."
  (let* ((z (of-string "(a (b c))"))
         (z2 (zip-down* z))   ; at 'a'
         (z3 (zip-next* z2))) ; at '(b c)'
    (is (eql :list (zip-tag z3)))))

(test zip-root
  "Test navigating to root."
  (let* ((z (of-string "(a (b c))"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))
         (z4 (zip-down* z3))
         (root (zip-root z4)))
    (is (equal '(a (b c)) (zip-sexpr root)))))

;;; Modification

(test zip-replace
  "Test replacing node."
  (let* ((z (of-string "(+ 1 2)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 1
         (z4 (zip-replace z3 (make-token-node 10))))
    (is (string= "(+ 10 2)" (zip-root-string z4)))))

(test zip-edit
  "Test editing node via sexpr."
  (let* ((z (of-string "(+ 1 2)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 1
         (z4 (zip-edit z3 #'1+)))
    (is (string= "(+ 2 2)" (zip-root-string z4)))))

(test zip-insert-left
  "Test inserting left sibling."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 'b'
         (z4 (zip-insert-left z3 (make-token-node 'x))))
    (is (equal '(a x b) (zip-sexpr (zip-root z4))))))

(test zip-insert-right
  "Test inserting right sibling."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-down* z))  ; at 'a'
         (z3 (zip-insert-right z2 (make-token-node 'x))))
    (is (equal '(a x b) (zip-sexpr (zip-root z3))))))

(test zip-remove
  "Test removing node."
  (let* ((z (of-string "(a b c)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at 'b'
         (z4 (zip-remove z3)))
    (is (equal '(a c) (zip-sexpr (zip-root z4))))))

(test zip-insert-child
  "Test inserting first child."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-insert-child z (make-token-node 'x))))
    (is (equal '(x a b) (zip-sexpr z2)))))

(test zip-append-child
  "Test appending child."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-append-child z (make-token-node 'x))))
    (is (equal '(a b x) (zip-sexpr z2)))))

;;; Find operations

(test zip-find-value
  "Test finding by value."
  (let* ((z (of-string "(defun foo (x) (+ x 1))"))
         (z2 (zip-find-value z '+)))
    (is (not (null z2)))
    (is (eq '+ (zip-sexpr z2)))))

(test zip-find-tag
  "Test finding by tag."
  (let* ((z (of-string "(a \"hello\" b)"))
         (z2 (zip-find-tag z :string)))
    (is (not (null z2)))
    (is (string= "hello" (zip-sexpr z2)))))

;;; Walk operations

(test zip-prewalk
  "Test prewalk transformation."
  (let* ((z (of-string "(+ 1 2)"))
         (z2 (zip-prewalk z
               (lambda (zz)
                 (if (and (member (zip-tag zz) '(:integer :number))
                          (numberp (zip-sexpr zz)))
                     (zip-edit zz (lambda (n) (* 2 n)))
                     zz)))))
    (is (string= "(+ 2 4)" (zip-root-string z2)))))

(test zip-postwalk
  "Test postwalk transformation."
  (let* ((z (of-string "(+ 1 (* 2 3))"))
         (z2 (zip-postwalk z
               (lambda (zz)
                 (if (and (member (zip-tag zz) '(:integer :number))
                          (numberp (zip-sexpr zz)))
                     (zip-edit zz #'1+)
                     zz)))))
    ;; All numbers should be incremented
    (let ((result (zip-sexpr (zip-root z2))))
      (is (= 2 (second result)))
      (is (= 3 (second (third result))))
      (is (= 4 (third (third result)))))))

;;; Sequence operations

(test zip-length
  "Test counting children."
  (let ((z (of-string "(a b c d)")))
    (is (= 4 (zip-length z)))))

(test zip-nth-child
  "Test accessing nth child."
  (let ((z (of-string "(a b c d)")))
    (is (eq 'a (zip-sexpr (zip-nth-child z 0))))
    (is (eq 'b (zip-sexpr (zip-nth-child z 1))))
    (is (eq 'd (zip-sexpr (zip-nth-child z 3))))))

(test zip-child-sexprs
  "Test getting child sexprs."
  (let ((z (of-string "(a b c)")))
    (is (equal '(a b c) (zip-child-sexprs z)))))

;;; Subedit

(test zip-subedit
  "Test isolated subtree editing."
  (let* ((z (of-string "(a (b c) d)"))
         (z2 (zip-down* z))
         (z3 (zip-right* z2))  ; at (b c)
         (z4 (zip-subedit z3
               (lambda (subz)
                 (zip-append-child subz (make-token-node 'x))))))
    (is (equal '(a (b c x) d) (zip-sexpr (zip-root z4))))))

;;; Whitespace operations

(test zip-whitespace-p
  "Test whitespace predicate."
  (let* ((z (of-string "(a b)"))
         (z2 (zip-down z))   ; 'a'
         (z3 (zip-right z2))) ; whitespace
    (is (not (zip-whitespace-p z2)))
    (is (zip-whitespace-p z3))))
