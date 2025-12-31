;;;; roundtrip-tests.lisp - Round-trip parsing tests
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl/test)

(def-suite roundtrip-tests
  :description "Tests for round-trip parsing"
  :in rewrite-cl-tests)

(in-suite roundtrip-tests)

(defun roundtrip (source)
  "Parse SOURCE and serialize back to string."
  (let ((nodes (parse-string-all source)))
    (rewrite-cl.node:nodes-string nodes)))

(defmacro test-roundtrip (name source)
  "Define a round-trip test."
  `(test ,name
     (is (string= ,source (roundtrip ,source)))))

;;; Basic forms

(test-roundtrip rt-symbol "foo")
(test-roundtrip rt-keyword ":foo")
(test-roundtrip rt-number "42")
(test-roundtrip rt-negative "-5")
(test-roundtrip rt-float "3.14")
(test-roundtrip rt-ratio "1/2")

;;; Strings

(test-roundtrip rt-string "\"hello\"")
(test-roundtrip rt-string-escape "\"hello\\nworld\"")
(test-roundtrip rt-string-quote "\"she said \\\"hi\\\"\"")

;;; Lists

(test-roundtrip rt-empty-list "()")
(test-roundtrip rt-simple-list "(a b c)")
(test-roundtrip rt-nested-list "(a (b c) d)")
(test-roundtrip rt-deep-nesting "(((a)))")

;;; Whitespace preservation

(test-roundtrip rt-extra-spaces "(a   b)")
(test-roundtrip rt-leading-space "( a b)")
(test-roundtrip rt-trailing-space "(a b )")
(test-roundtrip rt-newlines "(a
b
c)")
(test-roundtrip rt-mixed-whitespace "(a  b
  c)")

;;; Comments

(test-roundtrip rt-line-comment "; comment")
(test-roundtrip rt-comment-and-code "; comment
foo")
(test-roundtrip rt-inline-comment "(a ; comment
b)")
(test-roundtrip rt-block-comment "#| block |#")
(test-roundtrip rt-nested-block-comment "#| outer #| inner |# |#")

;;; Quote syntax

(test-roundtrip rt-quote "'foo")
(test-roundtrip rt-quote-list "'(a b c)")
(test-roundtrip rt-backquote "`foo")
(test-roundtrip rt-backquote-list "`(a ,b ,@c)")
(test-roundtrip rt-unquote ",foo")
(test-roundtrip rt-unquote-splicing ",@foo")

;;; Reader macros

(test-roundtrip rt-function "#'car")
(test-roundtrip rt-vector "#(1 2 3)")
(test-roundtrip rt-character "#\\x")
(test-roundtrip rt-character-name "#\\Space")
(test-roundtrip rt-feature-plus "#+sbcl foo")
(test-roundtrip rt-feature-minus "#-sbcl foo")
(test-roundtrip rt-binary "#b101")
(test-roundtrip rt-octal "#o777")
(test-roundtrip rt-hex "#xFF")
(test-roundtrip rt-pathname "#P\"foo.lisp\"")
(test-roundtrip rt-bit-vector "#*101")

;;; Complex expressions

(test-roundtrip rt-defun "(defun foo (x)
  \"A function.\"
  (+ x 1))")

(test-roundtrip rt-let "(let ((x 1)
      (y 2))
  (+ x y))")

(test-roundtrip rt-cond "(cond
  ((null x) 'empty)
  ((atom x) 'atom)
  (t 'list))")

(test-roundtrip rt-loop "(loop for i from 0 below 10
      collect i)")

;;; Package-qualified symbols

(test-roundtrip rt-external-symbol "cl:car")
(test-roundtrip rt-internal-symbol "pkg::internal")

;;; Edge cases

(test-roundtrip rt-nil "nil")
(test-roundtrip rt-t "t")
(test-roundtrip rt-empty-string "\"\"")
(test-roundtrip rt-only-whitespace "   ")
(test-roundtrip rt-only-newlines "

")

;;; Real-world patterns

(test-roundtrip rt-asdf-system "(defsystem \"my-system\"
  :version \"1.0.0\"
  :depends-on (\"alexandria\"
               \"cl-ppcre\")
  :components
  ((:file \"package\")
   (:file \"main\" :depends-on (\"package\"))))")

(test-roundtrip rt-method "(defmethod foo ((x string) (y integer))
  (format nil \"~A: ~D\" x y))")

(test-roundtrip rt-macro-with-backquote "(defmacro with-foo ((var) &body body)
  `(let ((,var (get-foo)))
     (unwind-protect
         (progn ,@body)
       (release-foo ,var))))")

;;; Stress tests

(test roundtrip-preserves-all
  "Test that various constructs round-trip correctly."
  (dolist (source (list "(+ 1 2)"
                        "(defun foo (x) x)"
                        "'(a b c)"
                        "`(,a ,@b)"
                        "#'car"
                        "#(1 2 3)"
                        (format nil "; comment~%code")
                        "#| block |#"
                        (format nil "(a ; inline~% b)")
                        "#+feature form"
                        "#\\Newline"))
    (is (string= source (roundtrip source))
        "Round-trip failed for: ~S" source)))

;;; Source file roundtrip test

(defun find-lisp-files (directory)
  "Find all .lisp files under DIRECTORY."
  (let ((files '()))
    (uiop:collect-sub*directories
     directory
     (constantly t)
     (constantly t)
     (lambda (dir)
       (dolist (file (uiop:directory-files dir "*.lisp"))
         (push file files))))
    (nreverse files)))

(test roundtrip-source-files
  "Test that all source files in src/ roundtrip exactly."
  (let* ((base-dir (asdf:system-source-directory "rewrite-cl"))
         (src-dir (merge-pathnames "src/" base-dir))
         (files (find-lisp-files src-dir)))
    (dolist (file files)
      (let* ((original (uiop:read-file-string file))
             (nodes (parse-string-all original))
             (output (rewrite-cl.node:nodes-string nodes)))
        (is (string= original output)
            "Source file ~A failed roundtrip at position ~D"
            (enough-namestring file base-dir)
            (mismatch original output))))))
