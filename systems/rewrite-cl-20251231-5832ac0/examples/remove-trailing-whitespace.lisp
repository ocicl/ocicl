;;;; remove-trailing-whitespace.lisp
;;;; Example: Remove whitespace before closing parens in CL source

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system "rewrite-cl")

(defpackage #:remove-trailing-ws
  (:use #:cl #:rewrite-cl)
  (:import-from #:rewrite-cl.node
                #:whitespace-node-p
                #:newline-node-p
                #:seq-node-p))

(in-package #:remove-trailing-ws)

(defun trailing-whitespace-p (zipper)
  "Check if current node is whitespace/newline at end of a sequence."
  (and (or (whitespace-node-p (zip-node zipper))
           (newline-node-p (zip-node zipper)))
       ;; No sibling to the right means we're at the end
       (null (zip-right zipper))
       ;; Parent must be a sequence (list or vector)
       (let ((parent (zip-up zipper)))
         (and parent (seq-node-p (zip-node parent))))))

(defun remove-trailing-whitespace (source)
  "Remove whitespace before closing parens in SOURCE string."
  (let ((z (of-string source)))
    (when z
      ;; Walk the tree, removing trailing whitespace in sequences
      (loop with z = (of-string source)
            with changed = t
            while changed
            do (setf changed nil)
               (setf z (zip-prewalk
                        z
                        (lambda (zz)
                          (if (trailing-whitespace-p zz)
                              (progn
                                (setf changed t)
                                (zip-remove zz))
                              zz))))
            finally (return (zip-root-string z))))))

;;; Demo
(defun demo ()
  (format t "~%=== Remove Trailing Whitespace Demo ===~%~%")

  (let ((examples
          '(;; Simple case
            "(defun foo (x)
  (+ x 1   ))"

            ;; Multiple levels
            "(let ((a 1  )
      (b 2  ))
  (+ a b ))"

            ;; Mixed - whitespace in strings should be preserved
            "(format t \"hello world   \"   )"

            ;; Comments should be preserved
            "(defun bar ()
  ;; comment with trailing spaces
  (+ 1 2  ))"

            ;; Vector
            "#(1 2 3   )"

            ;; Already clean
            "(+ 1 2)")))

    (dolist (source examples)
      (let ((result (remove-trailing-whitespace source)))
        (format t "BEFORE:~%~A~%~%" source)
        (format t "AFTER:~%~A~%~%" result)
        (format t "~A~%" (make-string 50 :initial-element #\-))))))

;; Run demo
(demo)
