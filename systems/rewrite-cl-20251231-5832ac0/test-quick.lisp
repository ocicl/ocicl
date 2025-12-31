;;;; Quick test script for rewrite-cl

(require :asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system "rewrite-cl")

(in-package :rewrite-cl)

(format t "~%=== Testing rewrite-cl ===~%")

;; Test basic parsing
(format t "~%1. Basic parsing:~%")
(let ((z (of-string "(+ 1 2)")))
  (format t "   Input: \"(+ 1 2)\"~%")
  (format t "   Parsed: ~A~%" (zip-string z))
  (format t "   Sexpr: ~A~%" (zip-sexpr z)))

;; Test navigation
(format t "~%2. Navigation:~%")
(let* ((z (of-string "(defun foo (x) x)"))
       (z2 (zip-down* z)))
  (format t "   Input: \"(defun foo (x) x)\"~%")
  (format t "   First child (zip-down*): ~A~%" (zip-sexpr z2))
  (let ((z3 (zip-right* z2)))
    (format t "   Second child (zip-right*): ~A~%" (zip-sexpr z3))))

;; Test modification
(format t "~%3. Modification:~%")
(let* ((z (of-string "(+ 1 2)"))
       (z2 (zip-down* z))
       (z3 (zip-right* z2))
       (z4 (zip-replace z3 (make-token-node 10))))
  (format t "   Input: \"(+ 1 2)\"~%")
  (format t "   Replace 1 with 10: ~A~%" (zip-root-string z4)))

;; Test round-trip
(format t "~%4. Round-trip:~%")
(let* ((source "(defun hello (name)
  \"Say hello.\"
  (format t \"Hello, ~A!~%\" name))")
       (nodes (rewrite-cl.parser:parse-string-all source))
       (result (rewrite-cl.node:nodes-string nodes)))
  (format t "   Original preserved: ~A~%" (string= source result)))

(format t "~%=== All tests passed! ===~%")
