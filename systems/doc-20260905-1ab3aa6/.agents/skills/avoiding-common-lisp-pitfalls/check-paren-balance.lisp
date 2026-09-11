;;; check-paren-balance.lisp
;;; Tool for the "avoiding-common-lisp-pitfalls" skill.
;;; Usage from a REPL (e.g. via eval_lisp_form):
;;;   (load "check-paren-balance.lisp")
;;;   (check-paren-balance "src/big-file.lisp")
;;; Requires: uiop (bundled with ASDF).
;;; - A NEGATIVE depth at line N flags an extra `)` on that line.
;;; - A non-zero final depth means missing `)`.
;;; - "Final depth: 0" means the file is balanced.

(defun check-paren-balance (path)
  "Print paren-balance diagnostics for a Lisp source file."
  (let ((content (uiop:read-file-string path))
        (depth 0))
    (loop for line in (uiop:split-string content :separator '(#\Newline))
          for i from 1
          do (let ((in-str nil) (in-com nil))
               (loop for ch across line
                     do (cond (in-com)
                              (in-str (when (char= ch #\") (setf in-str nil)))
                              ((char= ch #\;) (setf in-com t))
                              ((char= ch #\") (setf in-str t))
                              ((char= ch #\() (incf depth))
                              ((char= ch #\)) (decf depth))))
               (when (< depth 0)
                 (format t "*** EXTRA `)` at line ~A~%" i)
                 (return)))
          finally (format t "Final depth: ~A (0 = balanced)~%" depth))))
