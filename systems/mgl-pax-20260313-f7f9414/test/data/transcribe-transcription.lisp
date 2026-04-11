(defclass aaa ()
  ((aaa :initarg :aaa :reader aaa)))

(defmethod print-object ((aaa aaa) stream)
  #-ecl
  (print-unreadable-object (aaa stream :type t)
    (format stream "~%~%~S" (aaa aaa)))
  #+ecl
  (format stream "#<AAA ~%~%~S>" (aaa aaa)))

;;; Basic forms
42
(values 42 7)
(princ 42)
(values (princ 42) 7)
(values)

;;; Forms with output markers
42
..
(values 42 7)
..
(princ 42)
.. 42
(values (princ 42) 7)
.. 42
(values)
..

;;; Forms with values markers
42
=> 42
(values 42 7)
=> 42
=> 7
(princ 42)
=> 42
(values (princ 42) 7)
=> 42
=> 7
(values)
=> ; No value

;;; Forms with output and values markers
42
..
=> 42
(values 42 7)
..
=> 42
=> 7
(princ 42)
.. 42
=> 42
(values (princ 42) 7)
.. 42
=> 42
=> 7
(values)
..
=> ; No value

;;; unreadable value (with empty line, must emit no trailing
;;; whitespace)
(make-instance 'aaa :aaa 42)
==> #<AAA 
-->
--> 42>

;;; hand-formatted readable output
(list 1 2)
=> (1
      2)

;;; output with empty line must emit no trailing whitespace
(terpri)
..
..

;;; ending comment
