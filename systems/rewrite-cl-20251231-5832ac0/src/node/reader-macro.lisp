;;;; reader-macro.lisp - Reader macro nodes
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (reader-macro-node (:constructor %make-reader-macro-node))
  "Represents reader macro forms: #', #., #+, #-, etc.
TAG identifies the specific macro type.
PREFIX is the source prefix (e.g., \"#'\").
CHILDREN contains the arguments to the reader macro."
  (tag :reader-macro :type keyword :read-only t)
  (prefix "" :type string :read-only t)
  (children nil :type list :read-only t)
  (position nil :type (or null source-position) :read-only t))

;;; Function shorthand: #'symbol

(defun make-function-node (child &optional position)
  "Create a function shorthand node: #'form"
  (%make-reader-macro-node :tag :function
                           :prefix "#'"
                           :children (list child)
                           :position position))

;;; Read-time eval: #.form

(defun make-read-eval-node (child &optional position)
  "Create a read-time eval node: #.form"
  (%make-reader-macro-node :tag :read-eval
                           :prefix "#."
                           :children (list child)
                           :position position))

;;; Feature expressions: #+feature form, #-feature form

(defun make-feature-node (polarity feature-expr form &optional position)
  "Create a feature expression node.
POLARITY is :positive (#+) or :negative (#-).
FEATURE-EXPR is the feature test.
FORM is the conditional form."
  (%make-reader-macro-node :tag (if (eql polarity :positive)
                                    :feature-positive
                                    :feature-negative)
                           :prefix (if (eql polarity :positive)
                                       "#+"
                                       "#-")
                           :children (list feature-expr form)
                           :position position))

;;; Uninterned symbol: #:symbol

(defun make-uninterned-symbol-node (child &optional position)
  "Create an uninterned symbol node: #:symbol"
  (%make-reader-macro-node :tag :uninterned-symbol
                           :prefix "#:"
                           :children (list child)
                           :position position))

;;; Pathname: #p\"path\" or #P\"path\"

(defun make-pathname-node (child prefix &optional position)
  "Create a pathname node: #p\"path\" or #P\"path\""
  (%make-reader-macro-node :tag :pathname
                           :prefix prefix
                           :children (list child)
                           :position position))

;;; Bit vector: #*101

(defun make-bit-vector-node (source-text value &optional position)
  "Create a bit vector node: #*bits"
  (declare (ignore value))
  (%make-reader-macro-node :tag :bit-vector
                           :prefix source-text  ; Store full #*... as prefix
                           :children nil        ; Value stored implicitly
                           :position position))

;;; Array: #nA(...)

(defun make-array-node (prefix child &optional position)
  "Create an array node: #nA(...)"
  (%make-reader-macro-node :tag :array
                           :prefix prefix
                           :children (list child)
                           :position position))

;;; Radix: #b, #o, #x, #nr

(defun make-radix-node (prefix source-text value &optional position)
  "Create a radix number node: #b101, #o777, #xFF, #3r12"
  (declare (ignore prefix value))
  (%make-reader-macro-node :tag :radix
                           :prefix source-text  ; Store full source
                           :children nil
                           :position position))

;;; Complex: #c(real imag)

(defun make-complex-node (child &optional position)
  "Create a complex number node: #c(real imag)"
  (%make-reader-macro-node :tag :complex
                           :prefix "#c"
                           :children (list child)
                           :position position))

;;; Unknown/opaque macro

(defun make-unknown-macro-node (prefix children &optional position)
  "Create an unknown reader macro node."
  (%make-reader-macro-node :tag :unknown-macro
                           :prefix prefix
                           :children children
                           :position position))

;;; Protocol implementations

(defmethod node-tag ((node reader-macro-node))
  (reader-macro-node-tag node))

(defmethod node-string ((node reader-macro-node))
  (let ((tag (reader-macro-node-tag node)))
    (case tag
      ((:bit-vector :radix)
       ;; For these, prefix contains the full source text
       (reader-macro-node-prefix node))
      (otherwise
       (concatenate 'string
                    (reader-macro-node-prefix node)
                    (nodes-string (reader-macro-node-children node)))))))

(defmethod node-inner-p ((node reader-macro-node))
  (not (null (reader-macro-node-children node))))

(defmethod node-children ((node reader-macro-node))
  (reader-macro-node-children node))

(defmethod node-replace-children ((node reader-macro-node) children)
  (%make-reader-macro-node :tag (reader-macro-node-tag node)
                           :prefix (reader-macro-node-prefix node)
                           :children children
                           :position (reader-macro-node-position node)))

(defmethod node-sexpr ((node reader-macro-node))
  (let ((tag (reader-macro-node-tag node))
        (children (reader-macro-node-children node)))
    (case tag
      (:function
       `(function ,(node-sexpr (first children))))
      (:read-eval
       ;; Read-time eval - we just return the form, not evaluate it
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          ,(node-sexpr (first children))))
      ((:feature-positive :feature-negative)
       ;; Feature conditionals - return a symbolic representation
       ;; since we can't embed reader macros in sexpr output
       (let ((feature (node-sexpr (first children)))
             (form (node-sexpr (second children))))
         (if (eql tag :feature-positive)
             (list 'feature-if feature form)
             (list 'feature-if-not feature form))))
      (:uninterned-symbol
       (make-symbol (node-string (first children))))
      (:pathname
       (pathname (node-sexpr (first children))))
      (:complex
       (let ((pair (node-sexpr (first children))))
         (complex (first pair) (second pair))))
      (:bit-vector
       ;; Parse bit-vector from source: #*101 -> #(1 0 1)
       (let* ((source (reader-macro-node-prefix node))
              (bits (subseq source 2)))  ; skip "#*"
         (parse-bits-to-vector bits)))
      (:array
       ;; Array literal #nA(...) - construct array from children
       (let* ((prefix (reader-macro-node-prefix node))
              (dims (parse-integer (subseq prefix 1 (1- (length prefix)))))
              (contents (node-sexpr (first children))))
         (make-array-from-contents dims contents)))
      (otherwise
       ;; For radix (shouldn't happen - uses token-node) and unknown
       ;; Return a symbolic representation
       (list :reader-macro (reader-macro-node-prefix node)
             (mapcar #'node-sexpr children))))))

(defun parse-bits-to-vector (bits-string)
  "Convert a string of 0s and 1s to a bit-vector."
  (let* ((len (length bits-string))
         (bv (make-array len :element-type 'bit)))
    (dotimes (i len bv)
      (setf (aref bv i) (if (char= (char bits-string i) #\1) 1 0)))))

(defun make-array-from-contents (dims contents)
  "Create an array from nested list contents."
  (if (= dims 1)
      (coerce contents 'vector)
      (make-array (array-dimensions-from-contents dims contents)
                  :initial-contents contents)))

(defun array-dimensions-from-contents (dims contents)
  "Infer array dimensions from nested list structure."
  (if (= dims 0)
      nil
      (cons (length contents)
            (if (> dims 1)
                (array-dimensions-from-contents (1- dims) (first contents))
                nil))))

(defmethod node-position ((node reader-macro-node))
  (reader-macro-node-position node))

(defmethod node-set-position ((node reader-macro-node) position)
  (%make-reader-macro-node :tag (reader-macro-node-tag node)
                           :prefix (reader-macro-node-prefix node)
                           :children (reader-macro-node-children node)
                           :position position))
