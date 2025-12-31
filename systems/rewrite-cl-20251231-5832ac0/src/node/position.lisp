;;;; position.lisp - Source position tracking
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.node)

(defstruct (source-position (:conc-name pos-)
                            (:constructor make-source-position
                                (line column &optional end-line end-column)))
  "Represents a position or span in source code.
LINE and COLUMN are 1-based. END-LINE and END-COLUMN are optional
and represent the end of a span."
  (line 1 :type (integer 1) :read-only t)
  (column 1 :type (integer 1) :read-only t)
  (end-line nil :type (or null (integer 1)) :read-only t)
  (end-column nil :type (or null (integer 1)) :read-only t))

(defun position-span-p (pos)
  "Return T if POS represents a span (has end position)."
  (and pos (pos-end-line pos)))

(defun position= (pos1 pos2)
  "Return T if two positions are equal."
  (and (eql (pos-line pos1) (pos-line pos2))
       (eql (pos-column pos1) (pos-column pos2))
       (eql (pos-end-line pos1) (pos-end-line pos2))
       (eql (pos-end-column pos1) (pos-end-column pos2))))

(defmethod print-object ((pos source-position) stream)
  (print-unreadable-object (pos stream :type t)
    (if (pos-end-line pos)
        (format stream "~D:~D-~D:~D"
                (pos-line pos) (pos-column pos)
                (pos-end-line pos) (pos-end-column pos))
        (format stream "~D:~D"
                (pos-line pos) (pos-column pos)))))
