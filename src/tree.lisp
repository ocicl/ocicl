;;; tree.lisp
;;;
;;; SPDX-License-Identifier: MIT

(defpackage tree
  (:documentation "Simple tree printing utilities for displaying hierarchical data.")
  (:use :cl)
  (:export
   #:node-equal
   #:node-children
   #:print-node
   #:print-tree))

(in-package :tree)

(defmethod node-equal ((node-1 t) (node-2 t))
  (equal node-1 node-2))

(defmethod print-node ((node t) stream)
  (princ node stream))

(defmethod node-children ((node t))
  nil)

(defun %print-tree (nodes stream &key
                                   (last-node-prefix "`- ")
                                   (node-prefix "|- ")
                                   (continue-prefix "| ")
                                   (max-depth nil)
                                   (depth 0)
                                   prefix)
  "Internal helper for printing tree structures."
  (let ((node (first nodes)))
    (when prefix
      (write-string prefix stream)
      (when (> depth 1)
        (write-string " "))
      (write-string
       (if (rest nodes)
           node-prefix
           last-node-prefix)
       stream))
    (print-node node stream)
    (terpri)
    (unless (and max-depth (>= depth max-depth))
      (maplist (lambda (node)
                 (%print-tree node stream
                              :depth (1+ depth)
                              :max-depth max-depth
                              :last-node-prefix last-node-prefix
                              :node-prefix node-prefix
                              :continue-prefix continue-prefix
                              :prefix
                              (if prefix
                                  (concatenate 'string prefix
                                               (when (> depth 1)
                                                  " ")
                                               (if (rest nodes)
                                                   continue-prefix
                                                   "  "))
                                  ;; handle the top case
                                  "")))
               (node-children node)))
    (values)))

(defun print-tree (node &key (stream *standard-output*)
                          unicode
                          (max-depth nil))
  "Print a tree structure starting from NODE to STREAM."
  (if unicode
      (%print-tree (list node) stream :node-prefix "├─ "
                                      :last-node-prefix "└─ "
                                      :continue-prefix "│ "
                                      :max-depth max-depth)
      (%print-tree (list node) stream :max-depth max-depth)))
