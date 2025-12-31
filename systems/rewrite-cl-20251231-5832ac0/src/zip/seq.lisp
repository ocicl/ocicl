;;;; seq.lisp - Sequence operations for zippers
;;;; SPDX-License-Identifier: MIT

(in-package #:rewrite-cl.zip)

;;; Map over children

(defun zip-map-children (zipper fn)
  "Apply FN to each non-whitespace child, return modified zipper at same position.
FN receives a zipper and should return a zipper."
  (cond
    ((not (node-inner-p (zip-node zipper)))
     zipper)
    (t
     (let ((child (zip-down* zipper)))
       (cond
         ((not child) zipper)
         (t
          (let ((last-child nil))
            (loop while child
                  do (setf child (funcall fn child))
                     (setf last-child child)
                     (setf child (zip-right* child)))
            ;; Navigate back up from the last processed child
            (zip-up (zip-leftmost last-child)))))))))

(defun zip-map-children-all (zipper fn)
  "Apply FN to each child (including whitespace), return modified zipper."
  (cond
    ((not (node-inner-p (zip-node zipper)))
     zipper)
    (t
     (let ((child (zip-down zipper)))
       (cond
         ((not child) zipper)
         (t
          (loop while child
                do (setf child (funcall fn child))
                while (zip-right child)
                do (setf child (zip-right child)))
          (zip-up (zip-leftmost child))))))))

;;; Filter children

(defun zip-filter-children (zipper predicate)
  "Remove children not matching PREDICATE. Returns modified zipper."
  (if (not (node-inner-p (zip-node zipper)))
      zipper
      (let* ((children (node-children (zip-node zipper)))
             (filtered (remove-if-not predicate children)))
        (if (equal children filtered)
            zipper
            (zip-replace zipper
                         (node-replace-children (zip-node zipper) filtered))))))

;;; Get child sexprs

(defun zip-child-sexprs (zipper)
  "Return list of sexprs for all sexpr-able children."
  (rewrite-cl.node:child-sexprs (zip-node zipper)))

;;; Length

(defun zip-length (zipper)
  "Count non-whitespace children."
  (if (not (node-inner-p (zip-node zipper)))
      0
      (loop for z = (zip-down* zipper) then (zip-right* z)
            while z
            count t)))

;;; Access by index

(defun zip-nth-child (zipper n)
  "Return zipper at Nth non-whitespace child (0-indexed)."
  (when (node-inner-p (zip-node zipper))
    (loop for z = (zip-down* zipper) then (zip-right* z)
          for i from 0
          while z
          when (= i n) return z)))

(defun zip-first-child (zipper)
  "Return zipper at first non-whitespace child."
  (zip-nth-child zipper 0))

(defun zip-second-child (zipper)
  "Return zipper at second non-whitespace child."
  (zip-nth-child zipper 1))

(defun zip-last-child (zipper)
  "Return zipper at last non-whitespace child."
  (when (node-inner-p (zip-node zipper))
    (let ((child (zip-down* zipper)))
      (when child
        (loop for z = child then (or (zip-right* z) z)
              while (zip-right* z)
              finally (return z))))))

;;; Get/Set for associative structures (plists)

(defun zip-plist-get (zipper key &key (test #'eql))
  "Get value for KEY in plist-structured node."
  (when (node-inner-p (zip-node zipper))
    (loop for kz = (zip-down* zipper) then (zip-right* (zip-right* kz))
          while kz
          when (and (node-sexpr-able-p (zip-node kz))
                    (funcall test key (node-sexpr (zip-node kz))))
            return (let ((vz (zip-right* kz)))
                     (when vz (zip-sexpr vz))))))

(defun zip-plist-set (zipper key value &key (test #'eql))
  "Set KEY to VALUE in plist-structured node.
Adds key-value pair if key not found."
  (when (node-inner-p (zip-node zipper))
    (let ((found nil))
      ;; Try to find and update existing key
      (loop for kz = (zip-down* zipper) then (zip-right* (zip-right* kz))
            while kz
            when (and (node-sexpr-able-p (zip-node kz))
                      (funcall test key (node-sexpr (zip-node kz))))
              do (setf found t)
                 (let ((vz (zip-right* kz)))
                   (when vz
                     (return (zip-up
                              (zip-replace vz (coerce-to-node value)))))))
      ;; If not found, append to end
      (unless found
        (let ((key-node (coerce-to-node key))
              (value-node (coerce-to-node value)))
          (zip-append-child
           (zip-append-child
            (zip-append-child
             (zip-append-child zipper (spaces 1))
             key-node)
            (spaces 1))
           value-node))))))
