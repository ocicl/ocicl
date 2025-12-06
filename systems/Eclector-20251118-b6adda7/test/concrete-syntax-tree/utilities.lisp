(cl:in-package #:eclector.concrete-syntax-tree.test)

(defmacro both ((operator cst))
  (let ((cst-operator (find-symbol (symbol-name operator)
                                   (find-package '#:cst))))
    (alexandria:once-only (cst)
      `(values (,cst-operator ,cst) (,operator (cst:raw ,cst))))))

(defun is-consistent-with-raw (cst)
  (let* ((seen (make-hash-table :test #'eq))
         (atom->cst (make-hash-table :test #'eq))
         (tail (list cst))
         (worklist tail))
    (labels ((enqueue (item)
               (let ((cell (list item)))
                 (if (null worklist)
                     (setf worklist cell)
                     (setf (cdr tail) cell))
                 (setf tail cell)))
             (check-raw (actual-raw cst)
               (let ((raw (cst:raw cst)))
                 (unless (eq actual-raw raw)
                   (fail "~@<Expected the raw of CST ~A to be ~S but it is ~
                          ~S.~@:>"
                         cst actual-raw raw))))
             (check-cst (cst)
               (unless (gethash cst seen)
                 (setf (gethash cst seen) t)
                 (if (typep cst 'eclector.concrete-syntax-tree:wrapper-cst)
                     (check-cst (eclector.concrete-syntax-tree:target cst))
                     (multiple-value-bind (cst-atom-p raw-atom-p)
                         (both (atom cst))
                       (unless (eq raw-atom-p cst-atom-p)
                         (fail "~@<Expected raw and CST to either both be ~
                                atoms or both be conses but raw is ~
                                ~:[a cons~;an atom~] and CST is ~
                                ~:[a cons~;an atom~].~@:>"
                               raw-atom-p cst-atom-p))
                       (if raw-atom-p
                           (let ((raw-atom (cst:raw cst)))
                             ;; For atoms like pathnames, instances of
                             ;; structure classes or instances of
                             ;; standard classes, we expect multiple
                             ;; `eq' occurrences in the raw expression
                             ;; to have `eq' corresponding CSTs.
                             ;;
                             ;; We may have to refine this later since
                             ;; something like
                             ;;
                             ;;   (is-consistent-with-raw
                             ;;    (let ((* #P"foo"))
                             ;;      (eclector.concrete-syntax-tree:read-from-string
                             ;;       "(#.* #.*)")))
                             ;;
                             ;; would currently result in a false positive.
                             (unless (typep raw-atom '(or number
                                                          character
                                                          symbol))
                               (multiple-value-bind (existing-cst foundp)
                                   (gethash raw-atom atom->cst)
                                 (if foundp
                                     (unless (eq cst existing-cst)
                                       (fail "~@<Expected a single CST to be ~
                                              associated with ~S but ~
                                              encountered CSTs ~S and ~S.~@:>"
                                             raw-atom cst existing-cst))
                                     (setf (gethash raw-atom atom->cst) cst)))))
                           (multiple-value-bind (cst-car raw-car)
                               (both (first cst))
                             (multiple-value-bind (cst-cdr raw-cdr)
                                 (both (rest cst))
                               (check-raw raw-car cst-car)
                               (check-raw raw-cdr cst-cdr)
                               (enqueue cst-car)
                               (enqueue cst-cdr)))))))))
      (loop for cst = (pop worklist)
            until (null cst)
            do (check-cst cst)))))

(defun valid-cst-parse-result-p (client root-cst)
  (let* ((seen (make-hash-table :test #'eq))
         (tail (list root-cst))
         (worklist tail))
    (labels ((enqueue (item)
               (let ((cell (list item)))
                 (if (null worklist)
                     (setf worklist cell)
                     (setf (cdr tail) cell))
                 (setf tail cell)))
             (check-cst (cst)
               (unless (gethash cst seen)
                 (setf (gethash cst seen) t)
                 (typecase cst
                   (cst:atom-cst
                    (when (eclector.reader:labeled-object-state
                           client (cst:raw cst))
                      (return-from valid-cst-parse-result-p nil)))
                   (cst:cons-cst
                    (enqueue (cst:first cst))
                    (enqueue (cst:rest cst)))
                   (eclector.concrete-syntax-tree:wrapper-cst
                    (check-cst (eclector.concrete-syntax-tree:target cst)))))))
      (loop for cst = (pop worklist)
            until (null cst)
            do (check-cst cst))
      t)))
