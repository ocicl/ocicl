(cl:in-package #:eclector.test)

(defun gen-integer-sequence ()
  (let ((i 0))
    (lambda () (incf i))))

(defun gen-labels-and-references (&key (atom  (gen-integer-sequence))
                                       (depth (gen-integer :min 1 :max 5)))
  (labels ((gen-inner (depth depth-reached max-depth labels)
             (case (cond ((= depth max-depth)
                          (incf (car depth-reached))
                          (random 2))
                         ((zerop (car depth-reached))
                          3)
                         (t
                          (random 3)))
               (0
                (let ((labels (alexandria:mappend #'append labels)))
                  (if (null labels)
                      (funcall atom)
                      (let ((label (alexandria:random-elt labels)))
                        (lambda () (cdr label))))))
               (1
                (funcall atom))
               (t
                (let ((label (cons nil '())))
                  (push label (first labels))
                  (let* ((labels (list* '() labels))
                         (length (funcall (fiveam:gen-integer
                                           :min (if (plusp (car depth-reached))
                                                    0
                                                    1)
                                           :max 4)))
                         (item (map-into (make-list length)
                                         (alexandria:curry
                                          #'gen-inner (1+ depth) depth-reached max-depth labels))))
                    (setf (cdr label) item)
                    item)))))
           (gen-root ()
             (fixup (gen-inner 0 (list 0) (funcall depth) (list (list)))
                    (make-hash-table :test #'eq)))
           (fixup (expression seen)
             (typecase expression
               (function
                (funcall expression))
               (cons
                (alexandria:ensure-gethash
                 expression seen
                 (progn
                   (setf (car expression) (fixup (car expression) seen)
                         (cdr expression) (fixup (cdr expression) seen))
                   expression)))
               (t
                expression))))
    #'gen-root))
