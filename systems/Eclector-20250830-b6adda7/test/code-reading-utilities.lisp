(cl:in-package #:eclector.test)

(defun map-all-system-files (function
                             &key (systems '("eclector"
                                             "eclector/test"
                                             "eclector-concrete-syntax-tree"
                                             "eclector-concrete-syntax-tree/test"))
                                  (filter  (constantly t)))
  (labels ((map-component-files (component)
             (typecase component
               (asdf:source-file
                (when (equal (asdf:file-type component) "lisp")
                  (list (funcall function component))))
               (asdf:module
                (let ((children (asdf:component-children component)))
                  (mapcan #'map-component-files children))))))
    (alexandria:mappend (lambda (system-name)
                          (when (funcall filter system-name)
                            (let ((system (asdf:find-system system-name)))
                              (map-component-files system))))
                        systems)))

(defun map-all-system-expressions (function reader &rest args
                                                   &key systems filter)
  (declare (ignore systems filter))
  (apply #'map-all-system-files
         (lambda (file)
           (let ((filename (asdf:component-pathname file)))
             (alexandria:with-input-from-file (stream filename)
               (loop for form-number from 0
                     for object = (funcall reader stream nil stream)
                     until (eq object stream)
                     do (funcall function filename form-number object)))))
         args))
