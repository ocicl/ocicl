(cl:in-package #:acclimation)

(defgeneric report-function (restart language))

(defgeneric interactive-function (restart language))

(defmacro with-restarts (cases &body body)
  (let ((block-name (gensym))
        (instance-names (loop repeat (length cases)
                              collect (gensym))))
    `(block ,block-name
       (let ,(loop for instance-name in instance-names
                   for (nil nil (class-name . arguments))
                     in cases
                   collect `(,instance-name
                             (make-instance ',class-name ,@arguments)))
         (restart-bind
             ,(loop for (restart-name args nil . body) in cases
                    for instance-name in instance-names
                    collect `(,restart-name
                              (lambda ,args
                                (return-from ,block-name
                                  (progn ,@body)))
                              :interactive-function
                              (interactive-function ,instance-name
                                                    (language *locale*))
                              :report-function
                              (report-function ,instance-name
                                               (language *locale*))))
           ,@body)))))
