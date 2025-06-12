(in-package #:cl-template)

(defparameter *add-progn-to-if* t "Boolean; whether or not to implicitly add a progn to IF expressions.")
(defparameter *template-fn-cache* (make-hash-table :test 'equal) "Hash table which compiled template functions are cached in.")

(defun add-progn-to-if-expression (expression)
  (if (and *add-progn-to-if* (eql (car expression) 'if))
      (append expression (list (list 'progn)))
      expression))

(defun handle-end-expression (expressions stack)
  (let* ((last-expression (pop (car stack)))
         (contained
          (loop for expression in (car expressions) until (equal expression last-expression) collect (pop (car expressions)))))
    (if (equal (last last-expression) (list '(progn)))
        (nconc (car (last last-expression)) (reverse contained))
        (nconc last-expression (reverse contained)))
    last-expression))

(defun compile-expression (code stream expressions stack)
  (declare (ignore stream))
  (cond
    ((or (string= code "end") (string= code ")"))
     (handle-end-expression expressions stack)
     nil)
    ((string= code "else")
     (let ((last-expression
            (handle-end-expression expressions stack)))
       (nconc last-expression (list (list 'progn)))
       (push last-expression (car stack)))
     nil)
    (t
     (let* ((pairs (match-pairs-ignoring code '(#\( . #\)) :ignore-list '((#\" . #\"))))
            (needs-pushing
             (cond ((char/= (char code 0) #\()
                    (setf code (concatenate 'string "(" code ")"))
                    t)
                   ((> pairs 0)
                    (setf code (concatenate 'string code (make-string pairs :initial-element #\))))
                    t)
                   (t nil))))
       (let ((expression (add-progn-to-if-expression (read-from-string code))))
         (if needs-pushing
             (push expression (car stack)))
         (push expression (car expressions))
         expression)))))

(defun compile-echo-expression (code stream expressions stack)
  (declare (ignore stack))
  (let ((expression
          (cond
            ;; A function call, (format nil "~r" 123)
            ((char= (char code 0) #\()
             `(write-string ,(read-from-string code) ,stream))
            ;; A variable, stuff
            ((= (length code) (length (scan-string-until-ignoring code " " :ignore-list '((#\" . #\")))))
             `(write-string ,(read-from-string code) ,stream))
            ;; A function call without outer parens, format nil "~r" 123
            (t
             `(write-string ,(read-from-string (concatenate 'string "(" code ")")) ,stream)))))
    (push expression (car expressions))
    expression))

(defun compile-string (string stream expressions stack)
  (declare (ignore stack))
  (let ((expression `(write-string ,string ,stream)))
    (push expression (car expressions))
    expression))

(defun compile-template-part (string start start-delimiter start-echo-delimiter end-delimiter stream expressions stack)
  (let* ((start-index (search start-delimiter string :start2 start))
         (echo-index (search start-echo-delimiter string :start2 start))
         (delimiter-index (if (eql start-index echo-index) echo-index start-index)))
    (if (and delimiter-index (= (- delimiter-index start) 0))
        (let ((is-echo (string= (subseq string delimiter-index (+ delimiter-index (length start-echo-delimiter))) start-echo-delimiter)))
          (multiple-value-bind (inner end)
              (scan-between-delimiters string (if is-echo start-echo-delimiter start-delimiter)
                                       end-delimiter :start start :ignore-list '((#\" . #\")))
            (list (funcall
                   (if is-echo #'compile-echo-expression #'compile-expression)
                   (string-trim '(#\Space #\Tab #\Newline) inner) stream expressions stack)
                  (+ start end))))
        (list (compile-string (subseq string start delimiter-index) stream expressions stack) (or delimiter-index (length string))))))

(defun internal-compile-template (string start-delimiter start-echo-delimiter end-delimiter stream-name)
  (let ((start 0) (expressions (list nil)) (stack (list nil)))
    ;; Not quite sure why I have to use this weird read-from-string thing here,
    ;; but I get errors about things being in the wrong package otherwise.
    ;; TODO: Fix this, it's ugly and there's certainly a better way to do it.
    `(macrolet (,(read-from-string "(@ (var) `(getf cl-template::__data ,(intern (symbol-name var) :keyword)))"))
       (with-output-to-string (,stream-name)
         ,@(progn
            (loop
               for (form end) = (compile-template-part string start start-delimiter start-echo-delimiter end-delimiter stream-name expressions stack)
               until (>= end (length string)) do
                 (setf start end))
            (reverse (car expressions)))))))

(defun compile-template (string &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))
  "Compile a string template into a lambda, which can be invoked with
  a plist as the only argument and provides data to the template.  The
  data argument to the lambda is available in the template as
  `cl-template::__data`, although it's not recommended to use it
  directly.  See README.md for examples of the template syntax.
  It caches the result of the compilation.
  Examples:
    (compile-template \"the number is <%= (@ number) %>\")  ; (lambda (__data) ...)
    (compile-template \"{{= @ mood }} shark\" :start-delimiter \"{{\" :start-echo-delimiter \"{{=\" :end-delimiter \"}}\")  ; (lambda (__data) ...)
    (funcall (compile-template \"<%= format nil \"~@r\" (@ number) %>\") '(:number 12))  ; \"XII\"
  "
  (let ((cached-fn (gethash string *template-fn-cache*)))
    (if cached-fn
        cached-fn
        (let* ((stream (gensym))
               (fn
                (eval
                 `(lambda (__data)
                    ,(internal-compile-template string start-delimiter start-echo-delimiter end-delimiter stream)))))
          (setf (gethash string *template-fn-cache*) fn)))))
