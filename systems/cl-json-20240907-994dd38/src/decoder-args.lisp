;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10; Package: JSON -*-
(in-package :json)

;;; Custom variables

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *custom-vars* nil)

(defmacro with-shadowed-custom-vars (&body body)
  `(let ,(loop for (var) in *custom-vars*
            collect `(,var (if (boundp ',var) ,var)))
     ,@body))

(defmacro set-custom-vars (&rest key-args)
  `(setq
    ,@(loop for (supplied-key value) on key-args by #'cddr
         append (loop for (var . var-key) in *custom-vars*
                   thereis (if (eql var-key supplied-key)
                               (list var value))))))

)

(defmacro define-custom-var ((key name) &rest other-args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn (pushnew '(,name . ,key) *custom-vars* :test #'equal)
            (defvar ,name ,@other-args))))

;;; Token reader

(define-condition json-syntax-error (simple-error stream-error)
  ((stream-file-position :reader stream-error-stream-file-position
                         :initarg :stream-file-position))
  (:report
   (lambda (condition stream)
     (format stream "~? [in ~S~@[ at position ~D~]]"
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)
             (stream-error-stream condition)
             (stream-error-stream-file-position condition)))))

(defun json-syntax-error (stream format-control &rest format-arguments)
  (error 'json-syntax-error
         :stream stream
         :stream-file-position (file-position stream)
         :format-control format-control
         :format-arguments format-arguments))

(defun read-json-token (stream)
  (let ((c (peek-char t stream)))
    (case c
      ((#\{ #\[ #\] #\} #\" #\: #\,)
       (values :punct (read-char stream)))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
       (read-json-number-token stream))
      (t (if (alpha-char-p c)
             (read-json-symbol-token stream)
             (json-syntax-error stream "Invalid char on JSON input: `~C'"
                                c))))))

(defun read-json-number-token (stream)
  (let ((int (make-array 32 :adjustable t :fill-pointer 0
                         :element-type 'character))
        (frac (make-array 32 :adjustable t :fill-pointer 0
                          :element-type 'character))
        (exp (make-array 32 :adjustable t :fill-pointer 0
                         :element-type 'character))
        (type :integer)
        c)
    (flet ((safe-read-char (stream)
             (handler-case (read-char stream)
               (end-of-file ()
                 (return-from read-json-number-token
                   (values type (concatenate 'string int frac exp)))))))
      (macrolet
          ((read-digits (part)
             (let ((error-fmt
                    (format nil "Invalid JSON number: no ~(~A~) digits"
                            part)))
               `(loop while (char<= #\0 c #\9)
                   with count = 0
                   do (vector-push-extend c ,part 32)
                      (setq c (safe-read-char stream))
                      (incf count)
                   finally
                     (if (zerop count)
                         (json-syntax-error stream ,error-fmt))))))
        (setq c (read-char stream))
        (when (char= c #\-)
          (vector-push c int)
          (setq c (read-char stream)))
        (if (char= c #\0)
            (progn
              (vector-push c int)
              (setq c (safe-read-char stream)))
            (read-digits int))
        (when (char= c #\.)
          (vector-push c frac)
          (setq c (read-char stream)
                type :real)
          (read-digits frac))
        (when (char-equal c #\e)
          (vector-push c exp)
          (setq c (read-char stream)
                type :real)
          (when (or (char= c #\+) (char= c #\-))
            (vector-push c exp)
            (setq c (read-char stream)))
          (read-digits exp))
        (unread-char c stream)
        (values type (concatenate 'string int frac exp))))))

(defun read-json-symbol-token (stream)
  (let ((symbol (make-array 8 :adjustable t :fill-pointer 0
                            :element-type 'character)))
    (loop for c = (read-char stream nil)
       while (and c (alpha-char-p c))
       do (vector-push-extend c symbol 32)
       finally (if c (unread-char c stream)))
    (setq symbol (coerce symbol 'string))
    (if (or (string= symbol "true")
            (string= symbol "false")
            (string= symbol "null"))
        (values :boolean symbol)
        (json-syntax-error stream "Invalid JSON symbol: ~A" symbol))))

(define-condition no-char-for-code (error)
  ((offending-code :initarg :code :reader offending-code))
  (:report (lambda (condition stream)
             (format stream "No character corresponds to code #x~4,'0X."
                     (offending-code condition)))))

(defun read-json-string-char (stream)
  (let ((esc-error-fmt "Invalid JSON character escape sequence: ~A~A"))
    (case (peek-char nil stream)
      (#\" (read-char stream)
           nil)                         ; End of string
      (#\\ (read-char stream)
           (let ((c (read-char stream)))
             (case c
               ((#\" #\\ #\/) c)
               (#\r #\Return)
               (#\n #\Linefeed)
               (#\t #\Tab)
               (#\b #\Backspace)
               (#\f #\)
               (#\u (let ((hex (make-string 4)))
                      (dotimes (i 4)
                        (setf (aref hex i) (read-char stream)))
                      (let ((c (handler-case (parse-integer hex :radix 16)
                                 (parse-error ()
                                   (json-syntax-error stream esc-error-fmt
                                                      "\\u" hex)))))
                        (if (< c 256)
                            (code-char c)
                            (restart-case (error 'no-char-for-code :code c)
                              (substitute-char (char)
                                :report "Substitute another char."
                                :interactive
                                  (lambda ()
                                    (format *query-io* "Char: ")
                                    (list (read-char *query-io*)))
                                char)
                              (pass-code ()
                                :report "Pass the code to char handler."
                                c))))))
               (t (json-syntax-error stream esc-error-fmt "\\" c)))))
      (t (read-char stream)))))

;;; The decoder base

(define-custom-var (:integer *integer-handler*))
(define-custom-var (:real *real-handler*))
(define-custom-var (:boolean *boolean-handler*))

(define-custom-var (:beginning-of-string *beginning-of-string-handler*))
(define-custom-var (:string-char *string-char-handler*))
(define-custom-var (:end-of-string *end-of-string-handler*))

(define-custom-var (:beginning-of-array *beginning-of-array-handler*))
(define-custom-var (:array-element *array-element-handler*))
(define-custom-var (:end-of-array *end-of-array-handler*))

(define-custom-var (:beginning-of-object *beginning-of-object-handler*))
(define-custom-var (:object-key *object-key-handler*))
(define-custom-var (:object-value *object-value-handler*))
(define-custom-var (:end-of-object *end-of-object-handler*))

(defun decode-json (stream &optional parent-handler-state)
  (multiple-value-bind (type token) (read-json-token stream)
    (dispatch-on-token type token parent-handler-state stream)))

(defun dispatch-on-token (type token parent-handler-state stream)
  (ecase type
    (:punct
     (case token
       (#\" (decode-json-string stream parent-handler-state))
       (#\[ (decode-json-array stream parent-handler-state))
       (#\{ (decode-json-object stream parent-handler-state))
       (t (json-syntax-error stream
                             "Token out of place on JSON input: `~C'"
                             token))))
    (:integer
     (funcall *integer-handler* parent-handler-state token))
    (:real
     (funcall *real-handler* parent-handler-state token))
    (:boolean
     (funcall *boolean-handler* parent-handler-state token))))
    
(defun decode-json-array (stream &optional parent-handler-state)
  (let ((handler-state
         (funcall *beginning-of-array-handler* parent-handler-state)))
    (multiple-value-bind (type token) (read-json-token stream)
      (if (and (eql type :punct) (char= token #\]))
          (return-from decode-json-array
            (funcall *end-of-array-handler* handler-state))
          (setq handler-state
                (funcall *array-element-handler* handler-state
                         (dispatch-on-token type token handler-state
                                            stream)))))
    (loop
       (multiple-value-bind (type token) (read-json-token stream)
         (if (eql type :punct)
             (case token
               (#\] (return-from decode-json-array
                      (funcall *end-of-array-handler* handler-state)))
               (#\, (setq token nil))))
         (if token
             (json-syntax-error
              stream
              "Token out of place in array on JSON input: `~A'"
               token)))
       (setq handler-state
             (funcall *array-element-handler* handler-state
                      (decode-json stream handler-state))))))

(defun decode-json-object (stream &optional parent-handler-state)
  (loop with handler-state =
         (funcall *beginning-of-object-handler* parent-handler-state)
     with key = nil
     for first-time-p = t then nil
     do (multiple-value-bind (type token) (read-json-token stream)
          (if (eql type :punct)
              (case token
                (#\}
                 (if first-time-p
                     (return-from decode-json-object
                       (funcall *end-of-object-handler* handler-state))))
                (#\"
                 (setq key (decode-json-string stream handler-state t)))))
          (if key
              (setq handler-state
                    (funcall *object-key-handler* handler-state key))
              (json-syntax-error
               stream
               "Expected a key string in object on JSON input ~
                but found `~A'"
               token)))
       (multiple-value-bind (type token) (read-json-token stream)
         (unless (and (eql type :punct) (char= token #\:))
           (json-syntax-error
            stream
            "Expected a `:' separator in object on JSON input ~
             but found `~A'"
            token)))
       (setq handler-state
             (funcall *object-value-handler* handler-state
                      (decode-json stream handler-state)))
       (multiple-value-bind (type token) (read-json-token stream)
          (if (eql type :punct)
              (case token
                (#\} (return-from decode-json-object
                       (funcall *end-of-object-handler* handler-state)))
                (#\, (setq key nil))))
          (if key
              (json-syntax-error
               stream
               "Expected a `,' separator or `}' in object on JSON input ~
                but found `~A'"
               token)))))

(defun decode-json-string (stream &optional parent-handler-state
                                            as-object-key)
  (loop with handler-state = (funcall *beginning-of-string-handler*
                                      parent-handler-state as-object-key)
    for c = (read-json-string-char stream)
    while c
    do (setq handler-state
             (funcall *string-char-handler* handler-state c))
    finally (return
              (funcall *end-of-string-handler* handler-state))))


;;; Name translation

(defun camel-case-split (string)
  (let ((length (length string)))
    (macrolet ((shift-part (e new-cat &optional subst-cat)
                 `(prog1 (if b (cons ,(or subst-cat 'cat)
                                     (subseq string b ,e)))
                    (setq b ,e cat ,new-cat))))
      (loop for i from 0 to length
         with cat = nil and b = nil
         if (= i length)
           if (shift-part i nil) collect it end
         else if (let ((c (aref string i)))
                   (cond
                     ((upper-case-p c)
                      (case cat
                        ((:upper-1 :upper) (setq cat :upper) nil)
                        (t (shift-part i :upper-1))))
                     ((lower-case-p c)
                      (case cat
                        (:upper-1 (setq cat :mixed) nil)
                        (:upper (let ((subst-cat
                                       (if (> (- i b) 2) :upper :upper-1)))
                                  (shift-part (1- i) :mixed subst-cat)))
                        ((:numeric :punct nil) (shift-part i :lower))))
                     ((digit-char-p c)
                      (if (not (eql cat :numeric))
                          (shift-part i :numeric)))
                     (t (shift-part i :punct))))
           collect it))))

(defun camel-case-transform-all-caps (parts
                                      &optional cat-before from-numeric)
  (if (endp parts)
      (cond (from-numeric (throw 'all-caps nil))
            ((eql cat-before :punct) nil)
            (t '("+")))
      (destructuring-bind ((cat . part) . rest) parts
        (case cat
          ((:lower :mixed) (throw 'all-caps nil))
          (:punct
           (let ((transformed (if (string= part "_") "-" part)))
             (if (or from-numeric (eql cat-before :punct))
                 (cons transformed (camel-case-transform-all-caps rest cat))
                 (let ((transformed-rest
                        (catch 'all-caps
                          (camel-case-transform-all-caps rest cat))))
                   (if transformed-rest
                       (cons transformed transformed-rest)
                       (list* "+"
                              (if (string= part "_") "--" part)
                              (camel-case-transform rest cat)))))))
          ((:upper :upper1)
           (cons part (camel-case-transform-all-caps rest cat nil)))
          (t (cons part (camel-case-transform-all-caps
                         rest cat from-numeric)))))))

(defun camel-case-transform (parts &optional (cat-before :punct))
  (if (endp parts)
      '("")
      (destructuring-bind ((cat . part) . rest) parts
        (case cat
          (:upper
           (if (eql cat-before :punct)
               (let ((transformed-rest
                      (catch 'all-caps
                        (camel-case-transform-all-caps rest cat))))
                 (if transformed-rest
                     (list* "+" part transformed-rest)
                     (list* "+" part "+" (camel-case-transform rest cat))))
               (list* "-+" part "+" (camel-case-transform rest cat))))
          (:upper-1
           (case cat-before
             (:punct
              (let ((transformed-rest
                     (catch 'all-caps
                       (camel-case-transform-all-caps rest cat))))
                (if transformed-rest
                    (list* "+" part transformed-rest)
                    (list* "*" part (camel-case-transform rest cat)))))
             (:numeric (list* "-*" part (camel-case-transform rest cat)))
             (t (list* "-" part (camel-case-transform rest cat)))))
          (:numeric
           (case cat-before
             (:punct
              (let ((transformed-rest
                     (catch 'all-caps
                       (camel-case-transform-all-caps rest cat t))))
                (if transformed-rest
                    (list* "+" part transformed-rest)
                    (cons part (camel-case-transform rest cat)))))
             (t (list* "-" part (camel-case-transform rest cat)))))
          (:mixed
           (list* (case cat-before (:punct "*") (:numeric "-*") (t "-"))
                  (string-upcase part)
                  (camel-case-transform rest cat)))
          (:lower
           (list* (if (eql cat-before :punct) "" "-")
                  (string-upcase part)
                  (camel-case-transform rest cat)))
          (:punct
           (cons (if (string= part "_") "--" part)
                 (camel-case-transform rest cat)))))))

(defun camel-case-to-lisp (string)
  (apply #'concatenate 'string
         (camel-case-transform (camel-case-split string))))

(define-custom-var (:symbols-package *json-symbols-package*)
    (find-package 'keyword))

(defun json-intern (string)
  (intern string *json-symbols-package*))


;;; The list semantics

(define-custom-var (:array-type *json-array-type*) 'vector)

(defun parse-number (parent-handler-state token)
  (declare (ignore parent-handler-state))
  ;; We can be reasonably sure that nothing but well-formed (both in
  ;; JSON and Lisp sense) number literals gets to this point.
  (read-from-string token))

(defun json-boolean-to-lisp (parent-handler-state token)
  (declare (ignore parent-handler-state))
  (string= token "true"))

(defun make-accumulator (parent-handler-state)
  (declare (ignore parent-handler-state))
  (let ((head (cons nil nil)))
    (cons head head)))

(defun accumulator-add (accumulator element)
  (destructuring-bind (head . last) accumulator
    (cons head (setf (cdr last) (cons element nil)))))

(defun accumulator-add-key (accumulator key)
  (destructuring-bind (head . last) accumulator
    (cons head
          (let ((key (json-intern (camel-case-to-lisp key))))
            (setf (cdr last) (cons (cons key nil) nil))))))

(defun accumulator-add-value (accumulator value)
  (destructuring-bind (head . last) accumulator
    (declare (ignore head))
    (setf (cdar last) value)
    accumulator))

(defun accumulator-get-sequence (accumulator)
  (coerce (cdar accumulator) *json-array-type*))

(defun accumulator-get (accumulator)
  (cdar accumulator))

(defun make-vector-accumulator (parent-accumulator &optional as-object-key)
  (declare (ignore parent-accumulator as-object-key))
  (make-array 32 :adjustable t :fill-pointer 0))

(defun vector-accumulator-add (accumulator element)
  (vector-push-extend element accumulator (fill-pointer accumulator))
  accumulator)

(defun vector-accumulator-get-sequence (accumulator)
  (coerce accumulator *json-array-type*))

(defun vector-accumulator-get-string (accumulator)
  (coerce accumulator 'string))

(defun set-decoder-simple-list-semantics ()
  (set-custom-vars
   :integer #'parse-number
   :real #'parse-number
   :boolean #'json-boolean-to-lisp
   :beginning-of-array #'make-accumulator
   :array-element #'accumulator-add
   :end-of-array #'accumulator-get-sequence
   :beginning-of-object #'make-accumulator
   :object-key #'accumulator-add-key
   :object-value #'accumulator-add-value
   :end-of-object #'accumulator-get
   :beginning-of-string #'make-vector-accumulator
   :string-char #'vector-accumulator-add
   :end-of-string #'vector-accumulator-get-string))

(defmacro with-decoder-simple-list-semantics (&body body)
  `(with-shadowed-custom-vars
     (set-decoder-simple-list-semantics)
     ,@body))


;;; The CLOS semantics

(defvar *prototype-prototype*
  (make-instance 'prototype
    :lisp-class 'prototype
    :lisp-package :json))

(defun make-accumulator-with-prototype (parent-handler-state)
  (if (and (consp parent-handler-state)
           (consp (car parent-handler-state))
           (eql (caar parent-handler-state) t))
      (let ((head (cons *prototype-prototype* nil)))
        (cons head head))
      (make-accumulator parent-handler-state)))

(defun accumulator-add-key-or-set-prototype (accumulator key)
  (destructuring-bind (head . last) accumulator
    (let ((key (camel-case-to-lisp key)))
      (if (and (not (car head))
               *prototype-name*
               (string= key (symbol-name *prototype-name*)))
          (progn (setf (car head) t)
                 accumulator)
          (cons head
                (setf (cdr last) (cons (cons key nil) nil)))))))

(defun accumulator-add-value-or-set-prototype (accumulator value)
  (destructuring-bind (head . last) accumulator
    (declare (ignore last))
    (if (and (consp head) (eql (car head) t))
        (progn
          (assert (typep value 'prototype) (value)
            "Invalid prototype: ~S.  Want to substitute something else?"
            value)
          (setf (car head) value)
          accumulator)
        (accumulator-add-value accumulator value))))

(defun accumulator-get-object (accumulator)
  (destructuring-bind (head . last) accumulator
    (declare (ignore last))
    (flet ((as-symbol (value)
             (if (stringp value)
                 (json-intern (camel-case-to-lisp value))
                 value))
           (intern-keys (bindings)
             (loop for (key . value) in bindings
                collect (cons (json-intern key) value))))
      (if (typep (car head) 'prototype)
          (with-slots (lisp-class lisp-superclasses lisp-package)
              (car head)
            (let* ((*json-symbols-package*
                    (or (find-package (as-symbol lisp-package))
                        *json-symbols-package*))
                   (class (as-symbol lisp-class))
                   (superclasses (mapcar #'as-symbol lisp-superclasses))) 
              (make-object (intern-keys (cdr head)) class
                           :superclasses superclasses)))
          (make-object (intern-keys (cdr head)) nil)))))

(defun set-decoder-simple-clos-semantics ()
  (set-custom-vars
   :integer #'parse-number
   :real #'parse-number
   :boolean #'json-boolean-to-lisp
   :beginning-of-array #'make-vector-accumulator
   :array-element #'vector-accumulator-add
   :end-of-array #'vector-accumulator-get-sequence
   :beginning-of-object #'make-accumulator-with-prototype
   :object-key #'accumulator-add-key-or-set-prototype
   :object-value #'accumulator-add-value-or-set-prototype
   :end-of-object #'accumulator-get-object
   :beginning-of-string #'make-vector-accumulator
   :string-char #'vector-accumulator-add
   :end-of-string #'vector-accumulator-get-string))

(defmacro with-decoder-simple-clos-semantics (&body body)
  `(with-shadowed-custom-vars
     (set-decoder-simple-clos-semantics)
     ,@body))
