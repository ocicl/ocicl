(cl:in-package #:eclector.reader.test)

;;; Printing eclector quasiquote expressions using standard quasiquote
;;; syntax

(defun make-quasiquote-pprint-dispatch (&optional (dispatch (copy-pprint-dispatch)))
  (loop :for (operator string) :in '((eclector.reader:quasiquote       "`")
                                     (eclector.reader:unquote          ",")
                                     (eclector.reader:unquote-splicing ",@"))
        :do (set-pprint-dispatch
             `vector
             (lambda (stream object)
               (pprint-logical-block (stream (list object) :prefix "#(" :suffix ")")
                 (loop :for element :across object
                       :do (princ element stream)
                           (write-string " " stream))))
             1 dispatch)
        :do (set-pprint-dispatch
             `string
             (lambda (stream object)
               (let ((*print-pretty* nil))
                 (prin1 object stream)))
             2 dispatch)
        :do (set-pprint-dispatch
             `(cons (eql ,operator))
             (let ((string string))
               (lambda (stream object)
                 (write-string string stream)
                 (princ (second object) stream)))
             2 dispatch)
        :do (set-pprint-dispatch
             `cons
             (lambda (stream object)
               (pprint-logical-block (stream object :prefix "(" :suffix ")")
                 (princ (car object) stream)
                 (write-string " . " stream)
                 (princ (cdr object) stream)))
             1 dispatch))
  dispatch)

(defun hostify (expression)
  (handler-case
      (with-standard-io-syntax
        (read-from-string
         (let ((*print-pprint-dispatch* (make-quasiquote-pprint-dispatch))
               (*print-pretty* t))
           (prin1-to-string expression))))
    (error (condition)
      ;; Leave dynamic scope of all the reader and printer binding so
      ;; errors can be printed properly.
      (error condition))))

;;; Fiveam random testing generator for eclector quasiquote
;;; expressions.
;;;
;;; The central function, GEN-QUASIQUOTE-EXPRESSION, uses the helper
;;; generators to make UNQUOTE, UNQUOTE-SPLICING, QUASIQUOTE, CONS,
;;; vector and form sub-expressions. GEN-QUASIQUOTE-EXPRESSION takes
;;; helper generators as arguments which it applies in a randomized
;;; fashion while it also makes sure that the following constraints
;;; hold:
;;;
;;; 1) the depth and width of the resulting expression tree is within
;;;    given limits
;;;
;;; 2) helper generators are only composed in ways that produce valid
;;;    forms (in the sense that the expressions can be EVALed once)

(defvar *quasiquote-depth* 0)

(defvar *depth* 0)

(defvar *max-depth*)

(defvar *need-list-p* nil)

(defun gen-atom (&key (integer (gen-integer :min -10 :max 10))
                      (string (gen-string :length (gen-integer :min 0 :max 5)
                                          :elements (gen-one-element
                                                     #\a #\b #\c #\d))))
  (let ((kind-gen (gen-one-element integer string)))
    (lambda ()
      (funcall (funcall kind-gen)))))

(defun gen-unquote (argument)
  (lambda ()
    (list 'eclector.reader:unquote (funcall argument))))

(defun gen-unquote-splicing (argument)
  (lambda ()
    (list 'eclector.reader:unquote-splicing (funcall argument))))

(defun gen-quasiquote (argument)
  (lambda ()
    (list 'eclector.reader:quasiquote (funcall argument))))

(defun gen-cons (car cdr)
  (lambda ()
    (cons (funcall car) (funcall cdr))))

(defun gen-vector (elements &key (length (gen-integer :min 0 :max 3)))
  (let ((elements (gen-list :length length :elements elements)))
    (lambda ()
      (coerce (funcall elements) 'vector))))

(defun gen-quoted-form (argument)
  (lambda ()
    (list 'quote (funcall argument))))

(defun gen-compound-form (arguments &key (operator (gen-one-element 'list))
                                         (length (gen-integer :min 0 :max 3)))
  (let ((args (gen-list :length length :elements arguments)))
    (lambda ()
      (list* (funcall operator) (funcall args)))))

(defun gen-quasiquote-expression (&key (atom (gen-atom))
                                       (depth (gen-integer :min 0 :max 7)))
  (labels ((allowed-generators (max-depth &rest args
                                &key (qq-allowed t) (qq-depth 0)
                                     splicing-allowed list-needed in-vector-p
                                &allow-other-keys)
             (flet ((make-gen (generator &rest gen-inner-args)
                      (funcall generator
                               (lambda ()
                                 (apply #'gen-inner max-depth
                                        (append gen-inner-args args))))))
               (append (when (and (plusp qq-depth) splicing-allowed)
                         (list (make-gen #'gen-unquote-splicing
                                         :qq-depth (1- qq-depth)
                                         :list-needed t
                                         :splicing-allowed nil)))
                       (when (plusp qq-depth)
                         (list (make-gen #'gen-unquote
                                         :qq-depth (1- qq-depth)
                                         :splicing-allowed nil)))
                       (unless (or (not qq-allowed) (plusp qq-depth)
                                   in-vector-p)
                         (list (make-gen #'gen-quasiquote
                                         :qq-depth (1+ qq-depth)
                                         :splicing-allowed nil)))
                       (when (plusp qq-depth)
                         (list (gen-cons (make-gen #'identity)
                                         (make-gen #'identity
                                                   :splicing-allowed nil))))
                       (unless list-needed
                         (list (make-gen #'gen-vector :splicing-allowed t
                                                      :in-vector-p t)))
                       (list (make-gen #'gen-quoted-form :qq-allowed nil))
                       (list (make-gen #'gen-compound-form
                                       :list-needed nil
                                       :splicing-allowed t)))))
           (gen-inner (max-depth &rest args
                       &key depth-reached list-needed &allow-other-keys)
             (when (zerop max-depth)
               (incf (car depth-reached)))
             (cond
               ;; If we haven't yet reached the desired depth,
               ;; randomly decide whether to recursively generate a
               ;; (potentially compound) expression. The probability
               ;; of doing this decreases each time an expression
               ;; reaches the desired depth.
               ((and (plusp max-depth)
                     (zerop (random (1+ (floor (car depth-reached) 4)))))
                (funcall (alexandria:random-elt
                          (apply #'allowed-generators (1- max-depth) args))))
               ;; We would like to terminate the recursion here but
               ;; need a list.
               (list-needed
                (funcall (alexandria:random-elt
                          (apply #'allowed-generators 0 args))))
               ;; We terminate the recursion by generating an atom.
               (t
                (funcall atom))))
           (gen-root ()
             (gen-inner (funcall depth) :depth-reached (list 0))))
    #'gen-root))
