(uiop:define-package #:40ants-doc-full/locatives/slots
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc-full/utils)
  (:import-from #:swank-mop)
  (:import-from #:40ants-doc/locatives
                #:accessor
                #:reader
                #:writer)
  (:import-from #:40ants-doc-full/commondoc/bullet)
  (:import-from #:40ants-doc-full/commondoc/arglist
                #:make-arglist)
  (:import-from #:40ants-doc-full/commondoc/markdown)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:reference-to-commondoc)
  (:import-from #:40ants-doc/docstring
                #:strip-docstring-indentation)
  (:import-from #:40ants-doc/source-api
                #:find-source))
(in-package #:40ants-doc-full/locatives/slots)


(define-locative-type accessor (class-name)
  "To refer to an accessor named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (accessor foo))")

(define-locative-type reader (class-name)
  "To refer to a reader named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (reader foo))")

(define-locative-type writer (class-name)
  "To refer to a writer named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (writer foo))")

(defmethod locate-object (symbol (locative-type (eql 'accessor))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the ACCESSOR locative is (ACCESSOR <CLASS-NAME>).")
  (find-accessor-slot-definition symbol (first locative-args))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defun find-accessor-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (and (find accessor-symbol
                     (swank-mop:slot-definition-readers slot-def))
               (find `(setf ,accessor-symbol)
                     (swank-mop:slot-definition-writers slot-def)
                     :test #'equal))
      (return-from find-accessor-slot-definition slot-def)))
  (locate-error "Could not find accessor ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'reader))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the READER locative is (READER <CLASS-NAME>).")
  (find-reader-slot-definition symbol (first locative-args))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defun find-reader-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-readers slot-def))
      (return-from find-reader-slot-definition slot-def)))
  (locate-error "Could not find reader ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'writer))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the WRITER locative is (WRITER <CLASS-NAME>).")
  (find-writer-slot-definition symbol (first locative-args))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-writers slot-def))
      (return-from find-writer-slot-definition slot-def)))
  (locate-error "Could not find writer ~S for class ~S." accessor-symbol
                class-symbol))


(defvar *definition-finders*
  (list 'reader #'find-reader-slot-definition
        'writer #'find-writer-slot-definition
        'accessor #'find-accessor-slot-definition))


(defun inner-reference-to-commondoc (symbol locative-type locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (definition-finder (getf *definition-finders* locative-type))
         (slot-def (funcall definition-finder symbol (first locative-args)))
         (initarg-strings
           (when (swank-mop:slot-definition-initargs slot-def)
             (mapcar #'40ants-doc-full/utils::prin1-and-escape-markdown
                     (swank-mop:slot-definition-initargs slot-def))))
         (arglist (list (make-arglist locative-args)
                        ;; TODO: make a special node for this kind of data where NAME = SOME-DATA or DEFAULT IS SOME-DATA
                        (make-arglist (format nil "(~{~A~^ ~}~A)"
                                              initarg-strings
                                              (if (swank-mop:slot-definition-initfunction slot-def)
                                                  (format nil "~A= ~A"
                                                          (if initarg-strings " " "")
                                                          (40ants-doc-full/utils::prin1-and-escape-markdown
                                                           (swank-mop:slot-definition-initform
                                                            slot-def)))
                                                  "")))))
         (docstring (unless (subtypep (find-class (first locative-args)) 'condition)
                      (let ((docstring (swank-mop:slot-definition-documentation slot-def)))
                        (when docstring
                          (strip-docstring-indentation docstring)))))
         (children (when docstring
                     (40ants-doc-full/commondoc/markdown:parse-markdown docstring))))
    (40ants-doc-full/commondoc/bullet:make-bullet reference
                                                  :arglist arglist
                                                  :children children
                                                  :ignore-words symbol)))


(defmethod reference-to-commondoc ((symbol symbol) (locative-type (eql 'reader)) locative-args)
  (inner-reference-to-commondoc symbol locative-type locative-args))

(defmethod reference-to-commondoc ((symbol symbol) (locative-type (eql 'writer)) locative-args)
  (inner-reference-to-commondoc symbol locative-type locative-args))

(defmethod reference-to-commondoc ((symbol symbol) (locative-type (eql 'accessor)) locative-args)
  (inner-reference-to-commondoc symbol locative-type locative-args))


(defmethod locate-and-find-source (symbol (locative-type (eql 'accessor))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'reader))
                                   locative-args)
  (let ((generic-function (symbol-function symbol))
        (arguments (list (find-class (first locative-args))))
        (specializers nil))
    (find-source (find-method generic-function
                              specializers
                              arguments))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'writer))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (mapcar #'find-class
                                        (list t (first locative-args))))))
