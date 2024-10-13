(uiop:define-package #:40ants-doc-full/commondoc/builder
  (:use #:cl)
  (:import-from #:40ants-doc-full/commondoc/markdown
                #:parse-markdown)
  (:import-from #:40ants-doc/object-package)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc/reference)
  (:export #:to-commondoc
           #:reference-to-commondoc))
(in-package #:40ants-doc-full/commondoc/builder)


(defgeneric to-commondoc (obj)
  (:documentation
   "Define methods for this generic function to render object's
    documentation into an intermediate CommonDoc format.

    Function should return a COMMON-DOC:DOCUMENT-NODE.

    To show a standard documentation item with locative,
    name and arguments, use 40ANTS-DOC-FULL/COMMONDOC/BULLET:MAKE-BULLET
    function.
   "))

(defgeneric reference-to-commondoc (obj locative-type locative-args)
  (:documentation
   "Define a method for this generic function, when there is no
    a lisp object to represent an object of given locative type.

    LOCATIVE-TYPE argument will be a symbol. OBJ argument also usually a symbol.
    LOCATIVE-ARGS argument is a list which will be non-nil in case if
    object is referenced in a 40ANTS-DOC:DEFSECTION like this:

    ```lisp
    (40ants-doc/source-api:find-source (method () (40ants-doc/reference:reference)))
    ```

    In this case LOCATIVE-ARGS argument will be `'(NIL (40ANTS-DOC/REFERENCE:REFERENCE))`.
    "))


(defmethod to-commondoc ((obj string))
  (parse-markdown
   (40ants-doc/docstring:strip-docstring-indentation obj)))


(defmethod to-commondoc ((obj 40ants-doc/reference::reference))
  (let* ((resolved (40ants-doc/reference:resolve obj))
         (locative (40ants-doc/reference:reference-locative obj))
         (locative-name (etypecase locative
                          (list (car locative))
                          (symbol locative)))
         (locative-args (etypecase locative
                          (list (cdr locative))
                          (symbol nil))))
    (typecase resolved
      (40ants-doc/reference:reference
       (let* ((reference-obj (40ants-doc/reference:reference-object obj))
              (*package* (or (40ants-doc/object-package::object-package reference-obj)
                             *package*)))
         (reference-to-commondoc reference-obj
                                 locative-name
                                 locative-args)))
      (t (to-commondoc resolved)))))


(defmethod to-commondoc :around ((obj t))
  "This methods sets the *PACKAGE* because other TO-COMMONDOC methods might
   read symbols from docstrings, and they should be referenced
   against the package OBJ argument belongs to."
  (let ((*package* (or (40ants-doc/object-package::object-package obj)
                       *package*)))
    (call-next-method)))


(defmethod to-commondoc ((obj t))
  (parse-markdown (format nil "Don't know how to render `~S`. ~
                               Implement `TO-COMMONDOC` (`~S`) method."
                          obj
                          (type-of obj))))


(defmethod reference-to-commondoc ((obj t) (locative t) locative-args)
  (let ((locative-name (etypecase locative
                         (list (first locative))
                         (symbol locative))))
    (parse-markdown (format nil "Don't know how to render reference `~S` (`~S`). ~
                               Implement a `REFERENCE-TO-COMMONDOC` (`~S` `~S` T) method."
                            obj
                            locative
                            (type-of obj)
                            (list 'eql locative-name)))))
