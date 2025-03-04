(uiop:define-package #:40ants-doc
  (:documentation "See 40ANTS-DOC:@INDEX.")
  (:use #:cl)
  (:nicknames #:40ants-doc/core)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/object-package)
  (:import-from #:40ants-doc/docstring
                #:strip-docstring-indentation)
  (:import-from #:serapeum
                #:soft-list-of)
  (:export #:defsection
           #:exportable-locative-type-p
           #:section
           #:section-name
           #:section-package
           #:section-readtable
           #:section-title
           #:section-link-title-to
           #:section-entries
           #:*discard-documentation-p*
           #:section-ignore-words
           #:defsection-copy
           #:section-external-docs
           #:*symbols-with-ignored-missing-locations*
           #:section-ignore-packages))
(in-package #:40ants-doc)


;;; Should this remove docstrings of referenced things?
(defvar *discard-documentation-p* nil
  "The default value of DEFSECTION's DISCARD-DOCUMENTATION-P argument.
  One may want to set `*DISCARD-DOCUMENTATION-P*` to true before
  building a binary application.")


(defvar *symbols-with-ignored-missing-locations* nil
  "Sometimes code might be generated without source location attached.

   For example Mito generates slot readers this way.
   Such symbols should be added to this list to skip warnings during the documentation build.

   Use such code to add a new symbol to ignore:

   ```lisp
   (eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew 'reblocks-auth/models:profile-user
              40ants-doc:*symbols-with-ignored-missing-locations*))
   ```
")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-package (package)
    (etypecase package
      (package package)
      (keyword (find-package package))
      (string (find-package package))
      ;; This is the case when a default value was given
      ;; like '*package
      (symbol (symbol-value package)))))


(defmacro defsection (name (&key
                            (package '*package*)
                            ;; TODO: Deprecate after 2023
                            (package-symbol nil)
                            (readtable-symbol '*readtable*)
                            (section-class 'section)
                            (export nil)
                            title
                            link-title-to
                            (discard-documentation-p *discard-documentation-p*)
                            (external-docs nil)
                            (external-links nil)
                            (ignore-words nil)
                            (ignore-packages nil))
                      &body entries)
  "Define a documentation section and maybe export referenced symbols.
  A bit behind the scenes, a global variable with NAME is defined and
  is bound to a [SECTION][class] object. By convention, section names
  start with the character `@`. See `40ANTS-DOC-FULL/DOC::@TUTORIAL` for an example.

  ENTRIES consists of docstrings and references. Docstrings are
  arbitrary strings in markdown format, references are defined in the
  forms:

      (symbol locative) or ((symbol1 symbol2 ... symboln) locative)

  For example, `(FOO FUNCTION)` refers to the function `FOO`, `(@BAR
  SECTION)` says that `@BAR` is a subsection of this
  one. `(BAZ (METHOD () (T T T)))` refers to the default method of the
  three argument generic function `BAZ`. `(FOO FUNCTION)` is
  equivalent to `(FOO (FUNCTION))`.

  A locative in a reference can either be a symbol or it can be a list
  whose CAR is a symbol. In either case, the symbol is the called the
  type of the locative while the rest of the elements are the locative
  arguments. See 40ANTS-DOC-FULL/DOC::@LOCATIVE-TYPES for the list of locative
  types available out of the box.

  The same symbol can occur multiple times in ENTRIES, typically
  with different locatives, but this is not required.

  The references are not looked up (see 40ANTS-DOC/REFERENCE:RESOLVE in the
  40ANTS-DOC-FULL/DOC:@EXTENSION-API) until documentation is generated, so it is
  allowed to refer to things yet to be defined.

  If you set :EXPORT to true, the referenced symbols and NAME are
  candidates for exporting. A candidate symbol is exported if

  - it is accessible in PACKAGE (it's not `OTHER-PACKAGE:SOMETHING`)
    and

  - there is a reference to it in the section being defined with a
    locative whose type is approved by EXPORTABLE-LOCATIVE-TYPE-P.

  The original idea with confounding documentation and exporting is to force
  documentation of all exported symbols. However when forking MGL-PAX into
  40ANTS-DOC I've decided explicit imports make code more readable, and
  changed the default for :EXPORT argument to NIL and added automatic
  warnings to help find exported symbols not referenced from the documention.

  If you decide to use `:EXPORT t` argument, note it will cause
  [package variance](http://www.sbcl.org/manual/#Package-Variance)
  error on SBCL. To prevent it, use UIOP:DEFINE-PACKAGE instead
  of CL:DEFPACKAGE. 

  :TITLE is a non-marked-up string or NIL. If non-nil, it determines
  the text of the heading in the generated output. :LINK-TITLE-TO is a
  reference given as an
  `(OBJECT LOCATIVE)` pair or NIL, to which the heading will link when
  generating HTML. If not specified, the heading will link to its own
  anchor.

  When :DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, ENTRIES will not be recorded to save memory.

  EXTERNAL-DOCS argument can be a list of URLs leading to documentation
  of other libraries. These libraries should be documented using 40ANTS-DOC
  and you'll be able to mention symbols from them and have automatic
  cross-links.

  EXTERNAL-LINKS argument could contain an alist of (\"name\" . \"URL\") pairs.
  These pairs will be tranformed to [name]: URL text and appended to each
  markdown part of the defined chapter. This argument is useful when you are
  having more than one text part in the chapter and want to reference same
  URL from all of them using short markdown links.

  :IGNORE-WORDS allows to pass a list of strings which should not cause
  warnings. Usually these are uppercased words which are not symbols
  in the current package, like SLIME, LISP, etc.

  Argument IGNORE-PACKAGES can be used to ignore mentions of all symbols from these
  packages. If given, it should be a list of strings. Comparison of
  package names is case-sensitive.
"
 
  ;; Let's check the syntax as early as possible.
  (setf entries
        (transform-locative-symbols
         entries))


  ;; TODO: Remove after end of 2023
  (when package-symbol
    (warn "Argument :PACKAGE-SYMBOL is deprecated in DEFSECTION macr. Use :PACKAGE instead.")
    (setf package package-symbol))
  
  (transform-entries entries external-links)
  (transform-link-title-to link-title-to)

  (when (and (typep ignore-words
                    'list)
             (typep (first ignore-words)
                    'string))
    ;; This allows to pass an unquoted list of words
    ;; to the macro, which is what you most commonly need.
    (setf ignore-words
          (list* 'list
                 ignore-words)))

  (when (and (typep ignore-packages
                    'list)
             (typep (first ignore-packages)
                    'string))
    ;; This allows to pass an unquoted list of words
    ;; to the macro, which is what you most commonly need.
    (setf ignore-packages
          (list* 'list
                 ignore-packages)))
  
  (let ((export-form
          (when export
            `((eval-when (:compile-toplevel :load-toplevel :execute)
                (export-some-symbols ',name ',entries ,(ensure-package package)))))))
    `(progn
       ,@export-form
      
       (defparameter ,name
         (make-instance ',section-class
                        :name ',name
                        :package ,(ensure-package package)
                        :readtable ,readtable-symbol
                        :title ,title
                        :link-title-to (transform-link-title-to ',link-title-to)
                        :entries ,(if discard-documentation-p
                                    ()
                                    `(transform-entries ',entries ',external-links))
                        :external-docs (list ,@(uiop:ensure-list external-docs))
                        :ignore-words (list
                                       ,@(eval ignore-words))
                        :ignore-packages (list
                                          ,@(eval ignore-packages)))))))

(defclass section ()
  ((name
    :initarg :name
    :reader section-name
    :type symbol
    :documentation "The name of the global variable whose value is
    this SECTION object.")
   (package
    :initarg :package
    :type package
    :reader section-package
    :documentation "*PACKAGE* will be bound to this package when
    generating documentation for this section.")
   (readtable
    :initarg :readtable
    :reader section-readtable
    :documentation "*READTABLE* will be bound to this when generating
    documentation for this section.")
   (title
    :initarg :title
    :reader section-title
    :documentation "STRING or NIL. Used in generated documentation.")
   (link-title-to
    :initform nil
    :initarg :link-title-to
    :reader section-link-title-to
    :documentation "A 40ANTS-DOC/REFERENCE:REFERENCE or NIL. Used in generated documentation.")
   (entries
    :initarg :entries
    :reader section-entries
    :documentation "A list of strings and 40ANTS-DOC/REFERENCE:REFERENCE objects in the
    order they occurred in DEFSECTION.")
   (external-docs
    :initarg :external-docs
    :initform nil
    :reader section-external-docs
    :documentation "A list of strings with URLs of other system's documentation.")
   (ignore-words
    :initarg :ignore-words
    :type (soft-list-of string)
    :initform nil
    :reader section-ignore-words
    :documentation "A list of strings to not warn about.")
   (ignore-packages
    :initarg :ignore-packages
    :type (soft-list-of string)
    :initform nil
    :reader section-ignore-packages
    :documentation "A list of strings denoting package names to not warn about."))
  (:documentation "DEFSECTION stores its :NAME, :TITLE, :PACKAGE,
  :READTABLE and :ENTRIES in [SECTION][class] objects."))


(defmacro defsection-copy (name section)
  "When you use [DOCS-BUILDER](https://40ants.com/docs-builder), you might want
   to define a @readme variable to make README.md file with the same content as
   your main documentation. This case might be popular for libraries having
   a short documentation.

   To define @readme as a copy of the main doc, export @readme symbol and do this in the code:

   ```lisp
   (defparameter @readme (40ants-doc:copy-section @index))
   ```"

  `(defparameter ,name
     (make-instance 'section
                    :name ',name
                    :package (section-package ,section)
                    :readtable (section-readtable ,section)
                    :title (section-title ,section)
                    :link-title-to (section-link-title-to ,section)
                    :entries (section-entries ,section)
                    :ignore-words (section-ignore-words ,section))))

(defmethod print-object ((section section) stream)
  (print-unreadable-object (section stream :type t)
    (format stream "~S" (section-name section))))


(defmethod 40ants-doc/object-package::object-package ((obj section))
  (let ((package-or-name (section-package obj)))
    (etypecase package-or-name
      (package package-or-name)
      (string (find-package package-or-name))
      (symbol (find-package package-or-name)))))


;; This function is from alexandria, to not
;; introduce any dependencies to 40ants-doc/core
(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun locative-equal (locative-1 locative-2)
  (equal (ensure-list locative-1)
         (ensure-list locative-2)))


(defun transform-entries (entries external-links)
  (let ((external-links
          (with-output-to-string (s)
            (loop for (name . url) in external-links
                  do (format s "[~A]: ~A~&"
                             name
                             url)))))
    (flet ((add-external-links (text)
             "Adds markdown references to the end of the section piece.

              Beside that, it adds a new line text entries to make
              markdown parser correctly parse a header at the
              end of the text.

              Often you might end your text entry with a header like:

              ## API"
             (format nil "~A~2&~A"
                     (strip-docstring-indentation text)
                     external-links)))
      (mapcar (lambda (entry)
                (typecase entry
                  (string (add-external-links entry))
                  (symbol
                   (let ((value (symbol-value entry)))
                     (unless (typep value 'string)
                       (error "~S value should be a string."
                              entry))
                     (add-external-links value)))
                  (t
                   (entry-to-reference entry))))
              entries))))

(defun entry-to-reference (entry)
  (destructuring-bind (symbol locative &key export) entry
    (declare (ignore export))
    (assert (symbolp symbol) ()
            "~S is not a valid reference its first element is not a ~
            symbol." entry)
    (40ants-doc/reference::make-reference symbol locative)))

(defun transform-link-title-to (link-title-to)
  (when link-title-to
    (if (typep link-title-to '40ants-doc/reference::reference)
        link-title-to
        (apply #'40ants-doc/reference::make-reference link-title-to))))

;;;; Exporting

(defun export-some-symbols (name entries package)
  (when (symbol-accessible-in-package-p name package)
    (export name package))
  (dolist (entry entries)
    (when (listp entry)
      (destructuring-bind (symbol locative) entry
        (when (and (symbol-accessible-in-package-p symbol package)
                   (exportable-locative-type-p (40ants-doc/locatives/base::locative-type locative)))
          (export symbol package))))))

(defun transform-locative-symbols (entries &aux (locatives-package (find-package "40ANTS-DOC/LOCATIVES")))
  (labels ((transform (locative)
             (etypecase locative
               (symbol (intern (symbol-name locative) locatives-package))
               (cons (cons (transform (car locative))
                           (cdr locative)))))
	   (transform-entry (entry)
	     (destructuring-bind (symbols locative) entry
	       (if (atom symbols)
		   (list
		    (list symbols
			  (transform locative)))
		   (loop for symbol in symbols
			 collect (list symbol (transform locative)))))))
    (loop for entry in entries
          if (listp entry)
          nconc (transform-entry entry)
          else collect entry)))

(defun symbol-accessible-in-package-p (symbol package)
  (eq symbol (find-symbol (symbol-name symbol) package)))

(defgeneric exportable-locative-type-p (locative-type)
  (:documentation "Return true if symbols in references with
  LOCATIVE-TYPE are to be exported when they occur in a
  DEFSECTION having `:EXPORT t` argument. The default method returns T, while the methods for
  PACKAGE, ASDF:SYSTEM and METHOD return NIL.

  DEFSECTION calls this function to decide what symbols to export when
  its EXPORT argument is true.")
  (:method (locative-type)
    (declare (ignore locative-type))
    t))

;;;; These methods must be defined here else the DEFSECTION forms in
;;;; pax.lisp will export too much.

(defmethod exportable-locative-type-p ((locative-type (eql 'asdf:system)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'package)))
  nil)

(defmethod exportable-locative-type-p ((locative-type (eql 'method)))
  nil)
