(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(declaim (ftype function document-object))
(declaim (ftype function mark-up-signatures-p))

(defsection @output-formats (:title "Output Formats")
  (*document-mark-up-signatures* variable)
  (@plain-output section)
  (@markdown-output section)
  (@pdf-output section)
  (@dummy-output section))

(defsection @plain-output (:title "Plain Output")
  "This is the default :FORMAT of DOCUMENT, intended to be a
  replacement for CL:DOCUMENTATION. :PLAIN (short for plain text) is
  very similar to :MARKDOWN, but most of the markup that would make
  reading in, say, the REPL unpleasant is removed.

  @PLAIN-FORMAT")

(defsection @markdown-output (:title "Markdown Output")
  "[document-object* (method (dref t))][docstring]

  With this default format, PAX supports all locative types, but for
  some DREF::@BASIC-LOCATIVE-TYPES defined in DRef and the
  @PAX-LOCATIVES, special provisions have been made.

  - [document-object* (method (variable-dref t))][docstring]
  - [document-object* (method (setf-dref t))][docstring]
  - [document-object* (method (method-dref t))][docstring]
  - [document-object* (method (accessor-dref t))][docstring]
  - [document-object* (method (structure-accessor-dref t))][docstring]
  - [document-object* (method (class-dref t))][docstring]
  - [document-object* (method (structure-dref t))][docstring]
  - [document-object* (method (condition-dref t))][docstring]
  - [document-object* (method (asdf-system-dref t))][docstring]
  - [document-object* (method (locative-dref t))][docstring]
  - [document-object* (method (section t))][docstring]
  - [document-object* (method (glossary-term t))][docstring]
  - [document-object* (method (go-dref t))][docstring]
  - [document-object* (method (include-dref t))][docstring]
  - [document-object* (method (clhs-dref t))][docstring]
  - [document-object* (method (unknown-dref t))][docstring]"
  (@public-superclasses glossary-term))

(defmethod document-object* ((dref dref) stream)
  "By default, [DREF][class]s are documented in the following format.

  ```
  - [<locative-type>] <name> <arglist>

      <docstring>
  ```

  The line with the bullet is printed with DOCUMENTING-DEFINITION. The
  docstring is processed with DOCUMENT-DOCSTRING while
  @LOCAL-DEFINITIONs established with WITH-DISLOCATED-NAMES are in
  effect for all variables locally bound in a definition with ARGLIST,
  and *PACKAGE* is bound to the second return value of DOCSTRING."
  (multiple-value-bind (arglist arglist-type) (arglist dref)
    (multiple-value-bind (docstring package) (docstring dref)
      (documenting-definition (stream :arglist arglist :package package)
        (with-dislocated-names (case arglist-type
                                 ((:macro :deftype :destructuring)
                                  (dref::macro-arg-names arglist))
                                 ((:ordinary)
                                  (or
                                   (ignore-errors
                                    (dref::function-arg-names arglist))
                                   (dref::macro-arg-names arglist))))
          (document-docstring docstring stream))))))

(declaim (ftype function prin1-to-string*))

(defmethod document-object* ((dref variable-dref) stream)
  "For definitions with a [VARIABLE][locative] or CONSTANT locative,
  their initform is printed as their arglist. The initform is the
  INITFORM argument of the locative if provided, or the global symbol
  value of their name. If no INITFORM is provided, and the symbol is
  globally unbound, then no arglist is printed.

  When the printed initform is too long, it is truncated."
  (let ((symbol (dref-name dref)))
    (destructuring-bind (&optional (initform nil initformp))
        (xref-locative-args (dref-origin dref))
      (let ((arglist (multiple-value-bind (value unboundp)
                         (autoload::symbol-global-value symbol)
                       (when (or initformp (not unboundp))
                         (let ((*print-pretty* t))
                           (escape-markdown
                            (shorten-string
                             (prin1-to-string* (if initformp initform value))
                             :n-lines 10 :n-chars 512 :ellipsis " ...")))))))
        (documenting-definition (stream :arglist arglist)
          (document-docstring (docstring dref) stream))))))

(defmethod document-object* ((dref setf-dref) stream)
  "Depending of what the SETF locative refers to, the ARGLIST of the
  [setf expander][clhs], [setf function][clhs], or the method
  signature is printed as with the METHOD locative."
  (let ((resolved (resolve dref nil)))
    (if (typep resolved 'method)
        (%document-method dref stream)
        (call-next-method))))

(defmethod document-object* ((dref method-dref) stream)
  "For definitions with a METHOD locative, the arglist printed is
  the method signature, which consists of the locative's `QUALIFIERS`
  and `SPECIALIZERS` appended."
  (%document-method dref stream))

(defun %document-method (dref stream)
  (declare (type (or method-dref setf-dref) dref))
  (let ((arglist (method-pretty-arglist dref)))
    (documenting-definition (stream :arglist `(:method ,@arglist))
      (with-dislocated-names (dref::function-arg-names (arglist dref))
        (document-docstring (docstring dref) stream)))))

;;; Return a "pretty" list of the method's specializers. Normal
;;; specializers are replaced by the name of the class, eql
;;; specializers are replaced by `(EQL ,OBJECT).
(defun method-pretty-arglist (dref)
  (multiple-value-bind (qualifiers specializers)
      (dref::method-locative-specializer-and-qualifiers
       (dref-locative-args dref))
    (append qualifiers
            (mapcar (lambda (name spec)
                      (if (eq spec t)
                          name
                          (list name spec)))
                    (dref::method-arglist (resolve dref))
                    specializers))))


;;;; Utilities

(defun md-reflink-from (name locative)
  (let ((md-name (if (stringp name)
                     (escape-markdown name)
                     (prin1-to-markdown name))))
    ;; To avoid warnings, do not link explicitly if there is nothing
    ;; to link to.
    (if (dref name locative nil)
        (format nil "[~A][~A]" md-name
                (let ((*print-readably* nil))
                  (prin1-to-markdown locative)))
        (format nil "~A" md-name))))


;;;; ACCESSOR, READER and WRITER locatives

(defmethod document-object* ((dref accessor-dref) stream)
  "For definitions with an [ACCESSOR][locative], [READER][locative] or
  WRITER locative, the class on which they are specialized is printed
  as their arglist."
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (generate-documentation-for-slot-definition
     (dref::find-accessor-slot-definition symbol (first locative-args))
     (first locative-args) stream)))

(defmethod document-object* ((dref reader-dref) stream)
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (generate-documentation-for-slot-definition
     (dref::find-reader-slot-definition symbol (first locative-args))
     (first locative-args) stream)))

(defmethod document-object* ((dref writer-dref) stream)
  (let ((symbol (dref-name dref))
        (locative-args (dref-locative-args dref)))
    (generate-documentation-for-slot-definition
     (dref::find-writer-slot-definition symbol (first locative-args))
     (first locative-args) stream)))

(defun generate-documentation-for-slot-definition (slot-def class stream)
  (let ((arglist (format nil "~A~@[ ~A~]" (md-link (dref class 'class))
                         (slot-def-to-string slot-def))))
    (documenting-definition (stream :arglist arglist)
      ;; There is no documentation for condition accessors, and some
      ;; implementations signal warnings.
      (unless (subtypep (find-class class) 'condition)
        (document-docstring (ignore-errors
                             (swank-mop:slot-definition-documentation slot-def))
                            stream)))))

(defun slot-def-to-string (slot-def)
  (when (and slot-def
             (or (swank-mop:slot-definition-initargs slot-def)
                 (swank-mop:slot-definition-initfunction slot-def)))
    (if (mark-up-signatures-p)
        (let ((initarg-strings
                (when (swank-mop:slot-definition-initargs slot-def)
                  (mapcar #'prin1-to-markdown
                          (swank-mop:slot-definition-initargs slot-def)))))
          (format nil "(~{~A~^ ~}~A)" initarg-strings
                  (if (swank-mop:slot-definition-initfunction slot-def)
                      (format nil "~A= ~A"
                              (if initarg-strings " " "")
                              (prin1-to-markdown
                               (swank-mop:slot-definition-initform
                                slot-def)))
                      "")))
        (prin1-to-markdown
         `(,@(when (swank-mop:slot-definition-initargs slot-def)
               (swank-mop:slot-definition-initargs slot-def))
           ,@(when (swank-mop:slot-definition-initfunction slot-def)
               `(=
                 ,(swank-mop:slot-definition-initform slot-def))))))))


;;;; STRUCTURE-ACCESSOR, STRUCTURE, CLASS and CONDITION locatives

(defmethod document-object* ((dref structure-accessor-dref) stream)
  "For definitions with a STRUCTURE-ACCESSOR locative, the arglist
  printed is the locative's CLASS-NAME argument if provided."
  (documenting-definition (stream :arglist (dref-locative-args dref))
    (document-docstring (docstring dref) stream)))

(defmethod document-object* ((dref structure-dref) stream)
  "For definitions with a STRUCTURE locative, the arglist printed is
  the list of @PUBLIC-SUPERCLASSES with STRUCTURE-OBJECT omitted."
  (%document-class dref stream))

(defmethod document-object* ((dref class-dref) stream)
  "For definitions with a CLASS locative, the arglist printed is the
  list of @PUBLIC-SUPERCLASSES with STANDARD-OBJECT and CONDITION
  omitted."
  (%document-class dref stream))

(defmethod document-object* ((dref condition-dref) stream)
  "For definitions with a CONDITION locative, the arglist printed is
  the list of @PUBLIC-SUPERCLASSES with STANDARD-OBJECT and
  CONDITION omitted."
  (%document-class dref stream))

(defun %document-class (dref stream)
  (let* ((class (find-class (dref-name dref)))
         (superclasses (remove-if (lambda (super)
                                    (member super '(standard-object condition
                                                    structure-object)))
                                  (public-superclasses class)))
         (arglist (when superclasses
                    (if (and (not *first-pass*) *document-mark-up-signatures*)
                        (mark-up-superclasses superclasses)
                        superclasses))))
    (documenting-definition (stream :arglist arglist)
      (document-docstring (docstring dref) stream))))

(define-glossary-term @public-superclasses (:title "public superclasses")
  "[public-superclasses function][docstring]")

(defun public-superclasses (class)
  "The public superclasses of a class are tightest envelope of
  superclasses with names exported from some package. This envelope is
  constructed by recursing depth-first into the superclass hierarchy.
  If the name of the superclass currently processed is exported from
  any package, then it is collected as a public superclass, and we do
  not recurse into its superclasses."
  (let ((superclasses ()))
    (labels
        ((recurse (class)
           (let ((direct-supers (swank-mop:class-direct-superclasses class)))
             (dolist (super direct-supers)
               (let ((super-name (class-name super)))
                 (if (external-symbol-in-any-package-p super-name)
                     (pushnew super-name superclasses)
                     (recurse super)))))))
      (recurse (if (typep class 'class)
                   class
                   (find-class class))))
    (reverse superclasses)))

(defun mark-up-superclasses (superclasses)
  (with-output-to-string (stream)
    (loop for class in superclasses
          for i upfrom 0
          do (unless (zerop i)
               (format stream " "))
             (write-md-link (dref class 'class) stream))))

(defun write-md-link (dref stream)
  (unless *first-pass*
    (let ((name (prin1-to-markdown (dref-name dref))))
      (if (find-link dref)
          (format stream "[~A][~A]" name (link-to-definition dref))
          (format stream "~A" name)))))

(defun md-link (dref)
  (with-output-to-string (stream)
    (write-md-link dref stream)))


;;;; ASDF:SYSTEM locative

;;; For testing
(defvar *omit-asdf-slots* nil)

(defmethod document-object* ((dref asdf-system-dref) stream)
  "For definitions with a ASDF:SYSTEM locative, their most
  important slots are printed as an unnumbered list."
  (let ((system (resolve dref)))
    (documenting-definition (stream)
      (flet ((foo (name fn &key type)
               (let ((value (funcall fn system)))
                 (when (and value (not (equal value "")))
                   (case type
                     ((:link)
                      (format stream "    - _~A:_ [~A](~A)~%" name value value))
                     ((:mailto)
                      (format stream "    - _~A:_ [~A](mailto:~A)~%"
                              name value value))
                     ((:source-control)
                      (cond ((and (listp value) (= (length value) 2))
                             (format stream "    - _~A:_ [~A](~A)~%"
                                     name (first value) (second value)))
                            ((stringp value)
                             (format stream "    - _~A:_ [~A](~A)~%"
                                     name value value))))
                     ((:docstring)
                      (format stream "    - _~A:_ " name)
                      (document-docstring value stream :indentation "        "
                                          :exclude-first-line-p t
                                          :paragraphp nil)
                      (format stream "~&"))
                     ((:list-of-systems)
                      (document-docstring
                       (format nil "- _~A:_ ~{~A~^, ~}~%"
                               name (asdf-deps value))
                       stream :paragraphp nil))
                     ((nil)
                      (format stream "    - _~A:_ ~A~%" name value)))))))
        (unless *omit-asdf-slots*
          (terpri stream)
          (foo "Version" 'asdf/component:component-version)
          (foo "Description" 'asdf/system:system-description :type :docstring)
          (foo "Long Description" 'asdf/system:system-long-description
               :type :docstring)
          (foo "Licence" 'asdf/system:system-licence)
          (foo "Author" 'asdf/system:system-author)
          (foo "Maintainer" 'asdf/system:system-maintainer)
          (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
          (foo "Homepage" 'asdf/system:system-homepage :type :link)
          (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
          (foo "Source control" 'asdf/system:system-source-control
               :type :source-control)
          (foo "Depends on" 'asdf:system-depends-on :type :list-of-systems)
          (foo "Defsystem depends on" 'asdf:system-defsystem-depends-on
               :type :list-of-systems))))))

(defun asdf-deps (dep-names)
  (let ((names* (sort (remove nil (mapcar #'extract-asdf-dep-name dep-names))
                      #'string< :key #'first)))
    (loop for (name conditionalp) in names*
          collect (format nil "~A~A" (md-reflink-from name 'asdf:system)
                          (if conditionalp "(?)" "")))))

;;; Return the name of the dependency and whether it's a conditional
;;; dependency.
(defun extract-asdf-dep-name (dep)
  ;; Typically, DEP is just the name of the system ...
  (if (stringp dep)
      (list dep nil)
      ;; ... but it may be conditional like (:FEATURE :CORMAN
      ;; (:REQUIRE "threads")).
      (let ((string (find-if-in-tree #'stringp dep)))
        (if string
            (list string t)
            ;; Survive the unexpected.
            nil))))


;;;; PACKAGE locative

(defmethod document-object* ((dref package-dref) stream)
  (let* ((nicknames (package-nicknames (resolve dref)))
         (arglist (when nicknames
                    (list :nicknames nicknames))))
    (documenting-definition (stream :arglist arglist)
      (document-docstring (docstring dref) stream))))


;;;; LOCATIVE locative

(defmethod document-object* ((dref locative-dref) stream)
  "For definitions with the LOCATIVE locative type, their
  LOCATIVE-TYPE-DIRECT-SUPERS and LOCATIVE-TYPE-DIRECT-SUBS are
  printed."
  (documenting-definition (stream)
    (let ((locative-type (dref-name dref)))
      (document-docstring
       (with-output-to-string (stream)
         (when-let ((direct-supers (locative-type-direct-supers locative-type)))
           (format stream "- Direct locative supertypes: ~{~A~^, ~}~%"
                   (loop for super in direct-supers
                         collect (md-link (dref super 'locative)))))
         (when-let ((direct-subs (locative-type-direct-subs locative-type)))
           (format stream "- Direct locative subtypes: ~{~A~^, ~}~%"
                   (loop for sub in direct-subs
                         collect (md-link (dref sub 'locative))))))
       stream))
    (document-docstring (docstring dref) stream)))


;;;; SECTION locative

(defvar *section*)

(defmacro documenting-section ((section stream) &body body)
  (with-gensyms (same-package)
    (once-only (section)
      `(let ((,same-package (and (eq *package* (section-package ,section))
                                 (or (boundp '*section*)
                                     *document-open-linking*)))
             (*package* (section-package ,section))
             (*readtable* (section-readtable ,section))
             (*section* ,section))
         (with-heading (,stream :dref *section*
                        :link-title-to (section-link-title-to ,section))
           (when (and (not ,same-package) *document-normalize-packages*)
             (format-in-package *package* ,stream))
           ,@body)))))

(defmethod document-object* ((section section) stream)
  "When documentation is being generated for a definition with
  the SECTION locative, a new (sub)section is opened (see
  WITH-HEADING), within which documentation for its each of its
  SECTION-ENTRIES is generated. A fresh line is printed after all
  entries except the last."
  (documenting-section (section stream)
    (let ((firstp t))
      (dolist (entry (section-entries section))
        (if firstp
            (setq firstp nil)
            (terpri stream))
        (document-object entry stream)))))

(defun format-in-package (package stream)
  (let ((name (escape-markdown (package-name package)))
        (nicknames (if (package-nicknames *package*)
                       (format nil " with nicknames ~{~A~^, ~}"
                               (mapcar #'escape-markdown
                                       (package-nicknames package)))
                       "")))
    (if (eq *format* :pdf)
        (format stream "`\\subsubsection*{\\normalfont~
                       \\textcolor[HTML]{606060}{[in package ~A~A]}}`{=latex}~%"
                name nicknames)
        (format stream "###### \\[in package ~A~A\\]~%" name nicknames))))


(defmethod document-object* ((glossary-term glossary-term) stream)
  "For definitions with a GLOSSARY-TERM locative, no arglist is
  printed, and if non-NIL, GLOSSARY-TERM-TITLE is printed as name."
  (documenting-definition (stream)
    (when (glossary-term-url glossary-term)
      (document-docstring
       (format nil "External link to [~A](~A)."
               (escape-markdown (glossary-term-url glossary-term))
               (glossary-term-url glossary-term))
       stream))
    (document-docstring (glossary-term-docstring glossary-term) stream)))

(defmethod document-object* ((dref go-dref) stream)
  "For definitions with a GO locative, its LOCATIVE-ARGS are printed
  as its arglist, along with a redirection message."
  (let ((locative-args (dref-locative-args dref)))
    (documenting-definition (stream :arglist locative-args)
      (document-docstring
       (format nil "See ~A." (apply #'md-reflink-from
                                    (first (dref-locative-args dref))))
       stream))))


;;;; INCLUDE locative

(defmethod document-object* ((dref include-dref) stream)
  "See the INCLUDE locative."
  (unless *first-pass*
    (let ((locative-args (dref-locative-args dref)))
      (destructuring-bind (source &key (line-prefix "") header footer
                                    header-nl footer-nl) locative-args
        (handler-case
            (multiple-value-bind (file start-loc end-loc)
                (include-region source)
              (let ((start (source-location-adjusted-file-position start-loc))
                    (end (source-location-adjusted-file-position end-loc)))
                (cond ((and start-loc (null start))
                       (warn "~@<~S cannot find ~S ~S.~:@>"
                             'include :start start-loc))
                      ((and end-loc (null end))
                       (warn "~@<~S cannot find ~S ~S.~:@>"
                             'include :end end-loc))
                      (t
                       (write-string
                        (codify-and-link
                         (with-output-to-string (stream)
                           (let ((text (file-subseq file start end)))
                             (unless text
                               (warn "~@<~S's ~S ~S is after its ~S ~S.~:@>"
                                     'include :start start :end end))
                             (when header
                               (format stream "~A" header))
                             (when header-nl
                               (format stream "~&")
                               (format stream header-nl)
                               (format stream "~%"))
                             (format stream "~A" (prefix-lines line-prefix
                                                               text))
                             (when footer
                               (format stream footer))
                             (when footer-nl
                               (format stream "~&")
                               (format stream footer-nl)
                               (format stream "~%")))))
                        stream)))))
          (include-error (c)
            (warn "~@<~?~:@>" (format-control c) (format-args c))))))))

(defun file-subseq (pathname &optional start end)
  (with-open-file (stream pathname)
    (let ((*print-pretty* nil)
          (start (or start 0))
          (end (or end (file-length stream)))
          (buffer-size 4096))
      (file-position stream start)
      (when (<= start end)
        (with-output-to-string (datum)
          (let ((buffer (make-array buffer-size :element-type 'character)))
            (loop
              for bytes-read = (read-sequence
                                buffer stream
                                :end (min buffer-size
                                          (- end (file-position stream))))
              do (write-sequence buffer datum :start 0 :end bytes-read)
              while (= bytes-read buffer-size))))))))


(defmethod document-object* ((dref clhs-dref) stream)
  "For definitions with a CLHS locative, the LOCATIVE-ARGS are printed
  as the arglist. For CLHS SECTIONs, the title is included in the
  arglist."
  (multiple-value-bind (title aliases)
      (find-hyperspec-section-title (dref-name dref))
    (documenting-definition (stream :arglist
                             (append (dref-locative-args dref)
                                     (ensure-list title)))
      (when aliases
        (format stream "Aliases: ~{~S~^, ~}~%" aliases)))))

(defmethod document-object* ((dref unknown-dref) stream)
  "For definitions with an UNKNOWN locative, the LOCATIVE-ARGS are
  printed as the arglist. There is no docstring."
  (let ((locative-args (dref-locative-args dref)))
    (documenting-definition
        (stream :arglist (escape-markdown
                          (with-standard-io-syntax*
                            ;; Are dspecs readable?
                            (let ((*print-readably* nil))
                              (prin1-to-string (first locative-args)))))))))


(defmethod docstring* ((dref note-dref))
  (let ((note (definition-property dref 'note)))
    (values (note-docstring note)
            (%note-package note))))

(defun note-docstring (note)
  (with-output-to-string (s)
    (let ((join (or (%note-join note)
                    #.(format nil "~A~A" #\Newline #\Newline)))
          (firstp t))
      (dolist (child (reverse (%note-children note)))
        (unless firstp
          (princ join s))
        (if (stringp child)
            (princ (sanitize-docstring child) s)
            (princ (note-docstring child) s))
        (setq firstp nil)))))



(defsection @dummy-output (:title "Dummy Output")
  "When the FORMAT argument of DOCUMENT is NIL, no output is
  generated, but @TRANSCRIPT-CONSISTENCY-CHECKING is still performed.
  Use this feature to quickly test documentation examples.

  For example, in [Try][try::@try-manual section] the test would look
  like this:

  ```
  (try:signals-not (transcription-consistency-error)
    (document ... :format nil))
  ```")
