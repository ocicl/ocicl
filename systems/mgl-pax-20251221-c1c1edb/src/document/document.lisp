(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @generating-documentation (:title "Generating Documentation")
  (@document-function section)
  (@browsing-live-documentation section)
  (@markdown-support section)
  (@codification section)
  (@linking section)
  (@local-definition section)
  (@overview-of-escaping section)
  (@output-formats section)
  (@document-implementation-notes section)
  (@documentation-utilities section))

;;; This is the core of the DOCUMENT function, presented here for an
;;; overview.
;;;
;;; Markdown is generated in 2.5 passes. In the 1st pass, the output
;;; is discarded, but we
;;;
;;; - COLLECT-HEADINGs,
;;; - populate PAGE-DEFINITIONS,
;;; - set PAGE-WRITTEN-IN-FIRST-PASS-Ps.
;;;
;;; In the 2nd pass, output goes to the real output stream(s) (see
;;; PAGE-TEMP-STREAM-SPEC), and with the information gathered in the
;;; first pass, we may
;;;
;;; - PRINT-TOPLEVEL-SECTION-LIST for each PAGE-WRITTEN-IN-FIRST-PASS-P,
;;; - PRINT-TABLE-OF-CONTENTS for top-level sections,
;;; - add prev/next/up links to sections (FANCY-NAVIGATION),
;;; - know what definitions are @LINKABLE.
;;;
;;; In the 2.5th pass, we add Markdown reference link definitions,
;;; headers, footers, and convert Markdown to another format if
;;; necessary.
(defmacro %document (documentable stream page-specs)
  (once-only (documentable stream page-specs)
    `(with-documentable-bindings (documentable)
       (with-link-maps ()
         (let ((*local-references* ())
               (*pages* (page-specs-to-pages ,documentable ,stream
                                             ,page-specs)))
           (with-headings ()
             ;; 1st pass
             (let ((*first-pass* t)
                   (*page* (last-elt *pages*)))
               (document-documentable ,documentable (make-broadcast-stream)))
             (finalize-headings)
             (finalize-pages *pages*)
             ;; 2nd pass
             (let ((*first-pass* nil))
               (print-toplevel-section-lists *pages*)
               ;; Initially, send output to the default page (built
               ;; for STREAM). Note that on PAGE-BOUNDARIES,
               ;; DOCUMENT-OBJECT (method (dref t)) redirects the
               ;; output.
               (with-temp-output-to-page (,stream (last-elt *pages*))
                 (document-documentable ,documentable ,stream))
               ;; 2.5th pass
               (mapcar #'finalize-page-output *pages*))))))))

(defvar *first-pass*)


;;;; Numbering and collecting headings

(defmacro with-headings (() &body body)
  `(let ((*headings* ())
         (*heading-number* ())
         (*heading-level* 0))
     ,@body))

;;; The section number of the most recent WITH-HEADING. It is a
;;; mutable list of integers, whose length is the nesting depth. The
;;; number of top-level headings is '(), but they append a 0 to the
;;; end of *HEADING-NUMBER* for the processing of headings nested in
;;; them. When such a nested WITH-HEADING is encountered, the last
;;; number is INCFed, and that will be its heading numbers (i.e. (1)
;;; here). Then another 0 is added to the end of *HEADING-NUMBER* for
;;; the BODY of WITH-HEADING, and processing goes on.
(defvar *heading-number* ())

;;; (LENGTH *HEADING-NUMBER*)
(defvar *heading-level* 0)

;;; This is a list of HEADING objects in the order of generation
;;; during the second pass. It's in reverse order while being
;;; accumulated in the first pass.
(defvar *headings* ())

;;; Called at the end of the first pass. Reverse the order of
;;; *HEADINGS*.
(defun finalize-headings ()
  (setq *headings* (reverse *headings*)))

(defstruct heading
  object
  title
  number
  level)

(defun collect-heading (object title)
  (push (make-heading :object object :title title
                      :number (copy-list *heading-number*)
                      :level *heading-level*)
        *headings*))

;;; PAX-APROPOS* binds this to :DETAILED or :TERSE.
(defvar *document-list-view* nil)

(defun document-definition-title-or-anchor (dref)
  (or (document-definition-title dref)
      (escape-markdown (dref-to-anchor dref))))

;;; This is the implementation of the WITH-HEADING macro.
(defun/autoloaded call-with-heading (stream dref link-title-to fn)
  (let ((level *heading-level*)
        (title (document-definition-title-or-anchor dref)))
    (when (plusp level)
      (incf (nth (1- level) *heading-number*)))
    (when *first-pass*
      (collect-heading dref title))
    (cond (*document-list-view*
           (documenting-definition (stream :arglist (ensure-list
                                                     (doctitle dref)))))
          (t
           (unless *first-pass*
             (print-section-title stream dref title link-title-to)
             (print-table-of-contents dref stream))
           (let ((*heading-number*
                   (append *heading-number*
                           (loop repeat (max 0 (- (1+ level)
                                                  (length *heading-number*)))
                                 collect 0)))
                 (*heading-level* (1+ *heading-level*)))
             (funcall fn stream))))))

;;; Add this many #\# to Markdown section headings in the output. This
;;; is for when a section that is a subsection of another is
;;; documented on its own page by DOCUMENT/OPEN.
(defvar *heading-offset* 0)

(defmacro with-heading-offset ((object) &body body)
  `(let ((*heading-offset* (heading-offset ,object)))
     ,@body))

;;; Determine what SECTION's *HEADING-LEVEL* would be under its root
;;; ancestor.
(defun heading-offset (object)
  ;; This calculation is quite expensive. Don't do it if it's not
  ;; going to be used.
  (unless *document-list-view*
    (multiple-value-bind (foundp depth) (and (not (stringp object))
                                             (find-root-section object))
      (if foundp
          depth
          ;; OBJECT is not a SECTION (or a reference to one), neither is
          ;; it contained in a SECTION. Start from H2. This only affects
          ;; ASDF:SYSTEMs in stock PAX.
          1))))


;;; A PAGE is basically a single Markdown or HTML file to where the
;;; documentation of some definitions is written. Constructed by
;;; PAGE-SPECS-TO-PAGES.
(defstruct page
  ;; DREFs for the :OBJECTS of the page-spec. In the second pass,
  ;; output is redirected to this page, when encountering one of these
  ;; definitions.
  boundaries
  ;; The second pass writes the Markdown output to this stream. It's
  ;; actually STREAM-SPEC (see WITH-OPEN-STREAM-SPEC) to allow the
  ;; temporary stream to
  ;;
  ;; - be created lazily so that no stray files are left around and
  ;;   only a small number of fds are needed even for a huge project;
  ;;
  ;; - be opened multiple times (which is not a given for string
  ;;   streams).
  temp-stream-spec
  ;; FINALIZE-PAGE-OUTPUT may convert the Markdown in TEMP-STREAM-SPEC
  ;; to a non-Markdown format or do final touchups.
  final-stream-spec
  ;; URI-FRAGMENT is a string such as "doc/manual.html" that specifies
  ;; where the page will be deployed on a webserver. It defines how
  ;; links between pages will look. If it's not specified, and OUTPUT
  ;; refers to a file, then it defaults to the name of the file. If
  ;; URI-FRAGMENT is NIL, then no links will be made to or from that
  ;; page.
  uri-fragment
  ;; See PAGE-SPECS-TO-PAGES for HEADER-FN, FOOTER-FN and
  ;; SOURCE-URI-FN.
  header-fn
  footer-fn
  source-uri-fn
  ;; The DREFs written to this page. Set in the first pass.
  definitions
  ;; Any output written to this page (including plain docstrings)?
  written-in-first-pass-p
  written-in-second-pass-p
  ;; LINKS made from this page. For LINK-TO-DEFINITION and
  ;; WRITE-MARKDOWN-REFERENCE-STYLE-LINK-DEFINITIONS.
  (used-links (make-hash-table :test #'eq) :type hash-table))

;;; All the PAGEs in a DOCUMENT call.
(defvar *pages*)

;;; Return the first page whose PAGE-BOUNDARIES have DREF.
(defun boundary-page (dref)
  (dolist (page *pages*)
    (when (find dref (page-boundaries page) :test #'xref=)
      (return page))))

;;; The current page where output is being sent.
(defvar *page* nil)

(defvar *page-stream*)

(defmacro with-temp-output-to-page ((stream page) &body body)
  (once-only (page)
    (with-gensyms (stream-spec)
      `(flet ((foo (,stream)
                ,@body))
         (if (or (null ,page) (eq ,page *page*))
             (foo *page-stream*)
             (let ((,stream-spec (page-temp-stream-spec ,page)))
               (with-open-stream-spec (,stream ,stream-spec
                                       :direction :output)
                 (let ((*page* ,page)
                       (*page-stream* ,stream))
                   (foo ,stream)))))))))

(defmacro with-temp-input-from-page ((stream page) &body body)
  `(with-open-stream-spec (,stream (page-temp-stream-spec ,page))
     ,@body))

(defmacro with-final-output-to-page ((stream page) &body body)
  `(with-open-stream-spec (,stream (page-final-stream-spec ,page)
                           :direction :output)
     ;; This allows HEADER-FN and FOOTER-FN to support linking
     ;; references with LINK-TO-URI.
     (let ((*page* ,page))
       ,@body)))


;;; A LINK (a possible link target, really) is a definition that
;;; resides on PAGE. Note that the same definition may be written to
;;; multiple pages.
(defstruct link
  (definition nil :type dref)
  ;; STRING pages denote URLs (see EXTERNAL-DREF-URLs). NULL pages are
  ;; to support "pax:" URLs for *DOCUMENT-OPEN-LINKING*.
  (page nil :type (or page string null)))

(defun make-link* (dref page)
  (or (when-let (url (external-dref-url dref))
        (make-link :definition dref :page url))
      (make-link :definition dref :page page)))

;;; An EQUAL hash table, mapping a DREF in (NAME . LOCATIVE) form to
;;; its LINK within a single DOCUMENT call. FINALIZE-PAGES populates
;;; this after the first pass with all definitions that are being
;;; documented. In case the same definition is documented on multiple
;;; pages, links will go to the last such page.
;;;
;;; FIND-LINK may add more links in the second pass for
;;; *DOCUMENT-OPEN-LINKING* and CLHS definitions.
(defvar *links*)

(defmacro link-key (dref)
  (once-only ((dref dref))
    `(cons (dref-name ,dref) (dref-locative ,dref))))

(declaim (inline set-link))
(defun set-link (key link)
  (setf (gethash key *links*) link))

(declaim (inline get-link))
(defun get-link (key)
  (gethash key *links*))

;;; Called at the end of the first pass. Reverse PAGE-DEFINITIONS so
;;; that it's in depth-first order, and add a LINK to *LINKS* for all
;;; definitions on each page.
(defun finalize-pages (pages)
  (dolist (page pages)
    (setf (page-definitions page) (reverse (page-definitions page)))
    (dolist (dref (page-definitions page))
      (set-link (link-key dref) (make-link* dref page)))))

;;; Whether all definitions present in the running Lisp are @LINKABLE
;;; with magic "pax:" URLs. This is to support
;;; @BROWSING-LIVE-DOCUMENTATION.
(defvar *document-open-linking* nil)

(defun find-link (dref)
  (declare (type dref dref))
  (assert (not *first-pass*))
  (let ((key (link-key dref)))
    (or (get-link key)
        (when (or *document-open-linking* (external-dref-p dref)
                  ;; KLUDGE: For @TITLEd definitions, fake a PAX link,
                  ;; so that they make it to MAKE-REFLINKS, which
                  ;; replaces the fake link with the title.
                  (doctitle dref)
                  ;; Similarly for NOTEs, except that MAKE-REFLINKS
                  ;; will auto-include the note.
                  (typep dref 'note-dref))
          (set-link key (make-link* dref nil)))
        ;; Maybe fall back on the CLHS definition.
        (when-let (clhs-link (and *document-link-to-hyperspec*
                                  (clhs-link dref)))
          (set-link key clhs-link)))))

(defun clhs-link (dref)
  (let ((clhs-dref (clhs-dref (dref-name dref) (dref-locative dref))))
    (when clhs-dref
      (or (get-link (link-key clhs-dref))
          ;; Ensure that if two CLHS DREFs are XREF=, then they are
          ;; EQ, so they have the same id (see ENSURE-LINK-ID).
          (set-link (link-key clhs-dref) (make-link* clhs-dref nil))))))


;;; Increment the link counter for the current page and return the
;;; link id.
(defun link-to (link)
  (declare (type link link))
  (when (let ((page (link-page link)))
          (or (null page)
              (eq *page* page)
              (stringp page)
              (and (page-uri-fragment *page*)
                   (page-uri-fragment page))))
    (setf (gethash link (page-used-links *page*)) t)
    (ensure-link-id link)))

(defun link-to-definition (dref)
  (link-to (find-link dref)))

;;; Link ids are short hashes (as STRINGs), and they go into Markdown
;;; reference links. Due to possible collisions, they are
;;; context-dependent, so to keep LINKs immutable, ids are in this
;;; hash table.
(defvar *link-to-id*)
;;; A LINK-ID to LINK hash table for MD5 collision detection.
(defvar *id-to-link*)

(defun link-id (link)
  (gethash link *link-to-id*))

(defun ensure-link-id (link)
  (or (gethash link *link-to-id*)
      (let ((id (hash-link (dref-to-anchor (link-definition link))
                           #'find-link-by-id)))
        (setf (gethash id *id-to-link*) link)
        (setf (gethash link *link-to-id*) id))))

(defun find-link-by-id (id)
  (gethash id *id-to-link*))

(defun definition-page (dref)
  (when-let (link (find-link dref))
    (link-page link)))

(defmacro with-link-maps (() &body body)
  `(let ((*links* (make-hash-table :test #'equal))
         (*link-to-id* (make-hash-table :test #'eq))
         (*id-to-link* (make-hash-table :test #'equal)))
     (locally ,@body)))


(defsection @documentable (:title "DOCUMENTABLE")
  "The DOCUMENTABLE argument of DOCUMENT may be a single object (e.g.
  `#'PRINT`'), a DREF::@DEFINITION such as `(DREF 'PRINT 'FUNCTION)`,
  a string, or a nested list of these. More precisely, DOCUMENTABLE is
  one of the following:

  - _single definition designator_: A [DREF][class] or anything else
    that is LOCATEable. This includes non-DREF [XREF][class]s and
    first-class objects such as [FUNCTION][class]s. The generated
    documentation typically includes the definition's DOCSTRING. See
    @MARKDOWN-OUTPUT for more.

  - _docstring_: A string, in which case it is processed like a
    docstring in DEFSECTION. That is, with [docstring sanitization]
    [@markdown-in-docstrings], @CODIFICATION, and @LINKING.

  - _list of documentables_: A nested list of LOCATEable objects and
    docstrings. The objects in it are documented in depth-first order.
    The structure of the list is otherwise unimportant."
  (*document-tight* variable))

(defvar/autoloaded *document-tight* nil
  "If NIL, then DOCUMENT adds a newline between consecutive
  [atomic][clhs] documentables on the same [page][@pages].")

(defvar *objects-being-documented* ())

(defun about-to-write-to-page ()
  (assert (not *first-pass*))
  (if (page-written-in-second-pass-p *page*)
      (when (and (not *document-tight*)
                 (list-of-one-p *objects-being-documented*))
        (terpri *page-stream*))
      (setf (page-written-in-second-pass-p *page*) t)))

;;; Stuff a description of *OBJECTS-BEING-DOCUMENTED* into known
;;; conditions.
(defmacro with-document-context (&body body)
  `(handler-bind ((locate-error
                    (lambda (e)
                      #-cmucl
                      (let ((args (list (slot-value e 'dref::message)
                                        (slot-value e 'dref::message-args)
                                        (print-document-context nil))))
                        (setf (slot-value e 'dref::message) "~?~A"
                              (slot-value e 'dref::message-args) args))))
                  (transcription-error
                    (lambda (e)
                      #-cmucl
                      (let ((args (list (slot-value e 'message)
                                        (slot-value e 'message-args)
                                        (print-document-context nil))))
                        (setf (slot-value e 'message) "~?~A"
                              (slot-value e 'message-args) args))))
                  (simple-error
                    (lambda (e)
                      (if (find-restart 'continue e)
                          (cerror "Continue."
                                  "~?~A" (simple-condition-format-control e)
                                  (simple-condition-format-arguments e)
                                  (print-document-context nil))
                          (error "~?~A" (simple-condition-format-control e)
                                 (simple-condition-format-arguments e)
                                 (print-document-context nil)))))
                  (simple-warning
                    (lambda (w)
                      (warn "~?~A" (simple-condition-format-control w)
                            (simple-condition-format-arguments w)
                            (print-document-context nil))
                      (muffle-warning w))))
     ,@body))

(defun print-document-context (stream)
  (let ((*package* (find-package :keyword))
        (context (loop for object in *objects-being-documented*
                       when (typep object 'dref)
                         collect (list (dref-name object)
                                       (dref-locative object)))))
    (if context
        (format stream "~%  [While documenting ~{~S~^~%   in ~}]~%" context)
        (format stream ""))))

(defun document-documentable (documentable stream)
  (with-document-context
    (map-documentable (lambda (object1)
                        (with-heading-offset (object1)
                          (document-object object1 stream)))
                      documentable)))

(defmacro with-documentable-bindings ((documentable) &body body)
  (assert (symbolp documentable))
  `(call-with-documentable-bindings ,documentable
                                    (lambda (,documentable)
                                      (declare (ignorable ,documentable))
                                      ,@body)))

;;; Call FN with each thing within DOCUMENTABLE (an argument of the
;;; same name of DOCUMENT). Handle special PROGV forms, which allow
;;; controlling the dynamic environment around DOCUMENT-OBJECT calls.
;;; This is only used by PAX-APROPOS* and is not part of DOCUMENT's
;;; contract.
(defun map-documentable (fn documentable)
  (if (not (listp documentable))
      (funcall fn documentable)
      (with-documentable-bindings (documentable)
        (dolist (element documentable)
          (if (atom element)
              (funcall fn element)
              (map-documentable fn element))))))

;;; If DOCUMENTABLE is a list with a PROGV form as its first element:
;;;
;;;   ((PROGV <symbols-form> <values-form>)
;;;    <reference> <string> ...)
;;;
;;; then establish dynamic variable bindings with PROGV, EVALuating
;;; SYMBOLS-FORM and VALUES-FORM, and call FN with the REST of the
;;; list.
;;;
;;; Else, just call FN with DOCUMENTABLE.
(defun call-with-documentable-bindings (documentable fn)
  (if (and (listp documentable)
           (listp (first documentable))
           (eq (caar documentable) 'progv))
      (destructuring-bind (symbols-form values-form) (rest (first documentable))
        (progv (eval symbols-form) (eval values-form)
          (funcall fn (rest documentable))))
      (funcall fn documentable)))



(defsection @document-function (:title "The DOCUMENT Function")
  (document function)
  (@documentable section)
  (@document-return section)
  (@pages section)
  (@package-and-readtable section))

(defmacro with-format ((format) &body body)
  (with-gensyms (fn)
    `(flet ((,fn ()
              ,@body))
       (call-with-format ,format #',fn))))

;;; NIL, :PLAIN (only possibly with *FORMAT* :MARKDOWN), or :W3M (only
;;; possibly with *FORMAT* :HTML or :MARKDOWN)
(defvar *subformat* nil)

(defun/autoloaded document (documentable &key (stream t) pages (format :plain))
  """Write DOCUMENTABLE in FORMAT to STREAM diverting some output to PAGES.
  FORMAT is one of [:PLAIN][@plain-output],
  [:MARKDOWN][@markdown-output], [:HTML][@html-output] and
  [:PDF][@pdf-output] or [NIL][@dummy-output]. STREAM may be a
  [STREAM][type] object, T or NIL as with [CL:FORMAT][].

  To look up the documentation of the DOCUMENT function itself:

      (document #'document)

  The same with fancy markup:

      (document #'document :format :markdown)

  To document a SECTION:

      (document pax::@pax-manual)

  To generate the documentation for separate libraries with automatic
  cross-links:

      (document (list pax::@pax-manual dref::@dref-manual) :format :markdown)

  See @DOCUMENTATION-UTILITIES for more.

  Definitions that do not define a first-class object are supported
  via [DRef][dref::@dref-manual]:

      (document (dref:locate 'foo 'type))

  There are quite a few special variables that affect how output is
  generated, see @CODIFICATION, @LINKING-TO-THE-HYPERSPEC,
  @LINKING-TO-SECTIONS, @LINK-FORMAT and @OUTPUT-FORMATS.

  For the details, see the following sections, starting with
  @DOCUMENTABLE. Also see @EXTENSION-API and DOCUMENT-OBJECT*."""
  ;; Autoloading mgl-pax/transcribe on demand would be enough for most
  ;; situations, but when documenting PAX itself, it would cause the
  ;; documentables to change from the 1st pass to the 2nd.
  (ensure-transcribe-loaded)
  (with-sections-cache ()
    (with-definitions-cached
      (dref::with-cover-dtype-cache
        (with-format (format)
          (let* ((*print-right-margin* (or *print-right-margin* 80))
                 (3bmd-grammar:*smart-quotes* nil)
                 (3bmd-math:*math* t)
                 (3bmd-code-blocks:*code-blocks* t)
                 (3bmd-code-blocks:*code-blocks-default-colorize*
                   (and (not (eq *subformat* :w3m))
                        :common-lisp))
                 (3bmd-code-blocks::*colorize-name-map*
                   (if (eq *subformat* :w3m)
                       (make-hash-table)
                       3bmd-code-blocks::*colorize-name-map*)))
            (document-return stream (%document documentable stream pages))))))))

(defun call-with-format (format fn)
  (case format
    (:plain
     ;; 3BMD's :PLAIN is very broken. Take matters into our hands, and
     ;; make :PLAIN equivalent to :MARKDOWN without all the bells and
     ;; whistles.
     (note @plain-format
       "@PLAIN-STRIP-MARKUP

       - No link anchors are emitted.

       - No [section numbering][*document-max-numbering-level*].

       - No [table of contents][*document-max-table-of-contents-level*]."
       (let ((*format* :markdown)
             (*subformat* :plain)
             (*document-mark-up-signatures* nil)
             (*document-max-numbering-level* 0)
             (*document-max-table-of-contents-level* 0)
             (*document-text-navigation* nil))
         (handler-bind ((unresolvable-reflink #'output-label))
           (funcall fn)))))
    (:pdf
     (let ((*format* format)
           (*subformat* nil)
           (*document-pandoc-pdf-metadata-block*
             (concatenate
              'string
              (cond ((plusp *document-max-numbering-level*)
                     (format nil "numbersections: true~%secnumdepth: ~A~%"
                             *document-max-numbering-level*))
                    (t
                     (format nil "numbersections: false~%")))
              (cond ((plusp *document-max-table-of-contents-level*)
                     (format nil "toc: true~%toc-depth: ~A~%"
                             *document-max-table-of-contents-level*))
                    (t
                     (format nil "toc: false~%")))
              *document-pandoc-pdf-metadata-block*))
           (*document-max-numbering-level* 0)
           (*document-max-table-of-contents-level* 0)
           (*document-text-navigation* nil)
           (*document-url-versions* '(1)))
       (funcall fn)))
    (:w3m
     (let ((*format* :html)
           (*subformat* :w3m)
           (*document-fancy-html-navigation* nil))
       (funcall fn)))
    ;; Testing only
    (:md-w3m
     (let ((*format* :markdown)
           (*subformat* :w3m)
           (*document-fancy-html-navigation* nil))
       (funcall fn)))
    (t
     (let ((*format* format)
           (*subformat* nil))
       (funcall fn)))))


(defsection @pages (:title "PAGES")
  """The PAGES argument of DOCUMENT is to create multi-page documents
  by routing some of the generated output to files, strings or
  streams. PAGES is a list of page specification elements. A page spec
  is a [property list][clhs] with keys :OBJECTS, :OUTPUT,
  :URI-FRAGMENT, :SOURCE-URI-FN, :HEADER-FN and :FOOTER-FN. OBJECTS is
  a list of objects (references are allowed but not required) whose
  documentation is to be sent to :OUTPUT.

  PAGES may look something like this:

  ```
  `((;; The section about SECTIONs and everything below it ...
     :objects (, @sections)
     ;; ... is so boring that it's not worth the disk space, so
     ;; send it to a string.
     :output (nil)
     ;; Explicitly tell other pages not to link to these guys.
     :uri-fragment nil)
    ;; Send the @EXTENSION-API section and everything reachable
    ;; from it ...
    (:objects (, @extension-api)
     ;; ... to build/tmp/pax-extension-api.html.
     :output "build/tmp/pax-extension-api.html"
     ;; However, on the web server html files will be at this
     ;; location relative to some common root, so override the
     ;; default:
     :uri-fragment "doc/dev/pax-extension-api.html"
     ;; Set html page title, stylesheet, charset.
     :header-fn 'write-html-header
     ;; Just close the body.
     :footer-fn 'write-html-footer)
    ;; Catch references that were not reachable from the above. It
    ;; is important for this page spec to be last.
    (:objects (, @pax-manual)
     :output "build/tmp/manual.html"
     ;; Links from the extension api page to the manual page will
     ;; be to ../user/pax-manual#<anchor>, while links going to
     ;; the opposite direction will be to
     ;; ../dev/pax-extension-api.html#<anchor>.
     :uri-fragment "doc/user/pax-manual.html"
     :header-fn 'write-html-header
     :footer-fn 'write-html-footer))
  ```

  Documentation is initially sent to a default stream (the STREAM
  argument of DOCUMENT), but output is redirected if the thing being
  currently documented is the :OBJECT of a PAGE-SPEC.

  - :OUTPUT can be a number things:

      - If it's NIL, then output will be collected in a string.

      - If it's T, then output will be sent to *STANDARD-OUTPUT*.

      - If it's a stream, then output will be sent to that stream.

      - If it's a list whose first element is a string or a pathname, then
        output will be sent to the file denoted by that and the rest of
        the elements of the list are passed on to CL:OPEN. One extra
        keyword argument is :ENSURE-DIRECTORIES-EXIST. If it's true,
        ENSURE-DIRECTORIES-EXIST will be called on the pathname before
        it's opened.

      Note that even if PAGES is specified, STREAM acts as a catch all,
      absorbing the generated documentation for references not claimed by
      any pages.

  - :HEADER-FN, if not NIL, is a function of a single stream argument,
    which is called just before the first write to the page. Since
    :FORMAT :HTML only generates HTML fragments, this makes it
    possible to print arbitrary headers, typically setting the title,
    CSS stylesheet, or charset.

  - :FOOTER-FN is similar to :HEADER-FN, but it's called after the
     last write to the page. For HTML, it typically just closes the
     body.

  - :URI-FRAGMENT is a string such as `"doc/manual.html"` that specifies
    where the page will be deployed on a webserver. It defines how
    links between pages will look. If it's not specified and :OUTPUT
    refers to a file, then it defaults to the name of the file. If
    :URI-FRAGMENT is NIL, then no links will be made to or from that
    page.

  - :SOURCE-URI-FN is a function of a single, [DREF][class] argument.
    If it returns a value other than NIL, then it must be a string
    representing an \URI. This affects *DOCUMENT-MARK-UP-SIGNATURES*
    and *DOCUMENT-FANCY-HTML-NAVIGATION*. Also see
    MAKE-GIT-SOURCE-URI-FN.""")

;;; Convert the PAGES argument of DOCUMENT to PAGE objects.
(defun page-specs-to-pages (documentable stream page-specs)
  (mapcar #'page-spec-to-page
          (ensure-default-page-spec page-specs documentable stream)))

(defun ensure-default-page-spec (page-specs documentable stream)
  (let ((default (find :default page-specs :key #'page-spec-objects)))
    (cond (default
           (let ((others (remove default page-specs))
                 (default (copy-list default)))
             (setf (getf default :objects) documentable)
             (setf (getf default :output) (list stream))
             (append others `(,default))))
          (t
           (append page-specs `((:objects ,documentable
                                 :output (,stream))))))))

(defun page-spec-objects (page-spec)
  (getf page-spec :objects))

(defun page-spec-to-page (page)
  (destructuring-bind (&key objects output header-fn footer-fn
                         (uri-fragment nil uri-fragment-p)
                         source-uri-fn)
      page
    (let ((stream-spec (make-stream-spec-from-page-spec-output output)))
      (make-page
       :boundaries (page-spec-objects-to-definitions objects)
       ;; See FINALIZE-PAGE-OUTPUT.
       :temp-stream-spec (if (and (eq *format* :markdown)
                                  (null *subformat*)
                                  (null header-fn)
                                  (null footer-fn))
                             stream-spec
                             (make-instance 'string-stream-spec))
       :final-stream-spec stream-spec
       :uri-fragment (or uri-fragment
                         (if (and (not uri-fragment-p)
                                  (typep stream-spec 'file-stream-spec))
                             (file-stream-spec-pathname stream-spec)
                             nil))
       :header-fn header-fn
       :footer-fn footer-fn
       :source-uri-fn (if (and (listp source-uri-fn)
                               (eq (first source-uri-fn) :maker))
                          ;; The maker returned by
                          ;; MAKE-GIT-SOURCE-URI-FN is slow as it
                          ;; invokes git. Don't call it if the
                          ;; PAGE-SOURCE-URI-FN won't be used.
                          (when (or (mark-up-signatures-p)
                                    (fancy-navigation-p))
                            (funcall (second source-uri-fn)))
                          source-uri-fn)))))

(defun make-stream-spec-from-page-spec-output (output)
  (destructuring-bind (spec &rest args) (if (and output (listp output))
                                            output
                                            (list output))
    (apply #'make-stream-spec spec
           (if (and (or (stringp spec) (pathnamep spec))
                    (eq *format* :pdf))
               (append args '(:element-type (unsigned-byte 8)))
               args))))

(defun page-spec-objects-to-definitions (objects)
  (loop for object in (ensure-list objects)
        for dref = (and (not (stringp object))
                        (locate object nil))
        when dref
          collect dref))


(defsection @package-and-readtable (:title "Package and Readtable")
  "While generating documentation, symbols may be read (e.g. from
  docstrings) and printed. What values of *PACKAGE* and *READTABLE*
  are used is determined separately for each definition being
  documented.

  - If the values of *PACKAGE* and *READTABLE* in effect at the time
    of definition were captured (e.g. by DEFINE-LOCATIVE-TYPE and
    DEFSECTION), then they are used.

  - Else, if the definition has a @HOME-SECTION (see below), then the
    home section's SECTION-PACKAGE and SECTION-READTABLE are used.

  - Else, if the definition has an argument list, then the package of
    the first argument that's not external in any package is used.

  - Else, if the definition is DREF::@NAMEd by a symbol, then its
    SYMBOL-PACKAGE is used, and *READTABLE* is set to the standard
    readtable `(NAMED-READTABLES:FIND-READTABLE :COMMON-LISP)`.

  - Else, *PACKAGE* is set to the `CL-USER` package and *READTABLE* to
    the standard readtable.

  The values thus determined come into effect after the name itself is
  printed, for printing of the arglist and the docstring.

      CL-USER> (pax:document #'foo)
      - [function] FOO <!> X Y &KEY (ERRORP T)

          Do something with X and Y.

  In the above, the `<!>` marks the place where *PACKAGE* and
  *READTABLE* are bound."
  (@home-section section)
  (*document-normalize-packages* variable))

(defsection @home-section (:title "Home Section")
  "[home-section function][docstring]")

(defun guess-package-and-readtable (requested-package requested-readtable
                                    reference arglist)
  (if (and requested-package requested-readtable)
      (values requested-package requested-readtable)
      (let ((home-section (first (find-parent-sections reference))))
        (if home-section
            (values (or requested-package (section-package home-section))
                    (or requested-readtable (section-readtable home-section)))
            (values (or requested-package
                        (guess-package-from-arglist arglist)
                        (and (symbolp (xref-name reference))
                             (symbol-package (xref-name reference)))
                        (find-package :cl-user))
                    (or requested-readtable
                        named-readtables::*standard-readtable*))))))

;;; Unexported argument names are highly informative about *PACKAGE*
;;; at read time. No one ever uses fully-qualified internal symbols
;;; from another package for arguments, right?
(defun guess-package-from-arglist (arglist)
  (let ((args (or (ignore-errors (dref::function-arg-names arglist))
                  (ignore-errors (dref::macro-arg-names arglist)))))
    (dolist (arg args)
      (when (and (symbolp arg)
                 (not (external-symbol-in-any-package-p arg)))
        (return (symbol-package arg))))))

(defmacro with-dref-doc-package-and-readtable ((dref) &body body)
  (alexandria:once-only (dref)
    `(multiple-value-bind (*package* *readtable*)
         (guess-package-and-readtable (nth-value 1 (docstring ,dref))
                                      *readtable* ,dref (arglist ,dref))
       ,@body)))

(defvar/autoloaded *document-normalize-packages* t
  "Whether to print `[in package <package-name>]` in the documentation
  when the package changes.")


;;;; DOCUMENT-OBJECT

(defgeneric document-object (object stream)
  (:method :around (object stream)
    (declare (ignorable stream))
    (let ((*objects-being-documented* (cons object *objects-being-documented*)))
      (call-next-method)))
  (:method (object stream)
    (document-object (locate object) stream))
  (:method ((string string) stream)
    (cond (*first-pass*
           (setf (page-written-in-first-pass-p *page*) t))
          (t
           (about-to-write-to-page)
           (document-docstring string stream :indentation "" :paragraphp nil)
           (terpri stream))))
  (:method :around ((xref xref) stream)
    (let ((*documenting-dref* xref))
      (call-next-method)))
  ;; LOCATE non-DREF XREFs.
  (:method ((xref xref) stream)
    (let ((warn-if-undefined
            (and *document-open-linking*
                 ;; It is an error for explicit arguments to DOCUMENT
                 ;; to have no definition even when open-linking (so
                 ;; that `mgl-pax-document' produces errors when it
                 ;; must), but we want to document what we can even if
                 ;; a section contains undefined stuff.
                 (boundp '*section*))))
      (if warn-if-undefined
          (multiple-value-bind (dref error)
              (handler-case
                  (locate xref)
                (locate-error (e) (values nil e)))
            (if dref
                (document-object dref stream)
                (when *first-pass*
                  (warn "~@<Not documenting ~S: ~A~:@>" xref error))))
          (document-object (locate xref) stream))))
  (:method ((dref dref) stream)
    (handler-bind
        (((and warning (not title-parsing-failure))
           (lambda (warning)
             (when (sanitize-aggressively-p)
               (muffle-warning warning)))))
      (let ((page (boundary-page dref)))
        (if *first-pass*
            (let ((*page* (or page *page*)))
              (push dref (page-definitions *page*))
              (setf (page-written-in-first-pass-p *page*) t)
              (document-object* (or (resolve dref nil) dref) stream))
            (with-temp-output-to-page (stream page)
              (about-to-write-to-page)
              (document-object* (or (resolve dref nil) dref) stream)))))))


(defun finalize-page-output (page)
  (when (and (page-written-in-first-pass-p page)
             ;; With @DUMMY-OUTPUT, discard anything that might be written.
             *format*)
    ;; Now that Markdown output for this PAGE is complete, we may
    ;; want to convert it to the requested *FORMAT*.
    (if (and (eq *format* :markdown)
             (null *subformat*)
             (null (page-header-fn page))
             (null (page-footer-fn page)))
        (with-temp-output-to-page (stream page)
          (write-markdown-reference-style-link-definitions stream))
        (let ((markdown-string
                (if (typep (page-temp-stream-spec page) 'string-stream-spec)
                    (string-stream-spec-string (page-temp-stream-spec page))
                    (with-temp-input-from-page (stream page)
                      (read-stream-content-into-string stream))))
              (markdown-reflinks
                (with-output-to-string (stream)
                  (unless (eq *subformat* :plain)
                    (let ((*page* page))
                      (write-markdown-reference-style-link-definitions stream))))))
          (delete-stream-spec (page-temp-stream-spec page))
          (with-final-output-to-page (stream page)
            (when (page-header-fn page)
              (funcall (page-header-fn page) stream))
            (cond ((and (eq *format* :markdown)
                        (null *subformat*))
                   (write-string markdown-string stream)
                   (write-string markdown-reflinks stream))
                  (t
                   (reprint-in-format markdown-string markdown-reflinks
                                      stream)))
            (when (page-footer-fn page)
              (funcall (page-footer-fn page) stream)))))
    (unmake-stream-spec (page-final-stream-spec page))))

;;; Emit Markdown definitions for links that were linked to on the
;;; current page.
(defun write-markdown-reference-style-link-definitions (stream)
  (let ((used-links (sort (hash-table-keys (page-used-links *page*))
                          #'string< :key #'link-id))
        (*package* (find-package :keyword)))
    (when used-links
      (format stream "~%")
      (dolist (link used-links)
        (assert (not (link-p (link-page link))))
        ;; The format is [label]: url "title"
        ;; E.g.  [1]: http://example.org/Hobbit#Lifestyle "Hobbit lifestyles"
        (format stream "  [~A]: ~A ~S~%"
                (link-id link)
                (if (stringp (link-page link))
                    ;; Link to external page.
                    (link-page link)
                    ;; Link to documentation generated in the same run.
                    (link-to-uri link))
                (reflink-definition-title link))))))

(defun reflink-definition-title (link)
  (let ((dref (link-definition link)))
    (or (document-definition-title dref)
        (dref-to-anchor dref))))

;;; Process MARKDOWN-STRING block by block to limit maximum memory usage.
(defun reprint-in-format (markdown-string markdown-reflinks stream)
  (if (eq *format* :pdf)
      (print-pandoc-pdf (prepare-parse-tree-for-printing-to-pandoc-pdf
                         (parse-markdown (concatenate 'string markdown-string
                                                      markdown-reflinks)))
                        stream)
      (note @markdown-reflink-definitions
        "- When @BROWSING-LIVE-DOCUMENTATION, the page displayed can be of,
  say, a single function within what would constitute the offline
  documentation of a library. Because Markdown reference link
  definitions, for example

          [Daring Fireball]: http://daringfireball.net/

      can be defined anywhere, they wouldn't be resolvable in that
      case, their use is discouraged. Currently, only reflink
      definitions within the documentation of the same
      DREF::@DEFINITION are guaranteed to be resolvable. This is left
      intentionally vague because the specifics are subject to change.

      See DEFINE-GLOSSARY-TERM for a better alternative to Markdown
      reference links."
        (multiple-value-bind (3bmd::*md-default-block-chars-to-escape*
                              3bmd::*md-default-inline-chars-to-escape*
                              3bmd::*extension-to-md-block-chars-to-escape*
                              3bmd::*extension-to-md-inline-chars-to-escape*)
            (if (eq *subformat* :plain)
                ;; Turn off all escaping.
                (values "" "" (make-hash-table) (make-hash-table))
                (values 3bmd::*md-default-block-chars-to-escape*
                        3bmd::*md-default-inline-chars-to-escape*
                        3bmd::*extension-to-md-block-chars-to-escape*
                        3bmd::*extension-to-md-inline-chars-to-escape*))
          (print-markdown (append (prepare-parse-tree-for-printing
                                   (parse-markdown markdown-string))
                                  (parse-markdown markdown-reflinks))
                          stream :format *format*)))))

(defun reflink-defs (tree)
  (remove-if-not (lambda (tree)
                   (parse-tree-p tree :reference))
                 tree))

(defun prepare-parse-tree-for-printing (parse-tree)
  (cond ((eq *subformat* :w3m)
         (prepare-parse-tree-for-printing-to-w3m parse-tree))
        ((eq *subformat* :plain)
         (prepare-parse-tree-for-printing-to-plain parse-tree))
        (t parse-tree)))


(defsection @document-return (:title "Return Values")
  "If PAGES are NIL, then DOCUMENT - like CL:FORMAT - returns a
  string (when STREAM is NIL) else NIL.

  If PAGES, then a list of output designators are returned, one for
  each non-empty page (to which some output has been written), which
  are determined as follows.

  - The string itself if the output was to a string.

  - The stream if the output was to a stream.

  - The pathname of the file if the output was to a file.

  If the default page given by the STREAM argument of DOCUMENT was
  written to, then its output designator is the first element of the
  returned list. The rest of the designators correspond to the
  non-empty pages in the PAGES argument of DOCUMENT in that order.")

(defun document-return (stream outputs)
  (let ((default-page-output (last-elt outputs))
        (page-outputs (remove nil (butlast outputs))))
    (cond (page-outputs
           (if default-page-output
               (cons default-page-output page-outputs)
               page-outputs))
          ((null stream)
           default-page-output)
          (t
           nil))))


;;;; URIs of stuff

(defun object-to-uri (object)
  (when-let (dref (locate object))
    (when-let (link (find-link dref))
      (link-to-uri link))))

;;; With w3m, the URLs are like "pax:clhs", but with MGL-PAX/WEB, they
;;; are like "http://localhost:8888/pax:clhs", so we need an extra /.
(defun finalize-pax-url (url)
  (if (eq *subformat* :w3m)
      url
      (format nil "/~A" url)))

(defun link-to-uri (link)
  (let ((target-page (link-page link)))
    (if (null target-page)
        (finalize-pax-url (dref-to-pax-url (link-definition link)))
        (let ((target-page-definitions (page-definitions target-page))
              (target-page-uri-fragment (page-uri-fragment target-page)))
          ;; Don't generate anchors when linking to the first
          ;; definition on the page.
          (if (and (xref= (link-definition link)
                          (first target-page-definitions))
                   target-page-uri-fragment)
              (if (eq target-page *page*)
                  ;; "xxx.html"
                  (format nil "~A.~A" (pathname-name target-page-uri-fragment)
                          (pathname-type target-page-uri-fragment))
                  ;; "../xxx.html"
                  (relative-page-uri-fragment target-page *page*))
              (format nil "~A#~A"
                      (if (eq target-page *page*)
                          ""
                          (relative-page-uri-fragment target-page *page*))
                      (anchor-id (link-definition link))))))))

(defun relative-page-uri-fragment (page definition-page)
  (let ((fragment (page-uri-fragment page))
        (reference-fragment (page-uri-fragment definition-page)))
    (assert (and fragment reference-fragment))
    (relativize-pathname fragment reference-fragment)))


(defsection @markdown-support (:title "Markdown Support")
  "@MARKDOWN in docstrings and titles is processed with the @3BMD library."
  (@markdown-in-docstrings section)
  (@markdown-in-titles section)
  (@markdown-syntax-highlighting section)
  (@mathjax section))

(defsection @markdown-in-docstrings (:title "Markdown in Docstrings")
  """[ strip-docstring-indent function][docstring]

  @MARKDOWN-REFLINK-DEFINITIONS

  [ sanitize-aggressively-p function][docstring]

  - [ round-up-indentation function][docstring]
  - [ escape-html-in-docstring function][docstring]
  - [ escape-heading-in-docstring function][docstring]""")

(defun sanitize-aggressively-p ()
  "Docstrings of definitions which do not have a @HOME-SECTION and are
  not PAX constructs themselves (e.g SECTION, GLOSSARY-TERM, NOTE) are
  assumed to have been written with no knowledge of PAX and to conform
  to Markdown only by accident. These docstrings are thus sanitized
  more aggressively."
  (and (not (boundp '*section*))
       ;; This is implicit in the above, but docstrings passed
       ;; directly to DOCUMENT are not treated aggressively.
       *documenting-dref*
       (not (typep *documenting-dref* '(or glossary-term-dref note-dref)))
       (null (home-section *documenting-dref*))))

(defvar *document-docstring-key* nil)

(defun/autoloaded document-docstring
    (docstring stream &key (indentation "    ")
               exclude-first-line-p (paragraphp t))
  "Write DOCSTRING to STREAM, [sanitizing the Markdown]
  [@markdown-in-docstrings] from it, performing @CODIFICATION and
  @LINKING, finally prefixing each line with INDENTATION. The prefix
  is not added to the first line if EXCLUDE-FIRST-LINE-P. If
  PARAGRAPHP, then add a newline before and after the output."
  (when (and docstring
             (not (equal docstring ""))
             ;; If the output is going to /dev/null, then skip this
             ;; operation because it's costly.
             (not *first-pass*))
    (let ((docstring (funcall (or *document-docstring-key* #'identity)
                              docstring)))
      (when docstring
        (let* ((docstring (sanitize-docstring
                           docstring :aggressivep (sanitize-aggressively-p)))
               (reindented (prefix-lines
                            indentation (codify-and-link docstring)
                            :exclude-first-line-p exclude-first-line-p)))
          (when (plusp (length docstring))
            (if paragraphp
                (format stream "~%~A~&" reindented)
                (format stream "~A" reindented))))))))


(defsection @markdown-in-titles (:title "Markdown in Titles")
  (@title glossary-term)
  "Titles undergo @CODIFICATION and may be a single paragraph
  containing explicit @MARKDOWN/INLINE-CODE, @MARKDOWN/EMPHASIS,
  @MARKDOWN/IMAGEs, inline @MATHJAX and HTML entities (e.g. `&quot;`).
  Other kinds of Markdown markup and block elements are not allowed."
  (doctitle function))

(define-glossary-term @markdown/emphasis
    (:title "Markdown emphasis"
     :url "https://daringfireball.net/projects/markdown/syntax#em"))

(define-glossary-term @markdown/image
    (:title "Markdown image"
     :url "https://daringfireball.net/projects/markdown/syntax#em"))

(define-glossary-term @title (:title "title")
  "A title is a STRING associated with a DREF::@DEFINITION (e.g. with
  the TITLE argument of DEFSECTION or DEFINE-GLOSSARY-TERM). Titles
  are accessible DOCTITLE and processed according to
  @MARKDOWN-IN-TITLES.")

(defun/autoloaded doctitle (object)
  "Return the @TITLE of OBJECT if it has one or NIL. For
  @CODIFICATION, the title is interpreted in the package returned by
  DOCSTRING. DOCTITLE can be extended via DOCTITLE*."
  (dref::nth-value-or-with-obj-or-def (object 0)
    (doctitle* object)))

(defmethod doctitle* ((section section))
  (section-title section))

(defmethod doctitle* ((glossary-term glossary-term))
  (glossary-term-title glossary-term))

;;; *PACKAGE* and *READTABLE* is assumed to be set up.
(defun document-title (string &key deemph (format :markdown) dref)
  (let ((tree (codify (parse-markdown string) :leave-autolink-escape nil)))
    (setq tree (check-title-parse-tree tree string :deemph deemph :dref dref))
    (if format
        (with-output-to-string (out)
          (print-markdown tree out :format format))
        tree)))

;;; Sets up *PACKAGE* and *READTABLE*.
(defun document-definition-title (dref &key deemph (format :markdown))
  (when-let (title (doctitle dref))
    (with-dref-doc-package-and-readtable (dref)
      (document-title title :deemph deemph :format format :dref dref))))

(defun check-title-parse-tree (tree title &key deemph dref)
  (transform-tree
   (lambda (parent tree)
     (declare (ignore parent))
     (cond ((atom tree)
            tree)
           ((and deemph
                 (member (first tree) '(:emph :strong)))
            (values (rest tree) t t))
           ((or (listp (first tree))
                (member (first tree) '(:plain :emph :strong :code :entity
                                       :image :explicit-link :math-inline-1
                                       :math-inline-2 :math-inline-3)))
            (values tree t))
           (t
            (warn 'title-parsing-failure :tag (first tree) :title title)
            (return-from check-title-parse-tree
              (parse-markdown (prin1-to-string* (dref-name dref)))))))
   tree))

(define-condition title-parsing-failure (warning)
  ((tag :initarg :tag :reader title-parsing-failure-tag)
   (title :initarg :title :reader title-parsing-failure-title))
  (:report print-title-parsing-failure))

(defun print-title-parsing-failure (title-parsing-failure stream)
  (let* ((c title-parsing-failure)
         (tag (title-parsing-failure-tag c))
         (title (title-parsing-failure-title c)))
    (format stream
            "~@<Unexpected tag ~S in the parse tree of title ~S. ~
            This means that the title does not follow the rules in ~S~:@>"
            tag title '@markdown-in-titles)
    (print-document-context stream)))


(defsection @markdown-syntax-highlighting (:title "Syntax Highlighting")
  "For syntax highlighting, GitHub's @FENCED-CODE-BLOCKS Markdown
  extension to mark up code blocks with triple backticks is enabled so
  all you need to do is write:

      ```elisp
      (defun foo ())
      ```

  to get syntactically marked up HTML output. Copy `src/style.css`
  from PAX and you are set. The language tag, `elisp` in this example,
  is optional and defaults to `common-lisp`.

  See the documentation of @3BMD and @COLORIZE for the details.")

(define-glossary-term @3bmd (:title "3BMD" :url "https://github.com/3b/3bmd"))

(define-glossary-term @colorize
    (:title "Colorize" :url "https://github.com/redline6561/colorize/"))

(define-glossary-term @fenced-code-blocks
    (:title "fenced code blocks"
     :url "https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks"))

(defsection @mathjax (:title "MathJax")
  """Displaying pretty mathematics between in TeX format is
  supported via MathJax.

  - _Inline_

      It can be done inline (within a paragraph):

          Pretty, eh? $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$ Yes.

      which is displayed as

      Pretty, eh? $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$ Yes.

      To avoid rendering `between $5 and $6` with inline math, both
      the opening and the closing `$` character must be followed /
      preceded by a non-space character. This agrees with Pandoc.

      Alternatively, the ``$`x_0`$`` syntax may be used (renders as
      $`x_0`$), which has no restriction on spacing.

  - _Block_

      The `$$` is supported as a block element:

          Pretty, eh?

          $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

          Yes.

      which will be rendered in its own paragraph:

      Pretty, eh?

      $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

      Yes.

      `$$` is also legal to use inline, but it's not recommended as it
      gets rendered inline on GitHub but as display math in
      @PDF-OUTPUT.

  MathJax will leave inline code (e.g. those between single backticks)
  and code blocks (triple backtricks) alone. Outside code, use
  `<span>$</span>` to scare MathJax off.

  Escaping all those backslashes in TeX fragments embedded in Lisp
  strings can be a pain. @PYTHONIC-STRING-READER can help with that.""")

(define-glossary-term @pythonic-string-reader
    (:title "Pythonic String Reader"
     :url "https://github.com/smithzvk/pythonic-string-reader"))


;;;; Automatic markup of symbols

;;; Take a string in Markdown format. Handle the DOCSTRING locative,
;;; markup symbols as code (if *DOCUMENT-UPPERCASE-IS-CODE*), autolink
;;; (if *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-LINK-CODE*) and always
;;; handle explicit links with locatives (e.g. [FOO][function]).
;;; Finally handle *DOCUMENT-BASE-URL* and return the transformed
;;; string.
(defun codify-and-link (string)
  (with-output-to-string (s)
    (print-markdown (codify-and-link-tree (parse-markdown string)) s)))

(defun codify-and-link-tree (tree)
  (let ((tree (include-docstrings tree)))
    (cond (*format*
           (add-base-url (link (codify tree))))
          (t
           ;; @DUMMY-OUTPUT
           (map-markdown-parse-tree '(3bmd-code-blocks::code-block)
                                    '(:code :verbatim :image :mailto :reference
                                      :raw-html)
                                    nil #'translate-code-block tree)
           ()))))


;;;; Including docstrings

(defun include-docstrings (parse-tree)
  (map-markdown-parse-tree (list :reference-link) () nil
                           #'translate-docstring-links parse-tree))

;;; This is the first of the translator functions, which are those
;;; passed to MAP-MARKDOWN-PARSE-TREE. See TRANSFORM-TREE for the
;;; semantics the return values.
(defun translate-docstring-links (parent tree)
  """DOCSTRING is a PSEUDO locative for including the parse tree of
  the Markdown [DOCSTRING][function] of a definition in the parse tree
  of a docstring when generating documentation. It has no source
  location information and only works as an explicit link. This
  construct is intended to allow docstrings to live closer to their
  implementation, which typically involves a non-exported definition.

  ```cl-transcript (:dynenv pax-std-env)
  (defun div2 (x)
    "X must be [even* type][docstring]."
    (/ x 2))

  (deftype even* ()
    "an even integer"
    '(satisfies evenp))

  (document #'div2)
  .. - [function] DIV2 X
  ..
  ..     X must be an even integer.
  ..
  ```"""
  (declare (ignore parent))
  (assert (parse-tree-p tree :reference-link))
  (let ((label (pt-get tree :label))
        (definition (parse-tree-to-text (pt-get tree :definition) :deemph t)))
    (nth-value-or 0
      (when (eq (parse-locative definition) 'docstring)
        (let ((label-string (parse-tree-to-text label :deemph t)))
          (nth-value-or 0
            (if-let (dref (parse-dref label-string))
              (translate-dref-docstring dref)
              (warn "~@<Including ~S failed because ~S cannot be ~Sd.~:@>"
                    'docstring label-string 'locate))
            (values '("") t t))))
      tree)))

(defun translate-dref-docstring (dref)
  (multiple-value-bind (docstring package) (docstring dref)
    (cond (docstring
           (values (or (let ((*package* (or package *package*)))
                         (parse-markdown (sanitize-docstring docstring)))
                       '(""))
                   ;; Detecting circular includes would be hard
                   ;; because the `recurse' return value is handled in
                   ;; the caller of this function.
                   t t))
          (t
           (warn "~@<Including the ~S of ~S failed because it is NIL.~:@>"
                 'docstring dref)))))


(defsection @codification (:title "Codification")
  (*document-uppercase-is-code* variable)
  (@codifiable glossary-term)
  (@interesting glossary-term)
  (*document-downcase-uppercase-code* variable))

(defvar/autoloaded *document-uppercase-is-code* t
  """When true, @INTERESTING @NAMEs extracted from @CODIFIABLE @WORDs
  marked up as code with backticks. For example, this docstring

      "T PRINT CLASSes SECTION *PACKAGE* MGL-PAX ASDF
      CaMeL Capital"

  is equivalent to this:

      "`T` `PRINT` `CLASS`es `SECTION` `*PACKAGE*` `MGL-PAX` `ASDF`
      CaMel Capital"

  and renders as

  `T` `PRINT` `CLASS`es `SECTION` `MGL-PAX` `ASDF` CaMel Capital

  where the links are added due to *DOCUMENT-LINK-CODE*.

  [handle-codification-escapes function][docstring]""")

(define-glossary-term @codifiable (:title "codifiable")
  "A @WORD is _codifiable_ if

  - it has a single uppercase character (e.g. it's `T`) and no
    lowercase characters at all, or

  - there is more than one uppercase character and no lowercase
    characters between them (e.g. `\\CLASSes`, `\\nonREADable`,
    `\\CLASS-NAMEs` but not `\\Classes` or `\\aTe`.")

(defun codifiable-word-p (string)
  (and
   ;; Check that it's a @WORD too.
   (notany #'whitespacep string)
   (uppercase-core-bounds string)))

(define-glossary-term @interesting (:title "interesting")
  "A @NAME is _interesting_ if

  - it names a symbol external to its package, or

  - it is at least 3 characters long and names an interned symbol, or

  - it names a @LOCAL-DEFINITION.

  See @PACKAGE-AND-READTABLE.")

(defun interesting-name-p (raw name)
  (or (and (symbolp name)
           (or (<= 3 (length raw))
               (external-symbol-p name)))
      (has-local-reference-p name)))

;;; The core of the implementation of *DOCUMENT-UPPERCASE-IS-CODE*.
;;;
;;; This is called by MAP-WORDS so the return values are NEW-TREE,
;;; SLICE. Also called by TRANSLATE-EMPH that expects only a single
;;; return value, the new tree.
(defun translate-uppercase-word (parent tree word)
  (declare (ignore parent))
  (let ((emph (and (listp tree) (eq :emph (first tree))))
        (codifiablep (codifiable-word-p word)))
    (nth-value-or 0
      (handle-codification-escapes emph codifiablep word)
      (cond ((or (not *document-uppercase-is-code*)
                 (not codifiablep))
             ;; Don't change anything.
             nil)
            (emph
             (codify-uppercase-word (format nil "*~A*" word)))
            (t
             (codify-uppercase-word word))))))

(defun handle-codification-escapes (emph codifiablep word)
  """To suppress codification, add a backslash to the beginning of the
  a @CODIFIABLE word or right after the leading `\\*` if it would
  otherwise be parsed as Markdown emphasis:

      "\\SECTION *\\PACKAGE*"

  The number of backslashes is doubled above because that's how the
  example looks in a docstring. Note that the backslash is discarded
  even if *DOCUMENT-UPPERCASE-IS-CODE* is false."""
  (cond ((and emph codifiablep (eql #\\ (first-elt word)))
         ;; E.g. "*\\DOCUMENT-NORMALIZE-PACKAGES*"
         ;; -> (:EMPH "DOCUMENT-NORMALIZE-PACKAGES")
         (values (list `(:emph ,(subseq word 1))) t))
        ((and codifiablep (eql #\\ (first-elt word)))
         ;; Discard the leading backslash escape.
         ;; E.g. "\\MGL-PAX" -> "MGL-PAX"
         (values (list (subseq word 1)) t))))

;;; Find the [approximately] longest @NAME in WORD. Return a 3BMD
;;; parse tree fragment with that substring marked up as code and the
;;; suffixes downcased (so that CLASSES turns into `CLASS`es).
;;;
;;; Handles the rules laid out in *DOCUMENT-UPPERCASE-IS-CODE* not
;;; already handled in the caller TRANSLATE-UPPERCASE-WORD. Trims
;;; separators and depluralizes.
(defun codify-uppercase-word (word)
  (when-let (match (parse-uppercase-word word))
    (destructuring-bind (raw name) match
      (when (and raw (interesting-name-p raw name))
        (let ((pos (search raw word :test #'char-equal)))
          (assert pos)
          (values `(,@(when (plusp pos)
                        `(,(subseq word 0 pos)))
                    (:code ,(maybe-downcase raw))
                    ,@(let ((tail-pos (+ pos (length raw))))
                        (when (< tail-pos (length word))
                          ;; CLASSES -> `CLASS`es
                          `(,(string-downcase (subseq word tail-pos))))))
                  t))))))

(defun parse-uppercase-word (word)
  (flet ((match (raw name)
           (when (and (notany #'lower-case-p raw)
                      (interesting-name-p raw name))
             (list raw name))))
    (find-name #'match word :pass-raw t :trim t :depluralize t)))

(defvar *leave-autolink-escape* nil)

;;; Handle *DOCUMENT-UPPERCASE-IS-CODE* in normal strings and :EMPH
;;; (to recognize *VAR*). Also, perform consistency checking of
;;; cl-transcript code blocks (see @TRANSCRIBING-WITH-EMACS).
(defun codify (parse-tree &key (leave-autolink-escape t))
  (let ((*leave-autolink-escape* leave-autolink-escape))
    (map-markdown-parse-tree
     (list :emph '3bmd-code-blocks::code-block :reference-link :explicit-link
           :code)
     '(:code :verbatim 3bmd-code-blocks::code-block
       :image :mailto :reference :raw-html
       :math-inline-1 :math-inline-2 :math-inline-3)
     t
     #'translate-to-code
     parse-tree)))

(defun translate-to-code (parent tree)
  (cond ((stringp tree)
         (let ((string tree))
           (values (map-words string
                              (lambda (string start end)
                                (let ((word (subseq string start end)))
                                  (translate-uppercase-word
                                   parent string word))))
                   ;; Don't recurse, do slice
                   nil t)))
        ((parse-tree-p tree :emph)
         (translate-emph parent tree))
        ((parse-tree-p tree '3bmd-code-blocks::code-block)
         (translate-code-block parent tree))
        ((or (parse-tree-p tree :reference-link)
             (parse-tree-p tree :explicit-link))
         (let ((replacement (copy-list tree)))
           (setf (pt-get replacement :label)
                 (codify (pt-get tree :label) :leave-autolink-escape nil))
           replacement))
        ((parse-tree-p tree :code)
         `(:code ,(maybe-downcase (second tree))))
        (t
         (error "~@<Unexpected tree type ~S.~:@>" (first tree)))))

;;; CODE-BLOCK looks like this:
;;;
;;;     (3BMD-CODE-BLOCKS::CODE-BLOCK :LANG "commonlisp" :CONTENT "42")
(defun translate-code-block (parent code-block)
  (declare (ignore parent))
  (let ((lang (getf (rest code-block) :lang)))
    (if (starts-with-subseq "cl-transcript" lang)
        (let* ((suffix (subseq lang (length "cl-transcript")))
               (args (read-from-string suffix nil nil))
               (original (getf (rest code-block) :content)))
          ;; The possibly manually tweaked (e.g. comments in output)
          ;; original goes to the output, but retranscribe it just for
          ;; consistency checking.
          (transcribe-code-block original args)
          `(3bmd-code-blocks::code-block :lang "common-lisp"
                                         :content ,original))
        code-block)))

;;; FIXME: Export this? One can also CONTINUE on
;;; TRANSCRIPTION-CONSISTENCY-ERROR.
(defvar *document-transcribe-check-consistency* t)

(defun transcribe-code-block (transcript args)
  (let ((dynenv (getf args :dynenv))
        (*transcribe-check-consistency*
          *document-transcribe-check-consistency*))
    (remf args :dynenv)
    (flet ((call-it ()
             (if *document-open-linking*
                 (handler-case
                     (apply #'transcribe transcript nil :update-only t args)
                   (transcription-error (e)
                     (warn "~A" e)
                     transcript))
                 (apply #'transcribe transcript nil :update-only t args))))
      (cond ((and dynenv (ignore-errors (fdefinition dynenv)))
             (funcall dynenv #'call-it))
            (t
             (when dynenv
               (funcall (if *document-open-linking* 'warn 'error)
                        "Undefined :DYNENV function ~S" dynenv))
             (call-it))))))

;;; Undo the :EMPH parsing for code references. E.g. (:EMPH "XXX") ->
;;; "*XXX*" if "*XXX*" is to be codified according to
;;; CODIFY-UPPERCASE-WORD-P.
(defun translate-emph (parent tree)
  (if (and (= 2 (length tree))
           (stringp (second tree)))
      (let ((translation (translate-uppercase-word parent tree (second tree))))
        (if translation
            ;; Replace TREE with TRANSLATION, don't process
            ;; TRANSLATION again recursively, slice the return value
            ;; into the list of children of PARENT.
            (values translation nil t)
            ;; leave it alone, don't recurse, don't slice
            (values tree nil nil)))
      ;; Tell MAP-MARKDOWN-PARSE-TREE to leave TREE unchanged,
      ;; recurse, don't slice.
      (values tree t nil)))


(defvar/autoloaded *document-downcase-uppercase-code* nil
  """If true, then all @MARKDOWN/INLINE-CODE (e.g. \`code\`, _which
  renders as_ `\code`) – including @CODIFICATION – which has no
  lowercase characters is downcased in the output. Characters of
  literal strings in the code may be of any case. If this variable is
  :ONLY-IN-MARKUP and the output format does not support markup (e.g.
  it's :PLAIN), then no downcasing is performed. For example,

      `(PRINT "Hello")`

  is downcased to

      `(print "Hello")`

  because it only contains uppercase characters outside the string.
  However,

      `MiXed "RESULTS"`

  is not altered because it has lowercase characters.

  If the first two characters are backslashes, then no downcasing is
  performed, in addition to @ESCAPING-AUTOLINKING. Use this to mark
  inline code that's not Lisp.

      Press `\\M-.` in Emacs.""")

(define-glossary-term @markdown/inline-code
    (:title "Markdown inline code"
     :url "https://daringfireball.net/projects/markdown/syntax#code"))

(defun/autoloaded downcasingp ()
  (or (and *document-downcase-uppercase-code*
           (not (eq *document-downcase-uppercase-code*
                    :only-in-markup)))
      (and (eq *document-downcase-uppercase-code*
               :only-in-markup)
           (not (eq *subformat* :plain)))))

(defun prin1-to-string* (object)
  (let ((*print-case* (if (downcasingp)
                          :downcase
                          :upcase)))
    (prin1-to-string object)))

(defun/autoloaded prin1-to-markdown
    (object &key (escape-inline t) (escape-mathjax t) (escape-html t)
            (escape-block t))
  "Like PRIN1-TO-STRING, but bind *PRINT-CASE* depending on
  *DOCUMENT-DOWNCASE-UPPERCASE-CODE* and *FORMAT*, and
  ESCAPE-MARKDOWN."
  (escape-markdown (prin1-to-string* object)
                   :escape-inline escape-inline :escape-mathjax escape-mathjax
                   :escape-html escape-html :escape-block escape-block))

(defun maybe-downcase (string)
  (if *leave-autolink-escape*
      (cond ((starts-with-subseq "\\\\" string)
             ;; Leave one backslash to escape autolinking in
             ;; TRANSLATE-TO-LINKS.
             (subseq string 1))
            ((downcasingp)
             (downcase-all-uppercase-code string))
            (t
             string))
      (cond ((starts-with-subseq "\\\\" string)
             (subseq string 2))
            ((starts-with-subseq "\\" string)
             (if (downcasingp)
                 (downcase-all-uppercase-code (subseq string 1))
                 (subseq string 1)))
            (t
             (if (downcasingp)
                 (downcase-all-uppercase-code string)
                 string)))))

(defun maybe-downcase-all-uppercase-code (string)
  (if (downcasingp)
      (downcase-all-uppercase-code string)
      string))

(defun downcase-all-uppercase-code (string)
  (with-output-to-string (s)
    (map-code-chars (lambda (char escaped in-string)
                      (when (and (not in-string)
                                 (lower-case-p char))
                        (return-from downcase-all-uppercase-code string))
                      (when escaped
                        (write-char #\\ s))
                      (if in-string
                          (write-char char s)
                          (write-char (char-downcase char) s)))
                    string)))

(defun map-code-chars (fn string)
  (let ((in-string nil)
        (escaped nil))
    (loop for char across string
          do (cond (escaped
                    (funcall fn char t in-string)
                    (setq escaped nil))
                   ((eq char #\\)
                    (setq escaped t))
                   ((eq char #\")
                    (funcall fn char nil in-string)
                    (setq in-string (not in-string)))
                   (t
                    (funcall fn char nil in-string))))))


(defsection @linking (:title "Linking")
  """PAX supports linking to DREF::@DEFINITIONS either with
  explicit @REFLINKs or with @AUTOLINKs.

  When generating offline documentation, only the definitions in
  @DOCUMENTABLE may be @LINKABLE, but when
  @BROWSING-LIVE-DOCUMENTATION, everything is linkable as
  documentation is generated on-demand.

  Many examples in this section link to standard Common Lisp
  definitions. In the offline case, these will link to [external
  \URLs][*DOCUMENT-HYPERSPEC-ROOT*], while in the live case to
  disambiguation pages that list the definition in the running Lisp
  and in the HyperSpec.

  _Invoking [`\\M-.`][@navigating-in-emacs section] on WORD or NAME in
  any of the following examples will disambiguate based on the textual
  context, determining the locative._ This is because navigation and
  linking use the same @PARSING algorithm, although linking is a bit
  more strict about trimming, depluralization, and it performs
  @FILTERING-LINKS. On the other hand, `\\M-.` cannot visit the
  [CLHS][locative] references because there are no associated source
  locations."""
  (@stable-printed-locative glossary-term)
  (@reflink section)
  (@autolink section)
  (@linking-to-the-hyperspec section)
  (@linking-to-sections section)
  (@filtering-links section)
  (@link-format section))

(define-glossary-term @stable-printed-locative
    (:title "stable printed locative")
  "The @LINK-FORMAT relies on DREF::@DEFINITIONS having a unique
  textual representation that doesn't change. More concretely, if
  PRIN1 under WITH-STANDARD-IO-SYNTAX but with *PRINT-READABLY* NIL
  produces the same unique string deterministically, then linking to
  DREF::@DEFINITIONS works even with non-readable locatives. The
  uniqueness condition requires that if two definitions are different
  under XREF=, then their textual representations are also different.

  On the other hand, for example, a method involving an EQL
  specializer with an object printed with PRINT-UNREADABLE-OBJECT
  :IDENTITY T does not produce a stable string and links will break.")


(defsection @filtering-links (:title "Filtering Links")
  (*document-link-code* variable)
  (@linkable glossary-term)
  (@specific-link section)
  (@unspecific-link section))

(defvar/autoloaded *document-link-code* t
  """Whether definitions of things other than [SECTION][class]s
  are allowed to be @LINKABLE.""")

;;; Handle *DOCUMENT-LINK-CODE* (:CODE for `SYMBOL` and
;;; :REFERENCE-LINK for [symbol][locative]). Don't hurt other links.
(defun link (parse-tree)
  (let ((linked-refs (make-array 0 :fill-pointer t :adjustable t
                                   :element-type 'dref)))
    (map-markdown-parse-tree
     '(:code :reference-link)
     '(:explicit-link :image :mailto :raw-html)
     nil
     (rcurry #'translate-to-links linked-refs)
     parse-tree)))

(defun translate-to-links (parent tree linked-refs)
  (nth-value-or 0
    (maybe-unescape-or-autolink parent tree linked-refs)
    (maybe-translate-reflink tree linked-refs)
    (assert nil)))

(define-glossary-term @linkable (:title "linkable")
  "When a reference is encountered to DREF::@DEFINITION D
  while processing documentation for some page C, we say that
  definition D is _linkable_ (from C) if

  - D denotes a SECTION and *DOCUMENT-LINK-SECTIONS* is true, or
  - D does not denote a SECTION and *DOCUMENT-LINK-CODE* is true

  ... and

  - We are @BROWSING-LIVE-DOCUMENTATION, or
  - D is an external definition ([CLHS][locative] or denotes a
    [GLOSSARY-TERM][class] with a [\URL][define-glossary-term]), or
  - D's page is C, or
  - D's page is relativizable to C.

  In the above, _D's page_ is the last of the pages in the
  @DOCUMENTABLE to which D's documentation is written (see :OBJECTS in
  @PAGES), and we say that a page is _relativizable_ to another if it
  is possible to construct a relative link between their
  :URI-FRAGMENTs.")

;;; See if we are allowed to link to DREF and that we know how to.
(defun linkable-dref-p (dref &key page)
  (let ((dref (replace-go-target dref)))
    (and (if (typep dref 'section-dref)
             *document-link-sections*
             *document-link-code*)
         (let ((page (or page (definition-page dref))))
           (or
            ;; pax: URLs are always linkable.
            (null page)
            ;; Intrapage links always work.
            (eq *page* page)
            ;; Absolute URLs always work.
            (stringp page)
            ;; PAGE is a PAGE structure. We need to know the
            ;; URI-FRAGMENT of both pages. See
            ;; RELATIVE-PAGE-URI-FRAGMENT.
            (and (page-uri-fragment *page*)
                 (page-uri-fragment page)))))))

(defun linkablep (link)
  (linkable-dref-p (link-definition link) :page (link-page link)))

(defun linkable-drefs (drefs)
  (remove-if-not #'linkable-dref-p drefs))

(defun drefs-to-links (drefs)
  ;; FIND-LINK may fall back on the CLHS definition, not knowing if
  ;; that CLHS definition is already present.
  (delete-duplicates
   (loop for dref in drefs
         for link = (find-link dref)
         when (and link (linkablep link))
           collect link)
   :key #'link-definition
   :test #'xref=))

(defsection @specific-link (:title "Specific Link")
  """Specific links are those @REFLINKs and @AUTOLINKs that have a
  single DREF::@LOCATIVE and therefore at most a single matching
  DREF::@DEFINITION. These are @SPECIFIC-REFLINK,
  @SPECIFIC-REFLINK-WITH-TEXT and @SPECIFIC-AUTOLINK.

  A specific link to a @LINKABLE definition produces a link in the
  output. If the definition is not linkable, then the output will
  contain only what would otherwise be the link text.""")

;;; Get the definitions for NAME and LOCATIVE. We expressly want to
;;; avoid depending on what we can link to because what is @LINKABLE
;;; is not predictable (consider PAX World and the value of
;;; *DOCUMENT-LINK-TO-HYPERSPEC*).
(defun specific-link-dref (name locative)
  (if (member (locative-type locative) '(dislocated argument))
      (xref name 'dislocated)
      (substitute-clhs-for-missing-standard-definition
       (dref name locative nil) name locative)))

(defun dref-to-links/specific (dref)
  (drefs-to-links (filter-clhs-dref (replace-go-targets (list dref)))))

(defsection @unspecific-link (:title "Unspecific Link")
  """Unspecific links are those @REFLINKs and @AUTOLINKs that do not
  specify a DREF::@LOCATIVE and match all [definitions][dref class]
  with a name. These are @UNSPECIFIC-REFLINK,
  @UNSPECIFIC-REFLINK-WITH-TEXT and @UNSPECIFIC-AUTOLINK.

  To make the links predictable and manageable in number, the
  following steps are taken.

  1. [filter-string-based-drefs function][docstring]

  2. [filter-locative-drefs function][docstring]

  3. Non-[@LINKABLE][] definitions are removed.

  4. [filter-method-links function][docstring]

  If at most a single definition remains, then the output is the same
  as with a @SPECIFIC-LINK. If multiple definitions remain, then the
  link text is output followed by a number of numbered links, one to
  each definition.""")

;;; Get the DEFINITIONS* for NAME. This includes ARGUMENTs (which
;;; depend on the documentation context via *LOCAL-REFERENCES*) and
;;; CLHS.
(defun unspecific-link-definitions* (name)
  ;; Although this is called only through FIND-NAME with :SYMBOLS-ONLY
  ;; T, we still need to remove string-based definitions due to e.g.
  ;; (DREF 'MGL-PAX 'PACKAGE).
  (filter-string-based-drefs (definitions* name)))

(defun drefs-to-links/unspecific (drefs)
  (filter-method-links
   (drefs-to-links
    (filter-locative-drefs
     (filter-clhs-dref
      (replace-go-targets drefs))))))

(defun replace-go-targets (drefs)
  (mapcar #'replace-go-target drefs))

(defun replace-go-target (dref)
  (if (eq (xref-locative-type dref) 'go)
      (go-target-dref dref)
      dref))

(defun filter-string-based-drefs (drefs)
  "Definitions that are not symbol-based (i.e. whose DREF-NAME
  is not a symbol) are filtered out to prevent unrelated
  [PACKAGE][locative]s, [ASDF:SYSTEM][locative]s and [CLHS][locative]
  sections from cluttering the documentation without the control
  provided by importing symbols."
  (remove-if #'string-based-dref-p drefs))

(defun string-based-dref-p (dref)
  (stringp (dref-name dref)))

(defun filter-method-links (links)
  "If the definitions include a [GENERIC-FUNCTION][locative], then
  all definitions with LOCATIVE-TYPE [METHOD][locative],
  [ACCESSOR][locative], [READER][locative] and [WRITER][locative] are
  removed to avoid linking to a possibly large number of methods."
  (flet ((non-method-links ()
           (remove-if (lambda (link)
                        (member (link-locative-type link)
                                '(accessor reader writer method)))
                      links)))
    (cond
      ;; If in doubt, prefer the generic function to methods.
      ((find 'generic-function links :key #'link-locative-type)
       (non-method-links))
      ;; No generic function, prefer non-methods to methods.
      ((non-method-links))
      (t
       links))))

(defun link-locative-type (link)
  (dref-locative-type (link-definition link)))

(defun filter-locative-drefs (drefs)
  "All references with LOCATIVE-TYPE LOCATIVE are filtered out."
  (remove 'locative drefs :key #'dref-locative-type))


(defsection @reflink (:title "Reflink")
  """The @MARKDOWN/REFLINK syntax `[label][id]` is
  repurposed for linking to DREF::@DEFINITIONS. In the following, we
  discuss the various forms of reflinks."""
  (@specific-reflink section)
  (@specific-reflink-with-text section)
  (@unspecific-reflink section)
  (@unspecific-reflink-with-text section)
  (@markdown-reflink section)
  (@unresolvable-reflinks section))

(defun maybe-translate-reflink (tree linked-refs)
  (when (eq :reference-link (first tree))
    (if (or (pt-get tree :definition) (pt-get tree :tail))
        (translate-reflink tree linked-refs)
        ;; (:REFERENCE-LINK :LABEL ("xxx") :TAIL NIL), the parse of [xxx].
        (values `(:plain "[" ,@(pt-get tree :label) "]") t nil))))

;;; This translator handles :REFERENCE-LINK nodes:
;;;
;;; - those with an explicit locative (:REFERENCE-LINK :LABEL ((:CODE
;;;   "SOMETHING")) :DEFINITION ("function")), the parse of
;;;   [`SOMETHING`][function],
;;;
;;; - and those with no locative (:REFERENCE-LINK :LABEL ((:CODE
;;;   "SOMETHING")) :TAIL "[]"), the parse of [`SOMETHING`][].
(defun translate-reflink (reflink linked-refs)
  (multiple-value-bind (links title-override replacement-tree)
      (dissect-reflink reflink linked-refs)
    (cond (replacement-tree
           ;; A non-PAX link like [something][user-defined-id] or
           ;; [something] or the return value of
           ;; SIGNAL-UNRESOLVABLE-REFLINK.
           (values replacement-tree nil t))
          (t
           (dolist (link links)
             (vector-push-extend (link-definition link) linked-refs))
           (values (make-reflinks (or title-override (pt-get reflink :label))
                                  title-override links)
                   nil t)))))

(defun dissect-reflink (reflink linked-refs)
  (assert (parse-tree-p reflink :reference-link))
  (destructuring-bind (&key label definition tail) (rest reflink)
    (let* ((empty-definition-p (and (zerop (length definition))
                                    (or (null tail)
                                        (equal tail "[]"))))
           (definition (trim-whitespace
                        (parse-tree-to-text definition :deemph t)))
           (locative-from-def (and definition (parse-locative definition)))
           (label-string (trim-whitespace
                          (parse-tree-to-text label :deemph nil))))
      (multiple-value-bind (links foundp label)
          (nth-value-or 1
            (when (and label-string locative-from-def)
              (specific-reflink label-string locative-from-def linked-refs))
            (when (and definition (null locative-from-def))
              (specific-reflink-with-text label definition))
            (when (and label-string empty-definition-p)
              (unspecific-reflink label-string))
            (when (null locative-from-def)
              (unspecific-reflink-with-text label definition)))
        (if foundp
            (values links label)
            (values nil nil
                    ;; [print][], [xxx][clhs] and
                    ;; [Try][try::@try-manual section] are almost
                    ;; definitely PAX links.
                    (if (or empty-definition-p
                            locative-from-def
                            (find-if #'whitespacep definition))
                        (signal-unresolvable-reflink reflink locative-from-def)
                        (list reflink))))))))

;;; Substitute a CLHS definition for standard stuff that's missing in
;;; the running Lisp. If we didn't do this, then:
;;;
;;; - "EQL type" could link to both (EQL TYPE) and (EQL FUNCTION) if
;;;   (EQL TYPE) were not defined in the Lisp (because AUTOLINK would
;;;   fall back on UNSPECIFIC-AUTOLINK.
;;;
;;; - [CONS][function] could link to (CON FUNCTION) if defined (via
;;;   FIND-NAME depluralizing).
;;;
;;; However, if *DOCUMENT-LINK-TO-HYPERSPEC* is NIL, this substitution
;;; must be later filtered out, but an explicit [EQL][(clhs type)]
;;; must be always kept, so we mark explicit CLHS links for
;;; FILTER-CLHS-REFERENCES.
(defun substitute-clhs-for-missing-standard-definition (dref name locative)
  (cond (dref
         (when (eq (dref-locative-type dref) 'clhs)
           (setf (clhs-dref-explicit-p dref) t))
         dref)
        (t
         (clhs-dref name locative))))


(defsection @specific-reflink (:title "Specific Reflink")
  """_Format:_ `[` [WORD][@WORD] `][` [LOCATIVE][locative] `]`

  The first @NAME in WORD (with depluralization) that forms a valid
  [DREF][class] with LOCATIVE is determined, and that definition is
  linked to. If there is no such DREF, then an UNRESOLVABLE-REFLINK
  warning is signalled.

  _Examples:_

  - ``[`EQL`][type]`` _renders as_ [EQL][type].

  - `[EQL][type]` _renders as_ [EQL][type].

  The Markdown link definition (i.e. `type` above) needs no backticks
  to mark it as code, but here and below, the second example relies on
  *DOCUMENT-UPPERCASE-IS-CODE* being true.
  """)

(defun specific-reflink (label-string locative-from-def linked-refs)
  (when-let (xref (find-name (rcurry #'specific-link-dref locative-from-def)
                             label-string :depluralize t))
    (values (cond ((eq (xref-locative-type xref) 'dislocated)
                   (vector-push-extend xref linked-refs)
                   ())
                  (t
                   (dref-to-links/specific xref)))
            t)))

(defsection @specific-reflink-with-text (:title "Specific Reflink with Text")
  """_Format:_ `[LINK TEXT][` [NAME][@name] [LOCATIVE][locative] `]`

  If NAME and LOCATIVE form a valid [DREF][class], then that
  definition is linked to with link text `LINK TEXT`. If there is no
  such DREF, then an UNRESOLVABLE-REFLINK warning is signalled.

  In this form, if NAME starts with `#\"`, then it's read as a string,
  else as a symbol.

  _Examples:_

  - `[see this][eql type]` _renders as_ [see this][eql type].

  - `[see this]["MGL-PAX" package]` _renders as_ [see this]["MGL-PAX" package].
  """)

(defun specific-reflink-with-text (label definition)
  (when-let (dref (parse-dref definition))
    (values (dref-to-links/specific dref) t label)))

(defsection @unspecific-reflink (:title "Unspecific Reflink")
  """_Format:_ `[` [WORD][@WORD] `][]`

  The first @NAME in WORD (with depluralization, symbols only) that
  has some DEFINITIONS is determined, and those definitions are linked
  to. If no @NAME with any definition is found, then an
  UNRESOLVABLE-REFLINK warning is signalled.

  _Examples:_

  - single link: `[PRINT][]` _renders as_ [PRINT][].

  - multiple links: `[EQL][]` _renders as_ [EQL][].

  - no definitions: `[BAD-NAME][]` _renders as_ BAD-NAME.
  """)

(defun unspecific-reflink (label-string)
  (when-let (drefs (find-name #'unspecific-link-definitions* label-string
                              :symbols-only t :depluralize t))
    (values (drefs-to-links/unspecific drefs) t)))

(defsection @unspecific-reflink-with-text
    (:title "Unspecific Reflink with Text")
  """_Format:_ `[LINK TEXT][` [NAME][@name] `]`

  The DEFINITIONS of NAME are determined, and those definitions are
  linked to. If NAME has no definitions, then an UNRESOLVABLE-REFLINK
  warning is signalled.

  In this form, if NAME starts with `#\"`, then it's read as a string,
  else as as symbol.

  _Examples:_

  - `[see this][print]` _renders as_ [see this][print].

  - `[see this][eql]` _renders as_ [see this][eql].
  """)

(defun unspecific-reflink-with-text (label definition)
  (when-let (drefs (find-name #'unspecific-link-definitions* definition
                              :symbols-only t))
    (values (drefs-to-links/unspecific drefs) t label)))

(defsection @markdown-reflink (:title "Markdown Reflink")
  """_Format:_ `[label][id]`

  This is a normal @MARKDOWN/REFLINK if `id` is not a valid locative.

   - `[see this][user-defined]` renders unchanged.

      ```cl-transcript (:dynenv pax-std-env)
      (dref:dref 'user-defined 'locative)
      .. debugger invoked on LOCATE-ERROR:
      ..   Could not locate USER-DEFINED LOCATIVE.
      ..   USER-DEFINED is not a valid locative type or locative alias.
      ```
      ```cl-transcript (:dynenv pax-std-env)
      (document "[see this][user-defined]" :format :markdown)
      .. [see this][user-defined]
      ..
      ```

  Use URLs with DEFINE-GLOSSARY-TERM as a better alternative to
  Markdown reference links (see @MARKDOWN-IN-DOCSTRINGS).
  """)


(defsection @unresolvable-reflinks (:title "Unresolvable Links")
  (unresolvable-reflink condition)
  (output-reflink function)
  (output-label function))

(define-condition unresolvable-reflink (warning)
  ((reflink :initarg :reflink :reader unresolvable-reflink-string)
   (locative :initarg :locative :reader unresolvable-reflink-locative))
  (:report print-unresolvable-reflink)
  (:documentation """When DOCUMENT encounters a @REFLINK that looks
  like a PAX construct but has no matching definition, it signals an
  UNRESOLVABLE-REFLINK warning.

  - If the OUTPUT-REFLINK restart is invoked, then no warning is
    printed and the Markdown link is left unchanged. MUFFLE-WARNING is
    equivalent to OUTPUT-REFLINK.

  - If the OUTPUT-LABEL restart is invoked, then no warning is printed
    and the Markdown link is replaced by its label. For example,
    `[NONEXISTENT][function]` becomes `NONEXISTENT`.

  - If the warning is not handled, then it is printed to
    *ERROR-OUTPUT*, and it behaves as if OUTPUT-LABEL was invoked."""))

(defun print-unresolvable-reflink (unresolvable-reflink stream)
  (let* ((c unresolvable-reflink)
         (reflink (unresolvable-reflink-string c))
         (locative #-cmucl (if (slot-boundp c 'locative)
                               (unresolvable-reflink-locative c)
                               nil)
                   #+cmucl (or (ignore-errors
                                (unresolvable-reflink-locative c))
                               'not-found)))
    (if locative
        (format stream "~@<No ~S found for ~S although ~S looks ~
                       like a ~S.~:@>"
                'dref reflink locative '@locative)
        (format stream "~@<No ~S found for ~S.~:@>" 'definitions reflink))
    (print-document-context stream)))

(defun signal-unresolvable-reflink (reflink locative)
  (restart-case
      (let ((string (reflink-to-string reflink)))
        (warn 'unresolvable-reflink :reflink string :locative locative)
        (pt-get reflink :label))
    (output-label ()
      :report "Output only the label."
      (pt-get reflink :label))
    (output-reflink ()
      :report "Output the whole reflink."
      reflink)))

(defun output-reflink (&optional condition)
  "Invoke the OUTPUT-REFLINK restart. See UNRESOLVABLE-REFLINK."
  (declare (ignore condition))
  (invoke-restart 'output-reflink))

(defun output-label (&optional condition)
  "Invoke the OUTPUT-LABEL restart. See UNRESOLVABLE-REFLINK."
  (declare (ignore condition))
  (invoke-restart 'output-label))

(defun reflink-to-string (tree)
  (with-output-to-string (stream)
    (print-markdown (list tree) stream)))


(defsection @autolink (:title "Autolink")
  "@MARKDOWN/INLINE-CODE automatically links to the corresponding
  definitions without having to use @REFLINKS. This works especially
  well in conjunction with @CODIFICATION. The following examples
  assume that *DOCUMENT-UPPERCASE-IS-CODE* is true. If that's not the
  case, explicit backticks are required on [WORD][@word] (but not on
  LOCATIVE)."
  (@specific-autolink section)
  (@unspecific-autolink section)
  (@escaping-autolinking section))

;;; This translator handles (:CODE "SOMETHING"), the parse of
;;; `SOMETHING`: looks for any references to "SOMETHING" and translates
;;; it to, for example, (:REFERENCE-LINK :LABEL ((:CODE "SOMETHING"))
;;; :DEFINITION ("function")) if there is a single function reference
;;; to it.
(defun autolink (parent tree word linked-refs)
  (multiple-value-bind (links foundp)
      (nth-value-or 1
        ;; This prefers a @SPECIFIC-AUTOLINK with a shorter name to an
        ;; @UNSPECIFIED-LOCATIVE with a longer one.
        (specific-autolink word parent tree linked-refs)
        (unspecific-autolink word linked-refs))
    (cond (foundp
           (dolist (link links)
             (vector-push-extend (link-definition link) linked-refs))
           (values (make-reflinks `(,tree) nil links)
                   nil t))
          (t
           tree))))


(defsection @specific-autolink (:title "Specific Autolink")
  """_Format:_ [WORD][@WORD] [LOCATIVE][LOCATIVE] or
  [LOCATIVE][LOCATIVE] [WORD][@WORD]

  The first @NAME in WORD (with depluralization) that forms a valid
  [DREF][class] with LOCATIVE is determined, and that definition is
  linked to. If no such name is found, then @UNSPECIFIC-AUTOLINK is
  attempted.

  _Examples:_

  - `\PRINT function` _renders as_ PRINT function.

  - `\type EQL` _renders as_ type EQL.

  - `\type EQL function` _renders as_ type EQL function.

  If LOCATIVE has spaces, then it needs to be marked up as code, too.
  For example,

      DREF-NAME `(reader dref)`

  _renders as_ DREF-NAME `(reader dref)`.
  """)

(defun specific-autolink (word parent tree linked-refs)
  (when-let (xref (find-name (curry #'specific-autolink-dref parent tree)
                             word :depluralize t))
    (cond ((eq (xref-locative-type xref) 'dislocated)
           (vector-push-extend xref linked-refs)
           (values () t))
          (t
           (values (dref-to-links/specific xref) t)))))

(defun specific-autolink-dref (parent tree name)
  (loop for locative in (find-locatives-around parent tree name)
          thereis (specific-link-dref name locative)))

;;; Find locatives just before or after TREE in PARENT. For example,
;;; PARENT is (:PLAIN "See" "function" " " (:CODE "FOO")), and TREE is
;;; (:CODE "FOO").
(defun find-locatives-around (parent tree name)
  (let ((locatives ()))
    (labels ((try-string (string)
               (let ((locative (parse-locative-around string
                                                      :junk-allowed t
                                                      :name name)))
                 (when locative
                   (push locative locatives))))
             (try (element)
               (cond ((stringp element)
                      (try-string element))
                     ((eq :code (first element))
                      (try-string (second element))))))
      ;; Note that (EQ (THIRD REST) TREE) may be true multiple times,
      ;; for example if strings are interned and "FOO" occurs multiple
      ;; times in PARENT.
      (loop for rest on (rest parent)
            do (when (and (eq (third rest) tree)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (first rest))
                 (return)))
      ;; For example, (:PLAIN "See" "the" "FOO" " " "function")
      (loop for rest on (rest parent)
            do (when (and (eq (first rest) tree)
                          (stringp (second rest))
                          (blankp (second rest)))
                 (try (third rest))
                 (return))))
    locatives))


(defsection @unspecific-autolink (:title "Unspecific Autolink")
  """_Format:_ [WORD][@WORD]

  The first @NAME in WORD (with depluralization, symbols only) that
  has some DEFINITIONS is determined, and those definitions are linked
  to. If no such name is found or the autolink to this name is
  _suppressed_ (see below), then WORD is left unchanged. If a locative
  is found before or after WORD, then @SPECIFIC-AUTOLINK is tried
  first.

  _Examples:_

  - `\PRINT` _renders as_ PRINT.

  - `\EQL` _renders as_ EQL.

  [suppressed-link-p function][docstring]
  """)

(defun unspecific-autolink (word linked-refs)
  (when-let (drefs (find-name #'unspecific-link-definitions* word
                              :symbols-only t :depluralize t))
    (values (drefs-to-links/unspecific
             (filter-suppressed-references drefs linked-refs))
            t)))

(defun filter-suppressed-references (refs linked-refs)
  (if (suppressed-link-p refs linked-refs)
      ()
      refs))

(declaim (inline xref-name=))
(defun xref-name= (name ref)
  (equal name (xref-name ref)))

(defun suppressed-link-p (drefs linked-refs)
  """@UNSPECIFIC-AUTOLINKing is suppressed if the name found has a
  @LOCAL-DEFINITION or was linked to before in the same docstring:

  - "`My other CAR is also a CAR`" _renders as_ "My other CAR is also a
    CAR".

  - "`[COS][] and COS`" _renders as_ "[COS][] and COS".

  - "`[EQL][type] and EQL`" _renders as_ "[EQL][type] and EQL".

  - "`EQ and the EQ function`" _renders as_ "EQ and the EQ function".

  @UNSPECIFIC-AUTOLINKing to T and NIL is also suppressed (see
  *DOCUMENT-LINK-TO-HYPERSPEC*):

  - "`T and NIL`" _renders as_ "T and NIL".

  As an exception, a single link (be it either a @SPECIFIC-LINK or an
  unambiguous @UNSPECIFIC-LINK) to a SECTION or GLOSSARY-TERM is not
  suppressed to allow their titles to be displayed properly:

  - "`@NAME and @NAME`" _renders as_ "@NAME and @NAME"."""
  (when drefs
    (or (some (lambda (dref)
                (let ((name (dref-name dref)))
                  (or (member name '(t nil))
                      (has-local-reference-p name))))
              drefs)
        (and
         (loop for dref in drefs
                 thereis (find (xref-name dref) linked-refs
                               :test #'xref-name=))
         (not (and (= (length drefs) 1)
                   (doctitle (first drefs))))))))


(defsection @escaping-autolinking (:title "Escaping Autolinking")
  """In the common case, when [*DOCUMENT-UPPERCASE-IS-CODE*][] is true,
  prefixing an uppercase @WORD with a backslash prevents it from being
  codified and thus also prevents @AUTOLINKing form kicking in. For
  example,

      \DOCUMENT

  renders as \DOCUMENT. If it should be marked up as code but not
  autolinked, the backslash must be within backticks like this:

      `\DOCUMENT`

  This renders as `\DOCUMENT`. Alternatively, the
  [DISLOCATED][locative] or the ARGUMENT locative may be used as in
  `[DOCUMENT][dislocated]`.""")

(defun maybe-unescape-or-autolink (parent tree linked-refs)
  (when (parse-tree-p tree :code)
    (let ((string (second tree)))
      (if (starts-with #\\ string)
          `(:code ,(subseq string 1))
          (autolink parent tree string linked-refs)))))


;;;; Common code for @LINKING

;;; With older versions it's a STRING or NIL.
(defparameter *3bmd-reflink-definition-is-list*
  (not (atom (pt-get (esrap:parse '3bmd-grammar::reference-link "[x][y]")
                     :definition))))

(declaim (inline %make-reflink))
(defun %make-reflink (label definition)
  (if *3bmd-reflink-definition-is-list*
      `(:reference-link :label ,label :definition (,definition))
      `(:reference-link :label ,label :definition ,definition)))

;;; For LABEL (a parse tree fragment) and some references to it
;;; (REFS), return a Markdown parse tree fragment to be spliced into a
;;; Markdown parse tree.
(defun make-reflinks (label explicit-label-p links)
  (if (endp links)
      ;; All references were filtered out.
      label
      (let* ((link-1 (first links))
             (ref-1 (link-definition link-1))
             (page (link-page link-1))
             ;; This DREF was let through in FIND-LINK with PAGE NIL,
             ;; as if with *DOCUMENT-OPEN-LINKING*, just to substitute
             ;; its title here.
             (fake-link-p (and (not *document-open-linking*)
                               (not (external-dref-p ref-1))
                               (null page))))
        (cond
          ((< 1 (length links))
           (cond ((eq *subformat* :plain)
                  label)
                 (*document-open-linking*
                  ;; [`label`](pax:name)
                  `((:explicit-link
                     :label ,label
                     :source ,(finalize-pax-url (name-to-pax-url
                                                 (dref-name ref-1))))))
                 (t
                  ;; `label`([1][link-id-1] [2][link-id-2])
                  `(,@label
                    "("
                    ,@(loop
                        for i upfrom 0
                        for link in (dref::sort-references
                                     links :key #'link-definition)
                        append `(,@(unless (zerop i)
                                     '(" "))
                                 ,(%make-reflink `(,(code-fragment i))
                                                 (link-to link))))
                    ")"))))
          ((member (xref-locative-type ref-1) '(dislocated argument))
           label)
          ((typep ref-1 'note-dref)
           (codify-and-link-tree (translate-dref-docstring ref-1)))
          (t
           (let ((label (or (and (not explicit-label-p)
                                 (document-definition-title ref-1 :format nil))
                            label)))
             (if fake-link-p
                 label
                 `(,(%make-reflink label (link-to link-1))))))))))


(defsection @linking-to-the-hyperspec (:title "Linking to the HyperSpec")
  (*document-link-to-hyperspec* variable)
  (*document-hyperspec-root* variable))

(defvar/autoloaded *document-link-to-hyperspec* t
  """If true, consider definitions found in the Common Lisp HyperSpec
  for linking. For example,

  - `\PRINT` _renders as_ PRINT.

  In offline documentation, this would be a link to the hyperspec
  unless `#'PRINT` in the running Lisp is @DOCUMENTABLE.

  When @BROWSING-LIVE-DOCUMENTATION, everything is @LINKABLE, so the
  generated link will go to a disambiguation page that lists the
  definition in the Lisp and in the HyperSpec.

  Locatives work as expected (see *DOCUMENT-LINK-CODE*): `\FIND-IF`
  links to FIND-IF, `\FUNCTION` links to FUNCTION, and
  `[FUNCTION][type]` links to [FUNCTION][type].

  @UNSPECIFIC-AUTOLINKing to T and NIL is suppressed. If desired, use
  @REFLINKs such as `[T][]` (that links to [T][]) or
  `[T][constant]` (that links to [T][constant]).

  Note that linking explicitly with the CLHS locative is not subject
  to the value of this variable.""")

(defvar/autoloaded *document-hyperspec-root*
  "http://www.lispworks.com/documentation/HyperSpec/"
  """A \URL of the Common Lisp HyperSpec.
  The default value is the canonical location. When [invoked from
  Emacs][ @browsing-live-documentation], the Elisp variable
  `common-lisp-hyperspec-root` is in effect.""")

(defun find-clhs-url (dref)
  (when (eq (dref-locative-type dref) 'clhs)
    (let* ((name (dref-name dref))
           (locative (dref-locative-args dref))
           (locative-type (locative-type locative)))
      ;; This parallels DREF* (METHOD (T (EQL CLHS) T)).
      (cond ((eq locative-type 'glossary-term)
             (find-hyperspec-glossary-entry-url name
                                                *document-hyperspec-root*))
            ((eq locative-type 'section)
             (or (find-hyperspec-issue-url name *document-hyperspec-root*)
                 (find-hyperspec-section-url name
                                             *document-hyperspec-root*)))
            (t
             (find-hyperspec-definition-url name locative
                                            *document-hyperspec-root*))))))

(defun filter-clhs-dref (drefs)
  (if *document-link-to-hyperspec*
      drefs
      (remove-if (lambda (dref)
                   (and (typep dref 'clhs-dref)
                        (not (clhs-dref-explicit-p dref))))
                 drefs)))

;;; Just for the generated docstrings. See CLHS locative.
(defvar *format-directive-alias-links* nil
  #.(with-output-to-string (out)
      (loop for alias in *hyperspec-format-directive-aliases*
            for i upfrom 0
            do (unless (zerop i)
                 (if (zerop (mod i 4))
                     (format out "~%")
                     (format out " ")))
               (format out "[~A][~A clhs]"
                       (escape-markdown alias)
                       (escape-markdown (prin1-to-string alias))))))
(defvar *reader-macro-alias-links* nil
  #.(with-output-to-string (out)
      (loop for alias in *hyperspec-reader-macro-char-aliases*
            for i upfrom 0
            do (unless (zerop i)
                 (if (zerop (mod i 4))
                     (format out "~%")
                     (format out " ")))
               (format out "[~A][~A clhs]"
                       (escape-markdown alias)
                       (escape-markdown (prin1-to-string alias))))))


;;;; External references: beginnings of an abstraction for DREFs
;;;; denoting stuff that lives outside the running image (e.g. CLHS
;;;; and GLOSSARY-TERMs with URLs). This could serve the needs of e.g.
;;;; linking to the MOP.

(defun external-dref-url (dref)
  (or (when (typep dref 'clhs-dref)
        (find-clhs-url dref))
      (when (typep dref 'glossary-term-dref)
        (when-let (glossary-term (resolve dref nil))
          (glossary-term-url glossary-term)))))

(defun external-dref-p (dref)
  (external-dref-url dref))


(defsection @linking-to-sections (:title "Linking to Sections")
  "The following variables control how to generate section numbering,
  table of contents and navigation links."
  (*document-link-sections* variable)
  (*document-max-numbering-level* variable)
  (*document-max-table-of-contents-level* variable)
  (*document-text-navigation* variable)
  (*document-fancy-html-navigation* variable))

(defvar/autoloaded *document-link-sections* t
  "When true, HTML anchors and PDF destinations are generated before
  the headings (e.g. of sections), which allows the table of contents
  to contain links and also code-like references to sections (like
  `@FOO-MANUAL`) to be translated to links with the
  [TITLE][DEFSECTION] being the link text.")

(defvar/autoloaded *document-max-numbering-level* 3
  "A non-negative integer. In their hierarchy, sections on levels less
  than this value get numbered in the format of `3.1.2`. Setting it to
  0 turns numbering off.")

(defvar/autoloaded *document-max-table-of-contents-level* 3
  "An integer that determines the depth of the table of contents.

  - If negative, then no table of contents is generated.

  - If non-negative, and there are multiple top-level sections on a
    page, then they are listed at the top of the page.

  - If positive, then for each top-level section a table of contents
    is printed after its heading, which includes a nested tree of
    section titles whose depth is limited by this value.

  If *DOCUMENT-LINK-SECTIONS* is true, then the tables will link to
  the sections.")

(defvar/autoloaded *document-text-navigation* nil
  "If true, then before each heading a line is printed with links to
  the previous, parent and next section. Needs
  *DOCUMENT-LINK-SECTIONS* to be on to work.")

(defvar/autoloaded *document-fancy-html-navigation* t
  "If true and the output format is HTML, then headings get a
  navigation component that consists of links to the previous, parent,
  next section, a self-link, and a link to the definition in the
  source code if available (see :SOURCE-URI-FN in DOCUMENT). This
  component is normally hidden, it is visible only when the mouse is
  over the heading. Has no effect if *DOCUMENT-LINK-SECTIONS* is
  false.")

(defun print-toplevel-section-lists (pages)
  (when (<= 0 *document-max-table-of-contents-level*)
    (dolist (page pages)
      (when (page-written-in-first-pass-p page)
        (print-toplevel-section-list page)))))

(defun print-toplevel-section-list (page)
  (let ((toplevel-headings (toplevel-headings-on-page page)))
    (when (< 1 (length toplevel-headings))
      (with-temp-output-to-page (stream page)
        (dolist (heading toplevel-headings)
          (print-table-of-contents-entry heading stream))
        (terpri stream)))))

(defun toplevel-headings-on-page (page)
  (loop for heading in *headings*
        when (and (zerop (heading-level heading))
                  (find (heading-object heading) (page-definitions page)
                        :test #'xref=))
          collect heading))

(defun print-table-of-contents (dref stream)
  (when (zerop *heading-level*)
    (let ((rest (list-headings dref *heading-level*))
          (toc-title-printed nil))
      (flet ((ensure-toc-title ()
               (unless toc-title-printed
                 (heading (+ *heading-level* 1 *heading-offset*) stream)
                 (format stream " Table of Contents~%~%")
                 (setq toc-title-printed t))))
        (loop for heading in (rest rest)
              while (plusp (heading-level heading))
              do (when (<= (heading-level heading)
                           *document-max-table-of-contents-level*)
                   (ensure-toc-title)
                   (print-table-of-contents-entry heading stream))))
      (when toc-title-printed
        (terpri stream)))))

;;; Return the tail of *HEADINGS* from DREF at HEADING-LEVEL or NIL.
(defun list-headings (dref heading-level)
  ;; DREF may be DOCUMENTed multiple times at different depths. See
  ;; MGL-PAX-TEST::TEST-TABLE-OF-CONTENTS-REAPATED-SECTION-DEPTH.
  (member-if (lambda (heading)
               (and (xref= (heading-object heading) dref)
                    (= (heading-level heading) heading-level)))
             *headings*))

(defun print-table-of-contents-entry (heading stream)
  (let ((dref (heading-object heading))
        (title (heading-title heading))
        (level (heading-level heading))
        (number (heading-number heading)))
    (loop repeat (* 4 (1- level))
          do (write-char #\Space stream))
    (let ((link-id (link-to-definition dref)))
      (if (and *document-link-sections* link-id)
          (format stream "- [~A~A][~A]" (format-heading-number number) title
                  link-id)
          (format stream "- ~A~A" (format-heading-number number) title)))
    (terpri stream)))

(defun print-section-title (stream dref title link-title-to)
  (when *document-link-sections*
    (unless (eq *format* :pdf)
      (anchor dref stream))
    (navigation-link dref stream)
    (format stream "~A" (fancy-navigation dref)))
  (heading (+ *heading-level* *heading-offset*) stream)
  (cond ((and *document-link-sections*
              (eq *format* :html))
         (print-section-title-link stream dref title link-title-to))
        (t
         (format stream " ~A~A~%" (format-heading-number) title)
         (when (and *document-link-sections*
                    (eq *format* :pdf))
           (anchor dref stream))
         (terpri stream))))

(defun print-section-title-link (stream dref title link-title-to)
  (if link-title-to
      ;; Hovering over the section title will show the title of
      ;; LINK-TITLE-TO from the Markdown reference link definition.
      (format stream " [~A~A][~A]~%~%"
              (format-heading-number) title
              (link-to-definition link-title-to))
      (format stream " <a href=\"~A\">~A~A</a>~%~%"
              ;; As in PRINT-DREF-BULLET, open linking is to a
              ;; separate page.
              (if *document-open-linking*
                  (finalize-pax-url (dref-to-pax-url dref))
                  (object-to-uri dref))
              (format-heading-number) title)))

(defun fancy-navigation-p ()
  (and *document-fancy-html-navigation*
       *document-link-sections*
       (eq *format* :html)))

(defun fancy-navigation (dref)
  (if (fancy-navigation-p)
      (let* ((position (position dref *headings* :key #'heading-object
                                 :test #'xref=))
             (level (heading-level (elt *headings* position)))
             (n (length *headings*))
             (prev (when (and (plusp position)
                              (plusp level))
                     (elt *headings* (1- position))))
             (up (when (plusp level)
                   (find (1- level) (subseq *headings* 0 position)
                         :from-end t :key #'heading-level)))
             (next (when (< position (1- n))
                     (let ((next (elt *headings* (1+ position))))
                       ;; Prev and next stay within the same top-level
                       ;; section.
                       (unless (zerop (heading-level next))
                         next))))
             (source-uri (source-uri dref)))
        (format nil "<span class=\"outer-navigation\">~
                    <span class=\"navigation\">~
                    ~@[ [&#8592;][~A]~]~
                    ~@[ [&#8593;][~A]~]~
                    ~@[ [&#8594;][~A]~] ~
                    [&#8634;][~A]~
                    ~A~
                    </span></span>~%"
                (when prev
                  (link-to-definition (heading-object prev)))
                (when up
                  (link-to-definition (heading-object up)))
                (when next
                  (link-to-definition (heading-object next)))
                (link-to-definition dref)
                (if source-uri
                    (format nil " <a href=~S>&#955;</a>" source-uri)
                    "")))
      ""))

(defun write-navigation-link (heading stream)
  (let ((link-id (link-to-definition (heading-object heading))))
    (format stream "[~A][~A]" (heading-title heading) link-id)))

(defun navigation-link (dref stream)
  (when (and *document-link-sections* *document-text-navigation*)
    (let* ((position (position dref *headings* :key #'heading-object
                               :test #'xref=))
           (level (heading-level (elt *headings* position)))
           (n (length *headings*))
           (writtenp nil))
      (when (< position (1- n))
        (format stream "Next: ")
        (write-navigation-link (elt *headings* (1+ position)) stream)
        (setq writtenp t))
      (when (plusp position)
        (when writtenp
          (format stream " "))
        (format stream "Prev: ")
        (write-navigation-link (elt *headings* (1- position)) stream)
        (setq writtenp t))
      (when (plusp level)
        (when writtenp
          (format stream " "))
        (let ((parent (find (1- level) (subseq *headings* 0 position)
                            :from-end t :key #'heading-level)))
          (format stream "Up: ")
          (write-navigation-link parent stream))
        (setq writtenp t))
      (when writtenp
        (format stream "~%~%")))))

(defun format-heading-number (&optional (heading-number *heading-number*))
  (format nil "~@[~{~D~^.~} ~]"
          (when (<= (length heading-number) *document-max-numbering-level*)
            heading-number)))


(defsection @local-definition (:title "Local Definition")
  """While documentation is generated for a definition, that
  definition is considered local. Other local definitions may also be
  established. Local definitions inform @CODIFICATION through
  @INTERESTING @NAMEs and affect @UNSPECIFIC-AUTOLINKing.

  ```
  (defun foo (x)
    "FOO adds one to X."
    (1+ x)
  ```

  In this example, while the docstring of FOO is being processed, the
  global definition `(DREF 'FOO 'FUNCTION)` is also considered local,
  which suppresses linking FOO in the FOO's docstring back to its
  definition. If FOO has other definitions, @UNSPECIFIC-AUTOLINKing to
  those is also suppressed.

  Furthermore, the purely local definition `(DREF 'X 'ARGUMENT)` is
  established, causing the argument name `X` to be
  [codified][@codification] because `X` is now @INTERESTING.

  See DOCUMENTING-DEFINITION and WITH-DISLOCATED-NAMES in
  @EXTENDING-DOCUMENT.
  """)

;;; A list of references with special rules for linking. The
;;; definition being documented is always on this list (see
;;; DOCUMENTING-DEFINITION). Arguments of functions and similar
;;; typically also are. Bound by WITH-LOCAL-REFERENCES.
(defvar *local-references*)

(defun has-local-reference-p (name)
  (find name *local-references* :test #'xref-name=))


(defsection @link-format (:title "Link Format")
  "The following variables control various aspects of links and \URLs."
  (*document-url-versions* variable)
  (*document-min-link-hash-length* variable)
  (*document-base-url* variable))

(defvar/autoloaded *document-url-versions* '(2 1)
  """A list of versions of PAX \URL formats to support in the
  generated documentation. The first in the list is used to generate
  links.

  PAX emits HTML anchors before the documentation of SECTIONs
  (see @LINKING-TO-SECTIONS) and other things (see @LINKING). For the
  function `FOO`, in the current version (version 2), the anchor is
  `<a id="MGL-PAX:FOO%20FUNCTION">`, and its \URL will end
  with `\\#MGL-PAX:FOO%20FUNCTION`.

  _Note that to make the \URL independent of whether a symbol is
  [internal or external][find-symbol] to their SYMBOL-PACKAGE, single
  colon is printed where a double colon would be expected. Package and
  symbol names are both printed verbatim except for escaping colons
  and spaces with a backslash. For exported symbols with no funny
  characters, this coincides with how PRIN1 would print the symbol,
  while having the benefit of making the \URL independent of the Lisp
  printer's escaping strategy and producing human-readable output for
  mixed-case symbols. No such promises are made for non-ASCII
  characters, and their \URLs may change in future versions. Locatives
  are printed with PRIN1._

  Version 1 is based on the more strict HTML4 standard and the id of
  `FOO` is `"x-28MGL-PAX-3A-3AFOO-20FUNCTION-29"`. This is supported
  by GitHub-flavoured Markdown. Version 2 has minimal clutter and is
  obviously preferred. However, in order not to break external links,
  by default, both anchors are generated.

  Let's understand the generated Markdown.

  ```
  (defun foo (x))

  (document #'foo :format :markdown)
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>
  <a id=\"MGL-PAX:FOO%20FUNCTION\"></a>

  - [function] **FOO** *X*
  ")

  (let ((*document-url-versions* '(1)))
    (document #'foo :format :markdown))
  => ("<a id=\"x-28MGL-PAX-3AFOO-20FUNCTION-29\"></a>

  - [function] **FOO** *X*
  ")
  ```
  """)

(defun anchor (dref stream)
  (cond ((eq *subformat* :plain))
        ((not (eq *format* :pdf))
         (let ((v1 (member 1 *document-url-versions*))
               (v2 (member 2 *document-url-versions*)))
           (when (or v1 v2)
             (when v1
               (format stream "<a id=~S></a>~%"
                       (html4-safe-name (dref-to-anchor-v1 dref))))
             (when v2
               (format stream "<a id=~S></a>~%"
                       (urlencode (dref-to-anchor dref))))
             (terpri stream))))
        ;; :PDF works only with URL v1. Since PDF destinations are not
        ;; really usable reliably anyway when opening the PDF, we
        ;; don't care what *DOCUMENT-URL-VERSIONS* says.
        ((typep dref 'section-dref)
         (format stream "`\\label{~A}`{=latex}~%"
                 (html4-safe-name (dref-to-anchor-v1 dref))))
        (t
         (format stream "`\\phantomsection\\label{~A}`{=latex}~%"
                 (html4-safe-name (dref-to-anchor-v1 dref))))))

(defun anchor-id (dref)
  (if (= (first *document-url-versions*) 1)
      (html4-safe-name (dref-to-anchor-v1 dref))
      (urlencode (dref-to-anchor dref))))

;;; Return the unescaped name of the HTML anchor for DREF. See
;;; URLENCODE.
(defun dref-to-anchor (dref)
  (with-standard-io-syntax*
    ;; The locative may not be readable (e.g. methods with EQL
    ;; specializers with unreadable stuff).
    (let ((*print-readably* nil))
      (format nil "~A ~S" (prin1-funny-to-string (dref-name dref))
              (dref-locative dref)))))

(defun dref-to-anchor-v1 (dref)
  (with-standard-io-syntax*
    (let ((*print-readably* nil))
      (format nil "(~A ~S)" (prin1-funny-to-string (dref-name dref))
              (dref-locative dref)))))

(defun dref-to-pax-url (dref)
  (urlencode
   (with-standard-io-syntax*
     (let ((*print-readably* nil))
       (format nil "pax:~A ~S" (prin1-funny-to-string (dref-name dref))
               (dref-locative dref))))))

(defun name-to-pax-url (name)
  (urlencode
   (with-standard-io-syntax*
     (format nil "pax:~A" (prin1-funny-to-string name)))))

;;; This is kind of the collective inverse of DREF-TO-PAX-URL and
;;; NAME-TO-PAX-URL.
(defun definitions-for-pax-url-path (path)
  (with-standard-io-syntax*
    (let ((*print-readably* nil))
      (with-input-from-string (stream path)
        (let ((name (read-funny stream nil)))
          (unless name
            (error "~@<Bad ~S name in PAX URL path ~S~:@>" 'dref::@name path))
          (let ((urlencoded-path (format nil "pax:~A" (urlencode path)))
                (drefs (definitions* name))
                (locative-string (trim-whitespace
                                  (read-stream-content-into-string stream))))
            (cond ((zerop (length locative-string))
                   drefs)
                  (t
                   ;; Don't try to READ the locative args because they
                   ;; may be unreadable. Just validate the locative
                   ;; type.
                   (unless (validate-parsed-locative-type locative-string)
                     (error "~@<Bad ~S name in PAX URL path ~S~:@>"
                            'dref::@locative-type path))
                   (ensure-list
                    ;; Since PATH was generated by DREF-TO-PAX-URL, we
                    ;; can simply require an exact match. This relies
                    ;; on @STABLE-PRINTED-LOCATIVES.
                    (find urlencoded-path drefs :key #'dref-to-pax-url
                                                :test #'string=))))))))))


(defvar/autoloaded *document-min-link-hash-length* 4
  "Recall that @MARKDOWN/REFLINKs (like `[label][id]`) are used for
  @LINKING. It is desirable to have ids that are short to maintain
  legibility of the generated Markdown, but also stable to reduce the
  spurious diffs in the generated documentation, which can be a pain
  in a version control system.

  Clearly, there is a tradeoff here. This variable controls how many
  characters of the MD5 sum of the full link id (the reference as a
  string) are retained. If collisions are found due to the low number
  of characters, then the length of the hash of the colliding
  reference is increased.

  This variable has no effect on the HTML generated from Markdown, but
  it can make Markdown output more readable.")

(defun hash-link (string detect-collision-fn
                  &key (min-n-chars *document-min-link-hash-length*))
  (let ((hex (byte-array-to-hex-string (md5:md5sum-string string))))
    (loop for i upfrom min-n-chars below 32
          do (let ((hash (subseq hex 0 (min 32 i))))
               (unless (funcall detect-collision-fn hash)
                 (return-from hash-link hash))))
    (assert nil () "MD5 collision detected.")))

(defun byte-array-to-hex-string (byte-array)
  (declare (type (vector (unsigned-byte 8)) byte-array))
  (let* ((n (length byte-array))
         (s (make-string (* 2 n) :element-type 'base-char))
         (hex-digits "0123456789abcdef"))
    (dotimes (i n)
      (let ((byte (aref byte-array i)))
        (multiple-value-bind (div rem) (floor byte 16)
          (setf (aref s (* 2 i)) (aref hex-digits div))
          (setf (aref s (1+ (* 2 i))) (aref hex-digits rem)))))
    s))


(defvar/autoloaded *document-mark-up-signatures* t
  "When true, some things such as function names and arglists are
  rendered as bold and italic. In @HTML-OUTPUT and @PDF-OUTPUT,
  locative types become links to sources (if :SOURCE-URI-FN is
  provided, see @PAGES), and the symbol becomes a self-link for your
  permalinking pleasure.

  For example, a reference is rendered in Markdown roughly as:

      - [function] foo x y

  With this option on, the above becomes:

      - [function] **foo** *x y*

  Also, in HTML `**foo**` will be a link to that very entry and
  `[function]` may turn into a link to sources.")

(defun mark-up-signatures-p ()
  (and *document-mark-up-signatures*
       ;; KLUDGE: GitHub has trouble displaying things like
       ;; '`*package*`, so disable this Markdown.
       (member *format* '(:html :pdf))))

;;; PRINT DREF to STREAM as:
;;;
;;;     - [locative-type] name
;;;
;;; When generating HTML, link NAME to the anchor of DREF.
(defun/autoloaded print-dref-bullet (dref stream)
  (let* ((locative-type (string-downcase (xref-locative-type dref)))
         (label (dref-bullet-label dref))
         (md-locative-type (escape-markdown locative-type)))
    (if (not *document-mark-up-signatures*)
        (format stream
                ;; Escape if we must. Otherwise, do not clutter the output.
                (if (or (eq *format* :pdf)
                        (eq *subformat* :plain))
                    "- \\[~A] ~A"
                    "- [~A] ~A")
                md-locative-type (escape-markdown label))
        (ecase *format*
          ((:html)
           (let ((source-uri (source-uri dref)))
             ;; When *DOCUMENT-OPEN-LINKING* (this includes (EQ
             ;; *SUBFORMAT* :W3M)), the name is linked to the a
             ;; pax: URL (which opens a separate page), else they
             ;; are self-links.
             (cond ((eq *subformat* :w3m)
                    (assert *document-open-linking*)
                    (format stream "- **\\[~A]** [~A](~A)"
                            md-locative-type label
                            (finalize-pax-url (dref-to-pax-url dref))))
                   (*document-open-linking*
                    (format stream
                            "- <span class=reference-bullet>~
                            <span class=reference>~
                            <span class=\"locative-type\">~
                            ~@[<a href=\"~A\" title=\"Edit in Emacs\">~]~
                            \\[~A]~:[~;</a>~]~
                            </span> ~
                            <span class=\"reference-object\">[~A](~A)</span>~
                            </span>"
                            source-uri md-locative-type source-uri label
                            (finalize-pax-url (dref-to-pax-url dref))))
                   (t
                    (format stream
                            "- <span class=reference-bullet>~
                            <span class=reference>~
                            <span class=\"locative-type\">~
                            ~@[<a href=\"~A\">~]\\[~A]~:[~;</a>~]~
                            </span> ~
                            <span class=\"reference-object\">[~A](#~A)</span>~
                            </span>"
                            source-uri md-locative-type source-uri label
                            (urlencode (dref-to-anchor dref)))))))
          ((:pdf)
           (let ((source-uri (source-uri dref)))
             (format stream "- ")
             (if source-uri
                 (format stream
                         "`\\paxlocativetypewithsource{~A}{~A}`{=latex}"
                         (escape-tex source-uri) (escape-tex locative-type))
                 (format stream "`\\paxlocativetype{~A}`{=latex}"
                         (escape-tex locative-type)))
             (format stream "`\\paxname{~A}`{=latex}" (escape-tex label))))
          ((:markdown)
           (if *subformat*
               (format stream "- \\[~A\\] ~A" locative-type
                       (escape-markdown label))
               (format stream "- [~A] ~A" locative-type (bold label nil))))
          ((nil))))))

(defun dref-bullet-label (dref)
  (let ((title (doctitle dref)))
    (if title
        (document-title title :dref dref
                        ;; Emphasis is set in the context.
                        :deemph t)
        (let ((name (prin1-to-string* (xref-name dref))))
          (if (eq *subformat* :plain)
              name
              (escape-markdown name))))))

(defun print-end-bullet (stream)
  (if (and (eq *format* :html)
           (not (eq *subformat* :w3m)))
      ;; end "reference-bullet" span
      (format stream "</span>~%")
      (format stream "~%")))

(defun source-uri (reference)
  (let ((fn (page-source-uri-fn *page*)))
    (if fn
        (funcall fn reference)
        nil)))

(defvar *print-arglist-key* nil)

(defun print-arglist (arglist stream)
  (let* ((string (if (stringp arglist)
                     ;; must be escaped Markdown
                     arglist
                     (arglist-to-markdown arglist)))
         (string (if *print-arglist-key*
                     (funcall *print-arglist-key* string)
                     string)))
    (cond ((not *document-mark-up-signatures*)
           (format stream "~A" string))
          ((and (eq *format* :html)
                (not (eq *subformat* :w3m)))
           (format stream "<span class=\"locative-args\">~A</span>" string))
          (t
           (italic string stream)))))

;;; Print a lambda list of any kind (ordinary, macro, etc) or a method
;;; arglist to a string. Arg names are printed without the package
;;; prefix, default values with the package prefix. For specializers
;;; in method arglists to be distinguishable from nested macro lambda
;;; lists, they look like
;;;
;;; (:method (x string) y (z (eql 7)) ...)
;;;
;;; else the specializers are printed without package information.
(defun arglist-to-markdown (arglist)
  (with-output-to-string (out)
    (multiple-value-bind (methodp arglist)
        (if (eq (first arglist) :method)
            (values t (rest arglist))
            (values nil arglist))
      (let ((*print-pretty* t)
            (*print-right-margin* 80))
        (labels
            ((resolve* (object)
               (if (mark-up-signatures-p)
                   (codify-and-link (prin1-to-markdown object))
                   (prin1-to-markdown object)))
             (print-arg (arg level)
               (declare (special *nesting-possible-p*))
               (cond ((member arg '(&key &optional &rest &body))
                      (when (member arg '(&key &optional))
                        (setq *nesting-possible-p* nil))
                      (format out "~A" (prin1-to-markdown arg)))
                     ((symbolp arg)
                      (format out "~A"
                              (escape-markdown
                               (maybe-downcase-all-uppercase-code
                                (symbol-name arg)))))
                     ((atom arg)
                      (format out "~A" (prin1-to-markdown arg)))
                     (*nesting-possible-p*
                      (print-arglist arg (1+ level)))
                     (t
                      (if (symbolp (first arg))
                          (format out "(~A~{ ~A~})"
                                  (escape-markdown
                                   (maybe-downcase-all-uppercase-code
                                    (symbol-name (first arg))))
                                  (mapcar #'resolve* (rest arg)))
                          (format out "~A" (prin1-to-markdown arg))))))
             (print-arglist (arglist level)
               (let ((*nesting-possible-p* (not methodp)))
                 (declare (special *nesting-possible-p*))
                 (unless (= level 0)
                   (format out "("))
                 (loop for i upfrom 0
                       for rest on arglist
                       do (unless (zerop i)
                            (format out " "))
                          (print-arg (car rest) level)
                          ;; Handle (&WHOLE FORM NAME . ARGS) and similar.
                          (unless (listp (cdr rest))
                            (format out " . ")
                            (print-arg (cdr rest) level)))
                 (unless (= level 0)
                   (format out ")")))))
          (print-arglist arglist 0))))))

(defun map-dotted (fn list*)
  (if (listp list*)
      (loop for rest on list*
            do (funcall fn (car rest) nil)
               (unless (listp (cdr rest))
                 (funcall (cdr rest) t)))
      (funcall fn list* t)))

(defun mapcan-dotted (fn list)
  (let ((results ()))
    (map-dotted (lambda (x dotp)
                  (push (funcall fn x dotp) results))
                list)
    (apply #'append (reverse results))))


(defvar/autoloaded *document-base-url* nil
  """When *DOCUMENT-BASE-URL* is non-NIL, this is prepended to all
  Markdown relative URLs. It must be a valid URL without no query and
  fragment parts (that is, _http://lisp.org/doc/_ but not
  _http://lisp.org/doc?a=1_ or _http://lisp.org/doc#fragment_). Note
  that intra-page links using only URL fragments (e.g. and explicit
  HTML links (e.g. `<a href="...">`) in Markdown are not
  affected.""")

(defun add-base-url (parse-tree)
  (if *document-base-url*
      (flet ((translate (parent tree)
               (declare (ignore parent))
               (ecase (first tree)
                 ((:explicit-link :reference)
                  (let ((source (pt-get tree :source)))
                    (assert source)
                    (unless (urlp source)
                      (setf (pt-get tree :source)
                            (append-to-url *document-base-url* source)))
                    tree)))))
        (multiple-value-bind (scheme authority path query fragment)
            (parse-url *document-base-url*)
          (declare (ignore path))
          (unless (and scheme authority (null query) (null fragment))
            (error "~@<~S should have scheme and authority ~
                   but no query and fragment parts.~:@>"
                   '*document-base-url*)))
        (map-markdown-parse-tree '(:explicit-link :reference)
                                 '() nil #'translate parse-tree))
      parse-tree))


(defsection @overview-of-escaping (:title "Overview of Escaping")
  """Let's recap how escaping @CODIFICATION,
  [downcasing][\*document-downcase-uppercase-code*], and @LINKING
  works.

  - One backslash in front of a @WORD turns codification off. Use this
    to prevent codification words such as \DOCUMENT, which is all
    uppercase hence @CODIFIABLE, and it names an exported symbol hence
    it is @INTERESTING.

  - One backslash right after an opening backtick turns autolinking
    off.

  - Two backslashes right after an opening backtick turns autolinking
    and downcasing off. Use this for things that are not Lisp code but
    which need to be in a monospace font."""
  """In the following examples capital C/D/A letters mark the presence,
  and a/b/c the absence of codification, downcasing, and autolinking
  assuming all these features are enabled by
  *DOCUMENT-UPPERCASE-IS-CODE*, *DOCUMENT-DOWNCASE-UPPERCASE-CODE*,
  and *DOCUMENT-LINK-CODE*.

      DOCUMENT                => [`document`][1234]    (CDA)
      \DOCUMENT               => DOCUMENT              (cda)
      `\DOCUMENT`             => `document`            (CDa)
      `\\DOCUMENT`            => `DOCUMENT`            (CdA)
      [DOCUMENT][]            => [`document`][1234]    (CDA)
      [\DOCUMENT][]           => [DOCUMENT][1234]      (cdA)
      [`\DOCUMENT`][]         => [`document`][1234]    (CDA) *
      [`\\DOCUMENT`][]        => [`DOCUMENT`][1234]    (CdA)
      [DOCUMENT][dislocated]  => `document`            (CDa)

  Note that in the example marked with `\*`, the single backslash,
  that would normally turn autolinking off, is ignored because it is
  in an explicit link.""")


(defsection @document-implementation-notes
    (:title "Documentation Generation Implementation Notes")
  """Documentation Generation is supported on ABCL, AllegroCL, CLISP,
  \CCL, CMUCL, ECL and SBCL, but their outputs may differ due to the
  lack of some introspective capability. SBCL generates complete
  output. see ARGLIST, DOCSTRING and SOURCE-LOCATION for
  implementation notes.

  In addition, CLISP does not support the ambiguous case of
  @BROWSING-LIVE-DOCUMENTATION because the current implementation
  relies on Swank to list definitions of symbols (as
  [VARIABLE][locative], [FUNCTION][locative], etc), and that simply
  doesn't work.""")


(defun pax-std-env (fn)
  ;; FIXME: Add all others too.
  (let ((*document-downcase-uppercase-code* nil)
        (*document-transcribe-check-consistency* (featurep :sbcl))
        (*package* *package*))
    (handler-bind ((warning #'muffle-warning))
      (unwind-protect
           (funcall fn)
        (unintern '@example-section :pax)))))
