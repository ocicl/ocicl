(uiop:define-package #:40ants-doc-full/commondoc/page
  (:use #:cl)
  (:import-from #:common-doc
                #:make-code
                #:make-text
                #:make-web-link)
  (:import-from #:common-doc.ops)
  (:import-from #:str)
  (:import-from #:common-html.emitter)
  (:import-from #:40ants-doc-full/commondoc/xref)
  (:import-from #:40ants-doc/locatives)
  (:import-from #:40ants-doc/reference)
  (:import-from #:common-html)
  (:import-from #:40ants-doc-full/commondoc/html
                #:with-html)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:40ants-doc-full/commondoc/section
                #:documentation-section
                #:section-definition)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:current-path
                #:with-node-path)
  (:import-from #:40ants-doc/ignored-words
                #:ignored-in-package
                #:ignored-words
                #:supports-ignored-words-p)
  (:import-from #:40ants-doc-full/commondoc/piece
                #:doc-reference
                #:documentation-piece)
  (:import-from #:40ants-doc-full/utils
                #:maybe-downcase
                #:make-relative-path
                #:is-external
                #:url-join)
  (:import-from #:40ants-doc/object-package
                #:object-package)
  (:import-from #:40ants-doc-full/commondoc/format)
  (:import-from #:40ants-doc-full/page
                #:page-title
                #:page-base-url
                #:base-filename
                #:page-format)
  (:import-from #:40ants-doc-full/rewrite
                #:*clean-urls*)
  (:import-from #:40ants-doc/locatives/base
                #:locative-equal)
  (:import-from #:40ants-doc-full/dislocated-symbols
                #:dislocated-symbols
                #:supports-dislocated-symbols-p)
  (:import-from #:40ants-doc-full/themes/api
                #:with-page-template)
  (:import-from #:40ants-doc
                #:section-ignore-packages
                #:section-external-docs)
  (:import-from #:40ants-doc-full/errors
                #:object-is-not-documented)
  (:export #:make-page
           #:page
           #:make-page-toc
           #:warn-on-missing-exports
           #:warn-on-undocumented-exports
           #:full-filename))
(in-package #:40ants-doc-full/commondoc/page)


(defclass page (40ants-doc-full/page::page-common-mixin
                common-doc:content-node)
  ())


(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "~A children: ~{~A~#[~:;, ~]~}"
            (base-filename page)
            (loop for child in (common-doc:children page)
                  collect (type-of child)))))


(defgeneric full-filename (page &key from)
  (:method :around ((page t) &key from)
    (declare (ignore from))
    
    (40ants-doc-full/rewrite::rewrite-file
     (call-next-method)))
  
  (:method ((page (eql :no-page)) &key from)
    (declare (ignore from))
    "")
  
  (:method ((page page) &key from)
    (check-type from (or page
                         null))
    (if from
        (40ants-doc-full/utils:make-relative-path (full-filename from)
                                                  (full-filename page))
        (let ((base (base-filename page))
              (extension (if (page-format page)
                             (40ants-doc-full/commondoc/format:files-extension (page-format page))
                             (40ants-doc-full/commondoc/format:current-files-extension))))
          ;; Base name can be empty only if FROM argument was given and
          ;; we are generating a link for a cross reference. In this case
          ;; we need to return an empty string to make links use only HTML fragment.
          (if (string= base "")
              base
              (concatenate 'string
                           base
                           "."
                           extension))))))


(defmethod base-filename ((obj (eql :no-page)))
  "If page is unknown, then we'll return an empty name for a file. We need this for unit-tests only."
  "")


(defun make-page (sections base-filename &key title format base-dir base-url)
  (make-instance 'page
                 :title title
                 :children (uiop:ensure-list sections)
                 :base-filename base-filename
                 :base-dir base-dir
                 :base-url base-url
                 :format format))


(defgeneric make-page-toc (page)
  (:method ((page t))
    nil))


(define-emitter (obj page)
  "Emit an piece of documentation."
  (with-page-template ((make-page-uri obj)
                       (page-title obj)
                       :toc (make-page-toc obj))
    (mapc #'common-html.emitter::emit
          (common-doc::children obj))))


(defun emit-search-page (page)
  "Emit an piece of documentation."
  (let* ((uri (make-page-uri page))
         (doc-builder (if *clean-urls*
                          "dirhtml"
                          "html")))
    (with-page-template (uri
                         (page-title page)
                         :toc (make-page-toc page))
      (with-html
        ;; This should go before doctools
        ;; URL_ROOT: document.getElementById('documentation_options').getAttribute('data-url_root'),
        (:script :type "text/javascript"
                 (:raw (format nil "
var DOCUMENTATION_OPTIONS = {
    URL_ROOT: '',
    VERSION: '5.0.0+',
    LANGUAGE: 'en',
    COLLAPSE_INDEX: false,
    BUILDER: '~A',
    FILE_SUFFIX: '.html',
    LINK_SUFFIX: '.html',
    HAS_SOURCE: true,
    SOURCELINK_SUFFIX: '.txt',
    NAVIGATION_WITH_KEYS: false
};
"
                               doc-builder)))
        (:script :src (make-relative-path uri "underscore.js"))
        (:script :src (make-relative-path uri "doctools.js"))
        (:script :src (make-relative-path uri "language_data.js"))
        (:script :src (make-relative-path uri "searchtools.js"))
        (:script :src (make-relative-path uri "searchindex.js"))
        
        (:div :id "search-results")))))


(defun warn-on-missing-exports (node)
  "Checks all documentation pieces if there are some documented but not exported symbols."
  
  (flet ((checker (node)
           (when (and (typep node 'documentation-piece)
                      ;; It is OK to have some documentation section which are not
                      ;; exported, because these can be some inner chapters.
                      (not (typep node '40ants-doc-full/commondoc/section:documentation-section)))
             (let* ((reference (40ants-doc-full/commondoc/piece::doc-reference node))
                    (package (40ants-doc/object-package::object-package node))
                    (obj (40ants-doc/reference::reference-object reference)))
               (when (and (typep obj 'symbol)
                          (not (is-external obj))
                          (not (typep obj 'keyword))
                          (not (ignored-in-package obj package)))
                 (warn "Symbol ~S is documented but not exported from it's package."
                       obj))))
           node))
    (40ants-doc-full/commondoc/mapper:map-nodes node #'checker))
  node)


(defvar *warn-on-undocumented-packages* nil
  "When true, then builder will check if there are other packages of the package-inferred
   system with external but not documented symbols.

   When nil, then external symbols are searched only in packages with at least one documented entity.")

(defun warn-on-undocumented-exports (node references)
  "Checks all documentation pieces if there are some documented but not exported symbols."
  
  (let ((packages nil)
	(common-lisp-package (find-package :common-lisp))
	(keyword-package (find-package :keyword))
        (references-symbols
          (loop for (reference . page) in references
                for obj = (40ants-doc/reference:reference-object reference)
                when (typep obj 'symbol)
                collect obj)))
    (flet ((collect-packages (node)
             (let ((package (40ants-doc/object-package::object-package node)))
               (when (and package
			  (not (eql package
                                    common-lisp-package))
			  (not (eql package
                                    keyword-package))
                          (not (str:starts-with-p "ASDF/"
                                                  (package-name package))))
                 (pushnew package packages)))
             node)
           (documented-p (symbol)
             (member symbol references-symbols)))
      
      (40ants-doc-full/commondoc/mapper:map-nodes node #'collect-packages)
     
      ;; This blocks extends PACKAGES list with all other
      ;; package-inferred packages for the system
      (when *warn-on-undocumented-packages* (loop with primary-names = nil
                                                  for package in packages
                                                  for name = (package-name package)
                                                  for primary-name = (first (str:split "/" name))
                                                  do (pushnew primary-name primary-names
                                                              :test #'string=)
                                                  finally (loop for primary-name in primary-names
                                                                for prefix = (concatenate 'string primary-name "/")
                                                                for sub-packages = (remove-if-not
                                                                                    (lambda (package)
                                                                                      (str:starts-with-p prefix
                                                                                                         (package-name package)))
                                                                                    (list-all-packages))
                                                                do (setf packages
                                                                         (nunion packages
                                                                                 sub-packages)))))

      ;; Now we'll check if some external symbols are absent from REFERENCES
      (loop with undocumented-symbols = (make-hash-table :test 'equal)
            for package in packages
            do (do-external-symbols (symbol package)
                 (unless (or (documented-p symbol)
                             (ignored-in-package symbol package)
                             (and (boundp symbol)
                                  (typep (symbol-value symbol)
                                         '40ants-doc:section)))
                   (push symbol (gethash package undocumented-symbols))))
            finally (unless (zerop (hash-table-count undocumented-symbols))
                      (warn 
                       (with-output-to-string (s)
                         (format s "These symbols are external, but not documented:")
                         (loop for package being the hash-key of undocumented-symbols
                                 using (hash-value symbols)
                               do (format s "~2&  ~A:"
                                          (package-name package))
                                  (loop for symbol in (sort symbols #'string<
                                                            :key #'symbol-name)
                                        do (format s "~&  - ~A"
                                                   symbol)))))))))
  node)

(defun warn-on-references-to-internals (document)
  "This function checks and warns on symbols references using :: notation.

   You shouldn't reference internal symbols in the public documentation.

   It is allowed to reference sections using this internal notations,
   because it is recommended to export only root sections which become
   separate pages."
  (with-node-path
    (flet ((check-xref (node)
             (when (typep node '40ants-doc-full/commondoc/xref:xref)
               (let* ((symbol (40ants-doc-full/commondoc/xref:xref-symbol node))
                      (name (40ants-doc-full/commondoc/xref:xref-name node))
                      (name (etypecase name
                              (common-doc:document-node (common-doc.ops:collect-all-text name))
                              (string name)))
                      (reference-to-section (if symbol
                                                (and (boundp symbol)
                                                     (typep (symbol-value symbol)
                                                            '40ants-doc:section))
                                                ;; When we are referencing entity from
                                                ;; other library, SYMBOL will be NIL,
                                                ;; and if it is a section, then by a convention,
                                                ;; its name should start from @ symbol:
                                                (str:containsp ":@" name))))
                 (when (and (not reference-to-section)
                            (str:containsp "::" name))
                   (warn "External symbol is referenced as internal: ~A mentioned at ~{~A~^ / ~}"
                         name
                         (current-path)))))
             node))
     
      (40ants-doc-full/commondoc/mapper:map-nodes document #'check-xref))))


(defun remove-references-to-other-document-formats (current-page found-references)
  "If there are two references with the same locative, then we need only one leading
   to the page having the same format as the CURRENT-PAGE argument's format."
  (let* ((current-format (page-format current-page))
         (seen-locatives nil)
         (same-format-references
           (loop for (reference . page) in found-references
                 for page-format = (page-format page)
                 when (eql page-format
                           current-format)
                   do (push (40ants-doc/reference:reference-locative reference)
                            seen-locatives)
                   and collect (cons reference page))))
    (append same-format-references
            (loop for (reference . page) in found-references
                  when (and (not (eql (page-format page)
                                      current-format))
                            ;; Here the place where we are filtering out
                            ;; references we've already have in the same format
                            ;; as the CURRENT-PAGE's format:
                            (not (member (40ants-doc/reference:reference-locative reference)
                                         seen-locatives
                                         :test #'equal)))
                    collect (cons reference page)))))


(defun make-page-uri (page &key from-page base-url)
  (let ((base-url (or (page-base-url page)
                      base-url)))
    (40ants-doc-full/rewrite::rewrite-url
     (cond
       ;; Links to HTML pages will be made absolute
       ;; if base HTML URL is known. This could be
       ;; the case when you are rendering a documentation
       ;; to be hosted on site and a README.md to be hosted
       ;; at the GitHub and README references items from
       ;; HTML version of documentation.
       ((and (eql (or (page-format page)
                      40ants-doc-full/commondoc/format::*current-format*)
                  'common-html:html)
             base-url)
        (url-join base-url
                  (full-filename page)))
       ;; When URL should remain relative:
       (t
        (full-filename page :from from-page))))))


(defun replace-xrefs (node known-references
                      &key base-url
                      &aux
                      ignored-words
                      ignored-packages
                      dislocated-symbols
                      current-page
                      inside-code-block
                      pages-stack
                      (common-lisp-package (find-package :common-lisp))
                      (keywords-package (find-package :keyword)))
  "Replaces XREF with COMMON-DOC:DOCUMENT-LINK.

   If XREFS corresponds to HTML page format, and BASE-URL argument is not none,
   then it URL will be absolute otherwise a relative link will be generated.

   Returns links which were not replaced because there wasn't
   a corresponding reference in the KNOWN-REFERENCES argument.

   KNOWING-REFERENCE argument should be a list of pairs
   of a COMMON-DOC:REFERENCE and a 40ANTS-DOC-FULL/COMMON-DOC/PAGE:PAGE objects.

   IGNORED-WORDS will be a list of list of strings where each sublist
   contains words, specified as IGNORE-WORDS argument of the 40ANTS-DOC:DEFSECTION macro.

   IGNORED-PACKAGES will be a list of list of strings where each sublist
   contains packagenames, specified as IGNORE-PACKAGES argument of the 40ANTS-DOC:DEFSECTION macro.
  "
  (with-node-path
    (labels ((collect-dislocated (node)
               (when (supports-dislocated-symbols-p node)
                 (push (dislocated-symbols node)
                       dislocated-symbols)))
             (pop-dislocated (node)
               (when (supports-dislocated-symbols-p node)
                 (pop dislocated-symbols)))
             (collect-ignored-words (node)
               (when (supports-ignored-words-p node)
                 (let ((words (ignored-words node)))
                   (push words
                         ignored-words))))
             (collect-ignored-packages (node)
               (when (typep node 'documentation-section)
                 (let* ((definition (section-definition node))
                        (section-ignored-packages (section-ignore-packages definition)))
                   (push section-ignored-packages
                         ignored-packages))))
             (pop-ignored-words (node)
               (when (supports-ignored-words-p node)
                 (pop ignored-words)))
             (pop-ignored-packages (node)
               (when (typep node 'documentation-section)
                 (pop ignored-packages)))
             (push-page (node)
               (when (typep node 'page)
                 (push node pages-stack)
                 (setf current-page node)))
             (pop-page (node)
               (when (typep node 'page)
                 (pop pages-stack)
                 (setf current-page
                       (car pages-stack))))
             (set-inside-code-block-if-needed (node)
               (when (typep node 'common-doc:code)
                 (setf inside-code-block t)))
             (unset-inside-code-block-if-needed (node)
               (when (typep node 'common-doc:code)
                 (setf inside-code-block nil)))
             (go-down (node)
               (collect-dislocated node)
               (collect-ignored-words node)
               (collect-ignored-packages node)
               (push-page node)
               (set-inside-code-block-if-needed node))
             (go-up (node)
               (pop-dislocated node)
               (pop-ignored-words node)
               (pop-ignored-packages node)
               (pop-page node)
               (unset-inside-code-block-if-needed node))
             (make-code-if-needed (obj &key (maybe-downcase t))
               ;; In some cases text should not be downcased.
               ;; For example, if user intentionally mentioned
               ;; an abbrebiation:
               (when maybe-downcase
                 (setf obj
                       (maybe-downcase obj)))
              
               ;; If obj is already a document node, then we need to leave it unchanged
               ;; because it could be a cross-referenced title, but we don't want
               ;; to make it a code:
               (etypecase obj
                 (common-doc:document-node
                    obj)
                 (t
                    (if inside-code-block
                      (make-text obj)
                      (make-code
                       (make-text obj))))))
             (package-specified (text)
               (find #\: text))
             (in-ignore-list (item ignore-lists)
               (loop for sublist in ignore-lists
                     thereis (member item sublist
                                     :test #'string=)))
             (should-be-ignored-p (text symbol locative)
               (or (and symbol
                        (not (package-specified text))
                        (not (40ants-doc-full/utils:is-external symbol)))

                   (eql locative
                        '40ants-doc/locatives:argument)
                   
                   (in-ignore-list text
                                   ignored-words)
                   ;; This is a special case
                   ;; because we can't distinguish between absent SYMBOL
                   ;; and NIL.
                   (string= text
                            "NIL")
                   (and symbol
                        (or (eql (symbol-package symbol)
                                 common-lisp-package)
                            (eql (symbol-package symbol)
                                 keywords-package)))))
             (apply-replacer (node)
               (handler-bind ((40ants-doc-full/errors:object-is-not-documented
                                (lambda (condition)
                                  ;; (log:error "Handler bind for" node)
                                  (let* ((reference (40ants-doc-full/errors:object-reference condition))
                                         (reference-package (object-package reference)))
                                    (when (and reference-package 
                                               (in-ignore-list (package-name reference-package)
                                                               ignored-packages))
                                      ;; The warning about not-documented object should be
                                      ;; ignored, if this object is defined in one of the
                                      ;; ignored packages:
                                      (muffle-warning))))))
                 
                 (40ants-doc-full/commondoc/mapper:map-nodes node #'replacer
                                                             :on-going-down #'go-down
                                                             :on-going-up #'go-up)))
             (replacer (node)
               (typecase node
                 (common-doc:code
                    ;; Here we replace CODE nodes having only one XREF child
                    ;; with this child
                    (let* ((children (common-doc:children node))
                           (first-child (first children)))
                      ;; We also need to continue replacing on results
                      (cond
                        ((and (= (length children) 1)
                              (typep first-child '40ants-doc-full/commondoc/xref::xref))
                         (apply-replacer first-child))
                        (t node))))
                 (40ants-doc-full/commondoc/xref:xref
                    (let* ((name (40ants-doc-full/commondoc/xref:xref-name node))
                           (text (etypecase name
                                   (string name)
                                   (common-doc:document-node
                                      ;; xref-name might return document-node
                                      (common-doc.ops:collect-all-text name))))
                           (symbol (40ants-doc-full/commondoc/xref:xref-symbol node))
                           (locative (40ants-doc-full/commondoc/xref:xref-locative node))
                           (found-in-dislocated
                             (loop for sublist in dislocated-symbols
                                   thereis (member text sublist
                                                   :test #'string-equal)))
                           (found-references
                             (unless found-in-dislocated
                               (loop for (reference . page) in known-references
                                     ;; This can be a symbol or a string.
                                     ;; For example, for SYSTEM locative, object
                                     ;; is a string name of a system.
                                     ;; 
                                     ;; TODO: Think about a GENERIC to compare
                                     ;;       XREF with references of different locative types.
                                     for reference-object = (40ants-doc/reference::reference-object reference)
                                     when (and (etypecase reference-object
                                                 (symbol
                                                    (eql reference-object
                                                         symbol))
                                                 (string
                                                    ;; Here we intentionally use case insensitive
                                                    ;; comparison, because a canonical reference
                                                    ;; to ASDF system contains it's name in a lowercase,
                                                    ;; but some other locatives like a PACKAGE, might
                                                    ;; keep a name in the uppercase.
                                                    (string-equal reference-object
                                                                  text)))
                                               (or (null locative)
                                                   (locative-equal (40ants-doc/reference::reference-locative reference)
                                                                   locative)))
                                       collect (cons reference
                                                     page))))
                           (found-references
                             (if current-page
                               (remove-references-to-other-document-formats current-page
                                                                            found-references)
                               found-references))
                           (should-be-ignored
                             (unless found-references
                               (or found-in-dislocated
                                   (should-be-ignored-p text symbol locative)))))

                      (cond
                        (should-be-ignored
                         (make-code-if-needed text :maybe-downcase nil))
                        (found-references
                         (labels ((make-link (reference page text)
                                    (let ((page-uri
                                            (when page
                                              (make-page-uri page :from-page current-page
                                                                  :base-url base-url)))
                                          (html-fragment
                                            (40ants-doc-full/utils::html-safe-name
                                             (40ants-doc/reference::reference-to-anchor reference))))
                                      (common-doc:make-document-link page-uri
                                                                     html-fragment
                                                                     (make-code-if-needed text)))))

                           (cond ((= (length found-references) 1)
                                  (destructuring-bind (reference . page)
                                      (first found-references)
                                    (typecase reference
                                      (40ants-doc/reference::external-reference
                                         (let ((url (40ants-doc/reference::external-reference-url reference))
                                               (text (40ants-doc/reference::reference-object reference)))
                                           (make-web-link url
                                                          (make-code-if-needed text))))
                                      (40ants-doc/reference::reference
                                         (let* ((object (40ants-doc/reference:resolve reference))
                                                (text (or (40ants-doc-full/commondoc/xref:link-text object)
                                                          text)))
                                           (make-link reference
                                                      page
                                                      text))))))
                                 (t
                                  (common-doc:make-content
                                   (append (list (make-code-if-needed text)
                                                 (common-doc:make-text " ("))
                                           (loop for (reference . page) in found-references
                                                 for index upfrom 1
                                                 for text = (format nil "~A" index)
                                                 collect (make-link reference page text)
                                                 unless (= index (length found-references))
                                                   collect (common-doc:make-text " "))
                                           (list (common-doc:make-text ")"))))))))
                     
                        (t
                         (warn 'object-is-not-documented
                               :reference node
                               :breadcrumbs (current-path))
                         node))))
                 (t
                    node))))
      (apply-replacer node))))
