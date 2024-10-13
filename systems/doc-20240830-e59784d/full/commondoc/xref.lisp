(uiop:define-package #:40ants-doc-full/commondoc/xref
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-doc.ops)
  (:import-from #:common-doc.format)
  (:import-from #:40ants-doc-full/commondoc/html
                #:with-html)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:40ants-doc-full/commondoc/utils
                #:read-locative
                #:left-word
                #:right-word)
  (:import-from #:cl-ppcre)
  (:import-from #:40ants-doc-full/utils
                #:maybe-downcase)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:with-node-package)
  (:import-from #:commondoc-markdown)
  (:import-from #:40ants-doc/object-package)
  (:import-from #:40ants-doc-full/link)
  (:export
   #:make-xref
   #:xref
   #:xref-name
   #:xref-symbol
   #:xref-locative
   #:fill-locatives
   #:link-text
   #:extract-xrefs))
(in-package #:40ants-doc-full/commondoc/xref)


(defclass xref (common-doc:document-node)
  ((name :accessor xref-name
         :initarg :name
         :type (or string
                   common-doc:document-node)
         :documentation "Original text, found in a documentation string")
   (symbol :accessor xref-symbol
           :initarg :symbol
           :type (or null symbol)
           :documentation "A symbol, matched to a XREF-NAME.

                           I can be NIL if no symbol was found.
                           In this case a warning will be shown.")
   (locative :accessor xref-locative
             :initarg :locative
             :type (or null symbol list)
             :documentation "Sometime xref might be followed by a locative name.
                             In this case this slot will be filled with a corresponding
                             locative symbol from 40ANTS-DOC/LOCATIVES package.

                             In some cases locative might be a list. For example METHOD
                             locative has a few arguments and XREFS to methods might
                             be like that (METHOD :AFTER (STRING T))")
   (format :reader xref-preferred-format
           :initform nil
           :initarg :format
           :type (or null symbol)
           :documentation "If this slot is not NULL then XREF will exists only
                           in given document format and all pages in other format will
                           link to the format from this slot.

                           This makes it possible to force linking from Markdown documents
                           to HTML in case if HTML entity representation is provides more information."))
  (:documentation "A link some entity, refered in markdown as a link like [Some text][the-id]
                   or just being UPPERCASED-SYMBOL mentioned."))


(defun make-xref (name &key symbol locative format)
  (check-type name (or string
                       common-doc:document-node))
  (check-type symbol (or null symbol))
  (check-type locative (or null symbol list))

  (when (typep locative 'list)
    (check-type (first locative)
                symbol))
  
  (make-instance 'xref
                 :name name
                 :symbol symbol
                 :locative locative
                 :format format))


(defmethod common-doc:text ((xref xref))
  (xref-name xref))


(defmethod print-object ((xref xref) stream)
  (print-unreadable-object (xref stream :type t)
    (format stream "~S~:[~; ~A~]"
            (or (xref-symbol xref)
                (xref-name xref))
            (xref-locative xref)
            (xref-locative xref))))


(defun fill-locatives (node)
  "This goes through nodes tree and fills LOCATIVE slot of XREF objects
   in case if this XREF is prepended or followed by a locative word like
   \"macro\" or \"function\"."

  (let ((locative-on-the-left nil)
        (prev-xref nil))
    (labels ((filler (node)
               (typecase node
                 (common-doc:text-node
                  (when (and prev-xref
                             (null (xref-locative prev-xref)))
                    (let* ((word (left-word node))
                           (locative-on-the-right (read-locative word)))
                      (setf (xref-locative prev-xref)
                            locative-on-the-right)))

                  (setf prev-xref
                        nil)

                  (setf locative-on-the-left
                        (read-locative (right-word node))))
                 (xref
                  (cond
                    (locative-on-the-left
                     (setf (xref-locative node)
                           locative-on-the-left)
                     (setf locative-on-the-left
                           nil))
                    (t
                     (setf prev-xref
                           node)))))
               (values node)))
      (40ants-doc-full/commondoc/mapper:map-nodes node
                                             #'filler)))
  node)


(defun extract-xrefs (node)
  "Returns a list of XREF nodes. Useful for debugging."

  (let ((results nil))
    (labels ((extractor (node)
               (when (typep node 'xref)
                 (push node results))
               (values node)))
      (40ants-doc-full/commondoc/mapper:map-nodes node
                                             #'extractor))
    results))


(defun all-digits (text)
  (loop for char across text
        always (digit-char-p char)))


(defun valid (symbol-name)
  (loop with trimmed-string = (string-left-trim
                               ;; it is ok for symbol to start with : if it is a keyword
                               ;; or from the @ if it is a section name
                               ;; but the rest should include at least one ASCII symbol
                               '(#\: #\@)
                               symbol-name)
        for char across trimmed-string
          thereis (alpha-char-p char)))


(defun extract-symbols-from-text (node)
  ;; TODO: Find if this a replacement for FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE.
  (let ((text (common-doc:text node))
        (new-nodes nil)
        (processed-to-idx 0))

    (cl-ppcre:do-matches (start end "([A-Z0-9][A-Z0-9-/.]+::?)?[+*@&:]?[A-Z0-9][A-Z0-9-]*[A-Z0-9]+[+*]?" text)
      (let ((symbol-name (subseq text start end)))
        (when (valid symbol-name)
          (when (> start processed-to-idx)
            (push (common-doc:make-text (subseq text processed-to-idx start))
                  new-nodes))

          (let* (;; (*package* (or
                 ;;             ;; For some reason, read-locative-from-string
                 ;;             ;; does now work when current package is COMMON-LISP
                 ;;             (40ants-doc-full/utils:get-package-from-symbol-name symbol-name)
                 ;;             *package*))
                 (symbol (40ants-doc-full/utils:get-symbol-from-string symbol-name)))

            ;; (when (string-equal symbol-name
            ;;                     "40ANTS-DOC/DOC:@DOCUMENTATION-PRINTER-VARIABLES")
            ;;   (break))
            
            (push (make-xref symbol-name
                             :symbol symbol)
                  new-nodes))

          (setf processed-to-idx end))))

    (when (< processed-to-idx
             (length text))
      (let ((text (subseq text processed-to-idx)))
        (push (common-doc:make-text text)
              new-nodes)))

    (cond
      ((> (length new-nodes) 1)
       (common-doc:make-content (nreverse new-nodes)))
      ((= (length new-nodes) 1)
       (first new-nodes))
      (t
       node))))


(defmethod 40ants-doc/object-package::object-package ((obj common-doc:document))
  *package*)

(defmethod 40ants-doc/object-package::object-package ((obj common-doc:document-node))
  *package*)


(defun extract-symbols (node &aux inside-code-block inside-inline-code)
  "Extracts non marked up symbols from COMMON-DOC:TEXT-NODE and replaces them with XREF objects."
  
  (labels
      ((go-down (node)
         ;; We need this flag because we want to turn off
         ;; symbol extraction inside code blocks
         (when (typep node
                      'common-doc:code-block)
           (setf inside-code-block t))
         (when (typep node
                      'common-doc:code)
           (setf inside-inline-code t)))
       (go-up (node)
         (when (typep node
                      'common-doc:code-block)
           (setf inside-code-block nil))
         (when (typep node
                      'common-doc:code)
           (setf inside-inline-code nil)))
       (extractor (node)
         (typecase node
           (common-doc:text-node
            (cond ((or inside-code-block
                       40ants-doc-full/commondoc/mapper::*inside-title*
                       40ants-doc-full/commondoc/mapper::*inside-link*)
                   node)
                  (inside-inline-code
                   ;; If whole content of inline code is recognized
                   ;; as a symbol, then we'll replace it with XREF:
                   (let ((result (extract-symbols-from-text node)))
                     (if (and (typep result 'xref)
                              (= (length (common-doc:text node))
                                 (length (common-doc:text result))))
                         result
                         node))
                   )
                  (t
                   (extract-symbols-from-text node))))
           (t node))))
    ;; Here we we need to change *package*
    ;; to make sure, that all symbol mentions are parsed as if we being
    ;; in the package where DOCUMENTATION-SECTION was defined.
    (with-node-package
      (40ants-doc-full/commondoc/mapper:map-nodes node #'extractor
                                                  :on-going-down #'go-down
                                                  :on-going-up #'go-up))))


(defgeneric link-text (object)
  (:documentation "Returns a string to be used as a text of `<a href=\"\">(link-text object)</a>` element.

                   By default, a symbol name will be used.")
  
  (:method ((object t))
    nil))


(define-emitter (obj xref)
  "Emit an reference which was not processed by 40ANTS-DOC-FULL/COMMONDOC/PAGE::REPLACE-XREFS function."
  (with-html
    (let ((name (xref-name obj)))
      (typecase name
        (string
         (if 40ants-doc-full/link:*document-link-code*
             (:code :class "unresolved-reference"
                    :title "Reference not found."
                    name)
             (:code name)))
        (t
         (common-html.emitter::emit (maybe-downcase name)))))))


(defmethod common-doc.format:emit-document ((format commondoc-markdown:markdown)
                                            (node xref)
                                            stream)
  (let ((name (xref-name node)))
    (typecase name
      (string
       (format stream "`~A`"
               name))
      (t
       (common-doc.format:emit-document format (maybe-downcase name) stream)))))


;; TODO: make a pull to make this generic a public:
(defmethod common-doc.ops::node-text ((node xref))
  "Extract text from a content node."
  (let ((name (xref-name node)))
    (typecase name
      (string
       (maybe-downcase name))
      (t
       (common-doc.ops::node-text name)))))
