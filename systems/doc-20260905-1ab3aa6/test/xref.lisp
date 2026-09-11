(uiop:define-package #:40ants-doc-test/xref
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:testing
                #:deftest)
  (:import-from #:40ants-doc)
  (:import-from #:40ants-doc-full/commondoc/builder)
  (:import-from #:40ants-doc-full/commondoc/reference)
  (:import-from #:40ants-doc/locatives)
  (:import-from #:40ants-doc/reference)
  (:import-from #:common-doc
                #:make-text
                #:make-content)
  (:import-from #:40ants-doc-full/commondoc/xref
                #:xref-name
                #:extract-symbols
                #:fill-locatives
                #:xref-locative
                #:make-xref)
  (:import-from #:40ants-doc-full/commondoc/page)
  (:import-from #:40ants-doc-full/commondoc/format
                #:with-format))
(in-package 40ants-doc-test/xref)


(defun bar ()
  "Returns the answer."
  42)

(defun foo ()
  "This function just calls [BAR][function]."
  (values))


(defgeneric blah (obj))

(defmethod blah ((obj string)))


(40ants-doc:defsection @foo-n-bar (:title "Test section")
  (foo function)
  (bar function))


(deftest test-reference-collection
  (testing "Checking if this section includes two references"
    (let* ((doc (40ants-doc-full/commondoc/builder::to-commondoc @foo-n-bar))
           (references-and-pages (40ants-doc-full/commondoc/reference::collect-references doc))
           (references (mapcar #'car references-and-pages)))
      (ok (= (length references)
             3))
      (let ((objects (sort (mapcar #'40ants-doc/reference::reference-object references)
                           #'string<)))
        (ok (equal objects
                   ;; Sections also should be collected as references
                   '(@foo-n-bar
                     bar
                     foo)))))))


(deftest test-reference-replacing
  (with-format (:html)
    (testing "Simple case"
      (let* ((reference (40ants-doc/reference::make-reference 'foo 'function))
             (doc (40ants-doc-full/commondoc/markdown:parse-markdown "[FOO][function]")))
        (flet ((first-child ()
                 (first (common-doc:children doc))))
          (testing "Before replacing we should have a paragraph with internal link"
            (ok (typep doc 'common-doc:paragraph))
            (ok (typep (first-child) '40ants-doc-full/commondoc/xref:xref)))
      
          (let ((result (40ants-doc-full/commondoc/page::replace-xrefs doc (list (cons reference
                                                                                  :no-page)))))
            (testing "Resulting document should remain the same, because only paragraph's child should be changed"
              (ok (eql doc result)))
            (testing "But it's child should be changed to a real web link"
              (ok (typep (first-child) 'common-doc:document-link)))))))

    (testing "Case when symbol name already the code"
      (let* ((reference (40ants-doc/reference::make-reference 'foo 'function))
             (doc (extract-symbols
                   (40ants-doc-full/commondoc/markdown:parse-markdown "`FOO` function"))))
        (flet ((first-child (node)
                 (first (common-doc:children node))))
          (testing "Before replacing we should have a paragraph with inline code block containing a XREF"
            (ok (typep doc 'common-doc:paragraph))
            (ok (typep (first-child doc)
                       'common-doc:code))
            (ok (typep (first-child
                        (first-child doc))
                       '40ants-doc-full/commondoc/xref:xref)))

          (let ((result (40ants-doc-full/commondoc/page::replace-xrefs doc (list (cons reference
                                                                                  :no-page)))))
          
            (testing "Resulting document should remain the same, because only paragraph's child should be changed"
              (ok (eql doc result)))
            (testing "But it's child should be changed to a real web link containing an inline code block around the text"
              (ok (typep (first-child result)
                         'common-doc:document-link))
              (ok (typep (first-child
                          (first-child result))
                         'common-doc:code))
              (ok (and
                   (typep (first-child
                           (first-child
                            (first-child result)))
                          'common-doc:text-node)
                   (string= (common-doc:text
                             (first-child
                              (first-child
                               (first-child result))))
                            ;; By default, uppercased symbol names
                            ;; are downcased:
                            "foo"))))))))
  
    (testing "Explicit method locative should be replaced with a single link"
      (let* ((generic-reference (40ants-doc/reference::make-reference 'blah 'generic-function))
             (method-reference (40ants-doc/reference::make-reference 'blah '(method () (string))))
             (doc (40ants-doc-full/commondoc/markdown:parse-markdown "[BLAH][(METHOD () (STRING))]")))
        (flet ((first-child ()
                 (first (common-doc:children doc))))
          
          (testing "First child should have given locative"
            (ok (equal (40ants-doc-full/commondoc/xref::xref-locative
                        (first-child))
                       '(method () (string)))))
          (let ((result (40ants-doc-full/commondoc/page::replace-xrefs doc (list (cons generic-reference
                                                                                  :no-page)
                                                                            (cons method-reference
                                                                                  :no-page)))))
            (testing "Resulting document should remain the same, because only paragraph's child should be changed"
              (ok (eql doc result)))
            (testing "But it's child should be changed to a single document link"
              (ok (typep (first-child) 'common-doc:document-link)))))))
  
    (testing "XREF nested in CODE block should not be replaced with just XREF"
      (let* ((original-xref (make-xref "FOO"))
             (doc (common-doc:make-code original-xref)))
      
        (let ((result (40ants-doc-full/commondoc/page::replace-xrefs doc nil)))
          (testing "Resulting document should remain the same"
            (ok (eql result original-xref))))))
  
    (testing "XREF not-nested in CODE block should not be surrounded by one"
      (let* ((original-xref (make-xref "FOO"))
             (doc (common-doc:make-content original-xref)))
      
        (let ((result (40ants-doc-full/commondoc/page::replace-xrefs doc nil)))
          (testing "Resulting document should remain the same"
            (ok (eql doc result)))

          (testing "And now it should contain original xref"
            (ok (eql (first (common-doc:children result))
                     original-xref))))))))


(deftest test-filling-locatives
  (testing "When no locatives are around the xref"
    (let* ((xref (make-xref "LISP"))
           (doc (make-content
                 (list (make-text "Hello")
                       xref
                       (make-text "World")))))
      (fill-locatives doc)
      (ok (null (xref-locative xref)))))
  
  (testing "When locative is on the right"
    (let* ((xref (make-xref "LISP"))
           (doc (make-content
                 (list (make-text "This is a ")
                       xref
                       (make-text " function.")))))
      (fill-locatives doc)
      (ok (eql (xref-locative xref)
               '40ants-doc/locatives:function))))
  
  (testing "When locative is on the left"
    (let* ((xref (make-xref "FOO"))
           (doc (make-content
                 (list (make-text "It returns an object of class ")
                       xref
                       (make-text " and calls a function.")))))
      (fill-locatives doc)
      (ok (eql (xref-locative xref)
               '40ants-doc/locatives:class))))
  
  (testing "When locative is on the left if it is preceeded by another XREF"
    (let* ((xref (make-xref "DEPTH"))
           (another-xref (make-xref "SOME"))
           (doc (common-doc:make-unordered-list
                 (common-doc:make-list-item
                  (list another-xref
                        (make-text " macro accepts key argument ")
                        xref
                        (make-text " to turn this feature on."))))))
      (fill-locatives doc)
      (ok (eql (xref-locative xref)
               '40ants-doc/locatives:argument)))))


(defun foo-bar ()
  )


(deftest test-symbol-extraction
  (testing "When TEXT-NODE contains an uppercased symbol name, it should be transformed into the XREF node."
    (let* ((doc (make-content
                 (list (make-text "This is a FOO-BAR function."))))
           (result (extract-symbols doc)))
      (let* ((content (first (common-doc:children result)))
             (children (common-doc:children content)))
        (testing "Now text node should be replaced with a content-node"
          (ok (typep content 'common-doc:content-node)))

        (testing "Instead of one text node there should be 3 nodes now"
          (ok (= (length children) 3))
          (ok (typep (first children) 'common-doc:text-node))
          (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
          (ok (typep (third children) 'common-doc:text-node))))))

  (testing "Symbol name may start from numbers"
    (let* ((doc (make-content
                 (list (make-text "This is a 40ANTS-DOC system."))))
           (result (extract-symbols doc)))
      (let* ((content (first (common-doc:children result)))
             (children (common-doc:children content)))
        (testing "Now text node should be replaced with a content-node"
          (ok (typep content 'common-doc:content-node)))

        (testing "Instead of one text node there should be 3 nodes now"
          (ok (= (length children) 3))
          (ok (typep (first children) 'common-doc:text-node))
          (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
          (ok (string= (xref-name (second children))
                       "40ANTS-DOC"))
          (ok (typep (third children) 'common-doc:text-node))))))

  (testing "Numbers should not be considered symbols."
    (let* ((doc (make-content
                 (list (make-text "This is a magic 100500 number."))))
           (result (extract-symbols doc)))
      (let* ((children (common-doc:children result)))
        (testing "Now text node should not be replaced with a content-node"
          (ok (= (length children) 1))
          (ok (typep (first children) 'common-doc:text-node))))))
  
  (testing "This also should work for variables"
    (let* ((doc (make-text "This is a *SOME-VAR* var."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "*SOME-VAR*"))))

  (testing "And for constants"
    (let* ((doc (make-text "This is a +SOME-VAR+ const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "+SOME-VAR+"))))

  (testing "It should be possible to parse keywords as well"
    (let* ((doc (make-text "This is a :SOME-VAR const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   ":SOME-VAR"))))

  
  (testing "Also, it might be prepended with a package name"
    (let* ((doc (make-text "This is a SOME-PACKAGE:+SOME-VAR+ const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "SOME-PACKAGE:+SOME-VAR+"))))
  
  (testing "And not external symbols are allowed too (but this is a bad taste :))"
    (let* ((doc (make-text "This is a SOME-PACKAGE::+SOME-VAR+ const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "SOME-PACKAGE::+SOME-VAR+"))))

  (testing "And package name might contain slashes"
    (let* ((doc (make-text "This is a SOME/PACKAGE::+SOME-VAR+ const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "SOME/PACKAGE::+SOME-VAR+"))))

  (testing "And also dots"
    (let* ((doc (make-text "This is a SOME.PACKAGE::+SOME-VAR+ const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "SOME.PACKAGE::+SOME-VAR+"))))
  
  (testing "Also, uppercased words starting from @ are matched"
    (let* ((doc (make-text "This is a SOME-PACKAGE:@SOME-SECTION const."))
           (result (extract-symbols doc))
           (children (common-doc:children result)))
      (ok (= (length children) 3))
      (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
      (ok (string= (xref-name (second children))
                   "SOME-PACKAGE:@SOME-SECTION")))))


(deftest test-symbol-extraction-can-remove-punctuation-only-if-symbol-found 
  (testing "When uppercased text corresponds to a known symbol and ends with a punctuation, then we should extract into xref only a symbol name"
    (let* ((doc (make-content
                 (list (make-text "This is a FOO-BAR."))))
           (result (extract-symbols doc)))
      (let* ((content (first (common-doc:children result)))
             (children (common-doc:children content)))
        (testing "Now text node should be replaced with a content-node"
          (ok (typep content 'common-doc:content-node)))

        (testing "Instead of one text node there should be 3 nodes now"
          (ok (= (length children) 3))
          (ok (typep (first children) 'common-doc:text-node))
          (ok (string= (common-doc:text (first children))
                       "This is a "))
          
          (ok (typep (second children) '40ants-doc-full/commondoc/xref::xref))
          (ok (string= (xref-name (second children))
                       "FOO-BAR"))
          
          (ok (typep (third children) 'common-doc:text-node))
          (ok (string= (common-doc:text (third children))
                       ".")))))))
