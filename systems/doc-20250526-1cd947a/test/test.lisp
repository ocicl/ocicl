(uiop:define-package #:40ants-doc-test/test
  (:use #:cl
        #:40ants-doc/locatives)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc-full/doc)
  (:import-from #:40ants-doc-full/args)
  (:import-from #:40ants-doc-full/builder)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc-full/page)
  (:import-from #:common-html)
  (:import-from #:commondoc-markdown)
  (:import-from #:alexandria)
  (:import-from #:rove
                #:ok
                #:deftest
                #:testing)
  (:import-from #:40ants-doc-full/utils
                #:transform-tree
                #:symbol-name-p)
  (:import-from #:40ants-doc-test/utils
                #:get-files-diff))
(in-package 40ants-doc-test/test)


(defsection @test (:export nil)
  "[*TEST-VARIABLE*][]
  [`*TEST-VARIABLE*`][]
  [*test-variable*][]
  [`*test-variable*`][]
  [40ants-doc-test/test:*test-variable*][]
  FOO function, function FOO,
  `FOO` function, function `FOO`,
  FOO `function`, `function` FOO,
  `FOO` `function`, `function` `FOO`,
  [foo][function],
  [foo][FUNCTION],
  [FOO][function],
  [FOO][FUNCTION],
  [`foo`][function],
  [`foo`][FUNCTION],
  [`FOO`][function],
  [`FOO`][FUNCTION],

  FOO-A `(accessor foo)`, `(accessor foo)` FOO-A,
  `FOO-A` `(accessor foo)`, `(accessor foo)` `FOO-A`,
  [foo-a][(accessor foo)],
  [foo-a][(ACCESSOR FOO)],
  [FOO-A][(accessor foo)],
  [FOO-A][(ACCESSOR FOO)],
  [`foo-a`][(accessor foo)],
  [`foo-a`][(ACCESSOR FOO)],
  [`FOO-A`][(accessor foo)],
  [`FOO-A`][(ACCESSOR FOO)]

  ->MAX

  Escaped: \\FOO [`FOO`][dislocated] *\\NAVIGATION-TEST-CASES*
  Non escaped: FOO *TEST-VARIABLE*
  @TEST-OTHER

  This should be no link because the page of @TEST-EXAMPLES
  has :URI-FRAGMENT NIL.

  This is code: T

  Plural uppercase ambiguous symbol: see FOOs

  Plural uppercase symbol: TEST-GFs

  Plural uppercase dislocated symbol: ->MAXs
  
  See
  FOO compiler-macro

  See FOO
  compiler-macro

  See
  compiler-macro FOO

  See compiler-macro
  FOO

  See
  compiler-macro 
  FOO

  See
  FOO

  ```cl-transcript
  (values (print (1+ 2)) :aaa)
  ..
  .. 3 
  => 3
  => :AAA
  ```

  ```cl-transcript
  (values '(1 2) '(3 4))
  ;=> (1 2)
  ;=> (3
  ;->  4)
  ```

  ```cl-transcript
  (make-array 12 :initial-element 0d0)
  => #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
       0.0d0)
  ```

  In documentation, when the only ambiguity is between a generic
  function and its methods, it's resolved in favor if the gf:
  TEST-GF."
  (foo function)
  (foo compiler-macro)
  (foo-a (accessor foo))
  (*test-variable* variable)
  (@test-examples section)
  (test-gf generic-function)
  (test-gf (method () (number)))
  (test-gf (method () ((eql 7))))
  (@test-section-with-link-to-other-page-in-title section)
  (@test-section-with-link-to-same-page-in-title section)
  (@test-tricky-title section))

(defsection @test-examples (:export nil)
  "example section")

(defsection @test-other (:export nil :title "Test other title")
  "backlink @TEST")

(defsection @test-section-with-link-to-other-page-in-title
    (:title "Link to @TEST-OTHER"
     :link-title-to (@test-other section))
  "Same link in docstring to @TEST-OTHER.")

(defsection @test-section-with-link-to-same-page-in-title
    (:title "Link to @TEST" :link-title-to (@test section))
  "Same link in docstring to @TEST.")

(defsection @test-tricky-title
    ;; TODO: new builder version should parse section titles as markdown
    (:export nil :title "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>")
  "backlink @TEST")

(defun foo ())
(define-compiler-macro foo ())
(defclass foo ()
  ((a :accessor foo-a)
   (r :reader foo-r)
   (w :writer foo-w)))
(defvar foo-a)
(defvar foo-b)
(defvar foo-c)

(defparameter *test-variable*
  '(xxx 34))

(defmacro bar ())
(deftype bar () 'null)
(defconstant bar 2)

(defgeneric baz ())
(defvar baz)
(defstruct baz
  aaa)

(defgeneric test-gf (x))
(defmethod test-gf ((x number)))
(defmethod test-gf ((x (eql 7))))

(defun ->max ())

(defparameter *navigation-test-cases*
  ;; (symbol locative prefix &optional alternative-prefix)
  '((foo function (defun foo))
    (foo type (defclass foo))
    (foo class (defclass foo))
    (foo compiler-macro (define-compiler-macro foo))
    (foo-a (accessor foo) (defclass foo) (a :accessor foo-a))
    (foo-r (reader foo) (defclass foo) (r :reader foo-r))
    (foo-w (writer foo) (defclass foo) (w :writer foo-w))
    (foo-a variable (defvar foo-a))
    (foo-b variable (defvar foo-b))
    (foo-c variable (defvar foo-c))
    (bar macro (defmacro bar))
    (bar type (deftype bar))
    (bar constant (defconstant bar))
    (baz generic-function (defgeneric baz))
    (baz variable (defvar baz))
    (40ants-doc-full/doc::@index section (defsection @index))
    (baz-aaa structure-accessor (defstruct baz))
    (40ants-doc package
     (cl:defpackage)
     (uiop:define-package))
    (40ants-doc system ())
    ;; Allegro has the location off by one form.
    #-allegro
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))))


(defun working-locative-p (locative)
  (declare (ignorable locative))
  ;; AllegroCL doesn't store source location for DEFPACKAGE.
  #+allegro (not (eq locative 'package))
  #-allegro t)


(deftest test-navigation
  (dolist (test-case *navigation-test-cases*)
    (destructuring-bind
        (symbol locative prefix &optional alternative-prefix) test-case
      (testing (format nil "(~S ~S)"
                       symbol locative)
        (when (working-locative-p locative)
          (let ((location (40ants-doc/source-api:find-source
                           (40ants-doc/locatives/base:locate symbol locative))))
            (ok (not (eq :error (first location)))
                (format nil "Could not find source location for (~S ~S)"
                        symbol locative))
            (let* ((file (second (second location)))
                   (position (1- (second (third location))))
                   (form (let ((*package* (find-package :40ants-doc-test/test)))
                           (read-form-from-file-position file position))))
              (ok
               (or (alexandria:starts-with-subseq prefix form
                                                  :test #'equal)
                   (and alternative-prefix
                        (alexandria:starts-with-subseq
                         alternative-prefix form :test #'equal)))
               (format nil "Could not find prefix ~S~@[ or ~S~] ~
                            at source location~%~S~%for reference (~S ~S).~%~
                            Form found was:~%~S."
                       prefix alternative-prefix
                       location symbol locative form)))))))))


(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))


(defvar *muffle-warnings* t)


(deftest test-symbol-name-predicate
  (testing "Symbol names"
    (ok (symbol-name-p "LOCATIVE"))
    (ok (symbol-name-p "40ANTS-DOC::SECTION"))
    (ok (symbol-name-p "DOCUMENT-OBJECT"))
    (ok (symbol-name-p "*DOCUMENT-UPPERCASE-IS-CODE*"))
    (ok (symbol-name-p "WITH/PACKAGE:*DOCUMENT-UPPERCASE-IS-CODE*")))
  
  (testing "Not symbol names"
    (ok (not (symbol-name-p "1")))
    (ok (not (symbol-name-p "100500")))
    (ok (not (symbol-name-p "Section")))
    (ok (not (symbol-name-p "colorize")))
    (ok (not (symbol-name-p "M-.")))
    (ok (not (symbol-name-p "==>")))
    (ok (not (symbol-name-p "..")))
    (ok (not (symbol-name-p "-->")))
    (ok (not (symbol-name-p "elisp/transcribe.el")))
    (ok (not (symbol-name-p "#\\|")))
    (ok (not (symbol-name-p "(UP DIRECTION)")))))


(deftest test-transform-tree
  (ok (equal '(1)
             (transform-tree
              (lambda (parent a)
                (declare (ignore parent))
                (values a (listp a) nil))
              '(1))))

  (ok (equal '(2 (3 (4 5)))
             (transform-tree
              (lambda (parent a)
                (declare (ignore parent))
                (values (if (listp a) a (1+ a))
                        (listp a)
                        nil))
              '(1 (2 (3 4))))))
  
  (ok (equal '(1 2 (2 3 (3 4 4 5)))
             (transform-tree
              (lambda (parent a)
                (declare (ignore parent))
                (values (if (listp a)
                            a
                            (list a (1+ a)))
                        (listp a)
                        (not (listp a))))
              '(1 (2 (3 4)))))))


(deftest test-macro-arg-names
  (ok (equal '(x a b c)
             (40ants-doc-full/args::macro-arg-names
              '((&key (x y)) (a b) &key (c d))))))


(defun test-document (format)
  ;; To ensure the URI-FRAGMENT will always exist in the keyword package.
  ;; Because at some point CI on GitHub was broken (probably because uri-fragment
  ;; keyword was added by some other ASDF system and disappeared.
  (alexandria:make-keyword "URI-FRAGMENT")
  
  (destructuring-bind (output-dir &rest pages-pathnames)
      (multiple-value-list
       (write-test-document-files
        (asdf:system-relative-pathname :40ants-doc "test/data/tmp/")
        format))
    
    (ok (pathnamep output-dir))
    
    (dolist (output pages-pathnames)
      (let ((baseline (make-pathname
                       :directory (substitute "baseline" "tmp"
                                              (pathname-directory output)
                                              :test #'equal)
                       :defaults output)))
        (unless (string= (alexandria:read-file-into-string baseline)
                         (alexandria:read-file-into-string output))
          (cerror "Update output file."
                  "~@<Output ~S ~_differs from baseline ~S:~2%~A~@:>"
                  output baseline
                  (get-files-diff baseline
                                  output))
          (update-test-document-baseline format))))))


(deftest test-markdown-document
  (test-document :markdown))


(deftest test-html-document
  (test-document :html))


(defun write-test-document-files (basedir format)
  (let ((pages (list (40ants-doc-full/page:make-page @test-other
                                                     :base-filename "other/test-other")
                     (40ants-doc-full/page:make-page @test
                                                     :base-filename "test"))))

    (40ants-doc-full/builder:render-to-files pages
                                             :base-dir basedir
                                             :base-url "https://40ants.com/doc/"
                                             :format (ecase format
                                                       (:markdown 'commondoc-markdown:markdown)
                                                       (:html 'common-html:html))
                                             :downcase-uppercase-code (eq format :html))))

(defun update-test-document-baseline (format)
  (write-test-document-files
   (asdf:system-relative-pathname :40ants-doc "test/data/baseline/")
   format))


(deftest test-core-dependencies
  (ok (equal (40ants-doc-full/utils::external-dependencies :40ants-doc)
             (list "asdf"
                   "mgl-pax-bootstrap"  ;; dependency of named-readtables
                   "named-readtables"
                   "pythonic-string-reader"
                   "uiop"))))
