(in-package :mgl-pax-test)

(defmacro dref-set= (set-1 set-2)
  `(endp (different-elements (dref::sort-references ,set-1)
                             (dref::sort-references ,set-2)
                             :pred (lambda (r1 r2)
                                     (and (typep r1 'xref)
                                          (typep r2 'xref)
                                          (xref= r1 r2))))))

(defun readtable-with-case (mode &optional (readtable *readtable*))
  (if (eq (readtable-case readtable) mode)
      readtable
      (let ((readtable (copy-readtable readtable)))
        (setf (readtable-case readtable) mode)
        readtable)))


(deftest test-navigate ()
  (test-depluralize)
  (test-map-names-in-raw)
  (test-parse-locative)
  (test-definitions-of-wall)
  (test-names-or-locatives-for-emacs)
  (test-locate)
  (test-navigation-to-source))

(deftest test-depluralize ()
  (is (equal (mgl-pax::depluralize "classes")
             '("classe" "class" "clas"))))

(defun list-names-in-raw (raw)
  (let ((names ()))
    (mgl-pax::map-names-in-raw (lambda (raw name)
                                 (declare (ignore raw))
                                 (push name names))
                               raw nil)
    (reverse names)))

(deftest test-map-names-in-raw ()
  (let ((*package* (find-package :mgl-pax-test))
        (*readtable* (readtable-with-case :upcase)))
    (is (equal (list-names-in-raw "foo") '(foo "foo" "FOO")))
    (is (equal (list-names-in-raw "FOO") '(foo "FOO")))
    (is (equal (list-names-in-raw "Foo") '("Foo" "FOO")))
    (is (equal (list-names-in-raw "|Foo|") '(|Foo|)))
    (is (equal (list-names-in-raw "\"foo\"") '("foo")))))

(deftest test-parse-locative ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (null (mgl-pax::parse-locative "non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (mgl-pax::parse-locative "find")))
    (is (null (mgl-pax::parse-locative "")))
    (is (match-values (mgl-pax::parse-locative "function")
          (eq * 'function)
          (= * 8)))
    (is (match-values (mgl-pax::parse-locative " function")
          (eq *'function)
          (= * 9)))
    (is (match-values (mgl-pax::parse-locative "function ")
          (eq * 'function)
          (= * 9)))
    (is (match-values (mgl-pax::parse-locative "function junk")
          (null *)))
    (is (match-values (mgl-pax::parse-locative "function junk" :junk-allowed t)
          (eq * 'function)
          (= * 9)))
    (is (null (mgl-pax::parse-locative "(function non-interned)")))
    (with-test ("markdown and M-.")
      (dolist (string '("function." "function," "function;" "function:"
                        "function`" "function'" "function>" "<function>"
                        "\"function\""))
        (is (eq (mgl-pax::parse-locative-around (% string)) 'function))))))

;;; We have no Emacs based tests. This tests the CL side of `M-.' when
;;; it's invoked with point on FOO in the test cases below. The actual
;;; OBJECT-AND-LOCATIVES-LIST argument that mgl-pax.el would send is
;;; reproduced explicitly.
(deftest test-definitions-of-wall ()
  (let ((*package* (find-package '#:mgl-pax-test)))
    ;; xxx FOO function
    (check-dowall '(("FOO" ("xxx" "function")))
                  '((foo function)))
    ;; function FOO xxx
    (check-dowall '(("FOO" ("function" "xxx")))
                  '((foo function)))
    ;; xxx [foo][function] xxx
    (check-dowall '(("[foo][function]" ("xxx" "xxx"))
                    ("foo" ("function")))
                  '((foo function)))
    ;; function FOO compiler-macro
    (check-dowall '(("FOO" ("function" "compiler-macro")))
                  '((foo compiler-macro)
                    (foo function)))
    (with-failure-expected ((alexandria:featurep :clisp))
      ;; xxx FOO xxx
      (check-dowall '(("FOO" ("xxx" "xxx")))
                    '((foo class)
                      (foo compiler-macro)
                      (foo function)))
      ;; xxx [foo][] xxx
      (check-dowall '(("[foo][]" ("xxx" "xxx"))
                      ("foo" ("xxx")))
                    '((foo class)
                      (foo compiler-macro)
                      (foo function)))
      ;; xxx [foo][xxx] xxx
      (check-dowall '(("[foo][xxx]" ("xxx" "xxx"))
                      ("foo" ("xxx")))
                    '((foo class)
                      (foo compiler-macro)
                      (foo function))))
    (with-failure-expected ((alexandria:featurep :abcl))
      ;; pax
      (check-dowall '(("pax" ()))
                    '(("MGL-PAX" package)))
      ;; "MGL-PAX"
      (check-dowall '(("\"MGL-PAX\"" ()))
                    '(("MGL-PAX" package)
                      ("mgl-pax" asdf:system)))
      ;; MGL-PAX
      (check-dowall '(("MGL-PAX" ()))
                    '(("MGL-PAX" package)
                      ("mgl-pax" asdf:system)))
      ;; :MGL-PAX
      (check-dowall '((":MGL-PAX" ()))
                    '(("MGL-PAX" package)
                      ("mgl-pax" asdf:system))))
    (with-test ("prefer uppercase")
      (check-dowall '(("XXXS" ()))
                    '((xxxs function)))
      (check-dowall '(("XXXs" ()))
                    '((xxx function))))
    (with-test ("deduplication")
      (check-dowall '(("print" ("function")) ("print" ("function")))
                    '((print function)))
      (check-dowall '(("lambda lis" ("clhs")) ("Lambda  Lists" ("clhs")))
                    '(("3.4" (clhs section)))))
    #+sbcl
    (with-test ("unreadable")
      (check-dowall '(("test-gf"
                       ("(method ((eql #<package \"COMMON-LISP\">)))")))
                    `((test-gf (method ((eql ,(find-package :cl))))))))))

(defun check-dowall (wall expected-refs)
  (let ((refs (mapcar (lambda (ref)
                        (list (xref-name ref)
                              (xref-locative ref)))
                      (dref::sort-references (pax::definitions-of-wall wall)))))
    (is (equal refs expected-refs)
        :ctx ("WORD-AND-LOCATIVES-LIST = ~S" wall))))

(defun sort-emacsrefs (emacsrefs)
  (sort (copy-seq emacsrefs) (lambda (r1 r2)
                               (or (string< (first r1) (first r2))
                                   (and (string= (first r1) (first r2))
                                        (string< (second r1) (second r2)))))))


(deftest test-names-or-locatives-for-emacs ()
  (let ((x (mgl-pax::names-or-locatives-for-emacs "print" "")))
    (is (eq (first x) :names))
    (dolist (locative (second x))
      (is (null (find #\Newline locative)))))
  (let ((x (mgl-pax::names-or-locatives-for-emacs "package" "")))
    (is (eq (first x) :names))
    (dolist (locative (second x))
      (is (null (search "BASE-CHAR" locative))))))


(deftest test-locate ()
  (test-locate/section)
  (test-locate/glossary-term)
  (test-locate/note)
  (test-locate/go)
  (test-locate/include))

(deftest test-locate/section ()
  (check-ref-sets (definitions '@test-examples)
                  `(,(xref '@test-examples 'section)))
  (with-test ("actualized")
    (check-ref-sets (definitions '@test-examples :dtype 'variable)
                    `(,(xref '@test-examples 'section))))
  (is (dref::locative-subtype-p 'section 'variable)))

(deftest test-locate/glossary-term ()
  (check-ref-sets (definitions '@some-term)
                  `(,(xref '@some-term 'glossary-term)))
  (with-test ("actualized")
    (check-ref-sets (definitions '@some-term)
                    `(,(xref '@some-term 'glossary-term))))
  (is (dref::locative-subtype-p 'glossary-term 'variable)))

(deftest test-locate/note ()
  (check-ref (dref '@1+* 'note nil) '@1+* 'note)
  (check-ref (dref '@1+*/1 'note nil) '@1+*/1 'note)
  (check-ref (dref '@in-1 'note nil) '@in-1 'note)
  (check-ref (dref '@in-2 'note nil) '@in-2 'note)
  (check-ref-sets (definitions '@in-1)
                  `(,(xref '@in-1 'note))))

(deftest test-locate/go ()
  ;; On GitHub, ECL fails with "Detected access to an invalid or
  ;; protected memory address.".
  (with-failure-expected ((alexandria:featurep '(:or :ecl)))
    (signals-not (serious-condition)
      (check-ref (dref 'xxx '(go (foo function)))
                 'xxx '(go (foo function)) 'pax::go-dref)
      (check-ref (dref "xxx" '(go (foo function)))
                 "xxx" '(go (foo function)) 'pax::go-dref)
      (signals (locate-error)
        (dref 'xxx '(go (undefined function))))
      (signals (locate-error :pred "Bad arguments")
        (dref 'xxx '(go 1 2))))))

(deftest test-locate/include ()
  (check-ref (dref nil '(include #.(asdf:system-relative-pathname
                                    "mgl-pax" "HACKING.md")))
             nil '(include #.(asdf:system-relative-pathname
                              "mgl-pax" "HACKING.md"))
             'pax::include-dref)
  (signals-not (locate-error)
    (dref nil '(include "/non-existent/file")))
  (signals-not (locate-error)
    (dref nil '(include (:start (*some-var* variable)))))
  (signals-not (locate-error)
    (dref nil '(include (:end (*some-var* variable)))))
  (signals-not (locate-error :pred "UNDEFINED")
    (dref nil '(include (:start (undefined variable)))))
  (signals-not (locate-error :pred "UNDEFINED")
    (dref nil '(include (:end (undefined variable))))))


;;; Keep *NAVIGATION-TEST-CASES* plus
;;; DREF-TEST::*SOURCE-LOCATION-TEST-CASES* and
;;; `mgl-pax-edit-definitions/test-defs' in test.el in sync.
(defparameter *navigation-test-cases*
  '((mgl-pax::@pax-manual section (defsection @pax-manual))
    (@some-term glossary-term (define-glossary-term @some-term))
    (mgl-pax.el (include #.(asdf:system-relative-pathname
                            :mgl-pax "src/mgl-pax.el")
                 :header-nl "```elisp" :footer-nl "```")
     ";;;; mgl-pax.el --- MGL-PAX Emacs integration -*- lexical-binding: t -*-")
    (foo-example (include (:start (dref-ext:make-source-location function)
                           :end (dref-ext:source-location-p function))
                  :header-nl "```"
                  :footer-nl "```")
     (defun/autoloaded make-source-location))
    (@1+* note (note @1+*))
    (@1+*/1 note (note (@1+*/1 :join #\Newline))
     #+ccl (defun 1+*) #-(or ccl sbcl) (note @1+*))
    (@in-1 note (note @in-1) #-sbcl (defun fn-with-inside-note))
    (@in-2 note (note @in-2) #-sbcl (defun fn-with-inside-note))))

(deftest test-navigation-to-source ()
  (let ((*package* (find-package :mgl-pax-test)))
    (dolist (test-case *navigation-test-cases*)
      (apply #'check-source-location test-case)))
  (signals-not (error)
    (source-location (xref 'function 'locative)))
  (with-failure-expected ()
    (signals-not (error)
      (source-location (xref 'locative 'function)))))
