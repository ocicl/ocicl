(in-package :mgl-pax-test)

(defun check-document (input expected
                       &key (package (find-package :mgl-pax-test)) msg
                         (url-versions '(2)) (format :markdown))
  (let ((output (let ((*package* package)
                      (*document-hyperspec-root* "CLHS/")
                      (*document-url-versions* url-versions))
                  (document input :stream nil :format format))))
    (is (null (mismatch% output expected))
        :msg msg
        :ctx ("Input: ~S" input))))

(defun document* (object &key (format :markdown))
  (let ((warnings ()))
    (handler-bind ((warning (lambda (w)
                              (push (princ-to-string w) warnings)
                              (muffle-warning w))))
      (values (funcall (if (member format '(:w3m :md-w3m))
                           #'pax::document/open
                           #'document)
                       object :stream nil :format format)
              warnings))))

(defun count-lines (string)
  (with-input-from-string (in string)
    (loop while (read-line in nil nil)
          count 1)))

(defun check-head (input expected &key (format :markdown) msg n-lines
                   (warnings 0) package)
  (let* ((*package* (or package (find-package :mgl-pax-test)))
         (*document-hyperspec-root* "CLHS/")
         (*document-url-versions* '(2))
         (n-expected-warnings warnings)
         (input (if (stringp input)
                    (format nil input)
                    input))
         (expected (format nil expected))
         (n-lines (or n-lines (count-lines expected))))
    (multiple-value-bind (full-output warnings)
        (document* input :format format)
      (let ((got (dref::first-lines full-output n-lines)))
        (is (equal got expected)
            :msg msg
            :ctx ("Input: ~S~%Full output:~%~S" input full-output))
        (is (= (length (% warnings)) n-expected-warnings)
            :ctx ("Input: ~S~%Full output:~%~S" input full-output))))))

(defun check-pred (input pred &key msg (format :markdown))
  (let* ((*package* (find-package :mgl-pax-test))
         (*document-hyperspec-root* "CLHS/")
         (*document-url-versions* '(2))
         (full-output (document* input :format format))
         (pred* (if (stringp pred)
                    (lambda (string)
                      (search pred string))
                    pred)))
    (is (funcall pred* full-output)
        :msg msg
        :ctx ("Input: ~S~%Pred: ~S~%" input pred))))

(defun internedp (name)
  (find-symbol (string name) :mgl-pax-test))


(deftest test-document ()
  (test-urlencode)
  (test-transform-tree)
  (test-markdown-workarounds)
  (test-sanitize-aggressively)
  (test-parse-dref)
  (test-parse-definitions*)
  (test-funny)
  (test-codify)
  (test-names)
  (test-downcasing)
  (test-link)
  (test-headings)
  (test-titles)
  (test-base-url)
  (test-url-versions)
  (test-pages)
  (test-locate-error)
  ;; PAX::@VARIABLELIKE-LOCATIVES
  (test-variable)
  (test-constant)
  ;; PAX::@MACROLIKE-LOCATIVES
  (test-macro)
  (test-symbol-macro)
  (test-setf)
  ;; PAX::@FUNCTIONLIKE-LOCATIVES
  (test-function)
  (test-generic-function)
  (test-method-combination)
  (test-method)
  (test-reader)
  (test-writer)
  (test-accessor)
  (test-structure-accessor)
  ;; PAX::@TYPELIKE-LOCATIVES
  (test-structure)
  (test-declaration)
  ;; PAX::@CONDITION-SYSTEM-LOCATIVES
  (test-condition)
  (test-restart)
  ;; PAX::@PACKAGELIKE-LOCATIVES
  (test-asdf-system)
  (test-package)
  (test-readtable)
  ;; PAX::@PAX-LOCATIVES
  (test-locative)
  (test-section)
  (test-glossary-term)
  (test-go)
  (test-docstring)
  (test-include)
  (test-hyperspec)
  (test-clhs-definitions)
  (test-clhs-section)
  (test-clhs-glossary-entries)
  (test-clhs-issue)
  (test-argument)
  (test-define-locative-alias)
  ;; Misc
  (test-apropos)
  (test-cl-transcript)
  (test-document/open)
  (test-map-documentable)
  (test-table-of-contents)
  (test-definitions-for-pax-url-path)
  (test-with-document-context)
  (test-asdf-system-name-of)
  (test-guess-package-from-arglist)
  (test-pdf)
  (test-dummy-output)
  (test-pax-transcripts))

(deftest test-urlencode ()
  (is (equal (mgl-pax::urlencode "hello") "hello"))
  (is (equal (mgl-pax::urlencode "@hello section") "@hello%20section"))
  (is (equal (mgl-pax::urlencode "\"") "%22"))
  (is (equal (mgl-pax::urlencode "á") "%C3%A1"))
  (is (match-values
          (mgl-pax::parse-url "http://x.org:8888/y/z.html?a=1&b=2#frag")
        (equal * "http")
        (equal * "x.org:8888")
        (equal * "/y/z.html")
        (equal * "a=1&b=2")
        (equal * "frag")))
  (is (match-values
          (mgl-pax::parse-url
           #.(format nil "pax:pax%3A%3A%40pax-manual%20pax%3Asection~
                          #pax%3Adefsection%20pax%3Amacro"))
        (equal * "pax")
        (null *)
        (equal * "pax::@pax-manual pax:section")
        (null *)
        (equal * "pax:defsection pax:macro")))
  (is (equal (mgl-pax::urlencode (format nil "~%")) "%0A")))

(deftest test-transform-tree ()
  (is (equal '(1)
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values a (listp a) nil))
                                      '(1))))

  (is (equal '(2 (3 (4 5)))
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values (if (listp a) a (1+ a))
                                                (listp a)
                                                nil))
                                      '(1 (2 (3 4))))))

  (is (equal '(1 2 (2 3 (3 4 4 5)))
             (mgl-pax::transform-tree (lambda (parent a)
                                        (declare (ignore parent))
                                        (values (if (listp a)
                                                    a
                                                    (list a (1+ a)))
                                                (listp a)
                                                (not (listp a))))
                                      '(1 (2 (3 4)))))))

(deftest test-markdown-workarounds ()
  (check-head "[\\\\][x]" "[\\\\][x]")
  (check-head "[\\\\][x]" "\\\\" :format :plain)
  (check-head "\\$x$" (format nil "~A~A" (code-char 8203) "\\$x\\$"))
  (check-head "\\$x$" "$x$" :format :plain))


(deftest test-sanitize-aggressively ()
  (test-sanitize-docstring-aggressively)
  (test-pax-constructs-are-not-sanitized-agressively))

(deftest test-sanitize-docstring-aggressively ()
  (flet ((test1 (test-name in out)
           (with-test (nil :name test-name)
             (let ((input (format nil in))
                   (expected (format nil out)))
               (is (equal (pax::sanitize-docstring
                           input :aggressivep t :first-line-special-p nil)
                          expected))))))
    (test1 "0"   "xxx~%~%(1~%2~%"           "xxx~%~%(1~%2~%")
    (test1 "1"   "xxx~%~% (1~% 2~%"         "xxx~%~%    (1~%    2~%")
    (test1 "2"   "xxx~%~%  (1~%  2~%"       "xxx~%~%    (1~%    2~%")
    (test1 "3"   "xxx~%~%   (1~%   2~%"     "xxx~%~%    (1~%    2~%")
    (test1 "4"   "xxx~%~%    (1~%    2~%"   "xxx~%~%    (1~%    2~%")
    (test1 "5"   "xxx~%~%     (1~%     2~%" "xxx~%~%        (1~%        2~%")
    (test1 "1/6" "xxx~%~% (1~%      2~%"    "xxx~%~%    (1~%         2~%")
    (test1 "1/6" "xxx~%~% (1~%      2~%"    "xxx~%~%    (1~%         2~%")
    (test1 "no newline"   "xxx"             "xxx")
    (test1 "no newline 2" "xxx~%~% (1"      "xxx~%~%    (1")
    (test1 "consecutive blocks"
           "xxx~%~% (1~%~% (2~%"
           "xxx~%~%    (1~%~%    (2~%")
    (test1 "simple"  "xxx~%~% (1~%~%xxx"    "xxx~%~%    (1~%~%xxx")
    (test1 "comment" "xxx~%~% ;1~%~%xxx"    "xxx~%~%    ;1~%~%xxx")
    (test1 "html"    "<x>&amp;&"                 "\\<x>\\&amp;&")
    (test1 "html in verbatim" "x~%~% (<x>&" "x~%~%    (<x>&")
    (test1 "heading"  "#x"                  "\\#x")
    (test1 "heading2" "# x"                 "\\# x")
    (test1 "heading3" "x~%~%    # x"        "x~%~%    \\# x")))

(deftest test-pax-constructs-are-not-sanitized-agressively ()
  (check-pred (dref '@1+* 'note) " ###")
  (check-pred @some-term " ###"))


(deftest test-parse-dref ()
  (let ((*package* (find-package :mgl-pax-test)))
    (unintern (read-from-string "non-interned"))
    (unintern (read-from-string "yyy"))
    (is (match-values (mgl-pax::parse-dref "yyy non-interned")
          (null *)
          (null *)
          (string= * "non-interned")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (find-symbol (string '#:yyy))))
    (is (match-values (mgl-pax::parse-dref "yyy (non-interned)")
          (null *)
          (null *)
          (string= * "(non-interned)")))
    (is (null (find-symbol (string '#:non-interned))))
    (is (null (find-symbol (string '#:yyy))))
    (is (match-values (mgl-pax::parse-dref "yyy find")
          (null *)
          (null *)
          (string= * "find")))
    (is (null (find-symbol (string '#:yyy))))
    (is (match-values (mgl-pax::parse-dref "foo function")
          (xref= * (dref 'foo 'function))
          (eq * 'function)
          (null *)))
    (is (match-values (mgl-pax::parse-dref " foo  function ")
          (xref= * (dref 'foo 'function))
          (eq * 'function)
          (null *)))
    (is (match-values (mgl-pax::parse-dref " foo  function  bar ")
          (null *)
          (null *)
          (string= * "function  bar")))
    (is (match-values (mgl-pax::parse-dref "mgl-pax:@codification section")
          (null *)
          (eq * 'section)
          (null *))
        :msg "internal symbol with single :")))

(deftest test-parse-definitions* ()
  (let ((*package* (find-package :mgl-pax-test)))
    (is (dref-set= (mgl-pax::parse-definitions* "deftest")
                   (definitions 'deftest)))
    (is (dref-set= (mgl-pax::parse-definitions* "MGL-PAX::@CODIFIABLE")
                   (definitions 'mgl-pax::@codifiable)))
    (is (dref-set= (mgl-pax::parse-definitions* "nil")
                   (mgl-pax::definitions* 'nil)))
    (is (dref-set= (mgl-pax::parse-definitions* "\"nil\"")
                   (mgl-pax::definitions* "nil")))))

(deftest test-funny ()
  (is (equal (mgl-pax::prin1-funny-to-string 'mgl-pax::@pax-manual)
             "MGL-PAX:@PAX-MANUAL"))
  (is (eq (mgl-pax::read-funny-from-string "MGL-PAX:@PAX-MANUAL")
          'mgl-pax::@pax-manual))
  (is (equal (mgl-pax::prin1-funny-to-string :if-exists) ":IF-EXISTS"))
  (is (eq (mgl-pax::read-funny-from-string ":IF-EXISTS") :if-exists))
  (let ((*package* (find-package :keyword)))
    (is (eq (mgl-pax::read-funny-from-string "PRINT") 'cl:print)))
  (is (equal (mgl-pax::prin1-funny-to-string '|Foo|) "MGL-PAX-TEST:Foo"))
  (is (equal (mgl-pax::read-funny*-from-string "x y:") "x"))
  (is (equal (mgl-pax::read-funny*-from-string "x:y") "x"))
  (is (equal (mgl-pax::read-funny*-from-string "x y") "x"))
  (is (equal (mgl-pax::read-funny*-from-string "x\\y") "xy"))
  (is (equal (mgl-pax::read-funny*-from-string "x\\:y") "x:y"))
  (signals (end-of-file)
    (mgl-pax::read-funny*-from-string "x\\")))


(deftest test-codify ()
  (with-test ("unadorned")
    (with-test ("uninterned")
      (with-test ("len=1")
        (is (not (internedp "U")))
        (check-head "U" "U")
        (check-head "\\U" "U")
        (check-head "-" "-"))
      (with-test ("len=2")
        (is (not (internedp "UN")))
        (check-head "UN" "UN")
        (check-head "\\UN" "UN")
        (check-head "/=" "/="))
      (with-test ("len=3")
        (is (not (internedp "UNI")))
        (check-head "UNI" "UNI")
        (check-head "\\UNI" "UNI")
        (is (not (internedp "*U*")))
        (check-head "*U*" "*U*")
        (check-head "*\\U*" "*U*")
        (check-head "///" "///")
        (check-head "Uni" "Uni")
        (check-head "UnI" "UnI")))
    (with-test ("internal")
      (with-test ("len=1")
        (is (not (mgl-pax::external-symbol-p 'q)))
        (check-head "Q" "Q")
        (check-head "\\Q" "Q"))
      (with-test ("len=2")
        (is (not (mgl-pax::external-symbol-p 'qq)))
        (check-head "QQ" "QQ")
        (check-head "\\QQ" "QQ"))
      (with-test ("len=3")
        (is (not (mgl-pax::external-symbol-p 'qqq)))
        (check-head "QQQ" "`QQQ`")
        (check-head "\\QQQ" "QQQ")
        (is (not (mgl-pax::external-symbol-p '*q*)))
        (check-head "*Q*" "`*Q*`")
        (check-head "*\\Q*" "*Q*")))
    (with-test ("external")
      (let ((*document-link-to-hyperspec* nil))
        (check-head "T" "`T`")
        (check-head "\\T" "T")
        (check-head "DO" "`DO`")
        (check-head "\\DO" "DO")
        (check-head "COS" "`COS`")
        (check-head "\\COS" "COS")))
    (with-test ("external with ref")
      ;; T is not autolinked.
      (check-head "T" "`T`")
      (check-head "\\T" "T")
      (check-head "DO" "[`DO`][5d2b]")
      (check-head "\\DO" "DO")
      (check-head "COS" "[`COS`][c4a3]")
      (check-head "\\COS" "COS"))
    (with-test (":EMPH")
      (let ((*document-link-code* nil))
        (check-head "*PACKAGE*" "`*PACKAGE*`")
        (check-head "CL:*PACKAGE*" "`CL:*PACKAGE*`"))))
  (with-test ("reflink")
    (with-test ("no refs")
      (check-head "[U]" "\\[U\\]")))
  (with-test ("in :REFERENCE")
    (check-document "xxx
xxx

  [some]: PRINT \"DO\""
                    "xxx
xxx

[some]: PRINT \"DO\"

"))
  (is (internedp 'references))
  (check-head "REFERENCEs" "`REFERENCE`s" :msg "interned lowercase plural")
  (check-head "<PRINT>" "<PRINT>" :msg "No codification in :RAW-HTML")
  (with-test ("funny names")
    (check-head "|Foo|" "|Foo|")
    (check-head "|F o|" "|F o|")
    (check-head "|F O|" "|F O|"))
  (with-test ("explicit link label")
    (check-head "[PACKAGEs](xxx)" "[`PACKAGE`s](xxx)"))
  (with-test ("code in math")
    (check-head "$x < T$" "$x < T$")))

(defun q ())
(defun qq ())
(defun qqq ())
(defvar *q*)


(deftest test-names ()
  (with-test ("Uppercase name with uppercase plural.")
    (check-head "CARS" "[`CAR`][d5a2]s")
    (check-head "CARS." "[`CAR`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "CLASSES" "[`CLASS`][1f37]es")
      (check-head "CLASSES." "[`CLASS`][1f37]es."))
    (check-head "ARRAY-DIMENSIONS" "[`ARRAY-DIMENSIONS`][b315]")
    (check-head "ARRAY-DIMENSIONS." "[`ARRAY-DIMENSIONS`][b315]."))
  (with-test ("Uppercase name with lowercase plural.")
    (check-head "CARs" "[`CAR`][d5a2]s")
    (check-head "CARs." "[`CAR`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "CLASSes" "[`CLASS`][1f37]es")
      (check-head "CLASSes." "[`CLASS`][1f37]es."))
    (check-head "ARRAY-DIMENSIONs" "[`ARRAY-DIMENSION`][6c28]s")
    (check-head "ARRAY-DIMENSIONs." "[`ARRAY-DIMENSION`][6c28]s."))
  (with-test ("Uppercase code + lowercase plural.")
    (check-head "`CAR`s" "[`CAR`][d5a2]s")
    (check-head "`CAR`s." "[`CAR`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "`CLASS`es" "[`CLASS`][1f37]es")
      (check-head "`CLASS`es." "[`CLASS`][1f37]es."))
    (check-head "`ARRAY-DIMENSION`s" "[`ARRAY-DIMENSION`][6c28]s")
    (check-head "`ARRAY-DIMENSION`s." "[`ARRAY-DIMENSION`][6c28]s."))
  (with-test ("Lowercase code + lowercase plural.")
    (check-head "`car`s" "[`car`][d5a2]s")
    (check-head "`car`s." "[`car`][d5a2]s.")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "`class`es" "[`class`][1f37]es")
      (check-head "`class`es." "[`class`][1f37]es."))
    (check-head "`array-dimension`s" "[`array-dimension`][6c28]s")
    (check-head "`array-dimension`s." "[`array-dimension`][6c28]s."))
  (with-test ("Lowercase code with lowercase plural.")
    (check-head "`cars`" "[`cars`][d5a2]")
    (check-head "`cars.`" "`cars.`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "`classes`" "[`classes`][1f37]"))
    (check-head "`classes.`" "`classes.`")
    (check-head "`array-dimensions`" "[`array-dimensions`][b315]")
    (check-head "`array-dimensions.`" "`array-dimensions.`"))
  (with-test ("Uppercase name with uppercase plural in reflink.")
    (check-head "[CARS][]" "[`CAR`s][d5a2]")
    (check-head "[CARS.][]" "`CAR`s." :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "[CLASSES][]" "[`CLASS`es][1f37]"))
    (check-head "[CLASSES.][]" "`CLASS`es." :warnings 1)
    (check-head "[ARRAY-DIMENSIONS][]" "[`ARRAY-DIMENSIONS`][b315]")
    (check-head "[ARRAY-DIMENSIONS.][]" "`ARRAY-DIMENSIONS`." :warnings 1))
  (with-test ("Uppercase name with lowercase plural in reflink.")
    (check-head "[CARs][]" "[`CAR`s][d5a2]")
    (check-head "[CARs.][]" "`CAR`s." :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "[CLASSes][]" "[`CLASS`es][1f37]"))
    (check-head "[CLASSes.][]" "`CLASS`es." :warnings 1)
    ;; Somewhat surprisingly, the ARRAY-DIMENSIONS is to be linked as
    ;; the PAX::@NAME is determined by PARSE-TREE-TO-TEXT.
    (check-head "[ARRAY-DIMENSIONs][]" "[`ARRAY-DIMENSION`s][6c28]")
    (check-head "[ARRAY-DIMENSIONs.][]" "`ARRAY-DIMENSION`s." :warnings 1))
  (with-test ("Uppercase code + lowercase plural in reflink.")
    (check-head "[`CAR`s][]" "[`CAR`s][d5a2]")
    (check-head "[`CAR`s.][]" "`CAR`s." :warnings 1)
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "[`CLASS`es][]" "[`CLASS`es][1f37]"))
    (check-head "[`CLASS`es.][]" "`CLASS`es." :warnings 1)
    (check-head "[`ARRAY-DIMENSION`s][]" "[`ARRAY-DIMENSION`s][6c28]")
    (check-head "[`ARRAY-DIMENSION`s.][]" "`ARRAY-DIMENSION`s." :warnings 1))
  (with-test ("Trimming")
    (check-head "`#<CLASS>`" "`#<CLASS>`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-head "\\#\\<CLASS>" "\\#\\<[`CLASS`][1f37]>")))
  (check-head "*PRINT-LENGTH*s" "[`*PRINT-LENGTH*`][8f7a]s")
  (check-head "\\Delta" "\\Delta" :msg "mathjax")
  (check-head "T." "`T`.")
  (check-head "`doc/`" "`doc/`")
  (check-head "non-NIL" "non-`NIL`")
  (check-head "nonNIL" "non`NIL`")
  (with-test ("READable")
    (check-head "READable" "[`READ`][fe58]able")
    (check-head "[READable][]" "`READ`able" :warnings 1)
    (check-head "[`READ`able][]" "`READ`able" :warnings 1))
  (with-test ("nonREADable")
    (check-head "nonREADable" "non[`READ`][fe58]able")
    (check-head "[nonREADable][]" "non`READ`able" :warnings 1)
    (check-head "[non`READ`able][]" "non`READ`able" :warnings 1))
  (with-test ("non-NIL")
    (check-head "non-NIL" "non-`NIL`")
    (check-head "-NIL" "-NIL" :msg "no lowercase before first uppercase")
    (check-head "NIL-" "NIL-" :msg "no lowercase after last uppercase")
    (check-head "NILable" "`NIL`able")
    (is (internedp '%nil))
    (check-head "%NILable" "`%NIL`able"))
  (with-test ("one uppercase character only")
    (check-head "anT" "anT")
    (check-head "Tea" "Tea")
    (check-head "Ts" "Ts")
    (check-head "T=3" "T=3"))
  (check-head "Classes" "Classes")
  (with-failure-expected ((alexandria:featurep :clisp))
    (check-head "`Classes`" "`Classes`"))
  (check-head "`\"=>\"`" "`\"=>\"`")
  (with-test ("no uppercase")
    (check-head "non-nil" "non-nil"))
  (with-test ("uppercase too short")
    (check-head "nonXX" "nonXX"))
  (with-test ("a longer definition exists but it's not being documented")
    (check-head (list "[XYZS][]" #'xyz) "`XYZS`")
    (check-head (list "XYZS" #'xyz) "`XYZS`")
    (check-head (list "[XYZs][]" #'xyz) "[`XYZ`s][1f17]")
    (check-head (list "XYZs" #'xyz) "[`XYZ`][1f17]s")))

(defun xyz ())
(defun xyzs ())


(deftest test-downcasing ()
  (test-downcasing-in-docstrings)
  (test-downcasing-of-section-names))

(defsection @section-without-title (:export nil))

(define-glossary-term @gt-escaped-title (:title "`\\\\COS`"
                                         :url "xxx"))

(defsection @section-escaped-title (:title "See `\\\\COS`"))

(deftest test-downcasing-in-docstrings ()
  (with-test ("unadorned")
    (check-downcasing "NOT-INTERNED" "NOT-INTERNED")
    (check-downcasing "CaMeL" "CaMeL")
    ;; has no refs
    (check-downcasing "TEST" "`test`")
    ;; has refs
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "CLASS" "[`class`][1f37]"))
    ;; has no refs
    (check-downcasing "*FORMAT*" "`*format*`")
    ;; has refs
    (check-downcasing "*PACKAGE*" "[`*package*`][5ed1]")
    ;; section with refs
    (check-downcasing (list "@SECTION-WITHOUT-TITLE" @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("escaped unadorned")
    (check-downcasing "\\NOT-INTERNED" "NOT-INTERNED")
    (check-downcasing "\\CaMeL" "\\CaMeL")
    (check-downcasing "*\\CaMeL*" "*\\CaMeL*")
    (check-downcasing "\\TEST" "TEST")
    (check-downcasing "\\CLASS" "CLASS")
    (check-downcasing "*\\FORMAT*" "*FORMAT*")
    (check-downcasing "*\\PACKAGE*" "*PACKAGE*")
    (check-downcasing (list "\\@SECTION-WITHOUT-TITLE" @section-without-title)
                      "@SECTION-WITHOUT-TITLE"))
  (with-test ("code")
    (check-downcasing "`NOT-INTERNED`" "`not-interned`")
    (check-downcasing "`CaMeL`" "`CaMeL`")
    (check-downcasing "`TEST`" "`test`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "`CLASS`" "[`class`][1f37]"))
    (check-downcasing "`*FORMAT*`" "`*format*`")
    (check-downcasing "`*PACKAGE*`" "[`*package*`][5ed1]")
    (check-downcasing (list "`@SECTION-WITHOUT-TITLE`" @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("escaped code")
    (check-downcasing "`\\NOT-INTERNED`" "`not-interned`")
    (check-downcasing "`\\CaMeL`" "`CaMeL`")
    (check-downcasing "`\\TEST`" "`test`")
    (check-downcasing "`\\CLASS`" "`class`")
    (check-downcasing "`\\*FORMAT*`" "`*format*`")
    (check-downcasing "`\\*PACKAGE*`" "`*package*`")
    (check-downcasing (list "`\\@SECTION-WITHOUT-TITLE`"
                            @section-without-title)
                      "`@section-without-title`"))
  (with-test ("doubly escaped code")
    (check-downcasing "`\\\\NOT-INTERNED`" "`NOT-INTERNED`")
    (check-downcasing "`\\\\CaMeL`" "`CaMeL`")
    (check-downcasing "`\\\\TEST`" "`TEST`")
    (check-downcasing "`\\\\CLASS`" "`CLASS`")
    (check-downcasing "`\\\\*FORMAT*`" "`*FORMAT*`")
    (check-downcasing "`\\\\*PACKAGE*`" "`*PACKAGE*`")
    (check-downcasing (list "`\\\\@SECTION-WITHOUT-TITLE`"
                            @section-without-title)
                      "`@SECTION-WITHOUT-TITLE`"))
  (with-test ("reflink unadorned")
    (check-downcasing "[NOT-INTERNED][]" "NOT-INTERNED" :warnings 1)
    (check-downcasing "[CaMeL][]" "CaMeL" :warnings 1)
    (check-downcasing "[TEST][]" "`test`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[CLASS][]" "[`class`][1f37]")
      (check-downcasing "[*FORMAT*][]" "`*format*`"))
    (check-downcasing "[*PACKAGE*][]" "[`*package*`][5ed1]")
    (check-downcasing (list "[@SECTION-WITHOUT-TITLE][]"
                            @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("reflink code")
    (check-downcasing "[`NOT-INTERNED`][]" "`not-interned`" :warnings 1)
    (check-downcasing "[`CaMeL`][]" "`CaMeL`" :warnings 1)
    (check-downcasing "[`TEST`][]" "`test`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[`CLASS`][]" "[`class`][1f37]")
      (check-downcasing "[`*FORMAT*`][]" "`*format*`"))
    (check-downcasing "[`*PACKAGE*`][]" "[`*package*`][5ed1]")
    (check-downcasing (list "[`@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("reflink escaped code")
    (check-downcasing "[`\\NOT-INTERNED`][]" "`not-interned`" :warnings 1)
    (check-downcasing "[`\\CaMeL`][]" "`CaMeL`" :warnings 1)
    (check-downcasing "[`\\TEST`][]" "`test`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[`\\CLASS`][]" "[`class`][1f37]")
      (check-downcasing "[`\\*FORMAT*`][]" "`*format*`"))
    (check-downcasing "[`\\*PACKAGE*`][]" "[`*package*`][5ed1]")
    (check-downcasing (list "[`\\@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@section-without-title`][eeac]"))
  (with-test ("reflink doubly escaped code")
    (check-downcasing "[`\\\\NOT-INTERNED`][]" "`NOT-INTERNED`" :warnings 1)
    (check-downcasing "[`\\\\CaMeL`][]" "`CaMeL`" :warnings 1)
    (check-downcasing "[`\\\\TEST`][]" "`TEST`")
    (with-failure-expected ((alexandria:featurep :clisp))
      (check-downcasing "[`\\\\CLASS`][]" "[`CLASS`][1f37]")
      (check-downcasing "[`\\\\*FORMAT*`][]" "`*FORMAT*`"))
    (check-downcasing "[`\\\\*PACKAGE*`][]" "[`*PACKAGE*`][5ed1]")
    (check-downcasing (list "[`\\\\@SECTION-WITHOUT-TITLE`][]"
                            @section-without-title)
                      "[`@SECTION-WITHOUT-TITLE`][eeac]"))
  (with-test ("multiple symbols")
    (check-downcasing "`(LIST :XXX 'PRINT)`" "`(list :xxx 'print)`")
    (check-downcasing "`(PRINT \"hello\")`" "`(print \"hello\")`")
    (check-downcasing "`(PRINT '\\\"hello\\\")`" "`(PRINT '\\\"hello\\\")`"))
  (with-test ("no-uppercase-is-code")
    (let ((*document-uppercase-is-code* nil))
      (check-downcasing "XXX" "XXX")
      (check-downcasing "`XXX`" "`xxx`")
      (check-downcasing "`(PRINT \"hello\")`" "`(print \"hello\")`")))
  (with-test ("escaped downcasing in title")
    ;; This is not a title, but escaping in titles should behave the
    ;; same.
    (check-downcasing "\\\\COS" "COS")
    (check-downcasing "`\\\\COS`" "`COS`")
    ;; This is linked to the GLOSSARY-TERM-URL.
    (check-downcasing "@GT-ESCAPED-TITLE" "[`COS`][5758]")
    ;; And this is linked to the SECTION.
    (check-downcasing (list "@SECTION-ESCAPED-TITLE"
                            @section-escaped-title)
                      "[See `COS`][5dbb]")
    (check-downcasing-pred @section-escaped-title "See `COS`")))

(defsection @xxx (:title "`\\\\COS` Prompting"))

(defsection @parent-section-without-title (:export nil)
  (@section-without-title section))

(deftest test-downcasing-of-section-names ()
  (let ((*document-downcase-uppercase-code* t))
    (check-document @parent-section-without-title
                    "<a id=\"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

# MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION

## Table of Contents

- [1 MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION][eeac]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## 1 MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION


  [eeac]: #MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION\"
")))

(defun check-downcasing (docstring expected &key (warnings 0))
  (let ((*document-downcase-uppercase-code* t))
    (check-head docstring expected :warnings warnings)))

(defun check-downcasing-pred (docstring pred)
  (let ((*document-downcase-uppercase-code* t))
    (check-pred docstring pred)))


(deftest test-link ()
  (test-autolink)
  (test-reflink)
  (test-explicit-label)
  (test-suppressed-links))


(deftest test-autolink ()
  (with-test ("object with multiple refs")
    (check-head (list "macro BAR function"
                      (dref 'bar 'type)
                      (dref 'bar 'macro))
                ;; "3e5e" is the id of the macro.
                "macro [`BAR`][3e5e] function"
                :msg "locative before, irrelevant locative after")
    (check-head (list "function BAR macro"
                      (dref 'bar 'type)
                      (dref 'bar 'macro))
                "function [`BAR`][3e5e] macro"
                :msg "locative after, irrelevant locative before")
    (check-head (list "macro BAR type"
                      (dref 'bar 'type)
                      (dref 'bar 'macro)
                      (dref 'bar 'constant))
                ;; "e2a5" is the the id of the type.
                "macro [`BAR`][e2a5] type"
                :msg "ambiguous locative")
    (check-head (list "macro BAR type"
                      (dref 'bar 'macro))
                "macro `BAR` type"
                :msg "definition with preferred locative no being documented"))
  (with-test ("locative in backticks")
    (check-head (list "`TEST-GF` `(method (number))`"
                      (dref 'test-gf '(method (number))))
                "[`TEST-GF`][fcc6] `(method (number))`")
    (check-head (list "`(method (number))` `TEST-GF`"
                      (dref 'test-gf '(method (number))))
                "`(method (number))` [`TEST-GF`][fcc6]"))
  (with-test ("escaped autolinking")
    (check-head "`\\PRINT`" "`PRINT`"))
  (with-test ("used to fail")
    (check-head " :KEY xxx" " `:KEY` xxx"))
  (with-test ("longer definition with unspecified locative")
    (check-head "class STRING>." "class [`STRING`][b93c]>."))
  (check-head "the EQL function's" "the [`EQL`][db03] function's")
  (with-test ("mixed case name")
    (check-head (list "`|Foo|`" #'|Foo|) "[`|Foo|`][5696]")
    (check-head (list "`|F o|`" #'|F o|) "[`|F o|`][775e]"))
  (with-test ("newline between name and locative")
    (check-head "x~%CLHS~%  `6.1.2.1`." "x
`CLHS`
  [`6.1.2.1`][142c].")))

(deftest test-reflink ()
  (with-test ("label is a single name")
    (check-head "[*PACKAGE*][]" "[`*PACKAGE*`][5ed1]")
    (check-head "[*PACKAGE*][variable]" "[`*PACKAGE*`][5ed1]")
    (check-head "[ *PACKAGE*][]" "[ `*PACKAGE*`][5ed1]")
    (check-head "[*PACKAGE*][ variable]" "[`*PACKAGE*`][5ed1]")
    (check-head "[*PACKAGE*]" "\\[[`*PACKAGE*`][5ed1]\\]")
    (check-head "[*PACKAGE*][normaldef]" "[`*PACKAGE*`][normaldef]")
    (with-failure-expected ((alexandria:featurep '(:or :clisp)))
      (check-head "[*FORMAT*][]" "`*FORMAT*`")))
  (with-test ("definition is a reference")
    (check-head "[see this][car function]" "[see this][d5a2]")
    (check-head "[`see` *this*][car function]" "[`see` *this*][d5a2]"))
  (with-test ("definition is an object")
    (check-head "[see this][print]" "[see this][d451]")
    (check-head "[ see this ][print]" "[ see this ][d451]")
    (check-head "[see this][ print]" "[see this][d451]")
    (check-head "[see this][\\*package*]" "[see this][5ed1]")
    (check-head "[see this][nil]" "see this([`0`][9990] [`1`][4df2])"))
  (with-test ("definition is both a locative and an object")
    (check-head (list "[see this][section]") "see this" :warnings 1)
    (check-head (list "[see this][section]" (dref 'section 'class)
                      (dref 'section 'locative))
                "see this" :warnings 1)
    (check-head (list "[FORMAT][dislocated]"
                      (dref 'dislocated 'locative))
                "`FORMAT`"
                :package (find-package '#:mgl-pax))
    (check-head (list "[NOT-CODE][dislocated]"
                      (dref 'dislocated 'locative))
                "NOT-CODE"
                :package (find-package '#:mgl-pax))
    (check-head (list "[`SOME-CODE`][dislocated]"
                      (dref 'dislocated 'locative))
                "`SOME-CODE`"
                :package (find-package '#:mgl-pax))
    (check-head "[locative][dislocated]" "locative")
    (check-head "[LOCATIVE][dislocated]" "`LOCATIVE`"))
  (with-test ("name with reference hiding in interesting object")
    (is (internedp 'sections))
    (check-head (list "[SECTIONS][class]"
                      (dref 'section 'class))
                "[`SECTIONS`][5fac]")
    (check-head (list "[SECTIONs][class]"
                      (dref 'section 'class))
                "[`SECTION`s][5fac]")
    (check-head (list "[SECTION][class]"
                      (dref 'section 'class))
                "[`SECTION`][5fac]"))
  (with-test ("normal markdown reference link")
    (with-test ("simple")
      (check-head "[see this][ddd]

  [ddd]: #ttt"
                  "[see this][ddd]"))
    (with-test ("definition is also an interned symbol")
      (is (internedp 'references))
      (check-head "[see this][references]

  [references]: #ttt"
                  "[see this][references]

[references]: #ttt"))
    (with-test ("definition is an interned symbol with a definition")
      (check-head "[see this][print]

  [print]: #ttt"
                  "[see this][d451]")))
  (when pax::*3bmd-reflink-definition-is-list*
    (with-test ("emph in reflink definition")
      (check-head "[xxx][*print-length* variable]" "[xxx][8f7a]"))
    (with-test ("backtick in reflink definition")
      (check-head "[xxx][`*print-length*` variable]" "[xxx][8f7a]")))
  (with-test ("emph around reflink")
    (check-head "*[x][y]*" "*[x][y]*"))
  (with-test ("mixed case name")
    (check-head (list "[|Foo|][function]" #'|Foo|) "[|Foo|][5696]"))
  (with-test ("spaces in names")
    (check-head (list "[see this][\"X Y\" package]" (find-package "X Y"))
                "[see this][4ef3]")
    (is (internedp '|X Y|))
    (check-head (list "[see this][|X Y| package]" (find-package "X Y"))
                "[see this][4ef3]")
    (check-head (list "[|X Y|][package]" (find-package "X Y"))
                "[|X Y|][4ef3]")
    (check-head (list "[X Y][package]" (find-package "X Y"))
                "[X Y][4ef3]")
    (check-head (list "[\"X Y\"][package]" (find-package "X Y"))
                "[\"X Y\"][4ef3]")))


(defsection @section-with-title (:title "My `Title`" :export nil))
(define-glossary-term @gt-with-title (:title "My `Title`") "")

(deftest test-explicit-label ()
  (with-test ("section")
    (check-downcasing (list "@SECTION-WITH-TITLE" @section-with-title)
                      "[My `Title`][619a]")
    (check-downcasing (list "`@SECTION-WITH-TITLE`" @section-with-title)
                      "[My `Title`][619a]")
    (check-downcasing (list "[@SECTION-WITH-TITLE][]" @section-with-title)
                      "[My `Title`][619a]")
    (check-downcasing (list "[`@SECTION-WITH-TITLE`][]" @section-with-title)
                      "[My `Title`][619a]"))
  (with-test ("glossary-term")
    (check-downcasing (list "@GT-WITH-TITLE" @gt-with-title)
                      "[My `Title`][fecf]")
    (check-downcasing (list "`@GT-WITH-TITLE`" @gt-with-title)
                      "[My `Title`][fecf]")
    (check-downcasing (list "[@GT-WITH-TITLE][]" @gt-with-title)
                      "[My `Title`][fecf]")
    (check-downcasing (list "[`@GT-WITH-TITLE`][]" @gt-with-title)
                      "[My `Title`][fecf]")))


(deftest test-suppressed-links ()
  (test-t-and-nil-links)
  (test-repeated-links)
  (test-self-referencing-links))

(deftest test-t-and-nil-links ()
  (check-head "T" "`T`")
  (check-head "NIL" "`NIL`"))

(deftest test-repeated-links ()
  (check-head "PRINT PRINT" "[`PRINT`][d451] `PRINT`")
  (check-head (list "PRINT" "PRINT") "[`PRINT`][d451]~%~%[`PRINT`][d451]")
  (check-head "[STRING][function] STRING" "[`STRING`][dae6] `STRING`")
  (check-head "[STRING][dislocated] STRING" "`STRING` `STRING`")
  (check-head "[STRING][function] STRING function"
              "[`STRING`][dae6] [`STRING`][dae6] function"))

(defun self-referencing ()
  "This is SELF-REFERENCING."
  ())

(define-glossary-term @self-referencing-term (:title "Self-referencing Term")
  "This is @SELF-REFERENCING-TERM.")

(defsection @self-referencing (:export nil :title "Self-referencing")
  "This is @SELF-REFERENCING.")

(deftest test-self-referencing-links ()
  (check-document #'self-referencing
                  "<a id=\"MGL-PAX-TEST:SELF-REFERENCING%20FUNCTION\"></a>

- [function] **SELF-REFERENCING**

    This is `SELF-REFERENCING`.
")
  (check-document @self-referencing-term
                  "<a id=\"MGL-PAX-TEST:@SELF-REFERENCING-TERM%20MGL-PAX:GLOSSARY-TERM\"></a>

- [glossary-term] **Self-referencing Term**

    This is [Self-referencing Term][a79b].

  [a79b]: #MGL-PAX-TEST:@SELF-REFERENCING-TERM%20MGL-PAX:GLOSSARY-TERM \"Self-referencing Term\"
")
  (let ((*document-max-table-of-contents-level* 0))
    (check-document @self-referencing
                    "<a id=\"MGL-PAX-TEST:@SELF-REFERENCING%20MGL-PAX:SECTION\"></a>

# Self-referencing

###### \\[in package MGL-PAX-TEST\\]
This is [Self-referencing][e042].

  [e042]: #MGL-PAX-TEST:@SELF-REFERENCING%20MGL-PAX:SECTION \"Self-referencing\"
")))


(deftest test-headings ()
  ;; See PAX::HEADING-OFFSET.
  (check-pred pax::@emacs-setup (lambda (string)
                                  (search (format nil "~%## Emacs Setup~%")
                                          string))))


(deftest test-titles ()
  (check-pred #'$x_0$ "**\\$X\\_0\\$**")
  (check-pred @mathjax-and-code-in-glossary-term-title "**hey `c` $x_0$**")
  (signals (warning :pred "Unexpected tag")
    (document @illegal-title-1))
  (check-pred @title-with-emph "**x y**")
  (check-document @parent-tricky-title
                  "<a id=\"MGL-PAX-TEST:@PARENT-TRICKY-TITLE%20MGL-PAX:SECTION\"></a>

# `CODE` *italic* *italic2* *bold* &quot;

## Table of Contents

- [1 `CODE` *italic* *italic2* *bold* &quot;][629a]
    - [1.1 \\`\\_\\*\\&][ea45]
    - [1.2 MGL-PAX-TEST:\\*\\*SUBTRICKY\\*\\* MGL-PAX:SECTION][35ca]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@TRICKY-TITLE%20MGL-PAX:SECTION\"></a>

## 1 `CODE` *italic* *italic2* *bold* &quot;

<a id=\"MGL-PAX-TEST:@SUBTRICKY%20MGL-PAX:SECTION\"></a>

### 1.1 \\`\\_\\*\\&


<a id=\"MGL-PAX-TEST:**SUBTRICKY**%20MGL-PAX:SECTION\"></a>

### 1.2 MGL-PAX-TEST:\\*\\*SUBTRICKY\\*\\* MGL-PAX:SECTION


  [35ca]: #MGL-PAX-TEST:**SUBTRICKY**%20MGL-PAX:SECTION \"MGL-PAX-TEST:**SUBTRICKY** MGL-PAX:SECTION\"
  [629a]: #MGL-PAX-TEST:@TRICKY-TITLE%20MGL-PAX:SECTION \"`CODE` *italic* *italic2* *bold* &quot;\"
  [ea45]: #MGL-PAX-TEST:@SUBTRICKY%20MGL-PAX:SECTION \"\\\\`\\\\_\\\\*\\\\&\"
")
  (with-test ("math in Up: link when browsing live")
    (is (equal (mgl-pax::format-up-links (list @mathjax-and-code-in-title)
                                         (locate #'$x_0$))
               '("Up: [hey `c` ​$x\\_0$](/pax:MGL-PAX-TEST:@MATHJAX-AND-CODE-IN-TITLE%20MGL-PAX:SECTION#MGL-PAX-TEST:%24X_0%24%20FUNCTION)"))))
  (with-test ("math in non-live links")
    (let ((*document-text-navigation* t))
      (check-document @mathjax-and-code-in-title
                      "<a id=\"MGL-PAX-TEST:@MATHJAX-AND-CODE-IN-TITLE%20MGL-PAX:SECTION\"></a>

Next: [MGL-PAX-TEST:@MATHJAX-SUBSECTION MGL-PAX:SECTION][e38e]

# hey `c` $x_0$

## Table of Contents

- [1 MGL-PAX-TEST:@MATHJAX-SUBSECTION MGL-PAX:SECTION][e38e]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:%24X_0%24%20FUNCTION\"></a>

- [function] **\\$X\\_0\\$**

<a id=\"MGL-PAX-TEST:@MATHJAX-SUBSECTION%20MGL-PAX:SECTION\"></a>

Prev: [hey `c` $x_0$][6e97] Up: [hey `c` $x_0$][6e97]

## 1 MGL-PAX-TEST:@MATHJAX-SUBSECTION MGL-PAX:SECTION


  [6e97]: #MGL-PAX-TEST:@MATHJAX-AND-CODE-IN-TITLE%20MGL-PAX:SECTION \"hey `c` $x_0$\"
  [e38e]: #MGL-PAX-TEST:@MATHJAX-SUBSECTION%20MGL-PAX:SECTION \"MGL-PAX-TEST:@MATHJAX-SUBSECTION MGL-PAX:SECTION\"
"))
    (is (equal (mgl-pax::format-up-links (list @mathjax-and-code-in-title)
                                         (locate #'$x_0$))
               '("Up: [hey `c` ​$x\\_0$](/pax:MGL-PAX-TEST:@MATHJAX-AND-CODE-IN-TITLE%20MGL-PAX:SECTION#MGL-PAX-TEST:%24X_0%24%20FUNCTION)")))))


(deftest test-base-url ()
  (dolist (*document-base-url* '("http://example.com" "http://example.com/"))
    (check-pred "[xxx](x.html)" "http://example.com/x.html"
                :msg "relative :EXPLICIT-LINK")
    (check-pred "[xxx](http://other.com/x.html)"
                "http://other.com/x.html"
                :msg "absolute :EXPLICIT-LINK")
    (check-pred (format nil "[xxx][def]~%~%  [def]: x.html")
                "http://example.com/x.html"
                :msg "relative :REFERENCE")
    (check-pred (format nil "[xxx][def]~%~%  [def]: http://other.com/x.html")
                "http://other.com/x.html"
                :msg "absolute :REFERENCE")
    (check-pred "![xxx](x.jpg)" "http://example.com/x.jpg"
                :msg ":IMG :EXPLICIT-LINK")
    (check-pred "![xxx](http://other.com/x.jpg)" "http://other.com/x.jpg"
                :msg "absolute :IMG :EXPLICIT-LINK")
    (check-pred (format nil "![xxx][def]~%~%  [def]: x.jpg")
                "http://example.com/x.jpg"
                :msg ":IMG :REFERENCE")
    (check-pred (format nil "![xxx][def]~%~%  [def]: http://other.com/x.jpg")
                "http://other.com/x.jpg"
                :msg "absolute :IMG :REFERENCE")
    (check-pred (list "FOO function" (dref 'foo 'function))
                "[bc64]: #MGL-PAX-TEST:FOO%20FUNCTION"
                :msg "intra-page")
    (check-pred "<a href='relative'>link</a>"
                "<a href='relative'>link</a>"
                :msg "explicit html link"))
  (signals (error :pred "no query and fragment")
    (let ((*document-base-url* "http://example.com/?q"))
      (document "xxx")))
  (signals (error :pred "no query and fragment")
    (let ((*document-base-url* "http://example.com/#z"))
      (document "xxx"))))


(deftest test-url-versions ()
  (check-document (xref 'foo2 'function)
                  "<a id=\"x-28MGL-PAX-TEST-3AFOO2-20FUNCTION-29\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
"
                  :url-versions '(1))
  (check-document (xref 'foo2 'function)
                  "<a id=\"MGL-PAX-TEST:FOO2%20FUNCTION\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
"
                  :url-versions '(2))
  (check-document (xref 'foo2 'function)
                  "<a id=\"x-28MGL-PAX-TEST-3AFOO2-20FUNCTION-29\"></a>
<a id=\"MGL-PAX-TEST:FOO2%20FUNCTION\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args `OOK` and `X`.
"
                  :url-versions '(1 2)))

(deftest test-pages ()
  (let ((*package* (find-package (find-package :mgl-pax-test))))
    (let ((outputs (document (list #'foo #'->max)
                             :stream nil
                             :pages `((:objects (,#'foo)
                                       :output nil)
                                      (:objects ()
                                       :output (nil))
                                      (:objects (,#'->max)
                                       :output (nil))))))
      (when (is (= (length outputs) 2))
        (is (equal (first outputs)
                   "- [function] FOO OOK X

    FOO has args OOK and X.
"))
        (is (equal (second outputs)
                   "- [function] ->MAX
"))))))


(defsection @section-with-undefined-stuff ()
  (undefined undefined))

(deftest test-locate-error ()
  (signals (locate-error)
    (document '@section-with-undefined-stuff)))


(defparameter *nasty-var* (coerce '(#\Space #\Linefeed #\Tab #\Newline
                                    #\Page #\Return)
                                  'string)
  "docstring")

(deftest test-variable ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (check-document (dref '*nasty-var* 'variable)
                    "<p><a id=\"MGL-PAX-TEST:*NASTY-VAR*%20VARIABLE\"></a></p>

<ul>
<li><p><span class=reference-bullet><span class=reference><span class=\"locative-type\">[variable]</span> <span class=\"reference-object\"><a href=\"#MGL-PAX-TEST:*NASTY-VAR*%20VARIABLE\" >*NASTY-VAR*</a></span></span> <span class=\"locative-args\">&quot; 
\\
\\&quot;</span></span></p>

<p>docstring</p></li>
</ul>
"
                    :format :html))
  (with-test ("initform")
    (check-pred (dref '*some-var* '(variable 7))
                "- [variable] **\\*SOME-VAR\\*** *7*")))


(deftest test-constant ()
  (check-pred (dref 'bar 'constant)
              "- [constant] **BAR** *2*")
  (with-test ("actualizing")
    (check-pred (dref 'bar 'variable)
                "- [constant] **BAR** *2*"))
  (with-test ("actualizing and initform")
    (check-pred (dref 'bar '(variable 7))
                "- [constant] **BAR** *7*")))


(deftest test-macro ()
  (test-macro/arglist))

(deftest test-macro/arglist ()
  ;; C is a parameter. If it were treated as a default value, then
  ;; *DOCUMENT-MARK-UP-SIGNATURES* would be accessed, and this would
  ;; fail.
  (progv (list '*document-mark-up-signatures*) ()
    (is (equal (mgl-pax::arglist-to-markdown '((&key a) (b c)))
               "(&KEY A) (B C)")))
  (with-test ("macro-with-whole-and-dot")
    (is (equal (mgl-pax::arglist-to-markdown '(name . args))
               "NAME . ARGS")))
  (with-test ("macro-with-body-and-dot")
    (is (equal (mgl-pax::arglist-to-markdown '(&body (name . args)))
               "&BODY (NAME . ARGS)")))
  (with-test ("macro-with-rest-and-dot")
    (is (equal (mgl-pax::arglist-to-markdown '(&rest (x y . z)))
               "&REST (X Y . Z)"))))


(defsection @test-symbol-macro (:export nil)
  (my-smac symbol-macro))

(deftest test-symbol-macro ()
  (check-document (dref 'my-smac 'symbol-macro)
                  "<a id=\"MGL-PAX-TEST:MY-SMAC%20MGL-PAX:SYMBOL-MACRO\"></a>

- [symbol-macro] **MY-SMAC**

    This is `MY-SMAC`.
"))


(deftest test-setf ()
  (is (null (dref 'undefined 'setf nil)))
  (test-setf/expander)
  (test-setf/function)
  (test-setf/generic-function)
  (test-setf/method))

(deftest test-setf/expander ()
  (with-failure-expected ((and (alexandria:featurep :abcl) 'failure))
    (check-document (dref 'has-setf-expander 'setf)
                    "<a id=\"MGL-PAX-TEST:HAS-SETF-EXPANDER%20SETF\"></a>

- [setf] **HAS-SETF-EXPANDER**

    ddd
")))

(deftest test-setf/function ()
  (with-failure-expected
      ((and (alexandria:featurep '(:or :abcl)) 'failure))
    (check-document (dref '(setf setf-fn) 'function)
                    "<a id=\"MGL-PAX-TEST:SETF-FN%20DREF:SETF-FUNCTION\"></a>

- [setf-function] **SETF-FN** *V*

    eee
")))

(deftest test-setf/generic-function ()
  (with-failure-expected ((and (alexandria:featurep '(:or :abcl :cmucl))
                               'failure))
    (check-document (dref '(setf setf-gf) 'generic-function)
                  "<a id=\"MGL-PAX-TEST:SETF-GF%20DREF:SETF-GENERIC-FUNCTION\"></a>

- [setf-generic-function] **SETF-GF** *V*

    fff
")))

(deftest test-setf/method ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (signals-not (locate-error)
      (check-document
       (dref '(setf setf-gf) '(method (string)))
       "<a id=\"MGL-PAX-TEST:SETF-GF%20%28DREF:SETF-METHOD%20%28STRING%29%29\"></a>

- [setf-method] **SETF-GF** *(V STRING)*

    ggg
"))))


(deftest test-function ()
  (test-function-args)
  (test-function/encapsulated)
  (test-non-function-function-arglist))

(deftest test-function-args ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (check-head (list #'foo2 #'ook)
                "<a id=\"MGL-PAX-TEST:FOO2%20FUNCTION\"></a>

- [function] **FOO2** *OOK X*

    `FOO2` has args [`OOK`][0e7e] and `X`.")))

(when (fboundp 'encapsulated-function)
  (handler-bind ((warning #'muffle-warning))
    (untrace encapsulated-function)))
(defun encapsulated-function (x &rest args)
  "This may be encapsulated."
  (declare (ignore x args))
  nil)
(handler-bind ((warning #'muffle-warning))
  (trace encapsulated-function))

(when (fboundp 'encapsulated-generic-function)
  (handler-bind ((warning #'muffle-warning))
    (untrace encapsulated-generic-function)))
(defgeneric encapsulated-generic-function (x)
  (:documentation "This may be encapsulated."))
(handler-bind ((warning #'muffle-warning))
  (trace encapsulated-generic-function))

(deftest test-function/encapsulated ()
  (let ((expected "<a id=\"MGL-PAX-TEST:ENCAPSULATED-FUNCTION%20FUNCTION\"></a>

- [function] **ENCAPSULATED-FUNCTION** *X &REST ARGS*

    This may be encapsulated.
"))
    (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp :ecl)))
      (check-document (dref 'encapsulated-function 'function nil)
                      expected)
      (signals-not (locate-error)
        (check-document #'encapsulated-function expected))))
  (let ((expected "<a id=\"MGL-PAX-TEST:ENCAPSULATED-GENERIC-FUNCTION%20GENERIC-FUNCTION\"></a>

- [generic-function] **ENCAPSULATED-GENERIC-FUNCTION** *X*

    This may be encapsulated.
"))
    (with-failure-expected
        ((cond ((alexandria:featurep '(:or :clisp :ecl))
                'failure)
               ((alexandria:featurep ':abcl)
                t)))
      (check-document (dref 'encapsulated-generic-function
                              'generic-function nil)
                      expected))
    (with-failure-expected ((and (alexandria:featurep '(:or :abcl))
                                 'failure))
      (signals-not (locate-error)
        (with-failure-expected ((and (alexandria:featurep '(:or :clisp :ecl))
                                     'failure))
          (check-document #'encapsulated-generic-function expected))))))

(deftest test-non-function-function-arglist ()
  #+sbcl
  (is (match-values (arglist (dref 'sb-c::ir1-convert-nlx-protect
                                   'function))
        (equal * '(sb-c::protected &body sb-c::cleanup))
        (eq * :ordinary)))
  ;; Check that DOCUMENT doesn't fail when the function lambda list is
  ;; really a macro lambda list.
  #+sbcl
  (check-document #'sb-c::ir1-convert-nlx-protect
                  "<a id=\"SB-C:IR1-CONVERT-NLX-PROTECT%20FUNCTION\"></a>

- [function] **SB-C::IR1-CONVERT-NLX-PROTECT** *PROTECTED &BODY CLEANUP*
"))


(deftest test-generic-function ()
  ;; Referring to a GENERIC-FUNCTION as FUNCTION
  (check-head (xref 'test-gf 'function)
              "<a id=\"MGL-PAX-TEST:TEST-GF%20GENERIC-FUNCTION\"></a>")
  (check-pred (dref 'test-gf 'generic-function)
              "`TEST-GF` is not a link."))


(deftest test-method-combination ()
  (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro)))
    (check-document (dref 'my-comb 'method-combination)
                    "<a id=\"MGL-PAX-TEST:MY-COMB%20METHOD-COMBINATION\"></a>

- [method-combination] **MY-COMB**

    This is `MY-COMB`.
")))


(deftest test-method ()
  (test-method/arglist)
  (signals-not (error)
    (document (dref 'test-gf '(method ((eql #.(find-package :cl)))))
              :stream nil))
  (is (equal (pax::urldecode "MGL-PAX-TEST:TEST-GF%20%28METHOD%20%28%28EQL%20%23%3CPACKAGE%20%22COMMON-LISP%22%3E%29%29%29")
             "MGL-PAX-TEST:TEST-GF (METHOD ((EQL #<PACKAGE \"COMMON-LISP\">)))"))
  (check-document (dref 'test-gf '(method (number)))
                  "<a id=\"MGL-PAX-TEST:TEST-GF%20%28METHOD%20%28NUMBER%29%29\"></a>

- [method] **TEST-GF** *(X NUMBER)*

    `TEST-GF` is not a link. `X` is not a link.
")
  #+sbcl
  (check-pred (list (definitions 'test-gf)
                    "TEST-GF `(method ((eql #<package \"COMMON-LISP\">)))`")
              ;; The id of the link varies by implementation.
              "[`TEST-GF`][")
  (with-failure-expected ((and (alexandria:featurep :clisp)
                               'failure))
    (check-pred (list (definitions 'test-gf)
                      "TEST-GF `(method ((eql #.(find-package :cl))))`")
                "[`TEST-GF`][")))

(deftest test-method/arglist ()
  (check-pred (dref 'test-gf '(method ((eql :bar))))
              "- [method] **TEST-GF** *(X (EQL :BAR))*"))

(deftest test-reader ()
  (check-pred (list (dref 'foo-r '(reader foo))
                    (dref 'foo 'class))
              "- [reader] **FOO-R** *[FOO][b01d]*")
  (check-head (list "FOO-R `(reader foo)`"
                    (dref 'foo-r '(reader foo))
                    (dref 'foo-r 'variable))
              "[`FOO-R`][618a] `(reader foo)`")
  (check-document (dref 'foo-r '(reader foo))
                  "<a id=\"MGL-PAX-TEST:FOO-R%20%28MGL-PAX:READER%20MGL-PAX-TEST::FOO%29\"></a>

- [reader] **FOO-R** *FOO*
"))

(deftest test-writer ()
  (check-pred (list (dref 'foo-w '(writer foo))
                    (dref 'foo 'class))
              "- [writer] **FOO-W** *[FOO][b01d]*")
  (check-head (list "FOO-W `(writer foo)`"
                    (dref 'foo-w '(writer foo))
                    (dref 'foo-w 'variable))
              "[`FOO-W`][2b65] `(writer foo)`"))

(deftest test-accessor ()
  (check-pred (list (dref 'foo-a '(accessor foo))
                    (dref 'foo 'class))
              "- [accessor] **FOO-A** *[FOO][b01d]*")
  (check-head (list "FOO-A `(accessor foo)`"
                    (dref 'foo-a '(accessor foo))
                    (dref 'foo-a 'variable))
              "[`FOO-A`][dbec] `(accessor foo)`"))

(deftest test-structure-accessor ()
  (check-pred (dref 'baz-aaa '(structure-accessor baz))
              "- [structure-accessor] **BAZ-AAA** *BAZ*
"))


(defstruct (baz2 (:include baz)))

(deftest test-structure ()
  (with-failure-expected ((and (alexandria:featurep :abcl)
                               'failure))
    (export '(baz) (find-package :mgl-pax-test))
    (unwind-protect
         (check-pred (dref 'baz2 'structure)
                     "- [structure] **BAZ2** *BAZ*")
      (unexport '(baz) (find-package :mgl-pax-test)))))


(deftest test-declaration ()
  (check-head "SAFETY" "[`SAFETY`][f384]")
  (check-head "SAFETY declaration" "[`SAFETY`][f384] declaration")
  (check-head "[safety][declaration]" "[safety][f384]"))


(deftest test-condition ()
  (check-document
   (list (dref 'transcription-values-consistency-error
                'condition)
         (dref 'transcription-consistency-error
                'condition))
   "<a id=\"MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION\"></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *[TRANSCRIPTION-CONSISTENCY-ERROR][a249]*

    Signalled (with [`CERROR`][4317]) by `TRANSCRIBE` when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.

<a id=\"MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION\"></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    `TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR` and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][238c].

  [238c]: #MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR%20CONDITION \"MGL-PAX:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION\"
  [4317]: CLHS/Body/f_cerror.htm \"CERROR (MGL-PAX:CLHS FUNCTION)\"
  [a249]: #MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR%20CONDITION \"MGL-PAX:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION\"
"))


(deftest test-restart ()
  (check-head "ABORT restart" "[`ABORT`][ae44] restart")
  (check-document (dref 'use-value 'restart)
                  "<a id=\"USE-VALUE%20RESTART\"></a>

- [restart] **USE-VALUE** *VALUE*

    This is the name of the [`RESTART`][38e4] to which [`USE-VALUE`][5406]
    transfers control.

  [38e4]: CLHS/Body/t_rst.htm \"RESTART (MGL-PAX:CLHS CLASS)\"
  [5406]: CLHS/Body/f_abortc.htm \"USE-VALUE (MGL-PAX:CLHS FUNCTION)\"
"))


(deftest test-asdf-system ()
  (with-test ("name is a symbol accessible in the current package")
    (is (find-symbol (string '#:mgl-pax/full) '#:mgl-pax-test))
    (check-head (list "MGL-PAX/FULL"
                      (dref :mgl-pax/full 'asdf:system))
                "`MGL-PAX/FULL`")
    (check-head (list "MGL-PAX/FULL asdf:system"
                      (dref 'mgl-pax/full 'asdf:system))
                "[`MGL-PAX/FULL`][d761] asdf:system")
    (check-head (list "[MGL-PAX/FULL][asdf:system]"
                      (dref 'mgl-pax/full 'asdf:system))
                "[`MGL-PAX/FULL`][d761]"))
  (with-test ("name is not a symbol accessible in the current package")
    (is (null (find-symbol (string '#:mgl-pax-test) '#:mgl-pax-test)))
    (check-head (list "MGL-PAX-TEST"
                      (dref "mgl-pax-test" 'asdf:system))
                "MGL-PAX-TEST")
    (check-head (list "MGL-PAX-TEST asdf:system"
                      (dref "mgl-pax-test" 'asdf:system))
                "MGL-PAX-TEST asdf:system")
    (check-head (list "`MGL-PAX-TEST` asdf:system"
                      (dref "mgl-pax-test" 'asdf:system))
                "[`MGL-PAX-TEST`][cad4] asdf:system")
    (check-head (list "[MGL-PAX-TEST][asdf:system]"
                      (dref "mgl-pax-test" 'asdf:system))
                "[MGL-PAX-TEST][cad4]")))


(defpackage interned-pkg-name)
(defpackage #:non-interned-pkg-name)

(deftest test-package ()
  (check-head (list "INTERNED-PKG-NAME"
                    (dref 'interned-pkg-name 'package))
              "`INTERNED-PKG-NAME`")
  (check-head (list "INTERNED-PKG-NAME package"
                    (dref 'interned-pkg-name 'package))
              "[`INTERNED-PKG-NAME`][0651] package")
  (check-head (list "[INTERNED-PKG-NAME][package]"
                    (dref 'interned-pkg-name 'package))
              "[`INTERNED-PKG-NAME`][0651]")
  (let ((*package* (find-package :mgl-pax-test)))
    (is (not (internedp '#:non-interned-pkg-name))))
  (check-head (list "NON-INTERNED-PKG-NAME"
                    (dref '#:non-interned-pkg-name 'package))
              "NON-INTERNED-PKG-NAME")
  (check-head (list "NON-INTERNED-PKG-NAME package"
                    (dref '#:non-interned-pkg-name 'package))
              "NON-INTERNED-PKG-NAME package")
  (check-head (list "[NON-INTERNED-PKG-NAME][package]"
                    (dref '#:non-interned-pkg-name 'package))
              "[NON-INTERNED-PKG-NAME][5a00]")
  (check-pred (dref "PAX" 'package)
              "- [package] **\"MGL-PAX\"** *NICKNAMES (\"PAX\")*"))


(deftest test-readtable ()
  (with-failure-expected ((alexandria:featurep :abcl))
    (check-document (named-readtables:find-readtable 'xxx-rt)
                    "<a id=\"MGL-PAX-TEST:XXX-RT%20READTABLE\"></a>

- [readtable] **XXX-RT**

    ddd
"))
  (check-head (list "[XXX-RT][readtable]"
                    (named-readtables:find-readtable 'xxx-rt))
              "[`XXX-RT`][ec74]"))


;;;; PAX::@PAX-LOCATIVES

(deftest test-locative ()
  (check-document (dref 'pax::funny-loc 'locative)
                  "<a id=\"MGL-PAX:FUNNY-LOC%20MGL-PAX:LOCATIVE\"></a>

- [locative] **MGL-PAX::FUNNY-LOC** *SOME-ARG*

    This is `SOME-ARG`.
" :package (find-package :cl)))

(define-glossary-term @external-link (:title "See X"
                                      :url "http://example.com/x")
                      "docstring")

(deftest test-section ()
  (check-head "PAX::@INTRODUCTION" "Introduction" :format :plain)
  (check-head "PAX::@INTRODUCTION" "Introduction")
  (check-head "PAX::@INTRODUCTION" "Introduction" :format :html))

(deftest test-glossary-term ()
  (with-test ("external links")
    (check-document "@EXTERNAL-LINK" "[See X][ffc6]

  [ffc6]: http://example.com/x \"See X\"
")
    (check-document "[xxx][@external-link]" "[xxx][ffc6]

  [ffc6]: http://example.com/x \"See X\"
")
    (check-document
     @external-link
     "<a id=\"MGL-PAX-TEST:@EXTERNAL-LINK%20MGL-PAX:GLOSSARY-TERM\"></a>

- [glossary-term] **See X**

    External link to [http://example.com/x](http://example.com/x).

    docstring
"))
  (check-head "PAX::@WORD" "word" :format :plain)
  (check-head "PAX::@WORD" "word")
  (check-head "PAX::@WORD" "word" :format :html))

(deftest test-go ()
  (let ((p (dref 'print 'function))
        (g1 (dref:dref nil '(go (print function))))
        (g2 (dref:dref nil '(go (nil (go (print function)))))))
    (with-failure-expected ((alexandria:featurep :clisp))
      (is (equal (multiple-value-list (arglist p))
                 (multiple-value-list (arglist g1))))
      (is (equal (multiple-value-list (arglist p))
                 (multiple-value-list (arglist g2))))
      (is (equal (multiple-value-list (docstring p))
                 (multiple-value-list (docstring g1)))))
    (is (equal (multiple-value-list (docstring p))
               (multiple-value-list (docstring g2))))
    (when (source-location #'print)
      (is (equal (multiple-value-list (source-location p))
                 (multiple-value-list (source-location g1))))
      (is (equal (multiple-value-list (source-location p))
                 (multiple-value-list (source-location g2))))))
  (with-test ("canonicalize GO target")
    (check-ref (dref 'xxx '(go (stream type)))
               'xxx '(go (stream #-abcl class
                                 #+abcl structure))))
  (check-head "[XXX][(go (3.4.1 clhs))]" "[`XXX`][4336]")
  (check-head "[&KEY][(go (3.4.1 clhs))]" "[`&KEY`][4336]")
  (check-head "&KEY" "[`&KEY`][4336]")
  (check-head "XXX `(go (3.4.1 clhs))`" "[`XXX`][4336] `(go (3.4.1 clhs))`")
  (check-head "&KEY `(go (1 clhs))`" "[`&KEY`][81be] `(go (1 clhs))`")
  (with-test ("accidental GO with no arguments")
    (check-head "PRINT go" "[`PRINT`][d451] go")))

(deftest test-docstring ()
  (check-head "[BAR CONSTANT][docstring]" "`BAR` is not a link.")
  (check-head (list "[BAR CONSTANT][docstring]"
                    (dref 'bar 'constant))
              "[`BAR`][f3f4] is not a link.")
  (check-head "[*TEST-VARIABLE* VARIABLE][docstring]"
              "`*TEST-VARIABLE*` is not a link.")
  (check-head "x[ook function][docstring]y" "xy" :warnings 1))

(deftest test-include ()
  (check-pred @inc "This is in [`@INC`][0a52]"))

(deftest test-hyperspec ()
  (check-head "FIND-IF" "[`FIND-IF`][5884]")
  (check-head "LIST" "`LIST`([`0`][79d8] [`1`][6d9f])")
  (check-head "[LIST][type]" "[`LIST`][79d8]")
  (check-head "T" "`T`")
  (check-head "NIL" "`NIL`")
  (check-head "[T][]" "`T`([`0`][9172] [`1`][fe21])")
  (check-head "[T][constant]" "[`T`][fe21]")
  (check-pred #'print (lambda (output)
                        (search "- [function] **PRINT**" output))))

(deftest test-clhs-definitions ()
  (check-ref (dref 'function '(clhs class) nil)
             'function '(clhs class))
  (check-ref (dref 'function '(clhs macro) nil)
             'function '(clhs macro))
  (check-ref-sets (dref-apropos '#:|print| :dtype 'clhs)
                  (list (xref 'print '(clhs function))))
  (is (null (dref 'function '(clhs xxx) nil)))
  (is (null (dref 'xxx '(clhs function) nil)))
  (with-test ("disambiguation paged preferred to section and glossary entry")
    (check-ref (dref 'function 'clhs nil)
               'function 'clhs))
  (check-head "[function][(clhs class)]" "[function][119e]")
  (check-head "[function][(clhs macro)]" "[function][81f7]")
  (with-test ("disambiguation page")
    (check-head "[function][clhs]" "[function][aeb6]"))
  (check-head "[PRINT][clhs]" "[`PRINT`][d451]")
  (with-test ("clhs entry does not clutter disambiguations")
    (check-document (list "[PRINT][clhs]" "PRINT")
                    "[`PRINT`][d451]

[`PRINT`][d451]

  [d451]: CLHS/Body/f_wr_pr.htm \"PRINT (MGL-PAX:CLHS FUNCTION)\"
")
    (check-document (list "PRINT" "[PRINT][clhs]")
                    "[`PRINT`][d451]

[`PRINT`][d451]

  [d451]: CLHS/Body/f_wr_pr.htm \"PRINT (MGL-PAX:CLHS FUNCTION)\"
"))
  (with-test ("prefer live definition to CLHS")
    (with-failure-expected ((alexandria:featurep '(:or :abcl :allegro :ecl)))
      (check-head (list "[PRINT][function]" #'print) "[`PRINT`][fdd1]"))
    (with-failure-expected ((alexandria:featurep
                             '(:or :abcl :allegro :ccl :clisp :cmucl)))
      (check-head (list "[DOCUMENTATION][generic-function]" #'documentation)
                  "[`DOCUMENTATION`][68f1]")))
  (when (null (dref 'otherwise 'macro nil))
    (with-test ("if no live definition, then link to CLHS")
      (check-head "[otherwise][macro]" "[otherwise][c9ce]")
      (check-head "[OTHERWISES][macro]" "[`OTHERWISE`s][c9ce]")))
  (with-test ("explicit definition link always works")
    (check-head "[PRINT][pax:clhs]" "[`PRINT`][d451]"))
  (with-test ("clhs fallback link is EQ to explicit link")
    (check-head (list "[PRINT][function] [PRINT][pax:clhs]")
                "[`PRINT`][d451] [`PRINT`][d451]")
    (check-head (list "[PRINT][clhs] [PRINT][function]")
                "[`PRINT`][d451] [`PRINT`][d451]")))

(deftest test-clhs-section ()
  (let ((*document-link-to-hyperspec* t))
    (test-clhs-section-1))
  (with-failure-expected ()
    (let ((*document-link-to-hyperspec* nil))
      (test-clhs-section-1)))
  (is (endp (definitions "lambda-li" :dtype 'top)))
  (check-ref-sets (definitions "lambda lists" :dtype 'top)
                  `(,(xref "3.4" '(pax:clhs section))))
  (is (endp (definitions "lambda-list-directed" :dtype 'top)))
  (check-ref-sets (dref-apropos "lambda-list-directed" :dtype 'top)
                  `(,(xref "3.4.4.1.2" '(pax:clhs section)))))

(defun test-clhs-section-1 ()
  ;; "A.1" and "3.4" are section ids in the CLHS.
  (check-ref (dref "A.1" '(clhs section) nil)
             "A.1" '(clhs section))
  (check-ref (dref "a.1" '(clhs section) nil)
             "A.1" '(clhs section))
  (check-ref (dref "lambda lists" '(clhs section) nil)
             "3.4" '(clhs section))
  (check-ref (dref "Lambda Lists" '(clhs section) nil)
             "3.4" '(clhs section))
  (check-head "A.1" "A.1")
  (check-head "`A.1`" "`A.1`")
  (check-head "CLHS A.1" "`CLHS` A.1")
  (check-head "CLHS 3.4" "`CLHS` 3.4")
  (check-head "CLHS `3.4`" "`CLHS` [`3.4`][e442]")
  (check-head "`3.4` CLHS" "[`3.4`][e442] `CLHS`")
  (check-head "[3.4][]" "3.4" :warnings 1)
  (check-head "[`3.4`][]" "`3.4`" :warnings 1)
  (check-head "[3.4][CLHS]" "[3.4][e442]")
  (check-head "[Lambda Lists][clhs]" "[Lambda Lists][e442]")
  (check-head "[03_d][clhs]" "[03\\_d][e442]")
  (check-head "[ Lambda  Lists ][clhs]" "[ Lambda  Lists ][e442]")
  (check-ref (dref "#:" '(clhs section) nil)
             "2.4.8.5" '(clhs section))
  (check-ref (dref "~f" 'clhs nil)
             "22.3.3.1" '(clhs section))
  (check-head "[#:][clhs]" "[\\#:][ac5e]")
  (check-head "[~~f][clhs]" "[~~f][cae2]"))

(deftest test-clhs-glossary-entries ()
  (check-head "[readably][(clhs glossary-term)]" "[readably][278a]")
  (check-document "[non-local exit][clhs]" "[non-local exit][b815]

  [b815]: CLHS/Body/26_glo_n.htm#non-local_exit \"\\\"non-local exit\\\" (MGL-PAX:CLHS MGL-PAX:GLOSSARY-TERM)\"
")
  (check-head "[ non-local~%exit ][(clhs glossary-term)]"
              "[ non-local~%exit ][b815]"))

(deftest test-clhs-issue ()
  (let ((*document-link-to-hyperspec* t))
    (test-clhs-issue-1))
  (let ((*document-link-to-hyperspec* nil))
    (test-clhs-issue-1)))

(defun test-clhs-issue-1 ()
  (check-ref (dref "ISSUE:AREF-1D" 'clhs nil)
             '"ISSUE:AREF-1D" '(clhs section))
  (check-ref (dref "issue:aref-1d" 'clhs nil)
             '"ISSUE:AREF-1D" '(clhs section))
  (check-ref (dref "iss009" 'clhs nil)
             '"SUMMARY:AREF-1D" '(clhs section))
  (check-ref (dref "ISS009" 'clhs nil)
             '"SUMMARY:AREF-1D" '(clhs section))
  (check-head "ISSUE:AREF-1D" "ISSUE:AREF-1D")
  (check-head "`ISSUE:AREF-1D`" "`ISSUE:AREF-1D`")
  (check-head "CLHS ISSUE:AREF-1D" "`CLHS` ISSUE:AREF-1D")
  (check-head "ISSUE:AREF-1D CLHS" "ISSUE:AREF-1D `CLHS`")
  (check-head "CLHS `ISSUE:AREF-1D`" "`CLHS` [`ISSUE:AREF-1D`][63ef]")
  (check-head "`ISSUE:AREF-1D` CLHS" "[`ISSUE:AREF-1D`][63ef] `CLHS`")
  (check-head "[ISSUE:AREF-1D][]" "ISSUE:AREF-1D" :warnings 1)
  (check-head "[`ISSUE:AREF-1D`][]" "`ISSUE:AREF-1D`" :warnings 1)
  (check-head "[ISSUE:AREF-1D][CLHS]" "[ISSUE:AREF-1D][63ef]")
  (check-head "[issue:aref-1d][CLHS]" "[issue:aref-1d][63ef]")
  (check-head "[iss009][clhs]" "[iss009][e357]")
  (check-head "[ISS009][clhs]" "[ISS009][e357]"))


(deftest test-argument ()
  (check-head "[PRINT][argument]" "`PRINT`")
  (check-head (list #'argument-shadow (dref 'section 'class))
              "<a id=\"MGL-PAX-TEST:ARGUMENT-SHADOW%20FUNCTION\"></a>

- [function] **ARGUMENT-SHADOW** *SECTIONS*

    `SECTIONS`, `SECTIONS`s, `SECTIONS`, `SECTIONS`s, `SECTIONS`,
    [`SECTIONS`][5fac]")
  (check-head "STREAM argument" "`STREAM` argument")
  (check-head "STREAM dislocated" "`STREAM` dislocated")
  (check-head "[STREAM][argument]" "`STREAM`")
  (check-head "[STREAM][dislocated]" "`STREAM`"))

(defun argument-shadow (sections)
  "SECTIONS, SECTIONSs, [SECTIONS][], [SECTIONSs][], [SECTIONS][argument],
  [SECTIONS][section class]"
  sections)


(define-locative-alias %%%defun function)

(deftest test-define-locative-alias ()
  (check-head (list "FOO %%%defun"
                    (dref 'foo 'function)
                    (dref 'foo 'compiler-macro))
              "[`FOO`][bc64] %%%defun"))


(deftest test-apropos ()
  (test-read-apropos-name-pattern-from-string)
  (test-read-apropos-dtype)
  (test-read-apropos-package-pattern-from-string)
  (test-pax-apropos*))

(deftest test-read-apropos-name-pattern-from-string ()
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "")
        (null *)
        (eql * 0)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string " ")
        (null *)
        (eql * 1)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "  ")
        (null *)
        (eql * 1)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string ":xyz")
        (equal (symbol-name *) "xyz")
        (eql * 4)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string ":xyz ")
        (equal (symbol-name *) "xyz")
        (eql * 4)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "xyz")
        (equal * "xyz")
        (eql * 3)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "xyz ")
        (equal * "xyz")
        (eql * 3)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "\"x y\"")
        (equal * "x y")
        (eql * 5)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "\"x y\" ")
        (equal * "x y")
        (eql * 5)))
  (is (match-values (mgl-pax::read-apropos-name-pattern-from-string "x y")
        (equal * "x")
        (eql * 1)))
  (signals (error)
    (mgl-pax::read-apropos-name-pattern-from-string "\"x")))

(deftest test-read-apropos-dtype ()
  (is (eq (mgl-pax::read-apropos-dtype "") t))
  (is (eq (mgl-pax::read-apropos-dtype " ") t))
  (signals (error)
    (mgl-pax::read-apropos-dtype "(fun"))
  (signals (error)
    (mgl-pax::read-apropos-dtype "|x")))

(deftest test-read-apropos-package-pattern-from-string ()
  (is (null (mgl-pax::read-apropos-package-pattern-from-string "")))
  (is (null (mgl-pax::read-apropos-package-pattern-from-string " ")))
  (is (eq (mgl-pax::read-apropos-package-pattern-from-string ":none")
          :none))
  (is (eq (mgl-pax::read-apropos-package-pattern-from-string ":NONE")
          :none))
  (is (eq (mgl-pax::read-apropos-package-pattern-from-string ":any")
          :any))
  (is (eq (mgl-pax::read-apropos-package-pattern-from-string " :any ")
          :any))
  (is (equal (mgl-pax::read-apropos-package-pattern-from-string "x y")
             "x y"))
  (is (uninterned-symbol=
       (mgl-pax::read-apropos-package-pattern-from-string ":cl")
       "cl"))
  (is (uninterned-symbol=
       (mgl-pax::read-apropos-package-pattern-from-string ":|x y|")
       "x y"))
  (signals (error :pred "Trailing junk from index 2")
    (mgl-pax::read-apropos-package-pattern-from-string " :x y"))
  (signals (error :pred "Trailing junk from index 3")
    (mgl-pax::read-apropos-package-pattern-from-string " \"x\" y")))

(deftest test-pax-apropos* ()
  (is (match-values (mgl-pax::pax-apropos*-process-args "foo" "")
        (equal * "foo")
        (eq * t)
        (null *)))
  (is (match-values (mgl-pax::pax-apropos*-process-args "\"lambda list\""
                                                        ":none")
        (equal * "lambda list")
        (eq * t)
        (eq * :none)))
  (is (match-values (mgl-pax::pax-apropos*-process-args "lambda" ":ANY")
        (equal * "lambda")
        (eq * t)
        (eq * :any)))
  (is (match-values (mgl-pax::pax-apropos*-process-args ":foo" "mgl")
        (uninterned-symbol= * "foo")
        (eq * t)
        (equal * "mgl")))
  (is (match-values (mgl-pax::pax-apropos*-process-args
                     ":|f o| (or function pax:macro)"
                     "\"x y\"")
        (uninterned-symbol= * "f o")
        (equal * '(or function macro))
        (equal * "x y")))
  (with-test ("for mgl-pax-apropos-package")
    (is (match-values (mgl-pax::pax-apropos*-process-args
                       "" (list (package-name (find-package '#:mgl-pax-test))))
          (null *)
          (eq * t)
          (uninterned-symbol= * (string '#:mgl-pax-test))))))

(defun uninterned-symbol= (obj name)
  (and (symbolp obj)
       (null (symbol-package obj))
       (equal (symbol-name obj) name)))


(deftest test-cl-transcript ()
  (let ((*document-hyperspec-root* "CLHS/")
        (input "```cl-transcript
(print :hello)
..
.. :HELLO 
=> :HELLO
```")
        (expected "```common-lisp
(print :hello)
..
.. :HELLO 
=> :HELLO
```

"))
    (check-document input expected)
    (signals (transcription-consistency-error)
      (document "```cl-transcript
(print :hello)
..
.. :WORLD
=> :HELLO
```"))))


(defvar *testing-bad-transcript* nil)

(defsection @must-have ()
  (foo-with-bad-transcript function))
(defun foo-with-bad-transcript ()
  "```cl-transcript (:dynenv check-transcript-only-if-testing)
  (1+ 2)
  => 7
  ```"
  nil)

(defun check-transcript-only-if-testing (fn)
  ;; Silence the warning if someone does an apropos on this.
  (let ((*transcribe-check-consistency* *testing-bad-transcript*))
    (funcall fn)))

(deftest test-document/open ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (with-test ("simplified ambiguous links")
      (check-head "AMBI" "[**`AMBI`**](pax:MGL-PAX-TEST:AMBI)" :format :md-w3m))
    (is (find 'package (definitions 'cl) :key #'xref-locative-type))
    (with-test ("escaping of non-ambiguous")
      (check-head "`foo<>&`"
                  "<a href=\"pax:MGL-PAX-TEST:FOO%3C%3E%26%20FUNCTION\" title=\"MGL-PAX-TEST:FOO&lt;&gt;&amp; FUNCTION\"><strong><code>foo&lt;&gt;&amp;</code></strong></a>"
                  :format :w3m))
    (with-test ("escaping of ambiguous")
      (check-head "`ambi<>&`"
                  "<a href=\"pax:MGL-PAX-TEST:AMBI%3C%3E%26\" ><strong><code>ambi&lt;&gt;&amp;</code></strong></a>"
                  :format :w3m)))
  (let ((*error-output* (make-broadcast-stream))
        (*testing-bad-transcript* t))
    (check-head (dref 'foo-with-bad-transcript 'function) ""
                :warnings 1 :format :w3m))
  (with-test ("[in package]")
    (let ((*package* (find-package '#:mgl-pax-test)))
      (check-pred @test-examples (lambda (output)
                                   (not (search "in package" output)))
                  :format :md-w3m)))
  (test-document/open/live-vs-static)
  (test-document/open/object)
  (test-document/open/clhs)
  (test-document/open/undefined))

(deftest test-document/open/live-vs-static ()
  (with-test ("prefer live definition to CLHS")
    (with-failure-expected
        ((alexandria:featurep '(:or :abcl :allegro :clisp :ecl)))
      (check-head "[PRINT][function]" "[**`PRINT`**][fdd1]" :format :md-w3m))
    (with-failure-expected ((alexandria:featurep
                             '(:or :abcl :allegro :ccl :clisp :cmucl)))
      (check-head "[DOCUMENTATION][generic-function]"
                  "[**`DOCUMENTATION`**][68f1]"
                  :format :md-w3m)))
  (when (null (dref 'otherwise 'macro nil))
    (with-test ("if no live definition, then link to CLHS")
      (check-head "[otherwise][macro]" "[otherwise][c9ce]" :format :md-w3m))))

(deftest test-document/open/object ()
  (check-head "[PAX][package] [MGL-PAX][package] [`mgl-pax`][asdf:system]"
              "[PAX][97b3] [MGL-PAX][97b3] [**`mgl-pax`**][6fdb]"
              :format :md-w3m))

(deftest test-document/open/clhs ()
  (let ((*document-hyperspec-root* "CLHS/"))
    (loop for (object locative) in '((print function)
                                     (single-float type))
          do (let ((reference (dref object `(clhs ,locative))))
               (signals-not (error :msg ("REFERENCE=~S" reference))
                 (let ((url (let ((*standard-output* (make-broadcast-stream)))
                              (pax::document-for-emacs/reference reference
                                                                 nil))))
                   (is (stringp url))
                   (is (alexandria:starts-with-subseq "CLHS/" url)))))))
  (with-test ("ambiguous link")
    ;; This used to fail because CLHS aliases are XREFs and not DREFs.
    (let ((pax::*document-open-linking* t))
      (pax:document "DOUBLE-FLOAT" :stream nil :format :html))))

(defsection @bad-section (:export nil)
  (undefined function))

(deftest test-document/open/undefined ()
  (check-head @bad-section
              "<a id=\"MGL-PAX-TEST:@BAD-SECTION%20MGL-PAX:SECTION\"></a>"
              :warnings 1 :format :md-w3m))

(defun ambi ())
(defclass ambi () ())
(defun foo<>& ())
(defun ambi<>& ())
(defclass ambi<>& () ())


(deftest test-map-documentable ()
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ 1))
             "1"))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ nil))
             ""))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ '(nil)))
             "NIL"))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ '(1 2 3)))
             "123"))
  (is (equal (with-output-to-string (*standard-output*)
               (mgl-pax::map-documentable 'princ '((progv '(*print-base*) '(2))
                                                   1 2 3)))
             "11011"))
  (with-failure-expected ((alexandria:featurep :ecl))
    (is (equal (with-output-to-string (*standard-output*)
                 (mgl-pax::map-documentable 'princ
                                            '(2
                                              ((progv '(*print-base*) '(2))
                                               3)
                                              4)))
               "2114"))))


(deftest test-table-of-contents ()
  (check-document (list @parent-section-without-title
                        @test-examples)
                  "- [MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION][74ce]
- [MGL-PAX-TEST:@TEST-EXAMPLES MGL-PAX:SECTION][bb1c]

<a id=\"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

# MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION

## Table of Contents

- [1 MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION][eeac]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## 1 MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION

<a id=\"MGL-PAX-TEST:@TEST-EXAMPLES%20MGL-PAX:SECTION\"></a>

# MGL-PAX-TEST:@TEST-EXAMPLES MGL-PAX:SECTION

###### \\[in package MGL-PAX-TEST\\]
example section

  [74ce]: #MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION\"
  [bb1c]: #MGL-PAX-TEST:@TEST-EXAMPLES%20MGL-PAX:SECTION \"MGL-PAX-TEST:@TEST-EXAMPLES MGL-PAX:SECTION\"
  [eeac]: #MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION\"
")
  (test-table-of-contents-reapated-section-depth))

(deftest test-table-of-contents-reapated-section-depth ()
  ;; Sometimes we want to document the same thing on two pages. For
  ;; example, a section maybe go on the page of its parent section,
  ;; and it may also have its own page. In that case, check that the
  ;; PAX::*HEADING-LEVEL* on the separate page is the same as the
  ;; heading level when in the parent context.
  (check-document (list @parent-section-without-title @section-without-title)
                  "- [MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION][74ce]
- [MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION][eeac]

<a id=\"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

# MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION

## Table of Contents

- [1 MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION][eeac]

###### \\[in package MGL-PAX-TEST\\]
<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## 1 MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION

<a id=\"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION\"></a>

## MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION

###### \\[in package MGL-PAX-TEST\\]

  [74ce]: #MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"MGL-PAX-TEST:@PARENT-SECTION-WITHOUT-TITLE MGL-PAX:SECTION\"
  [eeac]: #MGL-PAX-TEST:@SECTION-WITHOUT-TITLE%20MGL-PAX:SECTION \"MGL-PAX-TEST:@SECTION-WITHOUT-TITLE MGL-PAX:SECTION\"
"))


(deftest test-definitions-for-pax-url-path ()
  (signals (error :pred "Bad DREF::@NAME")
    (let ((*package* (find-package '#:mgl-pax)))
      (pax::definitions-for-pax-url-path "SECTION LOCATIVE")))
  (signals (error :pred "Bad DREF::@LOCATIVE-TYPE")
    (pax::definitions-for-pax-url-path "MGL-PAX:SECTION JUNK"))
  (with-failure-expected ((alexandria:featurep '(:or :abcl :clisp)))
    (dolist (dref (list (dref 'section 'locative)
                        (dref 'test-gf
                              '(method ((eql #.(find-package :cl)))))))
      (check-ref-sets
       (pax::definitions-for-pax-url-path
        (nth-value 2 (pax::parse-url (pax::dref-to-pax-url dref))))
       (list dref)))))

(deftest test-with-document-context ()
  (signals-not (error)
    ;; This used to fail trying to concatenate format control strings,
    ;; but apparently "format control" (CLHS) can be a function.
    (pax::with-document-context
      (let ((*error-output* (make-broadcast-stream)))
        ;; This is to ensure that the warning is not deferred until
        ;; the end of any enclosing compilation unit (e.g.
        ;; ASDF:TEST-SYSTEM).
        (with-compilation-unit (:override t)
          (compile nil '(lambda () x)))))))

(deftest test-asdf-system-name-of ()
  (with-failure-expected ((alexandria:featurep :clisp))
    (is (equal "mgl-pax" (mgl-pax::asdf-system-name-of mgl-pax::@pax-manual)))
    (with-test ("cached")
      (mgl-pax::with-filename-to-asdf-system-name-map
        (is (equal "mgl-pax"
                   (mgl-pax::asdf-system-name-of mgl-pax::@pax-manual)))))
    (autoload::without-redefinition-warnings
      (eval '(defun no-source ())))
    (is (null (mgl-pax::asdf-system-name-of (dref 'no-source 'function))))
    (with-failure-expected ((not (dref-test::working-locative-p 'package)))
      (is (equal (mgl-pax::asdf-system-name-of* (dref 'no-source 'function))
                 "mgl-pax-test")))))

(deftest test-guess-package-from-arglist ()
  (signals-not (error)
    (is (null (mgl-pax::guess-package-from-arglist '(1 3))))))


;;; See HACKING.md.
(deftest test-pdf ()
  ;; KLUDGE: Hangs on CLISP.
  (with-skip ((or (alexandria:featurep :clisp)
                  (not (zerop (nth-value 2 (uiop:run-program
                                            '("which" "pdflatex")
                                            :ignore-error-status t))))))
    (with-test ()
      (let ((*document-downcase-uppercase-code* t)
            (pax::*document-transcribe-check-consistency*
              (alexandria:featurep :sbcl))
            (pax::*pandoc-output-format* "latex")
            (pax::*git-version-for-test* "master")
            (*document-pandoc-pdf-options*
              (remove "--verbose" *document-pandoc-pdf-options*
                      :test #'equal))
            (documentable (list #'fn-with-mathjax)))
        ;; This is tolerably slow.
        (document
         documentable
         :pages (let ((source-uri-fn (make-git-source-uri-fn
                                      :mgl-pax
                                      "https://github.com/melisgl/mgl-pax"))
                      (file #+sbcl "test/data/test-output.tex"
                            #-sbcl "test/data/test-output-not-checked-in.tex"))
                  `((:objects (,@documentable)
                     :output (,(asdf:system-relative-pathname
                                "mgl-pax" file)
                              ,@pax::*default-output-options*)
                     :uri-fragment ,file
                     :source-uri-fn ,source-uri-fn)))
         :format :pdf)))))

(deftest test-dummy-output ()
  ;; The transcripts are created on SBCL, so they should match there.
  #+sbcl
  (is (zerop (length
              (with-output-to-string (*standard-output*)
                (document (pax::pax-and-dref-sections)
                          :pages (pax::pax-and-dref-pages :markdown)
                          :format nil))))))

(deftest test-pax-transcripts ()
  ;; The transcripts are created on SBCL, so they should match there.
  #+sbcl
  (signals-not (transcription-error :handler #'continue)
    (pax::update-pax-readmes :output-dir "test/data/"))
  ;; On other platforms, just power through transcription errors, and
  ;; check if the readmes can be generated at all.
  #-sbcl
  (signals-not (error)
    (handler-bind ((transcription-error #'continue))
      (time (document (list pax::@pax-manual dref::@dref-manual)
                      :format nil))))
  #+sbcl
  (check-files-the-same
   (asdf:system-relative-pathname "mgl-pax" "README")
   (asdf:system-relative-pathname "mgl-pax" "test/data/README"))
  #+sbcl
  (check-files-the-same
   (asdf:system-relative-pathname "mgl-pax" "README.md")
   (asdf:system-relative-pathname "mgl-pax" "test/data/README.md"))
  #+sbcl
  (check-files-the-same
   (asdf:system-relative-pathname "mgl-pax" "dref/README")
   (asdf:system-relative-pathname "mgl-pax" "test/data/dref/README"))
  #+sbcl
  (check-files-the-same
   (asdf:system-relative-pathname "mgl-pax" "dref/README.md")
   (asdf:system-relative-pathname "mgl-pax" "test/data/dref/README.md")))

(defun check-files-the-same (file1 file2)
  (is (equal (alexandria:read-file-into-string (% file1))
             (alexandria:read-file-into-string (% file2)))
      :capture nil))
