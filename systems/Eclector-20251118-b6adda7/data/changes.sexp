(:changes
 (:release "0.12" nil
  (:item
   (:paragraph
    "The" "deprecated" "generic" "functions"
    (:symbol "eclector.reader:call-with-current-package")
    "has" "been" "removed" "." "Clients" "should" "use"
    (:symbol "eclector.base:call-with-state-value") "with" "the"
    (:symbol "cl:*package*") "aspect" ".")))

 (:release "0.11" "2025-06-08"
  (:item
   (:paragraph
    "Major" "incompatible" "change")
   (:paragraph
    "A" (:tt "children") "parameter" "has" "been" "added" "to" "the" "lambda"
    "list" "of" "the" "generic" "function"
    (:symbol "eclector.parse-result:make-skipped-input-result") "so" "that"
    "results" "which" "represent" "skipped" "material" "can" "have" "children"
    "." "For" "example" "," "before" "this" "change" "," "a"
    (:symbol "eclector.parse-result:read") "call" "which" "encountered" "the"
    "expression" (:tt "#+no-such-feature foo bar") "potentially" "constructed"
    "parse" "results" "for" "all" "(" "recursive" ")" (:symbol "read") "calls"
    "," "that" "is" "for" "the" "whole" "expression" "," "for"
    (:tt "no-such-feature") "," "for" (:tt "foo") "and" "for" (:tt "bar") ","
    "but" "the" "parse" "results" "for" (:tt "no-such-feature") "and"
    (:tt "foo") "could" "not" "be" "attached" "to" "a" "parent" "parse"
    "result" "and" "were" "thus" "lost" "." "In" "other" "words" "the" "shape"
    "of" "the" "parse" "result" "tree" "was")
   (:code nil "skipped input result #+no-such-feature foo
expression result    bar")
   (:paragraph
    "With" "this" "change" "," "the" "parse" "results" "in" "question" "can"
    "be" "attached" "to" "the" "parse" "result" "which" "represents" "the"
    "whole" (:tt "#+no-such-feature foo") "expression" "so" "that" "the"
    "entire" "parse" "result" "tree" "has" "the" "following" "shape")
   (:code nil "skipped input result #+no-such-feature foo
  skipped input result no-such-feature
  skipped input result foo
expression result    bar")
   (:paragraph
    "Since" "this" "is" "a" "major" "incompatible" "change" "," "we" "offer"
    "the" "following" "workaround" "for" "clients" "that" "must" "support"
    "Eclector" "versions" "with" "and" "without" "this" "change" ":")
   (:code :common-lisp "(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((generic-function #'eclector.parse-result:make-skipped-input-result)
         (lambda-list      (c2mop:generic-function-lambda-list
                            generic-function)))
    (when (= (length lambda-list) 5)
      (pushnew 'skipped-input-children *features*))))
(defmethod eclector.parse-result:make-skipped-input-result
    ((client client)
     (stream t)
     (reason t)
     #+PACKAGE-THIS-CODE-IS-READ-IN::skipped-input-children (children t)
     (source t))
  ...
  #+PACKAGE-THIS-CODE-IS-READ-IN::skipped-input-children (use children)
  ...)")
   (:paragraph
    "The" "above" "code" "pushes" "a" "symbol" "that" "is" "interned" "in" "a"
    "package" "under" "the" "control" "of" "the" "respective" "client" "(" "as"
    "opposed" "to" "the" (:tt "KEYWORD") "package" ")" "onto"
    (:symbol "*features*") "before" "the" "second" "form" "is" "read" "and"
    "uses" "that" "feature" "to" "select" "either" "the" "version" "with" "or"
    "the" "version" "without" "the" (:tt "children") "parameter" "of" "the"
    "method" "definition" "." "See" "Maintaining" "Portable" "Lisp" "Programs"
    "by" "Christophe" "Rhodes" "for" "a" "detailed" "discussion" "of" "this"
    "technique" "."))
  (:item
   (:paragraph
    "The" "new" "condition" "type"
    (:symbol "eclector.reader:state-value-type-error")
    "can" "be" "used" "to" "indicate" "that" "a" "value" "of" "an"
    "unsuitable" "type" "has" "been" "provided" "for" "a" "reader" "state"
    "aspect" "."))
  (:item
   (:paragraph
    "The" "reader" "state" "protocol"
    (:when "manual" "(" (:ref :section "Reader state protocol") ")")
    "now" "provides" "the" "generic" "function"
    (:symbol "(setf eclector.reader:state-value)") "which" "allows"
    "clients" "to" "set" "reader" "state" "aspects" "in" "addition" "to"
    "establishing" "dynamically" "scoped" "bindings" "."))
  (:item
   (:paragraph
    "The" "macros" (:symbol "eclector.reader:unquote") "and"
    (:symbol "eclector.reader:unquote-splicing") "now" "signal" "sensible"
    "errors" "when" "used" "outside" "of" "the" "lexical" "scope" "of" "a"
    (:symbol "eclector.reader:quasiquote") "macro" "call" "." "Note" "that"
    "the" "name" "of" "the" "associated" "condition" "type" "is" "not"
    "exported" "for" "now" "since" "quasiquotation" "will" "be" "implemented"
    "in" "a" "separate" "module" "in" "the" "future" ".")
   (:paragraph
    "Such" "invalid" "uses" "can" "happen" "when" "the" "above" "macros"
    "are" "called" "directly" "or" "when" "the" (:tt ",") "," (:tt ",@")
    "and" (:tt ",.") "reader" "macros" "are" "used" "in" "a" "way" "that"
    "constructs" "the" "unquoted" "expression" "in" "one" "context" "and"
    "then" "\"injects\"" "it" "into" "some" "other" "context" "," "for"
    "example" "via" "an" "object" "reference" (:tt "#N#") "or" "read-time"
    "evaluation" (:tt "#.(...)") "." "Full" "example" ":")
   (:code :common-lisp "(progn
  (print `(a #1=,(+ 1 2) c))
  (print #1#))")
   (:paragraph
    "Another" "minor" "aspect" "of" "this" "change" "is" "that" "the"
    "condition" "types"
    (:symbol "eclector.reader:unquote-splicing-in-dotted-list") "and"
    (:symbol "eclector.reader:unquote-splicing-at-top") "are" "no" "longer"
    "subtypes" "of" (:symbol "common-lisp:stream-error") "." "The" "previous"
    "relation" "did" "not" "make" "sense" "since" "errors" "of" "those"
    "types" "are" "signaled" "during" "macro" "expansion" "."))
  (:item
   (:paragraph
    "Eclector" "now" "uses" "the" "reader" "state" "protocol"
    (:when "manual" "(" (:ref :section "Reader state protocol") ")")
    "instead" "of" "plain" "special" "variables" "to" "query" "and" "track"
    "the" "legality" "of" "quasiquotation" "operations" "and" "the" "consing"
    "dot" "." "The" "additional" "reader" "state" "aspects" "are" "documented"
    "but" "remain" "internal" "for" "now" ".")
   (:paragraph
    "The" "(" "internal" ")" "macro"
    (:symbol "eclector.reader::with-forbidden-quasiquotation")
    "is" "deprecated" "as" "of" "this" "release" "." "Clients" "which" "really"
    "need" "a" "replacement" "immediately" "can" "use" "the" "new" "("
    "internal" ")" "macro"
    (:symbol "eclector.reader::with-quasiquotation-state") "."))
  (:item
   (:paragraph
    "Eclector" "no" "longer" "returns" "incorrect" "parse" "results" "when"
    "custom" "reader" "macros" "bypass" "some" "reader" "functionality" "and"
    "the" "input" "contains" "labeled" "object" "definitions" "or" "references"
    ".")
   (:paragraph
    "An" "example" "of" "a" "situation" "that" "was" "previously" "handled"
    "incorrectly" "is" "the" "following")
   (:code :common-lisp "(defun bypassing-left-parenthesis (stream char)
  (declare (ignore char))
  (loop for peek = (eclector.reader:peek-char t stream t nil t)
        when (eq peek #\\))
          do (eclector.reader:read-char stream t nil t)
             (loop-finish)
        collect (let ((function (eclector.readtable:get-macro-character
                                 eclector.reader:*readtable* peek)))
                  (cond (function
                         (eclector.reader:read-char stream t nil t)
                         (funcall function stream peek))
                        (t
                         (eclector.reader:read stream t nil t))))))

(let ((eclector.reader:*readtable* (eclector.readtable:copy-readtable
                                    eclector.reader:*readtable*)))
  (eclector.readtable:set-macro-character
   eclector.reader:*readtable* #\\( #'bypassing-left-parenthesis)
  (describe (eclector.parse-result:read-from-string
             (make-instance 'eclector.parse-result.test::simple-result-client)
             \"(print (quote #1=(member :floor :ceiling)))\")))
;; [...]
;; Slots with :INSTANCE allocation:
;;   %RAW                           = (PRINT '(MEMBER :FLOOR :CEILING))
;;   %SOURCE                        = (0 . 43)
;; [...]
;; The %RAW slot used to contain (MEMBER :FLOOR :CEILING) instead of
;; (PRINT '(MEMBER :FLOOR :CEILING))."))
  (:item
   (:paragraph
    "The" "reader" "macros" "for" "non-decimal" "radices" "now" "accept"
    (:tt "+") "in" "the" "sign" "part" "." "For" "example" "," "Eclector" "now"
    "accepts" (:tt "#x+10") "as" "a" "spelling" "of" (:tt "16") "."))
  (:item
   (:paragraph
    "The" "reader" "macros" "for" "non-decimal" "radices" "now" "treat"
    "non-terminating" "macro" "characters" "that" "are" "valid" "digits"
    "for" "the" "respective" "rational" "syntax" "as" "digits" "instead"
    "of" "signaling" "an" "error" "." "This" "is" "in" "line" "with" "the"
    "behavior" "for" "tokens" "outside" "of" "those" "reader" "macros" ".")
   (:paragraph
    "As" "an" "example" "," "the" "following" "signaled" "an" "error" "before"
    "this" "change" ":")
   (:code :common-lisp "(let ((eclector.reader:*readtable*
        (eclector.readtable:copy-readtable eclector.reader:*readtable*)))
  (eclector.readtable:set-macro-character
   eclector.reader:*readtable*
   #\\1
   (lambda (stream char)
     (declare (ignore stream char))
     1)
   t) ; non-terminating
  (eclector.reader:read-from-string \"#x01\"))"))
  (:item
   (:paragraph
    "When" "producing" "parse" "results" "and" "recovering" "from" "an"
    "invalid" "input" "of" "a" "form" "like")
   (:code :common-lisp
          "#1=
;; a
;; b
<eof>")
   (:paragraph
    "Eclector" "no" "longer" "returns" "an" "invalid" "parse" "result" "graph"
    "."))
  (:item
   (:paragraph
    "When" "producing" "parse" "results" "and" "recovering" "from" "an"
    "invalid" "input" "of" "a" "form" "like" (:tt "#1=#1#")
    "," "Eclector" "no" "longer" "returns" "an" "invalid" "parse" "result"
    "graph" "."))
  (:item
   (:paragraph
    "The" "new" "generic" "function"
    (:symbol "eclector.reader:new-value-for-fixup") "is" "called" "by"
    (:symbol "eclector.reader:fixup") "to" "compute" "the" "replacement"
    "value" "for" "a" "labeled" "object" "marker" "," "both" "in" "ordinary"
    "objects" "and" "in" "parse" "results" "." "Clients" "can" "define"
    "methods" "on" "the" "new" "generic" "function" "to" "customize" "such"
    "replacements" "which" "is" "probably" "only" "useful" "when" "parse"
    "results" "are" "processed" "since" "there" "is" "not" "a" "lot" "of"
    "leeway" "in" "the" "processing" "of" "ordinary" "objects" "." ))
  (:item
   (:paragraph
    "There" "is" "now" "a" "default" "method" "on"
    (:symbol "eclector.reader:fixup-graph-p") "which" "returns" "true" "if"
    (:symbol "eclector.reader:labeled-object-state") "indicates" "that" "the"
    "labeled" "object" "in" "question" "is" "final" "and" "circular" "."))
  (:item
   (:paragraph
    "When" (:symbol "eclector.parse-result:parse-result-client") "is" "used" ","
    (:symbol "eclector.reader:labeled-object-state") "now" "returns" "inner"
    "labeled" "object" "as" "its" "fourth" "value" "."))
  (:item
   (:paragraph
    "Elector" "now" "breaks" "up" "long" "chains" "of" "recursive"
    (:symbol "eclector.reader:fixup") "calls" "in" "order" "to" "avoid"
    "exhausting" "available" "stack" "space" "." "As" "a" "consequence" ","
    "methods" "on" "the" "generic" "function" (:symbol "eclector.reader:fixup")
    "can" "no" "longer" "assume" "an" "unbroken" "chain" "of" "recursive"
    "calls" "that" "correspond" "to" "the" "nesting" "structure" "of" "the"
    "object" "graph" "that" "is" "being" "fixed" "up" "." "In" "particular" ","
    "a" "call" "for" "an" "inner" "object" "cannot" "rely" "on" "the" "fact"
    "that" "a" "particular" "dynamic" "environment" "established" "by" "a"
    "call" "for" "an" "outer" "object" "is" "still" "active" ".")))

 (:release "0.10" "2024-02-28"
  (:item
   (:paragraph
    "The" "deprecated" "generic" "functions"
    (:symbol "eclector.parse-result:source-position")
    "and" (:symbol "eclector.parse-result:make-source-range") "have" "been"
    "removed" "." "Clients" "should" "use"
    (:symbol "eclector.base:source-position") "and"
    (:symbol "eclector.base:make-source-range") "respectively" "instead"
    "."))
  (:item
   (:paragraph
    "The" "new" "reader" (:symbol "eclector.base:range-length") "can" "be"
    "applied" "to" "conditions" "of" "type"
    (:symbol "eclector.base:stream-position-condition") "(" "which"
    "includes" "almost" "all" "conditions" "related" "to" "syntax" "errors"
    ")" "to" "determine" "the" "length" "of" "the" "sub-sequence" "of" "the"
    "input" "to" "which" "the" "condition" "in" "question" "pertains" "."))
  (:item
   (:paragraph
    "Minor" "incompatible" "change")
   (:paragraph
    "The" "part" "of" "the" "labeled" "objects" "protocol" "that" "allows"
    "clients" "to" "construct" "parse" "results" "which" "represent"
    "labeled" "objects" "has" "been" "changed" "in" "an" "incompatible" "way"
    "." "The" "change" "allows" "parse" "results" "which" "represent"
    "labeled" "objects" "to" "have" "child" "parse" "results" "but" "requires"
    "that" "clients" "construct" "parse" "results" "which" "represent"
    "labeled" "objects" "differently" ":" "instead" "of" "eql-specializing"
    "the" (:tt "result") "parameters" "of" "methods" "on"
    (:symbol "eclector.parse-result:make-expression-result") "to"
    (:symbol "eclector.parse-result:**definition**") "and"
    (:symbol "eclector.parse-result:**reference**") "and" "receiving" "the"
    "labeled" "object" "in" "the" (:tt "children") "parameters" ","
    "the" (:tt "result") "parameters" "now" "have" "to" "be" "specialized"
    "to" "the" "classes" (:symbol "eclector.parse-result:definition") "and"
    (:symbol "eclector.parse-result:reference") "respectively" "." "The"
    "object" "passed" "as" "the" (:tt "result") "argument" "now" "contains"
    "the" "labeled" "object" "so" "that" "the" (:tt "children") "parameter"
    "can" "receive" "child" "parse" "results" ".")
   (:paragraph
    "This" "change" "is" "considered" "minor" "since" "the" "old" "mechanism"
    "described" "above" "was" "not" "documented" "." "For" "now" "," "the"
    "new" "mechanism" "also" "remains" "undocumented" "so" "that" "the"
    "design" "can" "be" "validated" "through" "experimentation" "before" "it"
    "is" "finalized" "."))
  (:item
   (:paragraph
    "The" "new" (:tt "syntax-extensions") "module" "contains" "a"
    "collection" "of" "syntax" "extensions" "which" "are" "implemented" "as"
    "either" "mixin" "classes" "for" "clients" "or" "reader" "macro"
    "functions" "."))
  (:item
   (:paragraph
    "The" "extended" "package" "prefix" "extension" "allows" "prefixing" "an"
    "expression" "with" "a" "package" "designator" "in" "order" "to"
    "read" "the" "expression" "with" "the" "designated" "package" "as" "the"
    "current" "package" "." "For" "example")
   (:code :common-lisp "my-package::(a b)")
   (:paragraph
    "is" "read" "as")
   (:code :common-lisp "(my-package::a my-package::b)")
   (:paragraph
    "with" "this" "extension" "."))
  (:item
   (:paragraph
    "A" "new" "syntax" "extension" "which" "is" "implemented" "by" "the"
    "reader" "macro"
    (:symbol "eclector.syntax-extensions.s-expression-comment:s-expression-comment")
    "allows" "commenting" "out" "s-expressions" "in" "a" "fashion" "similar"
    "to" "SRFI 62" "for" "scheme" "." "One" "difference" "is" "that" "a"
    "numeric" "infix" "argument" "can" "be" "used" "to" "comment" "out" "a"
    "number" "of" "s-expressions" "different" "from" "1" ":")
   (:code :common-lisp "(frob r1 r2 :k3 4 #4; :k5 6 :k6 7)"))
  (:item
   (:paragraph
    "The" (:tt "concrete-syntax-tree") "module" "now" "produces" "a"
    "better" "tree" "structure" "for" "certain" "inputs" "like"
    (:tt "(0 . 0)") "." "Before" "this" "change" "the" "produced" "CST"
    "had" "the" "same" (:symbol "concrete-syntax-tree:atom-cst") "object"
    "as" "the" (:symbol "concrete-syntax-tree:first") "and"
    (:symbol "concrete-syntax-tree:rest") "of" "the" "outer"
    (:symbol "concrete-syntax-tree:cons-cst") "node" "." "After" "this"
    "change" "the" (:symbol "concrete-syntax-tree:first") "child" "is" "the"
    (:symbol "concrete-syntax-tree:atom-cst") "which" "corresponds" "to" "the"
    "first" (:tt "0") "in" "the" "input" "and" "the"
    (:symbol "concrete-syntax-tree:rest") "child" "is" "the"
    (:symbol "concrete-syntax-tree:atom-cst") "which" "corresponds" "to" "the"
    "second" (:tt "0") "in" "the" "input" "." "In" "contrast" "to" "the"
    "previous" "example" "," "an" "input" "like" (:tt "(#1=0 . #1#)")
    "continues" "to" "result" "in" "a" "single"
    (:symbol "concrete-syntax-tree:atom-cst") "in" "both" "the"
    (:symbol "concrete-syntax-tree:first") "and"
    (:symbol "concrete-syntax-tree:rest") "slots" "of" "the" "outer"
    (:symbol "concrete-syntax-tree:cons-cst") "object" ".")))

 (:release "0.9" "2023-03-19"
  (:item
   (:paragraph
    "The" "deprecated" "function"
    (:symbol "eclector.concrete-syntax-tree:cst-read") "has" "been" "removed"
    "." "Clients" "should" "use" (:symbol "eclector.concrete-syntax-tree:read")
    "instead" "."))
  (:item
   (:paragraph
    (:symbol "eclector.reader:find-character") "receives" "characters"
    "names" "with" "unmodified" "case" "and" "is" "also" "called" "in" "the"
    (:tt "#\\<single character>") "case" "so" "that" "clients" "have" "more"
    "control" "over" "character" "lookup" "."))
  (:item
   (:paragraph
    "The" "new" "generic" "function"
    (:symbol "eclector.base:position-offset") "allows" "interested" "clients"
    "to" "refine" "the" "source" "positions" "of" "errors" "obtained" "by"
    "calling" (:symbol "eclector.base:stream-position") "."))
  (:item
   (:paragraph
    "Some" "condition" "and" "restart" "reports" "have" "been" "improved" "."))
  (:item
   (:paragraph
    "A" "discussion" "of" "the" "relation" "between" "circular" "objects"
    "and" "custom" "reader" "macros" "has" "been" "added" "to" "the"
    "manual"
    (:when "manual"
      "(" (:ref :section "Circular objects and custom reader macros") ")")
    "."))
  (:item
   (:paragraph
    "Problems" "in" "the" (:symbol "eclector.reader:fixup") "method" "for"
    "hash" "tables" "have" "been" "fixed" ":" "keys" "were" "not" "checked"
    "for" "circular" "structure" "and" "circular" "structures" "in" "values"
    "were" "not" "fixed" "up" "in" "some" "cases" "."))
  (:item
   (:paragraph
    "Eclector" "provides" "a" "new" "protocol" "for" "handling" "labeled"
    "objects" "," "that" "is" "the" "objects" "defined" "and" "referenced"
    "by" "the" (:tt "#=") "and" (:tt "##") "reader" "macros" "respectively"
    (:when "manual"
      "(" (:ref :section "Labeled objects and references") ")")
    "."))
  (:item
   (:paragraph
    "Eclector" "now" "avoids" "unnecessary" "fixup" "processing" "in"
    "object" "graphs" "with" "complicated" "definitions" "and" "references"
    ".")
   (:paragraph
    "Before" "this" "change" "," "cases" "like")
   (:code :common-lisp
    "#1=(1 #1# #2=(2 #2# ... #100=(100 #100#)))")
   (:paragraph "or")
   (:code :common-lisp
    "#1=(1 #2=(2 ... #2#) ... #1#)")
   (:paragraph
    "led" "to" "unnecessary" "and/or" "repeated" "traversals" "during"
    "fixup" "processing" "."))
  (:item
   (:paragraph
    "Fixup" "processing" "is" "now" "performed" "in" "parse" "result" "objects" ".")
   (:paragraph
    "Before" "this" "change" "," "something" "like")
   (:code :common-lisp
    "(eclector.concrete-syntax-tree:read-from-string \"#1=(#1#)\")")
   (:paragraph
    "produced" "a" "CST" "object" "," "say" (:tt "cst") "," "which" "failed"
    "to" "satisfy")
   (:code :common-lisp
    "(eq (cst:first cst)       cst)
(eq (cst:raw (first cst)) (cst:raw cst))")
   (:paragraph
    "The" "properties" "now" "hold" "."))
  (:item
   (:paragraph
    "Clients" "can" "use" "the" "new" "mixin" "classes"
    (:symbol "eclector.concrete-syntax-tree:definition-csts-mixin") "and"
    (:symbol "eclector.concrete-syntax-tree:reference-csts-mixin") "to"
    "represent" "labeled" "object" "definitions" "and" "references" "as"
    "instances" "of"
    (:symbol "eclector.concrete-syntax-tree:definition-cst")
    "and" (:symbol "eclector.concrete-syntax-tree:reference-cst")
    "respectively" "."))
  (:item
   (:paragraph
    "The" "stream" "position" "in" "conditions" "signaled" "by"
    (:symbol "eclector.reader::sharpsign-colon") "is" "now" "always"
    "present" "."))
  (:item
   (:paragraph
    "When" "Eclector" "is" "used" "to" "produce" "parse" "results" "," "it"
    "no" "longer" "confuses" "end-of-input" "with" "having" "read"
    (:symbol "nil") "when" (:tt "nil") "is" "used" "as" "the"
    (:tt "eof-value") "(" (:tt "nil") "makes" "sense" "as" "an"
    (:tt "eof-value") "in" "that" "case" "since" (:tt "nil") "is"
    "generally" "not" "a" "possible" "parse" "result" ")" "."))
  (:item
   (:paragraph
    "A" "detailed" "description" "of" "the" "constraints" "on" "return"
    "values" "of" "the" "generic" "functions" "in" "the" "Reader" "behavior"
    "protocol" "has" "been" "added" "to" "the" "manual"
    (:when "manual"
      "(" (:ref :section "Reader behavior protocol") ")")
    "."))
  (:item
   (:paragraph
    "The" (:tt "eclector-concrete-syntax-tree") "system" "now" "works"
    "with" "and" "requires" "version" "0.2" "of" "the"
    (:tt "concrete-syntax-tree") "system" "."))
  (:item
   (:paragraph
    "Eclector" "provides" "a" "new" "protocol" "for" "querying" "and"
    "binding" "behavior-changing" "aspects" "of" "the" "current" "state" "of"
    "the" "reader" "such" "as" "the" "current" "package" "," "the" "current"
    "readtable" "and" "the" "current" "read" "base"
    (:when "manual"
      "(" (:ref :section "Reader state protocol") ")")
    ".")
   (:paragraph
    "Clients" "can" "use" "this" "protocol" "to" "control" "the" "reader"
    "state" "in" "other" "ways" "than" "binding" "the" "Common" "Lisp"
    "variables" "," "for" "example" "by" "storing" "the" "values" "of"
    "reader" "state" "aspects" "in" "context" "objects" ".")
   (:paragraph
    "Furthermore" "," "implementations" "which" "use" "Eclector" "as" "the"
    "Common" "Lisp" "reader" "can" "use" "this" "protocol" "to" "tie" "the"
    (:symbol "cl:*readtable*") "aspect" "to" "the" (:symbol "cl:*readtable*")
    "variable" "instead" "of" "the" (:symbol "eclector.reader:*readtable*")
    "variable" ".")
   (:paragraph
    "The" "new" "protocol" "subsumes" "the" "purpose" "of" "the" "generic"
    "function" (:symbol "eclector.reader:call-with-current-package") "which"
    "is" "deprecated" "as" "of" "this" "Eclector" "version" "."))
  (:item
   (:paragraph
    "Eclector" "now" "provides" "and" "uses" "by" "default" "a" "relaxed"
    "version" "of" "the" (:symbol "eclector.reader::sharpsign-s") "reader"
    "macro" "function" "which" "requires" "the" "input" "following"
    (:tt "#S") "to" "be" "read" "as" "a" "list" "but" "not" "necessarily"
    "be" "literally" "written" "as" (:tt "(TYPE INITARG₁ VALUE₁ …)") ".")
   (:paragraph
    "A" "detailed" "discussion" "of" "the" "topic" "has" "been" "added" "to"
    "the" "manual"
    (:when "manual"
      "(" (:ref :section "Interpretation of Sharpsign C and Sharpsign S") ")")
    ".")))

 (:release "0.8" "2021-08-24"
  (:item
   (:paragraph
    "The" "default" (:symbol "eclector.reader:read-token") "method" "and"
    "the" "functions" (:symbol "eclector.reader::sharpsign-colon") "and"
    (:symbol "eclector.reader::sharpsign-backslash") "are" "now" "more"
    "efficient" "as" "well" "as" "less" "redundant" "in" "terms" "of"
    "repeated" "code" "."))
  (:item
   (:paragraph
    "The" "feature" (:symbol ":eclector-define-cl-variables") "now" "controls"
    "whether" "the" "file" (:tt "code/reader/variables.lisp") "is" "loaded"
    "and" "thus" "whether" "the" "variables"
    (:symbol "eclector.reader:*package*") ","
    (:symbol "eclector.reader:*read-eval*") "," "etc" "." "are" "defined" ".")))

 (:release "0.7" "2021-05-16"
  (:item
   (:paragraph
    "The" "incorrectly" "committed" "generic" "function"
    (:symbol "eclector.reader:check-symbol-token") "has" "been" "fixed" "."))
  (:item
   (:paragraph
    "Empty" "escape" "ranges" "like" (:tt "||") "are" "no" "longer"
    "interpreted" "as" "potential" "numbers" "."))
  (:item
   (:paragraph
    "The" "default" (:symbol "eclector.reader:interpret-symbol") "method"
    "now" "signals" "specific" "conditions" "and" "offers" "restarts" "for"
    "recovering" "from" "situations" "related" "to" "non-existent" "packages"
    "and" "symbols" "as" "well" "as" "non-exported" "symbols" ".")
   (:paragraph
    "The" "default" "error" "recovery" "strategy" "for" "invalid" "symbols"
    "now" "constructs" "an" "uninterned" "symbol" "of" "the" "given" "name"
    "instead" "of" "using" (:symbol "nil") "."))
  (:item
   (:paragraph
    "The" "\"consing dot\"" "is" "no" "longer" "accepted" "in"
    "sub-expressions" "of" (:symbol "eclector.reader::left-parenthesis") ".")
   (:paragraph
    "At" "the" "same" "time" "," "it" "is" "now" "possible" "to" "recover"
    "from" "encountering" "the" "\"consing dot\"" "in" "invalid" "positions"
    "."))
  (:item
   (:paragraph
    "The" "default" (:symbol "eclector.reader:interpret-token") "method" "has"
    "been" "optimized" "substantially" "."))
  (:item
   (:paragraph
    "The" (:symbol "eclector.reader:*client*") "variable" "and" "the"
    "source" "location" "protocol" "(" "that" "is" "the" "generic" "functions"
    (:symbol "eclector.parse-result:source-position") "and"
    (:symbol "eclector.parse-result:make-source-range") ")" "have" "been"
    "moved" "to" "a" "new" (:tt "base") "module" "and" "package"
    (:tt "eclector.base") "which" "the" (:tt "reader") "module" "and" "the"
    (:tt "eclector.reader") "package" "can" "use" "." "This" "structure"
    "allows" "code" "in" "the" (:tt "reader") "module" "to" "work" "with"
    "source" "locations" ".")
   (:paragraph
    "The" "name" (:symbol "eclector.base:*client*") "remains" "exported" "as"
    (:symbol "eclector.reader:*client*") ".")
   (:paragraph
    "The" "old" "names" (:symbol "eclector.parse-result:source-position")
    "and" (:symbol "eclector.parse-result:make-source-range") "still" "exist"
    "but" "are" "now" "deprecated" "and" "will" "be" "removed" "in" "a"
    "future" "release" "."))
  (:item
   (:paragraph
    "Conditions" "signaled" "by" "code" "in" "the" (:tt "reader") "module"
    "now" "include" "source" "positions" "which" "are" "obtained" "by"
    "calling" (:symbol "eclector.base:source-position") ".")))

 (:release "0.6" "2020-11-29"
  (:item
   (:paragraph
    "Bogus" (:tt "nil") "parse" "results" "are" "no" "longer" "generated"
    "by" (:symbol "eclector.parse-result:make-skipped-input-result") "calls"
    "when" (:symbol "cl:*read-suppress*") "is" "true" "."))
  (:item
   (:paragraph
    "The" "new" "generic" "functions"
    (:symbol "eclector.reader:read-maybe-nothing") "and"
    (:symbol "eclector.reader:call-as-top-level-read") "give" "clients"
    "additional" "entry" "points" "to" "the" "reader" "as" "well" "as"
    "customization" "possibilities" "." "With" "these" "functions" "," "the"
    "chain" "of" "functions" "calls" "for" "a" (:symbol "read") "call"
    "looks" "like" "this:")
   (:code nil "eclector.reader:read
  eclector.reader:call-as-top-level-read
    eclector.reader:read-common
      eclector.reader:read-maybe-nothing
        ...
          eclector.reader:read-char
          eclector.reader:peek-char")
   (:paragraph
    "Diagrams" "which" "illustrate" "the" "relations" "between" "the" "new"
    "and" "existing" "functions" "have" "been" "added" "to" "the" "manual"
    (:when "manual"
      "(" (:ref :figure "read-call-sequence-ordinary") ","
      (:ref :figure "read-call-sequence-customization") ","
      (:ref :figure "read-call-sequence-parse-result") ")")
    "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader::read-rational") "now"
    "better" "respects" "the" "value" "of" (:symbol "*read-suppress*") "."))
  (:item
   (:paragraph
    "Fix" "return" "value" "of"
    (:symbol "eclector.readtable:set-syntax-from-char") "," "fix"
    (:symbol "(setf eclector.readtable:syntax-from-char)") "to" "also" "copy"
    "the" "macro" "character" "information" "."))
  (:item
   (:paragraph
    "The" "semicolon" "reader" "macro" "now" "consumes" "the" "terminating"
    "newline" "character" "."))
  (:item
   (:paragraph
    "Eclector" "now" "provides" "the" "generic" "function"
    (:symbol "eclector.reader:wrap-in-function") "."))
  (:item
   (:paragraph
    "Reset" (:symbol "eclector.reader::*list-reader*") "around" "recursive"
    "read" "in" (:symbol "eclector.reader::sharpsign-dot") "."))
  (:item
   (:paragraph
    "Implement" "and" "default" "to" "relaxed" "syntax" "for"
    (:symbol "eclector.reader::sharpsign-c") "." "The" "strict" "version" "is"
    "still" "available" "as" (:symbol "eclector.reader:strict-sharpsign-c")
    "and" "can" "be" "installed" "into" "a" "custom" "readtable" ".")
   (:paragraph
    "A" "detailed" "discussion" "of" "the" "topic" "has" "been" "added" "to"
    "the" "manual"
    (:when "manual"
      "(" (:ref :section "Interpretation of Sharpsign C and Sharpsign S") ")")
    "."))
  (:item
   (:paragraph
    "Eclector" "can" "now" "recover" "from" "reading" "invalid" "inputs"
    "like" (:tt "..") "and" (:tt "...") "."))
  (:item
   (:paragraph
    "Implement" "and" "default" "to" "relaxed" "syntax" "for"
    (:symbol "eclector.reader::sharpsign-single-quote") "." "The" "strict"
    "version" "is" "still" "available" "as"
    (:symbol "eclector.reader:strict-sharpsign-single-quote") "and" "can"
    "be" "installed" "into" "a" "custom" "readtable" ".")
   (:paragraph
    "A" "detailed" "discussion" "of" "the" "topic" "has" "been" "added" "to"
    "the" "manual"
    (:when "manual"
      "(" (:ref :section "Interpretation of Backquote and Sharpsign Single Quote") ")")
    "."))
  (:item
   (:paragraph
    "Eclector" "now" "provides" "the" "generic" "function"
    (:symbol "eclector.reader:check-symbol-token") "."))
  (:item
   (:paragraph
    "Input" "of" "the" "form" (:tt "PACKAGE::||") "is" "now" "correctly"
    "read" "as" "a" "symbol" "."))
  (:item
   (:paragraph
    "Eclector" "can" "now" "recover" "from" "reading" "the" "invalid"
    "input" (:tt ":") ".")))

 (:release "0.5" "2020-06-09"
  (:item
   (:paragraph
    "The" "generic" "function"
    (:symbol "eclector.reader:call-with-current-package") "has" "been"
    "added" "."))
  (:item
   (:paragraph
    "The" "previously" "missing" "functions"
    (:symbol "eclector.parse-result:read-preserving-whitespace") "and"
    (:symbol "eclector.parse-result:read-from-string") "have" "been" "added"
    "."))
  (:item
   (:paragraph
    "The" "previously" "missing" "functions"
    (:symbol "eclector.concrete-syntax-tree:read-preserving-whitespace")
    "and" (:symbol "eclector.concrete-syntax-tree:read-from-string") "have"
    "been" "added" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.concrete-syntax-tree:cst-read") "has"
    "been" "renamed" "to" (:symbol "eclector.concrete-syntax-tree:read") "."
    (:symbol "eclector.concrete-syntax-tree:cst-read") "still" "exists" "but"
    "is" "deprecated" "and" "will" "be" "removed" "in" "a" "future" "version"
    "."))
  (:item
   (:paragraph
    "Quasiquote" "and" "unquote" "are" "now" "opt-out" "instead" "of" "opt-in"
    "." "This" "allows" "quasiquotation" "in" "custom" "reader" "macros" "by"
    "default" "." "The" "new" "macro"
    (:symbol "eclector.reader::with-forbidden-quasiquotation") "is" "used" "by"
    "Eclector" "(" "and" "can" "be" "used" "in" "custom" "reader" "macros" ")"
    "to" "control" "this" "behavior" "."))
  (:item
   (:paragraph
    "A" "method" "on" (:symbol "eclector.readtable:readtablep") "for" "the"
    "simple" "readtable" "implementation" "has" "been" "added" "."))
  (:item
   (:paragraph
    "The" "condition" "type" (:symbol "eclector.base:end-of-file") "is" "now"
    "a" "subtype" "of" (:symbol "cl:stream-error") "but" "not" "of"
    (:symbol "cl:reader-error") "."))
  (:item
   (:paragraph
    "An" "error" "is" "now" "always" "signaled" "independently" "of" "the"
    "the" "value" "of" "the" (:tt "eof-error") "parameter" "when" "the" "end"
    "of" "input" "is" "encountered" "a" "after" "single" "escape" "or"
    "within" "a" "multiple" "escape" "." "The" "new" "error" "conditions"
    (:symbol "eclector.reader:unterminated-single-escape") "and"
    (:symbol "eclector.reader:unterminated-multiple-escape") "are" "signaled"
    "in" "such" "situations" "."))
  (:item
   (:paragraph
    "The" "set" "invalid" "sub-characters" "for" (:tt "#") "now" "conforms"
    "to" "the" "specification" "."))
  (:item
   (:paragraph
    "The" "value" "of" (:symbol "cl:*read-base*") "is" "now" "used"
    "correctly" "when" "distinguishing" "numbers" "and" "symbols" "."))
  (:item
   (:paragraph
    "When" "a" "number" "with" "a" "denominator" "of" "zero" "is" "read"
    "the" "new" "condition" (:symbol "eclector.reader:zero-denominator") "is"
    "signaled" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader:read-delimited-list") "has"
    "been" "added" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function" (:symbol "eclector.reader::sharpsign-s")
    "now" "accepts" "string" "designators" "as" "slot" "names" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "functions"
    (:symbol "eclector.reader::sharpsign-equals") "and"
    (:symbol "eclector.reader::sharpsign-sharpsign") "respect" "the" "value"
    "of" (:symbol "cl:*read-suppress*") "."))
  (:item
   (:paragraph
    "The" "default" "methods" "on" "the" "generic" "function"
    (:symbol "eclector.reader:fixup") "now" "works" "correctly" "for"
    (:symbol "standard-object") "instances" "with" "unbound" "slots" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function"
    (:symbol "eclector.reader::left-parenthesis") "now" "always" "reads"
    "until" (:tt "#\\)") "," "not" "some" "\"opposite\"" "character" "."))
  (:item
   (:paragraph
    (:symbol "eclector.reader:*skip-reason*") "is" "now" "set" "correctly"
    "when" "a" "line" "comment" "at" "the" "end" "of" "input" "is" "read"
    "."))
  (:item
   (:paragraph
    "In" "almost" "all" "situations" "in" "which" "Eclector" "signals" "a"
    "syntax" "error" "," "a" "restart" "named"
    (:symbol "eclector.reader:recover") "is" "now" "established" "which" ","
    "when" "invoked" "performs" "some" "action" "which" "allows" "the"
    "remainder" "of" "the" "expression" "to" "be" "read" "." "The"
    "convenience" "function" (:symbol "eclector.reader:recover") "can" "be"
    "used" "to" "invoke" "the" "restart" ".")))

 (:release "0.4" "2019-05-11"
  (:item
   (:paragraph
    "The" "reader" "macro" "function"
    (:symbol "eclector.reader::sharpsign-plus-minus") "now" "sets"
    (:symbol "eclector.reader:*skip-reason*") "so" "that" "parse" "results"
    "can" "be" "created" "with" "an" "accurate" "\"reason\"" "value" "."))
  (:item
   (:paragraph
    "Constituent" "traits" "are" "now" "represented" "and" "used" "properly"
    "."))
  (:item
   (:paragraph
    "The" "lambda" "lists" "of" "the" "functions"
    (:symbol "eclector.reader:read-char") "and"
    (:symbol "eclector.reader:peek-char") "have" "been" "fixed" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader::read-rational") "now"
    "respects" (:symbol "cl:*read-suppress*") "and" "handles" "inputs" "of"
    "the" "form" (:tt "1 2") "correctly" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function" (:symbol "eclector.reader::sharpsign-r")
    "now" "handles" (:symbol "cl:*read-suppress*") "better" "."))
  (:item
   (:paragraph
    "The" "default" "method" "on" "the" "generic" "function"
    (:symbol "eclector.reader:interpret-token") "now" "distinguishes"
    "positive" "and" "negative" "float" "zeros" "and" "uses" "radix" "10"
    "instead" "of" "the" "value" "of" (:symbol "cl:*read-base*") "for"
    "float" "digits" "."))
  (:item
   (:paragraph
    "The" "input" (:tt ".||") "is" "now" "interpreted" "as" "a" "symbol"
    "instead" "of" "the" "\"consing dot\"" "."))
  (:item
   (:paragraph
    "Long" "lists" "are" "now" "read" "into" "concrete" "syntax" "tree"
    "results" "without" "relying" "on" "unbounded" "recursion" "."))
  (:item
   (:paragraph
    "Syntax" "errors" "in" "the" "initial" "contents" "part" "of" (:tt "#A")
    "expressions" "now" "signal" "appropriate" "errors" "."))
  (:item
   (:paragraph
    "Source" "ranges" "of" "parse" "results" "no" "longer" "include"
    "whitespace" "which" "followed" "the" "corresponding" "expression" "in"
    "the" "input" "."))
  (:item
   (:paragraph
    "The" "lambda" "list" "of" "the" "function"
    (:symbol "eclector.parse-result:read") "is" "now" "accurate" ".")))

 (:release "0.3" "2018-11-28"
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader:peek-char") "has" "been"
    "added" "." "The" "new" "function" "is" "like" (:symbol "cl:peek-char")
    "but" "signals" "Eclector" "conditions" "and" "uses" "the" "Eclector"
    "readtable" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader:read-from-string") "has"
    "been" "added" "." "The" "new" "function" "is" "like"
    (:symbol "cl:read-from-string") "but" "uses" "Eclector's" "reader"
    "implementation" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function" (:symbol "eclector.reader::sharpsign-s")
    "and" "the" "generic" "function"
    (:symbol "eclector.reader:make-structure-instance") "have" "been" "added"
    "." "Eclector" "does" "not" "define" "any" "methods" "on" "the" "latter"
    "generic" "function" "since" "there" "is" "no" "portable" "way" "of"
    "creating" "a" "structure" "instance" "when" "only" "the" "symbol"
    "naming" "the" "structure" "is" "known" "."))
  (:item
   (:paragraph
    "The" "generic" "function" (:symbol "eclector.reader:interpret-symbol")
    "is" "now" "called" "when" "the" "reader" "creates" "uninterned" "symbols"
    "."))
  (:item
   (:paragraph
    "The" "generic" "function" (:symbol "eclector.reader:fixup") "now"
    "accepts" "a" "client" "object" "as" "the" "the" "argument" "."))
  (:item
   (:paragraph
    "In" "the" "generic" "functions"
    (:symbol "eclector.reader:wrap-in-quasiquote") ","
    (:symbol "eclector.reader:wrap-in-unquote") "and"
    (:symbol "eclector.reader:wrap-in-unquote-splicing") "," "the" "client"
    "parameter" "is" "now" "the" "first" "parameter" "."))
  (:item
   (:paragraph
    "The" "generic" "function" (:symbol "eclector.reader:wrap-in-quote")
    "has" "been" "added" ".")))

 (:release "0.2" "2018-08-13"
  (:item
   (:paragraph
    "The" (:tt "concrete-syntax-tree") "module" "has" "been" "generalized"
    "into" "a" (:tt "parse-result") "module" "which" "provides" "a" "protocol"
    "for" "constructing" "arbitrary" "custom" "parse" "results" "." "The"
    (:tt "concrete-syntax-tree") "module" "is" "now" "based" "on" "this"
    "new" "module" "but" "can" "be" "used" "as" "before" "by" "clients" "."))
  (:item
   (:paragraph
    "The" "default" "value" "of" "the" (:tt "eof-error-p") "parameter" "of"
    "the" (:symbol "eclector.reader:read-char") "function" "is" "now" "true"
    ".")))

 (:release "0.1" "2018-08-10"
  (:item
   (:paragraph
    "Eclector" "was" "created" "by" "extracting" "the" "reader" "module"
    "from" "the" "SICL" "repository" "."))
  (:item
   (:paragraph
    "The" "initial" "release" "includes" "many" "improvements" "over" "the"
    "original" "SICL" "reader" "," "particularly" "in" "the" "areas" "of"
    "customizability" "," "error" "reporting" "and" "constructing" "parse"
    "results" "."))))
