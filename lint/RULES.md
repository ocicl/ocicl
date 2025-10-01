# List of ocicl Linter Rules 

## Formatting & Whitespace (9 rules)
1. trailing-whitespace - Line has trailing whitespace
2. no-tabs - Tab character found (use spaces instead)
3. max-line-length - Line exceeds character limit (default 120)
4. consecutive-blank-lines - More than 2 consecutive blank lines
5. final-newline - File must end with a newline
6. whitespace-after-open-paren - Remove whitespace after opening parenthesis
7. whitespace-before-close-paren - Remove whitespace before closing parenthesis
8. closing-parens-same-line - Consecutive closing parentheses should be on same line

## File Structure & Metadata (3 rules)
9. spdx-license-identifier - Missing SPDX-License-Identifier in file header
10. in-package - No package declaration found
11. reader-error - Syntax/parsing errors (unmatched parens, etc.)

## Naming Conventions (6 rules)
12. naming-underscore - Symbol contains underscore; prefer hyphens
13. special-name-style - Special variables should be named *like-this*
14. constant-name-style - Constants should be named +like-this+
15. check-prefix - CHECK prefix is vague (describe what function does)
16. helper-suffix - HELPER suffix is vague (use descriptive name)
17. verbose-generic-var-name - Avoid generic names (x, y, tmp, val, etc.)

## Package & Documentation (3 rules)
18. defpackage-use - Avoid :use in defpackage (prefer :import-from)
19. missing-docstring - Missing docstring in defun/defmacro/defpackage
20. defvar-without-value - DEFVAR without initial value

## Boolean & Conditional Logic (18 rules)
21. boolean-context - Simplify: condition is already boolean
22. if-single-branch - Use WHEN or UNLESS instead of IF for single-branch
23. if-no-else - IF without else branch: use WHEN or UNLESS
24. if-for-not - Replace (IF test NIL T) with (NOT test)
25. if-or - Use (OR test else) instead of (IF test T else)
26. when-for-unless - Use UNLESS instead of (WHEN (NOT ...))
27. unless-for-when - Use WHEN instead of (UNLESS (NOT ...))
28. cond-vs-if - Use IF instead of COND with single clause
29. cond-without-default - COND without T or OTHERWISE clause
30. cond-or - Use (OR test else) instead of COND pattern
31. cond-test-no-exp - COND branch with test but no action
32. needless-cond - Use IF instead of COND with two clauses
33. needless-cond-not - Simplify (COND ((NOT test) ...))
34. needless-if - Simplify redundant IF expressions
35. needless-when - WHEN with constant true test
36. use-typecase - Use TYPECASE instead of COND with TYPE-OF
37. progn-in-if - Unnecessary PROGN in IF with no else
38. progn-in-when - Unnecessary PROGN in WHEN

## Logic Simplification (12 rules)
39. needless-and - AND with single argument is redundant
40. needless-and-t - Trailing T in AND expression is redundant
41. needless-or - OR with single argument is redundant
42. needless-or-nil - Trailing NIL in OR expression is redundant
43. nested-and-or - Nested AND/OR can be flattened
44. and-or-simplification - Simplify AND/OR with constants
45. not-null - Simplify (NOT (NULL x)) to just x
46. not-consp - Use ATOM instead of (NOT (CONSP ...))
47. equal-with-nil - Use NULL or NOT instead of EQUAL with NIL
48. use-zerop - Use (ZEROP X) instead of (= X 0)
49. length-zero - Use (ZEROP (LENGTH ...)) or NULL
50. typep-primitive - Avoid TYPEP with T or NIL (result is trivial)

## Arithmetic & Numbers (4 rules)
51. plus-one - Use (1+ X) instead of (+ X 1)
52. minus-one - Use (1- X) instead of (- X 1)
53. add-zero - Adding zero has no effect
54. quote-number - Numbers are self-evaluating (no need to quote)

## List Operations (13 rules)
55. use-first-rest - Use FIRST/REST instead of CAR/CDR for better readability
56. car-cdr - Use CADR instead of (CAR (CDR ...))
57. cdr-cdr - Use CDDR instead of (CDR (CDR ...))
58. nth-for-cdr - Use NTH instead of nested CDRs
59. nth-on-list - NTH is expensive on lists (use FIRST/REST or pointer access)
60. cons-with-nil - Use (LIST x) instead of (CONS x NIL)
61. cons-list - Use (LIST ...) instead of (CONS x (LIST ...))
62. cons-cons-acons - Use ACONS instead of (CONS (CONS ...))
63. append-single - Use COPY-LIST instead of (APPEND x NIL)
64. append-list-list - Use (CONS x y) instead of (APPEND (LIST x) y)
65. use-push - Use (PUSH item var) instead of (SETF var (CONS item var))
66. list-length - LIST-LENGTH is for circular lists (use LENGTH for known-proper lists)
67. concatenate-list - Use APPEND instead of CONCATENATE for lists

## Comparison & Equality (5 rules)
68. use-eql - Use EQL instead of EQ for numbers/chars/keywords
69. use-string-equal - Use STRING-EQUAL instead of EQUAL for strings
70. use-char-equal - Use CHAR-EQUAL instead of EQL for chars
71. use-member - Use MEMBER instead of FIND with :test
72. eq-misuse - EQ used incorrectly (comparing numbers/strings)

## Sequence Operations (6 rules)
73. use-find-if-not - Use FIND-IF-NOT instead of (FIND-IF (COMPLEMENT ...))
74. use-position-if-not - Use POSITION-IF-NOT instead of (POSITION-IF (COMPLEMENT ...))
75. use-remove-if-not - Use REMOVE-IF-NOT instead of (REMOVE-IF (COMPLEMENT ...))
76. find-member-for-assoc - Use ASSOC instead of (FIND ... :key #'car)
77. mapcar-for-mapc - Consider MAPC if you don't need the result list
78. use-mapc - Use MAPC instead of loop with side effects

## Variable & Assignment (7 rules)
79. setf-vs-setq - Prefer SETF over SETQ for consistency
80. setf-incf - Use (INCF var amount) instead of (SETF var (+ var amount))
81. setf-decf - Use (DECF var amount) instead of (SETF var (- var amount))
82. setq-incf - Use INCF instead of SETQ with +
83. needless-shiftf - SHIFTF with two arguments is just SETF
84. rplaca - Use (SETF (CAR ...)) instead of RPLACA
85. rplacd - Use (SETF (CDR ...)) instead of RPLACD

## Function & Lambda (7 rules)
86. use-identity - Use #'IDENTITY instead of (LAMBDA (X) X)
87. use-constantly - Use (CONSTANTLY value) for constant-returning lambda
88. unnecessary-lambda - Remove unnecessary lambda wrapper around function call
89. lambda-list-invalid - Invalid lambda list syntax
90. destructuring-bind-invalid - Invalid destructuring pattern
91. avoid-optional-and-key - Avoid mixing &OPTIONAL and &KEY in lambda lists (defun/defmacro/lambda)

## Control Flow (6 rules)
93. redundant-progn - PROGN with single form is redundant
94. redundant-block - BLOCK with single form is redundant
95. avoid-nesting - Deep nesting detected (readability issue)
96. uses-prog - PROG is obsolete (use LET, BLOCK, TAGBODY, or LOOP instead)
97. eval-usage - Avoid EVAL (consider alternatives like FUNCALL or macros)
98. use-with-open-file - Use WITH-OPEN-FILE instead of manual OPEN/CLOSE

## Quoting & Literals (5 rules)
99. quoted-nil - Use NIL instead of 'NIL
100. quote-false - Quoted FALSE evaluates to true in Lisp (use NIL for false)
101. quote-true - Use T instead of 'TRUE
102. quote-keyword - Keywords are self-evaluating (no need to quote)

## Advanced & Safety (25 rules)
103. case-duplicate-key - Duplicate CASE key detected
104. destructive-on-constant - Destructive operation on constant data
105. substitute-use - SUBSTITUTE copies entire sequence (use only when necessary)
106. multiple-optionals - Multiple optional arguments get confusing (use &KEY)
107. multiple-value-list - Multiple values avoid consing - MULTIPLE-VALUE-LIST defeats this
108. constant-bad-eof - Don't use constants for EOF markers (use local variable)
109. use-alexandria-when-let - Suggest ALEXANDRIA:WHEN-LET for (let ((x ...)) (when x ...))
110. use-alexandria-if-let - Suggest ALEXANDRIA:IF-LET for (let ((x ...)) (if x ...))
111. use-alexandria-ensure-list - Suggest ALEXANDRIA:ENSURE-LIST for (if (listp x) x (list x))
112. use-alexandria-ensure-cons - Suggest ALEXANDRIA:ENSURE-CONS for (if (consp x) x (cons x nil))
113. use-alexandria-lastcar - Suggest ALEXANDRIA:LASTCAR for (car (last x))
114. use-alexandria-mappend - Suggest ALEXANDRIA:MAPPEND for (apply #'append (mapcar ...))
115. use-alexandria-emptyp - Suggest ALEXANDRIA:EMPTYP for (zerop (length ...)) or (= (length ...) 0)
116. use-alexandria-ensure-car - Suggest ALEXANDRIA:ENSURE-CAR for (if (consp x) (car x) x)
117. use-alexandria-first-elt - Suggest ALEXANDRIA:FIRST-ELT for (elt sequence 0)
118. use-alexandria-last-elt - Suggest ALEXANDRIA:LAST-ELT for (elt seq (1- (length seq)))
119. use-serapeum-nor - Suggest SERAPEUM:NOR for (not (or ...))
120. use-serapeum-nand - Suggest SERAPEUM:NAND for (not (and ...))
121. use-serapeum-filter-map - Suggest SERAPEUM:FILTER-MAP for (remove nil (mapcar ...))
122. use-serapeum-car-safe - Suggest SERAPEUM:CAR-SAFE for (if (consp x) (car x) nil)
123. use-serapeum-cdr-safe - Suggest SERAPEUM:CDR-SAFE for (if (consp x) (cdr x) nil)
124. use-serapeum-append1 - Suggest SERAPEUM:APPEND1 for (append list (list item))
125. use-uiop-file-exists-p - Suggest UIOP:FILE-EXISTS-P instead of PROBE-FILE
126. use-uiop-read-file - Suggest UIOP:READ-FILE-STRING/LINES for file reading
127. malformed-let - Malformed LET/LET* binding structures
128. unused-parameter - Parameter is unused (add (declare (ignore ...)) if intentional)

