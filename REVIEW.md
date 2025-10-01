# Review of Issues #150-172

## Opinions on Issues

### Issues suggesting disabling rules by default (#150-152, 156-157, 159-160, 164-166, 171)
- **Valid concern**: Many of these highlight that the linter is being overly prescriptive about style preferences vs actual bugs
- **Recommendation**: Consider implementing suppression configs or severity levels (warning vs error). The suggestion in #171 for "beginner" vs "advanced" rule sets is worth considering.

### Specific technical issues:

**#153 (cdr/rest)** - ✓ FIXED: All rules now treat CAR/CDR and FIRST/REST equivalently.

**#154 (setf-incf generalization)** - ✓ FIXED: setf-incf now works with all place forms, not just simple variables.

**#155 (use-identity message)** - ✓ ALREADY DONE: Already uses `#'IDENTITY` with the function quote.

**#158 (quoted atoms)** - ✓ ALREADY DONE: Comprehensive rules already exist for nil, t, keywords, and numbers.

**#161 (setq-incf/setf-incf)** - ✓ FIXED: Combined logic, added setq-decf, removed duplicate rule.

**#162 (redundant lambda-list rules)** - ✓ FIXED: Removed redundant lambda-list-optional-and-key rule.

**#163 (missing-docstring for exports only)** - Excellent suggestion. Only requiring docstrings on exported symbols is much more reasonable.

**#167 (consistency in phrasing)** - ✓ ALREADY DONE: All messages are consistently phrased as statements.

**#168 (nth-on-list closed)** - Already addressed by author.

**#169 (cons-list message)** - ✓ FIXED: Message now shows actual values like cons-with-nil does.

**#170 (concatenate-list)** - ✓ FIXED: No longer suggests CONS, only APPEND.

**#172 (find-member-for-assoc)** - ✓ FIXED: Message now clarifies that FIND works on sequences including vectors.

## Overall Assessment

These issues reveal a pattern: **the linter is too opinionated**. Many rules enforce style preferences that are valid in certain contexts. I recommend:

1. Add configuration system for rule severity/enabling
2. Consider "beginner" vs "advanced" rule sets (#171)
3. Make some rules (like docstring requirements) context-aware (#163)

## Completed

- ✓ #153: CAR/CDR and FIRST/REST now treated equivalently in all rules
- ✓ #154: setf-incf generalized to work with all place forms
- ✓ #155: use-identity already uses #'IDENTITY (was already done)
- ✓ #158: Quoted atoms already comprehensively handled (was already done)
- ✓ #161: Combined setq-incf/setf-incf, added setq-decf
- ✓ #162: Removed redundant lambda-list-optional-and-key rule
- ✓ #167: All messages consistently phrased as statements (was already done)
- ✓ #169: cons-list message now shows actual values
- ✓ #170: concatenate-list no longer suggests CONS
- ✓ #172: find-member-for-assoc message clarified

## Issue Details

### #150 - `plus-one` and `minus-one`
Would turn off. `1+` and `1-` should be removed from the language, not encouraged. `(a:curry #'+ 1)` would be an onerous replacement.

### #151 - `when-for-unless`
Would turn off. There is something about `unless` that just confuses my brain.

### #152 - `use-alexandria`
Perhaps this should be disabled by default since it suggests using something outside the core language.

### #153 - Checking for `'cdr` and `'rest`
All the rules that look for `'cdr` should also check for `'rest`, and vice versa.

### #154 - `setf-incf`
Generalize to work with all "places".

### #155 - `use-identity`
The message should refer to `#'identity` rather than just `identity`.

### #156 - `use-constantly`
Honestly, the lambda expression is clearer.

### #157 - `not-null`
`(not (null x))` is a valid way of turning `x` into a boolean.

### #158 - `quoted-nil`
Generalize this to all `'<atom>` forms? quote-keyword, quote-number, quote-true.

### #159 - `defvar-without-value`
`(defvar *foo*)` is a perfectly valid expression. If you want to advocate for `(declaim (special *foo*))` instead, then the message should say that.

### #160 - `use-with-open-file`
I dunno about this one. Sometimes you might need to hold a file open with indefinite extent.

### #161 - `setq-incf`
Can this be combined with `setf-incf`? It doesn't match all the same forms but perhaps should. (No `setq-decf`?)

### #162 - Redundant rules
`lambda-list-optional-and-key` and `avoid-optional-and-key` are redundant?

### #163 - `missing-docstring`
Can this be made to only trigger for exported symbols?

### #164 - `if-or`
`(if test t else)` is a valid way of turning `test` into `T`. `(or test else)` has a different meaning.

### #165 - `cond-or`
Same thing. Sometimes you really just want `T` instead of a generalized boolean.

### #166 - `needless-and-t`
Same thing. Sometimes you really just want `T`, not a generalized boolean.

### #167 - General note
Some warnings are phrased as questions and some as statements. Keep it consistent as statements?

### #168 - `nth-on-list` (closed)
I don't understand what the message expects me to do as an alternative if my list is 1000 elements long. I think my problem occurred during the design phase when I selected list as the data structure. :)

### #169 - `cons-list`
The message for `cons-with-nil` is much better; maybe you can do similar here.

### #170 - `concatenate-list`
Don't suggest using `cons` since it does something different.

### #171 - `quote-true` and `quote-false`
I understand the reasoning for this, but again this is perfectly valid code in some situations. Perhaps linters should be grouped into "beginner" and "advanced"?

### #172 - `find-member-for-assoc`
`find` works on sequences (vectors and lists) while `assoc` only works on lists.
