---
name: handling-json-in-common-lisp
description: Use when encoding or decoding JSON, building dictionary/map data, or choosing between hash-tables, alists, and plists in Common Lisp — especially with YASON or serapeum:dict, or when a JSON null check via (null ...) returns the wrong answer.
---

# Handling JSON in Common Lisp

## Overview
JSON objects map to **hash-tables with string keys**, not alists or plists. Build them with `serapeum:dict`; read with `gethash`. JSON `null` is the keyword `:null`, not CL `nil`.

## When to Use
- Encoding/decoding JSON with YASON
- Building a nested map/dictionary value
- A `(null value)` check on parsed JSON returns the wrong answer
- Deciding between hash-table / alist / plist for lookup data

## Build objects with `serapeum:dict`
```lisp
(serapeum:dict "a" 1
               "b" (serapeum:dict "nested" 42))
;; => #<HASH-TABLE :test equal :size 2>
```
`dict` returns a hash-table with `:test 'equal` and string keys — exactly what YASON encodes as a JSON object. Do **not** use alists or plists for JSON/lookup data:
```lisp
;; Bad — alist for a JSON object (string keys, O(n) lookup, trips YASON)
'(("a" . 1) ("b" . (("nested" . 42))))
```

## Read with `gethash`, not `assoc`
`serapeum:dict` returns a hash-table, so `assoc`/`cdr` silently return `nil`. Use `gethash`:
```lisp
(gethash "a" obj)            ;; good
(cdr (assoc "a" obj :test #'string=))  ;; wrong on a hash-table — returns nil
```

## JSON null is `:null`, not CL `nil`
With the round-trip flags below, YASON parses JSON `null` as the keyword `:null` and JSON `false` as `nil`. Check for null explicitly:
```lisp
(eq :null (gethash "field" obj))   ;; good
(null (gethash "field" obj))        ;; wrong — returns T for :null's absence handling AND for actual nil/false
```

## YASON round-trip flags
For faithful JSON → Lisp → JSON round-tripping, parse with these flags:
```lisp
(yason:parse string
             :json-arrays-as-vectors t
             :json-booleans-as-symbols t
             :json-null-as-keyword t)
```
Without `:json-null-as-keyword t`, both `null` and `[]` collapse ambiguously; `nil` becomes indistinguishable from JSON `false`/empty.

## Encode without a stream argument inside `with-output-to-string*`
```lisp
(yason:with-output-to-string* ()
  (yason:encode obj))            ;; good — no second argument
```
Passing an explicit stream/`nil` to `yason:encode` here suppresses output and returns an empty string.

## Common Mistakes
| Symptom | Fix |
|---|---|
| Built a JSON object as `'((:a . 1) ...)` alist | Use `(serapeum:dict "a" 1 ...)` — hash-table, string keys |
| `(assoc "k" obj)` returns nil on a `serapeum:dict` | `obj` is a hash-table — use `(gethash "k" obj)` |
| `(null x)` is `T` unexpectedly for a JSON null field | JSON null is `:null`; test with `(eq :null x)` |
| `null` and `[]`/`false` collide after parse | Parse with `:json-null-as-keyword t`, `:json-booleans-as-symbols t`, `:json-arrays-as-vectors t` |
| `yason:with-output-to-string*` returns `""` | Don't pass a stream/`nil` to `yason:encode` inside it |

## Related skills
For general package/style of the file holding JSON code: **writing-common-lisp**. For interactive parse/encode from a live image: **using-common-lisp-repl**.
