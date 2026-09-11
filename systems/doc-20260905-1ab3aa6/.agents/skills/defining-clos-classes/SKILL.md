---
name: defining-clos-classes
description: Use when defining CLOS classes and their constructors in Common Lisp, adding an optional slot to an existing class, or converting a defstruct to a defclass — especially when writing a make-<name> constructor that hides make-instance.
---

# Defining CLOS Classes

## Overview
Every data type in these projects is a `defclass` with a **constructor function** `make-<name>` that hides `make-instance`. The constructor takes `&rest restargs` so it **never duplicates the class's `:initform` defaults**.

## When to Use
- Defining a new class and its constructor
- Adding an optional slot to an existing class
- Converting a `defstruct` to a `defclass`

## Use `defclass`, not `defstruct`
`defclass` integrates with CLOS — generic methods, inheritance, the MOP. A `defstruct` metaclass cannot be changed later in a live image and does not play cleanly with CLOS extensions.

## The constructor pattern (the important part)
```lisp
(defclass session ()
  ((project-dir :initarg :project-dir :reader session-project-dir)
   (state       :initarg :state       :initform nil :accessor session-state)))

(defun make-session (project-dir &rest restargs &key state)
  (declare (ignore state))
  (apply #'make-instance 'session :project-dir project-dir restargs))
```

Rules:
- **Required slots → required positional parameters** of the constructor (`project-dir`).
- **Optional slots → `&key` parameters**, received via `&rest restargs` and forwarded with `apply`. List every `&key` in the lambda list, then `(declare (ignore ...))` for each one — for documentation and compile-time checking.
- **Never duplicate the `:initform` default in the constructor.** `make-instance` already uses `:initform` when a key is absent and the supplied value when it is present. Duplicating the default is the common wrong form (every baseline agent writes it).

### Why `&rest` + `apply` instead of passing `state` explicitly
- No default to keep in sync with the class.
- Adding an optional slot costs one line in the `&key` list and one `ignore` — the constructor body never changes.
- Absent keys fall through to `:initform`; present keys override it.

```lisp
;; Wrong — duplicates the nil default from the class, and the body must be edited for every new slot
(defun make-session (project-dir &key (state nil))
  (make-instance 'session :project-dir project-dir :state state))
```

## Slot accessors
Use `:reader` / `:accessor` with `:initarg` keywords. Name accessors `session-<slot>` to avoid clashes across classes.

## Slot type and initform invariant
Every `:initform` must satisfy its slot's `:type`; strict safety settings expose
violations during ordinary construction. For an optional string slot whose
default is `nil`, declare the precise CLOS type:
```lisp
(value :type (or null string) :initarg :value :initform nil :reader item-value)
```
When a serializer has its own wire schema, attach that schema separately and
keep the CLOS type precise:
```lisp
(value :type (or null string) :schema string
       :required nil :initform nil :reader item-value)
```

## Redefining `defstruct` → `defclass` in a live image
A running image cannot just reload the file: the metaclass change is refused and the old accessors stay `fboundp`. Delete the package first, then force-reload — see **using-common-lisp-repl** for the exact `delete-package` + `load-system :force` recipe.

## Common Mistakes
| Symptom | Fix |
|---|---|
| Constructor duplicates the class default (`&key (state nil)`) | Use `&rest restargs &key state` + `(declare (ignore state))` + `apply` — no duplication |
| `defstruct` used where CLOS is expected | Switch to `defclass` (CLOS methods / inheritance / MOP) |
| Adding a slot forces editing the constructor body | With `&rest restargs`, only the `&key` list and `declare ignore` change |
| `defstruct` → `defclass` reload errors, or old accessors linger | `delete-package` first — see **using-common-lisp-repl** |

## Related skills
For live-image reload after redefinition: **using-common-lisp-repl**. For overall file and package style: **writing-common-lisp**.
