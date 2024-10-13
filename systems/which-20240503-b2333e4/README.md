# which

[![Build Status](https://travis-ci.org/eudoxia0/which.svg?branch=master)](https://travis-ci.org/eudoxia0/which)
[![Coverage Status](https://coveralls.io/repos/github/eudoxia0/which/badge.svg?branch=master)](https://coveralls.io/github/eudoxia0/which?branch=master)

The which UNIX command in Common Lisp.

# Usage

```lisp
CL-USER> (ql:quickload :which)
To load "which":
  Load 1 ASDF system:
    which
; Loading "which"
[package which]
(:WHICH)

CL-USER> (which:which "bash")
#P"/bin/bash"

CL-USER> (which:which "qooblooxbar")
NIL
```

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.
