# path-parse

[![Build Status](https://travis-ci.org/eudoxia0/path-parse.svg?branch=master)](https://travis-ci.org/eudoxia0/path-parse)
[![Coverage Status](https://coveralls.io/repos/github/eudoxia0/path-parse/badge.svg?branch=master)](https://coveralls.io/github/eudoxia0/path-parse?branch=master)

Parse the `PATH` environment variable, portable.

# Usage

```lisp
CL-USER> (ql:quickload :path-parse)
To load "path-parse":
  Load 1 ASDF system:
    path-parse
; Loading "path-parse"

(:PATH-PARSE)
CL-USER> (path-parse:path)
(#P"/usr/local/bin/" #P"/usr/bin/" #P"/bin/" #P"/usr/local/games/"
 #P"/usr/games/")
```

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.
