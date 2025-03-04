# copy-directory

[![Build Status](https://travis-ci.org/ceramic/copy-directory.svg?branch=master)](https://travis-ci.org/ceramic/copy-directory)
[![Quicklisp](http://quickdocs.org/badge/copy-directory.svg)](http://quickdocs.org/<project name>/)


Copy a directory, using the native `cp` utility if available.

# Overview

It just copies directories.

# Usage

```lisp
(copy-directory:copy #p"/path/to/source/" #p"/path/to/destination/")
```

# License

Copyright (c) 2016 Fernando Borretti

Licensed under the MIT License.
