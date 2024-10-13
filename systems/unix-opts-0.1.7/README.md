# Unix-style command line options parser

[![License MIT](https://img.shields.io/badge/license-MIT-green.svg)](http://opensource.org/licenses/MIT)
[![Build Status](https://travis-ci.org/mrkkrp/unix-opts.svg?branch=master)](https://travis-ci.org/mrkkrp/unix-opts)
[![Quicklisp](http://quickdocs.org/badge/unix-opts.svg)](http://quickdocs.org/unix-opts/)

This is a minimalistic parser of command line options. The main advantage of
the library is the ability to concisely define command line options once and
then use this definition for parsing and extraction of command line
arguments, as well as printing description of command line options (you get
`--help` for free). This way you don't need to repeat yourself. Also,
`unix-opts` doesn't depend on anything and allows to precisely control
behavior of the parser via Common Lisp restarts.

Inspired by Haskell's `optparse-applicative` and Python's `argparse`.

It is portable accross implementations.

## Installation

Copy files of this library in any place where ASDF can find them. Then you
can use it in system definitions and ASDF will take care of the rest.

Via Quicklisp (recommended):

```common-lisp
(ql:quickload "unix-opts")
```

Now you can also use its shorter nickname `opts`.

## Functions

```
option condition
```

Take a condition `condition` (`unknown-option`, `missing-arg`, or
`arg-parser-failed`) and return a string representing the option in
question.

----

```
raw-arg condition
```

Take a condition of type `arg-parser-failed` and return the raw argument
string.

----

```
define-opts &rest descriptions
```

Define command line options. Arguments of this macro must be plists
containing various parameters. Here we enumerate all allowed parameters:

* `:name`—keyword that will be included in list returned by `get-opts`
  function if actual option is supplied by user.

* `:description`—description of the option (it will be used in `describe`
  function). This argument is optional, but it's recommended to supply it.

* `:short`—single character, short variant of the option. You may omit this
  argument if you supply `:long` variant of option.

* `:long`—string, long variant of option. You may omit this argument if you
  supply `:short` variant of option.

* `:arg-parser`—if actual option must take an argument, supply this
  argument, it must be a function that takes a string and parses it.

* `:meta-var`—if actual option requires an argument, this is how it will be
  printed in option description.

* `:required`—whether the option is required. This only makes sense if the
  option takes an argument.

----

```
argv
```

Return a list of the program's arguments, including the command used to
execute the program as the first element of the list. Portable across
implementations.

----

```
get-opts &optional options
```

Parse command line options. If `options` is given, it should be a list to
parse. If it's not given, the function will use the `argv` function to get
the list of command line arguments.

Returns two values:

* a list that contains keywords associated with command line options with
  `define-opts` macro, and
* a list of free arguments.

If some option requires an argument, you can use `getf` to test the presence
of the option and get its argument if the option is present.

The parser may signal various conditions. Let's list them all specifying
which restarts are available for every condition, and what kind of
information the programmer can extract from the conditions.

* `unknown-option` is thrown when the parser encounters an unknown (not
  previously defined with `define-opts`) option. Use the `option` reader to
  get the name of the option (string). Available restarts: `use-value`
  (substitute the option and try again), `skip-option` (ignore the option).

* `missing-arg` is thrown when some option wants an argument, but there is
  no such argument given. Use the `option` reader to get the name of the
  option (string). Available restarts: `use-value` (supplied value will be
  used), `skip-option` (ignore the option).

* `arg-parser-failed` is thrown when some option wants an argument, it's
  given but cannot be parsed by argument parser. Use the `option` reader to
  get name of the option (string) and `raw-arg` to get raw string
  representing the argument before parsing. Available restarts: `use-value`
  (supplied value will be used), `skip-option` (ignore the option),
  `reparse-arg` (supplied string will be parsed instead).

* `missing-required-option` is thrown when a required option cannot be
  found. Use the `missing-options` reader to get all option objects that are
  missing. Available restarts: `use-value` (supplied list of values will be
  used), `skip-option` (ignore all these options, effectively binding them
  to `nil`)

```
describe &key prefix suffix usage-of args stream
```

Return a string describing all the options of the program that were defined
with the previous `define-opts` macro. You can supply `prefix` and `suffix`
arguments that will be printed before and after the options respectively. If
`usage-of` is supplied, it should be a string, the name of the program for
an "Usage: " section. This section is only printed if this name is given. If
your program takes arguments (apart from options), you can specify how to
print them in "Usage: " section with an `args` option (should be a string
designator). Output goes to `stream` (default value is `*standard-output*`).

----

```
exit &optional (status 0)
```

Exit the program returning `status`.

## Example

Go to the `example` directory. Now, you can use `example.lisp` file to see
if `unix-opts` is cool enough for you to use. SBCL users can use
`example.sh` file.

Take a look at `example.lisp` and you will see that the library is pretty
sexy! Basically, we have defined all the options just like this:

```common-lisp
(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose")
  (:name :level
   :description "the program will run on LEVEL level"
   :short #\l
   :long "level"
   :required t
   :arg-parser #'parse-integer
   :meta-var "LEVEL")
  (:name :output
   :description "redirect output to file FILE"
   :short #\o
   :long "output"
   :arg-parser #'identity
   :meta-var "FILE"))
```

and we read them with `(opts:get-opts)` which returns two values, a list of
parsed options and the remaining arguments, so:

```common-lisp
(multiple-value-bind (options free-args)
    (opts:get-opts)
  (if (getf options :verbose)
      ...
```

See the example for helpers and how to handle malformed or incomplete arguments.

And here is some action:

```
$ sh example.sh --help
example—program to demonstrate unix-opts library

Usage: example.sh [-h|--help] [-v|--verbose] [-l|--level LEVEL]
                  [-o|--output FILE] [FREE-ARGS]

Available options:
  -h, --help               print this help text
  -v, --verbose            verbose output
  -l, --level LEVEL        the program will run on LEVEL level
  -o, --output FILE        redirect output to file FILE

so that's how it works…
free args:

$ sh example.sh --level 1 -v file1.txt file2.txt
OK, running in verbose mode…
I see you've supplied level option, you want 1 level!
free args: file1.txt, file2.txt

$ sh example.sh --level 10 --output foo.txt bar.txt
I see you've supplied level option, you want 10 level!
I see you want to output the stuff to "foo.txt"!
free args: bar.txt

$ sh example.sh --level kitty foo.txt
fatal: cannot parse "kitty" as argument of "--level"
free args:

$ sh example.sh --hoola-boola noola.txt
warning: "--hoola-boola" option is unknown!
fatal: missing required options: "--level"

$ sh example.sh -vgl=10
warning: "-g" option is unknown!
OK, running in verbose mode…
I see you've supplied level option, you want 10 level!
free args:
```
## License

Copyright © 2015–2018 Mark Karpov

Distributed under MIT License.
