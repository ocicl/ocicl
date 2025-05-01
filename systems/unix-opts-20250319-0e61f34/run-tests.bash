#!/bin/bash

if [[ -n "$COVERALLS" ]]; then
    ros -e '(ql:quickload :cl-coveralls)
            (cl-coveralls.impls:enable-coverage)
            (asdf:load-system :unix-opts/tests)
            (asdf:load-system :unix-opts)
            (let (suc)
              (cl-coveralls:with-coveralls (:exclude (list "tests.lisp"))
                (setf suc (unix-opts/tests:run)))
              (unix-opts:exit (if suc 0 1)))'

else
    cl -l unix-opts/tests -l unix-opts \
        -e '(unix-opts:exit (if (unix-opts/tests:run) 0 1))'

fi
