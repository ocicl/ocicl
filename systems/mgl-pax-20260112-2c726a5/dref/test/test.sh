#!/bin/bash

lisp="$1"
stop_on_failure="${2:-t}"
debug="${3:-nil}"
print="${4:-(quote try:unexpected)}"
describe="${5:-(quote try:unexpected)}"
num_passes=
num_failures=

function run_test_case {
  local test_case_name="\"$1\""
  shift
  echo "SHTEST: Running ${test_case_name} $@"
  $@
  local retval=$?
  if ((retval == 22)); then
    echo
    echo "SHTEST: ${test_case_name} PASS"
    num_passes=$((num_passes+1))
  else
    echo
    echo "SHTEST: ${test_case_name} FAIL"
    num_failures=$((num_failures+1))
  fi
}

function lisp_tests {
  local lisp_name="$1"
  shift

  run_test_case "lisp test suite on ${lisp_name}" $@ <<EOF
(require :asdf)
(asdf:load-system :dref-test)
(when (try:passedp (dref-test:test :debug ${debug} :print ${print}
                                   :describe ${describe}))
  (uiop/image:quit 22))
EOF
}

function autoload_tests {
  local lisp_name="$1"
  shift

  run_test_case "test-locate-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-locate-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-resolve-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-resolve-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-arglist-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-arglist-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-docstring-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-docstring-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-source-location-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-source-location-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-definitions-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-definitions-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-dref-apropos-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-dref-apropos-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-dref-this-source-location-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :dref-test/autoload)
(in-package :dref-test-autoload)
(when (passedp (try 'test-dref-this-source-location-autoload))
  (uiop/image:quit 22))
EOF
}

function basic_load_tests {
  local lisp_name="$1"
  shift

  run_test_case "load dref on ${lisp_name}" $@ <<EOF
(progn
  (asdf:load-system :dref)
  (uiop/image:quit 22))
EOF
}

function run_tests {
  local test_suite="$1"
  local lisp="$2"
  shift; shift
  echo
  echo "SHTEST: running test suite ${test_suite} with ${lisp} $@"
  num_failures=0
  num_passes=0
  ros --lisp "${lisp}" run --eval '(ql:quickload :dref)' --quit -- $@
  ${test_suite} ${lisp} ros --lisp ${lisp} run -- $@
  if ((num_failures > 0)); then
    if [ "${stop_on_failure}" = "t" ]; then
      echo "SHTEST: Aborting with ${num_failures} failures,"\
           "${num_passes} passes."
      exit 1
    fi
  fi
}

export LC_ALL=en_US.UTF-8

if [ -n "${lisp}" ]; then
  run_tests lisp_tests ${lisp}
  run_tests autoload_tests ${lisp}
else
  # Most lisps take only 10s or so to run the tests. CLISP takes 4x
  # longer. ABCL is 25x slower.
  run_tests lisp_tests sbcl --noinform --disable-debugger
  # run_tests lisp_tests allegro --batch --backtrace-on-error
  run_tests lisp_tests ccl-bin --batch
  run_tests lisp_tests cmu-bin -batch
  run_tests lisp_tests ecl
  run_tests lisp_tests clisp -on-error exit;
  run_tests lisp_tests abcl-bin

  # We run the autoload tests on the faster ones only.
  run_tests autoload_tests sbcl --noinform --disable-debugger
  # run_tests autoload_tests allegro --batch --backtrace-on-error
  run_tests autoload_tests ccl-bin --batch
  # run_tests autoload_tests cmu-bin -batch
  # run_tests autoload_tests ecl
fi

echo "SHTEST: ${num_failures} failures, ${num_passes} passes."
