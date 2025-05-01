#!/bin/bash

cd `dirname $0`
EMACS=emacs
LISP=sbcl
# FIXME: This is hardcoded.
SLIME_DIR=~/src/slime/
LOAD_PATH="-L ../src/ -L . -L ${SLIME_DIR}"
SELECTOR=${1:-"\"mgl-pax\""}
# Randomization is a poor man's autoload test.
RANDOMIZE=t

${EMACS} -q --no-splash --batch ${LOAD_PATH} \
         --eval "(require 'mgl-pax-tests)" \
	 --eval "(slime-setup)" \
	 --eval "(setq inferior-lisp-program \"${LISP}\")" \
	 --eval "(slime-batch-test (quote ${SELECTOR}) ${RANDOMIZE})"
