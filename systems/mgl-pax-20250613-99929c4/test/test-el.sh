#!/bin/bash

set -e

cd `dirname $0`
EMACS=emacs
LISP="${1:-sbcl}"
# FIXME: This is hardcoded to where the github action expects it. Make
# this a symlink locally.
SLIME_DIR=~/.roswell/local-projects/slime/
LOAD_PATH="-L ../src/ -L . -L ${SLIME_DIR}"
SELECTOR=${2:-"\"mgl-pax\""}
# Randomization is a poor man's autoload test.
RANDOMIZE=t

${EMACS} -q --no-splash --batch ${LOAD_PATH} \
         --eval "(require 'mgl-pax-tests)" \
	 --eval "(slime-setup)" \
	 --eval "(setq inferior-lisp-program \"${LISP}\")" \
	 --eval "(slime-batch-test (quote ${SELECTOR}) ${RANDOMIZE})"
