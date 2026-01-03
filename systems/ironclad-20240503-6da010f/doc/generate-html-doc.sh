#!/usr/bin/env bash

DIR=$(dirname $(readlink -f "$0"))

emacs --batch \
      --eval "(progn (package-initialize) (require (quote htmlize)) (require (quote ox-html)))" \
      --file "${DIR}/../README.org" \
      --funcall "org-html-export-to-html"

mv "${DIR}/../README.html" "${DIR}/ironclad.html"
