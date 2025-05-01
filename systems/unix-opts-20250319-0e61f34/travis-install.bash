#!/bin/bash

if [[ -n "$COVERALLS" ]]; then
    curl -L https://raw.githubusercontent.com/roswell/roswell/master/scripts/install-for-ci.sh | sh
    ros asdf install
    ros -e '(ql:update-all-dists :prompt nil)'
else
    curl https://raw.githubusercontent.com/sionescu/cl-travis/master/install.sh | sh;
    cl -e '(ql:update-all-dists :prompt nil)'
fi
