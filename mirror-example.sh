#!/bin/bash

# This is just an example script showing how to use skopeo to copy
# artifacts out of ghcr.io/ocicl into a locally hosted (no TLS)
# registry (tested with zot).
#
# Once you've created your mirror, configure ocicl with the --registry
# option.

project=""

while IFS= read -r line; do
  # Check if the line is empty or contains a colon (indicating a new project)
  if [[ -z $line || $line == *":" ]]; then
    # If line contains a colon, strip it and assign to project
    if [[ $line == *":" ]]; then
      project="${line%:}"
    else
      project=""
    fi
  # If line contains a version tag
  elif [[ -n $project ]]; then
    version=$(echo $line | awk '{ print $1 }')
    echo "Running command on $project:$version"
    skopeo copy --dest-tls-verify=false  docker://ghcr.io/ocicl/${project}:${version} docker://localhost:8080/ocicl/${project}:${version}
  fi
done < file-with-list-of-packages.txt
