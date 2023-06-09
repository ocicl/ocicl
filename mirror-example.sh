#!/bin/bash

# This is just an example script showing how to use skopeo to copy
# artifacts out of ghcr.io/ocicl into a locally hosted (no TLS)
# registry (tested with zot).
#
# Once you've created your mirror, configure ocicl with the --registry
# option.

system=""

for system in $(curl -S https://raw.githubusercontent.com/ocicl/request-system-additions-here/main/all-ocicl-systems.txt); do
    ocicl list $system | while IFS= read -r line; do
        # Check if the line is empty or contains a colon (indicating a new project)
        if [[ -z $line || $line == *":" ]]; then
            # If line contains a colon, strip it and assign to project
            if [[ $line == *":" ]]; then
                system="${line%:}"
            else
                system=""
            fi
        # If line contains a version tag
        elif [[ -n $system ]]; then
            version=$(echo $line | awk '{ print $1 }')
            echo "Copying system and signature for $system:$version"
            skopeo copy --dest-tls-verify=false  docker://ghcr.io/ocicl/${system}:${version} docker://localhost:8080/ocicl/${system}:${version}
            skopeo copy --dest-tls-verify=false  docker://ghcr.io/ocicl/${system}.sig:${version} docker://localhost:8080/ocicl/${system}.sig:${version}
        fi
    done
done

