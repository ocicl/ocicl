#!/bin/bash

set -o pipefail  # trace ERR through pipes
set -o errtrace  # trace ERR through 'time command' and other functions
set -o nounset   ## set -u : exit the script if you try to use an uninitialised variable
set -o errexit   ## set -e : exit the script if any statement returns a non-true return value
set -o xtrace    # print commands as they are executed

ARCH=amd64
case "$1" in
    allegro | *32 ) ARCH=i386 ;;
esac

KEYFILE=/etc/apt/keyrings/obs-sionescu.key
cat > /etc/apt/sources.list.d/libfixposix.list <<EOF
deb [signed-by=${KEYFILE}] http://download.opensuse.org/repositories/home:/sionescu/Ubuntu_Jammy/ ./
EOF

cat > /etc/apt/preferences.d/libfixposix <<EOF
Package: libfixposix*
Pin: origin download.opensuse.org
Pin-Priority: 1001
EOF

mkdir -vp /etc/apt/keyrings/
curl https://download.opensuse.org/repositories/home:/sionescu/Ubuntu_Jammy/Release.key | sudo tee "${KEYFILE}"

apt-get update -qq

apt-get -y install libfixposix4:${ARCH} libfixposix-dev:${ARCH}

dpkg -L libfixposix4 libfixposix-dev
