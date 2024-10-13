#
# Common Lisp development environment (Docker)
#

FROM --platform=$TARGETPLATFORM debian:stable

# The following two arguments are supported by "docker buildx"
ARG TARGETPLATFORM
ARG BUILDPLATFORM

WORKDIR /Lisp

# User should create a named volume (FBK) and call "docker run -v Lisp:/Lisp"
VOLUME /Lisp

# Use this mode when you need zero interaction while installing or upgrading the system via apt
ENV DEBIAN_FRONTEND=noninteractive
ENV LD_LIBRARY_PATH=/usr/local/lib
ENV PATH=/Lisp/bin:$PATH

# Essential system packages
RUN apt-get update -qy
RUN apt-get install -qy build-essential bzip2 cmake tar wget unzip git pkg-config \
    libreadline-dev libncurses5-dev libtinfo-dev libz-dev libicu-dev \
    liblzma-dev libgmp-dev psutils terminfo man aptitude flex bison m4

# Lisp packjages
RUN apt-get install -qy clisp sbcl ecl

# i386 specific packages
RUN if [ "linux/386" = "$TARGETPLATFORM" ]; then \
       apt-get install -qy cmucl; \
    fi

# for Unicode display, learnt from Magnus Myreen
RUN apt-get install -qy locales-all

# Additional packages
RUN apt-get install -qy libxml2-dev

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# cleanup downloaded package files
RUN apt-get clean

# For SSH-based Git access
RUN mkdir /root/.ssh && chmod go-rwx /root/.ssh

# Entry command
ENTRYPOINT ["/bin/bash"]
