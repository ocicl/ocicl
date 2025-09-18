SHELL := /bin/bash

FILES = package.lisp iterate.lisp iterate-test.lisp iterate-pg.lisp iterate.asd
TEXFILES = doc/iter-man.tex doc/iter-bare.tex doc/aimemo.sty doc/GNUmakefile
PDFFILES = doc/iter-man.pdf doc/iter-bare.pdf
defaultLisps = abcl allegro ccl clasp clisp cmucl ecl lispworks sbcl
ifdef ITERATE_TEST_LISPS
	lisps ?= ${ITERATE_TEST_LISPS}
else
	lisps ?= ${defaultLisps}
endif

# If you need to sudo in order to use docker, modify this.
DOCKER ?= docker

sourceDirectory := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))


l ?= sbcl

ABCL ?= abcl
ALLEGRO ?= alisp
ALLEGROMODERN ?= mlisp
CCL ?= ccl
CLISP ?= clisp
CLASP ?= clasp
CMUCL ?= cmucl
ECL ?= ecl
GCL ?= gcl
LISPWORKS ?= lispworks
MKCL ?= mkcl
SBCL ?= sbcl
SCL ?= scl
XCL ?= xcl

ifeq ($(l), abcl)
	command ?= ${ABCL}
	loadfile = --noinit --nosystem --noinform --load

else ifeq ($(l), allegro)
	command ?= ${ALLEGRO}
	loadfile = -q -L

else ifeq ($(l), ccl)
	loadfile = --no-init --quiet --load
	command ?= ${CCL}

else ifeq ($(l), clasp)
	command ?= ${CLASP}
	loadfile = --norc --load

else ifeq ($(l), clisp)
	command ?= ${CLISP}
	loadfile = -norc --silent -ansi

else ifeq ($(l), cmucl)
	command ?= ${CMUCL}
	loadfile = -noinit -batch -load

else ifeq ($(l), ecl)
	command ?= ${ECL}
	loadfile = --norc --load

else ifeq ($(l), lispworks)
	command ?= ${LISPWORKS} # this is just a convention...
	loadfile = -siteinit - -init - -load

else ifeq ($(l), sbcl)
	command ?= ${SBCL}
	loadfile = --no-userinit --no-sysinit --load
else
	$(error Don\'t know how to operate on implementation $(l))
endif

.PHONY: test clear-cache test-all-lisps test-docker-repl test-docker-lisp

distrib:
	tar czf iterate.tgz $(FILES) $(TEXFILES) $(PDFFILES)

clear-cache:
	rm -rf test/cache

test:	clear-cache
	mkdir -p test
	set -o pipefail ; \
	$(command) $(loadfile) test.lisp | tee test/${l}-test.text ; \
	exit ${PIPESTATUS[0]}


# Useful for reproducing test failures with Docker.
test-docker-repl:	clear-cache
	@${DOCKER} run --rm -i -t --pull always -u $(shell id -u):$(shell id -g) -v $(sourceDirectory):$(sourceDirectory) -w $(sourceDirectory)/test clfoundation/${l}:latest

test-docker-lisp:	clear-cache
	@${DOCKER} run --rm -i -t --pull always -u $(shell id -u):$(shell id -g) -v $(sourceDirectory):$(sourceDirectory) -w $(sourceDirectory) clfoundation/${l}:latest make test l=${l} t=${t}

test-all-lisps:
	@for lisp in ${lisps} ; do ${MAKE} test l=$$lisp t=${t}|| { echo "$${lisp} failed" ; exit 1 ; } ; done
