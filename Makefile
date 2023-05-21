ocicl: *.lisp *.asd Makefile runtime/ocicl-runtime.lisp
	rm -rf systems systems.csv;
	sbcl --no-userinit --eval "(require 'asdf)" --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :ocicl) (sb-ext:quit))"

DESTDIR ?= ${HOME}/.local

install:
	@mkdir -p ${DESTDIR}/bin || true ;
	@cp ocicl${EXE} ${DESTDIR}/bin ;
	@tar xvf oras/oras_1.0.0_linux_amd64.tar.gz -C /tmp oras > /dev/null 2>&1;
	@mv /tmp/oras ${DESTDIR}/bin/ocicl-oras
	@${DESTDIR}/bin/ocicl${EXE} setup


clean:
	-rm -rf ocicl .*~ *~ systems systems.csv
