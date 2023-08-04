ocicl: *.lisp *.asd Makefile runtime/ocicl-runtime.lisp
	rm -rf systems systems.csv;
	sbcl --dynamic-space-size 2560 --no-userinit --eval "(require 'asdf)" --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :ocicl) (sb-ext:quit))"

DESTDIR ?= ${HOME}/.local

ifeq ($(OS),Windows_NT)     # is Windows_NT on XP, 2000, 7, Vista, 10...
    detected_OS := Windows
else
    detected_OS := $(shell uname)# same as "uname -s"
    UNAME_M := $(shell uname -m)
endif



install:
	mkdir -p ${DESTDIR}/bin || true ;
	echo $(OS)
	echo $(detected_OS)
ifeq ($(detected_OS),Windows)
	echo "Installing for Windows..."
	cp ocicl.exe ${DESTDIR}/bin
	unzip oras/oras_1.0.0_windows_amd64.zip oras.exe
	cp oras.exe ${DESTDIR}/bin/ocicl-oras.exe
else ifeq ($(detected_OS),Darwin)
	echo "Installing for macOS $(UNAME_M)..."
	cp ocicl ${DESTDIR}/bin ;
ifneq (,$(findstring $(UNAME_M),x86_64 arm arm64))
		tar xvf oras/oras_1.0.0_darwin_$(UNAME_M).tar.gz -C /tmp oras > /dev/null 2>&1;
else
		echo "Unsupport macOS type: $(UNAME_M)"
endif
	mv /tmp/oras ${DESTDIR}/bin/ocicl-oras
else
	echo "Installing for Linux..."
	cp ocicl ${DESTDIR}/bin ;
	tar xvf oras/oras_1.0.0_linux_amd64.tar.gz -C /tmp oras > /dev/null 2>&1;
	mv /tmp/oras ${DESTDIR}/bin/ocicl-oras
endif
	${DESTDIR}/bin/ocicl setup


clean:
	-rm -rf ocicl .*~ *~ systems systems.csv
