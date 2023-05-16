ocicl: *.lisp *.asd Makefile
	rm -rf systems systems.csv;
	sbcl --no-userinit --eval "(require 'asdf)" --eval "(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:make :ocicl) (sb-ext:quit))"

clean:
	-rm -rf ocicl .*~ *~ systems systems.csv
