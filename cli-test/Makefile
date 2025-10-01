cli-test: src/*.lisp *.asd
	sbcl --eval "(asdf:make :cli-test)" --quit

clean:
	rm -rf *~ cli-test
