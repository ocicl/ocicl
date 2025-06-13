<%= @ app-name %>: src/*.lisp *.asd
	sbcl --eval "(asdf:make :<%= @ app-name %>)" --quit

clean:
	rm -rf *~ <%= @ app-name %>
