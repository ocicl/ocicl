# IDNA - International Domain Name functions for Common Lisp.

## Usage (unicode -> punycode only at the moment):

Loading:

        (asdf:load-system :idna)

Encoding strings as IDNA:

        (idna:to-ascii "中央大学.tw")
        ;; => "xn--fiq80yua78t.tw"

        (idna:to-ascii "müller.example.com")
        ;; => "xn--mller-kva.example.com"
        
Decoding strings from IDNA notation to unicode text:

        (idna:to-unicode "xn--fiq80yua78t.tw")
        ;; => "中央大学.tw"
        
## Known bugs

The functions punycode-encode accept a :preserve-case keyword argument
that currently doesn't work: punycode-encode returns wrong results,
and punycode-decode doesn't implement it at all.
