(in-package #:org.shirakumo.float-features)

(docs:define-docs
  (variable short-float-positive-infinity
    "The positive infinity for short-floats.

Supported on:
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-POSITIVE-SHORT-FLOAT on unsupported implementations.")
  
  (variable short-float-negative-infinity
    "The negative infinity for short-floats.

Supported on:
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-NEGATIVE-SHORT-FLOAT on unsupported implementations.")

  (variable short-float-nan
    "A positive quiet NaN value with zero-cleared payload for short-floats.
This value should NOT be used for comparison with a float value for the pupose of testing NaN
because NaN has unused bits (called payload) which are explicitly allowed to store additional information.
Comparing this value with other NaN representations via = will fail.
To test if a number is a NaN, use FLOAT-NAN-P.

It is supported on implementations which support BITS-SHORT-FLOAT.

Defaults to MOST-POSITIVE-SHORT-FLOAT on unsupported implementations.")

  (variable single-float-positive-infinity
    "The positive infinity for single-floats.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-POSITIVE-SINGLE-FLOAT on unsupported implementations.")
  
  (variable single-float-negative-infinity
    "The negative infinity for single-floats.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-NEGATIVE-SINGLE-FLOAT on unsupported implementations.")

  (variable single-float-nan
    "A positive quiet NaN value with zero-cleared payload for single-floats.
This value should NOT be used for comparison with a float value for the pupose of testing NaN
because NaN has unused bits (called payload) which are explicitly allowed to store additional information.
Comparing this value with other NaN representations via = will fail.
To test if a number is a NaN, use FLOAT-NAN-P.

It is supported on implementations which support BITS-SINGLE-FLOAT.

Defaults to MOST-POSITIVE-SINGLE-FLOAT on unsupported implementations.")

  (variable double-float-positive-infinity
    "The positive infinity for double-floats.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-POSITIVE-DOUBLE-FLOAT on unsupported implementations.")
  
  (variable double-float-negative-infinity
    "The negative infinity for double-floats.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-NEGATIVE-DOUBLE-FLOAT on unsupported implementations.")

  (variable double-float-nan
    "A positive quiet NaN value with zero-cleared payload for double-floats.
This value should NOT be used for comparison with a float value for the pupose of testing NaN
because NaN has unused bits (called payload) which are explicitly allowed to store additional information.
Comparing this value with other NaN representations via = will fail.
To test if a number is a NaN, use FLOAT-NAN-P.

It is supported on implementations which support BITS-DOUBLE-FLOAT.

Defaults to MOST-POSITIVE-DOUBLE-FLOAT on unsupported implementations.")

  (variable long-float-positive-infinity
    "The positive infinity for long-floats.

Supported on:
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-POSITIVE-LONG-FLOAT on unsupported implementations.")
  
  (variable long-float-negative-infinity
    "The negative infinity for long-floats.

Supported on:
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* MKCL
* SBCL

Defaults to MOST-NEGATIVE-LONG-FLOAT on unsupported implementations.")

  (variable long-float-nan
    "A positive quiet NaN value with zero-cleared payload for long-floats.
This value should NOT be used for comparison with a float value for the pupose of testing NaN
because NaN has unused bits (called payload) which are explicitly allowed to store additional information.
Comparing this value with other NaN representations via = will fail.
To test if a number is a NaN, use FLOAT-NAN-P.

It is supported on implementations which support BITS-LONG-FLOAT.

Defaults to MOST-POSITIVE-LONG-FLOAT on unsupported implementations.")
  
  (function float-infinity-p
    "Returns T if the given float is an infinity, NIL otherwise.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* SBCL

Defaults to comparing against the individual constants on unsupported
implementations.")
  
  (function float-nan-p
    "Returns T if the given float is in not-a-number state, NIL otherwise.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS
* MEZZANO
* SBCL

Defaults to returning NIL on unsupported implementations.")
  
  (function with-float-traps-masked
    "Attempts to mask the given floating point traps.

Masking a floating point trap causes the given floating point exception
to not signal a condition in the lisp world, and instead lets the
operation return a float that is either a NaN or an infinity.

The following traps are recognised:

  :underflow
  :overflow
  :inexact
  :invalid 
  :divide-by-zero
  :denormalized-operand

The traps argument may be either a list of the above trap identifiers,
or T to signify all maskable traps.

Note that not all implementations will signal conditions on all of the
above floating point traps anyway, and some implementations may only
support masking some of the above traps.

Supported on:
* ABCL (:overflow :underflow)
* CCL (:overflow :underflow :inexact :invalid :divide-by-zero)
* CLISP (:underflow)
* CMUCL T
* ECL (:underflow :overflow :inexact :invalid :divide-by-zero)
* MEZZANO T
* SBCL T

Defaults to a progn on unsupported implementations and ignores
unsupported traps.")

    (function with-rounding-mode
    "Temporarily set rounding mode when execute body.

The following modes are recognised:

  :nearest Round to nearest, ties to even
  :positive Round toward positive infinity
  :negative Round toward negative infinity
  :zero Round toward zero

Note that many compilers would by default try to fold floating point
expressions into constants before the rounding mode can take effect.
It is advisory to check disassembled functions to see if that is the
case when the result is not expected.

Supported on:
* CCL
* CMUCL
* MEZZANO
* SBCL

Signals an error on unsupported implementations.")

  (function short-float-bits
    "Returns the bit representation of the short-float as an (unsigned-byte 16).

Supported on:
* MEZZANO

Supported (with 32bit short-float = single-float) on:
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS (64-bit only. on 32-bit lw, short-float is not 16 or 32 bits)
* SBCL

On platforms with 32-bit short-float, low bits of significand are
dropped without rounding, and out of range exponents are converted to
infinities. All values returned by bits-short-float should convert
back to the same bits.

Defaults to signalling an error on unsupported implementations.")
  
  (function single-float-bits
    "Returns the bit representation of the single-float as an (unsigned-byte 32).

Supported on:
* ABCL
* ALLEGRO
* CCL
* CLASP
* CMUCL
* LISPWORKS
* MEZZANO
* SBCL

Defaults to signalling an error on unsupported implementations.")
  
  (function double-float-bits
    "Returns the bit representation of the double-float as an (unsigned-byte 64).

Supported on:
* ABCL
* ALLEGRO
* CCL
* CLASP
* CMUCL
* LISPWORKS
* MEZZANO
* SBCL

Defaults to signalling an error on unsupported implementations.")
  
  (function long-float-bits
    "Returns the bit representation of the long-float as an (unsigned-byte 128).

Supported on:

Defaults to signalling an error on unsupported implementations.")
  
  (function bits-short-float
    "Encodes the (unsigned-byte 16) bit representation into a native short-float.

Supported on:
* MEZZANO

Supported (with 32bit short-float = single-float) on:
* ALLEGRO
* CCL
* CMUCL
* ECL
* LISPWORKS (64-bit only. on 32-bit lw, short-float is not 16 or 32 bits)
* SBCL

Defaults to signalling an error on unsupported implementations.")
  
  (function bits-single-float
    "Encodes the (unsigned-byte 32) bit representation into a native single-float.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CLASP
* CMUCL
* LISPWORKS
* MEZZANO
* SBCL

Defaults to signalling an error on unsupported implementations.")
  
  (function bits-double-float
    "Encodes the (unsigned-byte 64) bit representation into a native double-float.

Supported on:
* ABCL
* ALLEGRO
* CCL
* CLASP
* CMUCL
* LISPWORKS
* MEZZANO
* SBCL

Defaults to signalling an error on unsupported implementations.")
  
  (function bits-long-float
    "Encodes the (unsigned-byte 128) bit representation into a native long-float.

Supported on:

Defaults to signalling an error on unsupported implementations."))
