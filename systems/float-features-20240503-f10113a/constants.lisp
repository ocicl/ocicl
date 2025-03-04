(in-package #:org.shirakumo.float-features)

(defconstant SHORT-FLOAT-POSITIVE-INFINITY
  #+ccl 1S++0
  #+clasp EXT:SHORT-FLOAT-POSITIVE-INFINITY
  #+cmucl EXTENSIONS:SHORT-FLOAT-POSITIVE-INFINITY
  #+ecl EXT:SHORT-FLOAT-POSITIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:SHORT-FLOAT-POSITIVE-INFINITY
  #+mkcl EXT:SHORT-FLOAT-POSITIVE-INFINITY
  #+sbcl SB-EXT:SHORT-FLOAT-POSITIVE-INFINITY
  #+lispworks 1S++0
  #+allegro (coerce excl:*infinity-single* 'short-float)
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks allegro)
  MOST-POSITIVE-SHORT-FLOAT)

(defconstant SHORT-FLOAT-NEGATIVE-INFINITY
  #+ccl -1S++0
  #+clasp EXT:SHORT-FLOAT-NEGATIVE-INFINITY
  #+cmucl EXTENSIONS:SHORT-FLOAT-NEGATIVE-INFINITY
  #+ecl EXT:SHORT-FLOAT-NEGATIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:SHORT-FLOAT-NEGATIVE-INFINITY
  #+mkcl EXT:SHORT-FLOAT-NEGATIVE-INFINITY
  #+sbcl SB-EXT:SHORT-FLOAT-NEGATIVE-INFINITY
  #+lispworks -1S++0
  #+allegro (coerce excl:*negative-infinity-single* 'short-float)
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks allegro)
  MOST-NEGATIVE-SHORT-FLOAT)

(defconstant SINGLE-FLOAT-POSITIVE-INFINITY
  #+abcl EXTENSIONS:SINGLE-FLOAT-POSITIVE-INFINITY
  #+allegro excl:*infinity-single*
  #+ccl 1F++0
  #+clasp EXT:SINGLE-FLOAT-POSITIVE-INFINITY
  #+cmucl EXTENSIONS:SINGLE-FLOAT-POSITIVE-INFINITY
  #+ecl EXT:SINGLE-FLOAT-POSITIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:SINGLE-FLOAT-POSITIVE-INFINITY
  #+mkcl MKCL:SINGLE-FLOAT-POSITIVE-INFINITY
  #+sbcl SB-EXT:SINGLE-FLOAT-POSITIVE-INFINITY
  #+lispworks 1F++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  MOST-POSITIVE-SINGLE-FLOAT)

(defconstant SINGLE-FLOAT-NEGATIVE-INFINITY
  #+abcl EXTENSIONS:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+allegro excl:*negative-infinity-single*
  #+ccl -1F++0
  #+clasp EXT:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+cmucl EXTENSIONS:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+ecl EXT:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+mkcl MKCL:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+sbcl SB-EXT:SINGLE-FLOAT-NEGATIVE-INFINITY
  #+lispworks -1F++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  MOST-NEGATIVE-SINGLE-FLOAT)

(defconstant DOUBLE-FLOAT-POSITIVE-INFINITY
  #+abcl EXTENSIONS:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+allegro excl:*infinity-double*
  #+ccl 1D++0
  #+clasp EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+cmucl EXTENSIONS:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+ecl EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+mkcl MKCL:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+sbcl SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+lispworks 1D++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  MOST-POSITIVE-DOUBLE-FLOAT)

(defconstant DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+abcl EXTENSIONS:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+allegro excl:*negative-infinity-double*
  #+ccl -1D++0
  #+clasp EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+cmucl EXTENSIONS:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+ecl EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+mkcl MKCL:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+sbcl SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY
  #+lispworks -1D++0
  #-(or abcl allegro ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  MOST-NEGATIVE-DOUBLE-FLOAT)

(defconstant LONG-FLOAT-POSITIVE-INFINITY
  #+ccl 1L++0
  #+clasp EXT:LONG-FLOAT-POSITIVE-INFINITY
  #+cmucl EXTENSIONS:LONG-FLOAT-POSITIVE-INFINITY
  #+ecl EXT:LONG-FLOAT-POSITIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:LONG-FLOAT-POSITIVE-INFINITY
  #+mkcl EXT:LONG-FLOAT-POSITIVE-INFINITY
  #+sbcl SB-EXT:LONG-FLOAT-POSITIVE-INFINITY
  #+lispworks 1L++0
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  MOST-POSITIVE-LONG-FLOAT)

(defconstant LONG-FLOAT-NEGATIVE-INFINITY
  #+ccl -1L++0
  #+clasp EXT:LONG-FLOAT-NEGATIVE-INFINITY
  #+cmucl EXTENSIONS:LONG-FLOAT-NEGATIVE-INFINITY
  #+ecl EXT:LONG-FLOAT-NEGATIVE-INFINITY
  #+mezzano MEZZANO.EXTENSIONS:LONG-FLOAT-NEGATIVE-INFINITY
  #+mkcl EXT:LONG-FLOAT-NEGATIVE-INFINITY
  #+sbcl SB-EXT:LONG-FLOAT-NEGATIVE-INFINITY
  #+lispworks -1L++0
  #-(or ccl clasp cmucl ecl mezzano mkcl sbcl lispworks)
  MOST-NEGATIVE-LONG-FLOAT)

(handler-case
    (progn
      (bits-short-float 0)
      (defconstant SHORT-FLOAT-NAN
        (bits-short-float #b0111111000000000)))
  (error ()
    (define-symbol-macro SHORT-FLOAT-NAN
        (bits-short-float #b0111111000000000))))

(handler-case
    (progn
      (bits-single-float 0)
      (defconstant SINGLE-FLOAT-NAN
        (bits-single-float #b01111111110000000000000000000000)))
  (error ()
    (define-symbol-macro SINGLE-FLOAT-NAN
        (bits-single-float #b01111111110000000000000000000000))))

(handler-case
    (progn
      (bits-double-float 0)
      (defconstant DOUBLE-FLOAT-NAN
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000)))
  (error ()
    (define-symbol-macro DOUBLE-FLOAT-NAN
        (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000))))

(handler-case
    (progn
      (bits-long-float 0)
      (defconstant LONG-FLOAT-NAN
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
  (error ()
    (define-symbol-macro LONG-FLOAT-NAN
        (bits-long-float #b01111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))))

