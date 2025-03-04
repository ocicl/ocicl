(defpackage #:float-features
  (:nicknames #:org.shirakumo.float-features)
  (:use #:cl)
  (:export
   #:short-float-positive-infinity
   #:short-float-negative-infinity
   #:short-float-nan
   #:single-float-positive-infinity
   #:single-float-negative-infinity
   #:single-float-nan
   #:double-float-positive-infinity
   #:double-float-negative-infinity
   #:double-float-nan
   #:long-float-positive-infinity
   #:long-float-negative-infinity
   #:long-float-nan
   #:float-infinity-p
   #:float-nan-p
   #:with-float-traps-masked
   #:with-rounding-mode
   #:short-float-bits
   #:single-float-bits
   #:double-float-bits
   #:long-float-bits
   #:bits-short-float
   #:bits-single-float
   #:bits-double-float
   #:bits-long-float))
