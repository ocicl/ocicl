(in-package #:org.shirakumo.precise-time)

(defconstant REALTIME-CLOCK
  #+linux   0
  #+freebsd 0
  #+openbsd 0
  #+netbsd  0
  #-(or linux freebsd openbsd netbsd)
  (error "Do not know the value of CLOCK_REALTIME on this platform."))

(defconstant MONOTONIC-CLOCK
  #+linux   1
  #+freebsd 4
  #+openbsd 3
  #+netbsd  3
  #-(or linux freebsd openbsd netbsd)
  (error "Do not know the value of CLOCK_MONOTONIC on this platform."))

(cffi:defcstruct (timespec :conc-name timespec-)
  (secs #+64-bit :int64 #-64-bit :int32)
  (nsecs :long))

(define-constant PRECISE-TIME-UNITS-PER-SECOND 1000000000)
(define-constant MONOTONIC-TIME-UNITS-PER-SECOND 1000000000)

(define-implementation get-precise-time ()
  (cffi:with-foreign-objects ((timespec '(:struct timespec)))
    (if (= 0 (cffi:foreign-funcall "clock_gettime" :int REALTIME-CLOCK :pointer timespec :int))
        (values (+ (timespec-secs timespec) (encode-universal-time 0 0 0 1 1 1970 0))
                (timespec-nsecs timespec))
        (fail))))

(define-implementation get-monotonic-time ()
  (cffi:with-foreign-objects ((timespec '(:struct timespec)))
    (if (= 0 (cffi:foreign-funcall "clock_gettime" :int MONOTONIC-CLOCK :pointer timespec :int))
        (values (timespec-secs timespec) (timespec-nsecs timespec))
        (fail))))
