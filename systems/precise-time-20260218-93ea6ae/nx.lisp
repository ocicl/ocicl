(in-package #:org.shirakumo.precise-time)

(cffi:defcstruct (timespec :conc-name timespec-)
  (secs #+64-bit :int64 #-64-bit :int32)
  (nsecs :long))

(define-constant PRECISE-TIME-UNITS-PER-SECOND 1000000000)

(define-protocol-constant MONOTONIC-TIME-UNITS-PER-SECOND (unsigned-byte 64)
  (cffi:foreign-funcall "nxgl_ticks_per_sec" :uint64))

(define-implementation get-precise-time ()
  (cffi:with-foreign-objects ((timespec '(:struct timespec)))
    (if (= 0 (cffi:foreign-funcall "clock_gettime" :int 0 :pointer timespec :int))
        (values (+ (timespec-secs timespec) (encode-universal-time 0 0 0 1 1 1970 0))
                (timespec-nsecs timespec))
        (fail))))

(define-protocol-fun get-monotonic-time () ((unsigned-byte 64) (unsigned-byte 64))
  (let ((time (cffi:foreign-funcall "nxgl_tick" :uint64)))
    (values (truncate time MONOTONIC-TIME-UNITS-PER-SECOND)
            (mod time MONOTONIC-TIME-UNITS-PER-SECOND))))
