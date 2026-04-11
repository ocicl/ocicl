(in-package #:org.shirakumo.precise-time)

(cffi:defcstruct (timespec :conc-name timespec-)
  (secs #+64-bit :int64 #-64-bit :int32)
  (nsecs :long))

(define-constant PRECISE-TIME-UNITS-PER-SECOND 1000000000)

(define-constant MONOTONIC-TIME-UNITS-PER-SECOND
    (cffi:with-foreign-objects ((tb :uint32 2))
      (if (= 0 (cffi:foreign-funcall "mach_timebase_info" :pointer tb :int))
          (let ((ticks-to-nanos (/ (cffi:mem-aref tb :uint32 1) (cffi:mem-aref tb :uint32 0))))
            (floor (* 1000000000 ticks-to-nanos)))
          (error "Failed to get time scale for monotonic time."))))

(define-implementation get-precise-time ()
  (cffi:with-foreign-objects ((timespec '(:struct timespec)))
    (if (= 0 (cffi:foreign-funcall "clock_gettime" :int 0 :pointer timespec :int))
        (values (+ (timespec-secs timespec) (encode-universal-time 0 0 0 1 1 1970 0))
                (timespec-nsecs timespec))
        (fail))))

(define-implementation get-monotonic-time ()
  (let ((val (cffi:foreign-funcall "mach_absolute_time" :uint64)))
    (if (< 0 val)
        (values (truncate val MONOTONIC-TIME-UNITS-PER-SECOND)
                (mod val MONOTONIC-TIME-UNITS-PER-SECOND))
        (fail))))
