(in-package #:org.shirakumo.float-features)

(eval-when (:compile-toplevel :load-toplevel)
  #+ecl
  (when (find-symbol "BITS-SINGLE-FLOAT" "SYSTEM") (pushnew :ecl-float-bit-translations *features*)))

(declaim (inline float-infinity-p
                 float-nan-p))

(defun float-infinity-p (float)
  #+abcl (system:float-infinity-p float)
  #+allegro (excl:infinityp float)
  #+ccl (ccl::infinity-p float)
  #+clasp (ext:float-infinity-p float)
  #+cmucl (extensions:float-infinity-p float)
  #+ecl (ext:float-infinity-p float)
  #+mezzano (mezzano.extensions:float-infinity-p float)
  #+sbcl (sb-ext:float-infinity-p float)
  #-(or abcl allegro ccl clasp cmucl ecl mezzano sbcl)
  (etypecase float
    (short-float (or (= float SHORT-FLOAT-NEGATIVE-INFINITY)
                     (= float SHORT-FLOAT-POSITIVE-INFINITY)))
    (single-float (or (= float SINGLE-FLOAT-NEGATIVE-INFINITY)
                      (= float SINGLE-FLOAT-POSITIVE-INFINITY)))
    (double-float (or (= float DOUBLE-FLOAT-NEGATIVE-INFINITY)
                      (= float DOUBLE-FLOAT-POSITIVE-INFINITY)))
    (long-float (or (= float LONG-FLOAT-NEGATIVE-INFINITY)
                    (= float LONG-FLOAT-POSITIVE-INFINITY)))))

(defun float-nan-p (float)
  #+abcl (system:float-nan-p float)
  #+allegro (and (excl:nanp float) t)
  #+ccl (and (ccl::nan-or-infinity-p float)
             (not (ccl::infinity-p float)))
  #+clasp (ext:float-nan-p float)
  #+cmucl (extensions:float-nan-p float)
  #+ecl (ext:float-nan-p float)
  #+mezzano (mezzano.extensions:float-nan-p float)
  #+sbcl (sb-ext:float-nan-p float)
  #+lispworks (sys::nan-p float)
  #-(or abcl allegro ccl clasp cmucl ecl mezzano sbcl lispworks)
  (/= float float))

(defmacro with-float-traps-masked (traps &body body)
  (let ((traps (etypecase traps
                 ((eql T) '(:underflow :overflow :inexact :invalid :divide-by-zero :denormalized-operand))
                 (list traps))))
    #+abcl
    (let ((previous (gensym "PREVIOUS")))
      `(let ((,previous (extensions:get-floating-point-modes)))
         (unwind-protect
              (progn
                (extensions:set-floating-point-modes
                 :traps ',(intersection traps '(:overflow :underflow)))
                NIL ,@body)
           (apply #'extensions:set-floating-point-modes ,previous))))
    #+ccl
    (let ((previous (gensym "PREVIOUS"))
          (traps (loop for thing in traps
                       for trap = (case thing
                                    (:underflow :underflow)
                                    (:overflow :overflow)
                                    (:divide-by-zero :division-by-zero)
                                    (:invalid :invalid)
                                    (:inexact :inexact))
                       when trap collect trap)))
      `(let ((,previous (ccl:get-fpu-mode)))
         (unwind-protect
              (progn
                (ccl:set-fpu-mode
                 ,@(loop for trap in traps
                         collect trap collect NIL))
                NIL ,@body)
           (apply #'ccl:set-fpu-mode ,previous))))
    #+clisp
    (if (find :underflow)
        `(ext:without-floating-point-underflow
           ,@body)
        `(progn
           ,@body))
    #+cmucl
    `(extensions:with-float-traps-masked #+x86 ,traps #-x86 ,(remove :denormalized-operand traps)
       ,@body)
    #+ecl
    (let ((previous (gensym "PREVIOUS")))
      `(let ((,previous (ext:trap-fpe 'last NIL)))
         (unwind-protect
              (progn
                ,@(loop for trap in traps
                        for keyword = (case trap
                                        (:underflow 'floating-point-underflow)
                                        (:overflow 'floating-point-overflow)
                                        (:inexact 'floating-point-inexact)
                                        (:invalid 'floating-point-invalid-operation)
                                        (:divide-by-zero 'division-by-zero))
                        when keyword collect `(ext:trap-fpe ',keyword NIL))
                NIL ,@body)
           (ext:trap-fpe ,previous T))))
    #+clasp
     `(ext:with-float-traps-masked ,traps
       ,@body)
    #+mezzano
    (let ((previous (gensym "PREVIOUS"))
          (traps (loop for thing in traps
                       for trap = (case thing
                                    (:underflow :underflow)
                                    (:overflow :overflow)
                                    (:divide-by-zero :divide-by-zero)
                                    (:invalid :invalid-operation)
                                    (:inexact :precision)
                                    #+x86-64
                                    (:denormalized-operand :denormal-operand))
                       when trap collect trap)))
      `(let ((,previous (mezzano.runtime::get-fpu-mode)))
         (unwind-protect
              (progn
                (mezzano.runtime::set-fpu-mode
                 ,@(loop for trap in traps
                         collect trap collect T))
                NIL ,@body)
           (apply #'mezzano.runtime::set-fpu-mode ,previous))))
    #+sbcl
    `(sb-int:with-float-traps-masked #+x86 ,traps #-x86 ,(remove :denormalized-operand traps)
       ,@body)
    #-(or abcl ccl clasp clisp cmucl ecl mezzano sbcl)
    (declare (ignore traps))
    #-(or abcl ccl clasp clisp cmucl ecl mezzano sbcl)
    `(progn ,@body)))

(defmacro with-rounding-mode (mode &body body)
  #+(or ccl cmucl mezzano sbcl)
  (let ((previous (gensym "PREVIOUS"))
        (mode #+ccl
              (case mode
                    (:nearest :nearest)
                    (:positive :positive)
                    (:negative :negative)
                    (:zero :zero))
              #+(or cmucl sbcl mezzano)
              (case mode
                    (:nearest :nearest)
                    (:positive :positive-infinity)
                    (:negative :negative-infinity)
                    (:zero :zero))))
    `(let ((,previous #+ccl
                      (ccl:get-fpu-mode :rounding-mode)
                      #+cmucl
                      (getf (extensions:get-floating-point-modes) :rounding-mode)
                      #+mezzano
                      (getf (mezzano.runtime::get-fpu-mode) :rounding-mode)
                      #+sbcl
                      (getf (sb-int:get-floating-point-modes) :rounding-mode)))
       (unwind-protect
            (progn
              #+ccl
              (ccl:set-fpu-mode :rounding-mode ,mode)
              #+cmucl
              (extensions:set-floating-point-modes :rounding-mode ,mode)
              #+mezzano
              (mezzano.runtime::set-fpu-mode :rounding-mode ,mode)
              #+sbcl
              (sb-int:set-floating-point-modes :rounding-mode ,mode)
              NIL ,@body)
         #+ccl
         (ccl:set-fpu-mode :rounding-mode ,previous)
         #+cmucl
         (extensions:set-floating-point-modes :rounding-mode ,previous)
         #+mezzano
         (mezzano.runtime::set-fpu-mode :rounding-mode ,previous)
         #+sbcl
         (sb-int:set-floating-point-modes :rounding-mode ,previous))))
  #-(or ccl cmucl mezzano sbcl)
  (declare (ignore traps))
  #-(or ccl cmucl mezzano sbcl)
  `(error "Implementation not supported."))

(declaim (inline short-float-bits
                 single-float-bits
                 double-float-bits
                 long-float-bits
                 bits-short-float
                 bits-single-float
                 bits-double-float
                 bits-long-float))

(declaim (ftype (function (T) (unsigned-byte 32)) single-float-bits))
(defun single-float-bits (float)
  #+abcl
  (ldb (byte 32 0) (system:single-float-bits float))
  #+allegro
  (multiple-value-bind (high low) (excl:single-float-to-shorts float)
    (logior low (ash high 16)))
  #+ccl
  (ccl::single-float-bits float)
  #+clasp
  (ext:single-float-to-bits float)
  #+cmucl
  (ldb (byte 32 0) (kernel:single-float-bits float))
  #+ecl-float-bit-translations
  (si:single-float-bits float)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'single-float v 0) float)
    (sys:typed-aref '(unsigned-byte 32) v 0))
  #+mezzano
  (mezzano.extensions:single-float-to-ieee-binary32 float)
  #+sbcl
  (ldb (byte 32 0) (sb-kernel:single-float-bits float))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn float (error "Implementation not supported.")))

(declaim (ftype (function (T) (unsigned-byte 16)) short-float-bits))
(defun short-float-bits (float)
  (declare (ignorable float))
  #+mezzano
  (mezzano.extensions:short-float-to-ieee-binary16 float)
  #+(or ecl sbcl cmucl allegro ccl
        (and 64-bit lispworks))
  (let* ((bits (single-float-bits float))
         (sign (ldb (byte 1 31) bits))
         (exp (- (ldb (byte 8 23) bits) 127))
         (sig (ldb (byte 23 0) bits)))
    (cond
      ((or (eql 0s0 float)
           (< exp -24))
       ;;underflow
       (ash sign 15))
      ((< exp -14)
       ;; encode as denormal if possible
       (logior (ash sign 15)
               0
               (ash (ldb (byte 11 13)
                         (logior (ash 1 23) sig))
                    (+ exp 14))))
      ((< exp 16)
       ;; encode directly
       (logior (ash sign 15)
               (ash (+ exp 15) 10)
               (ash sig -13)))
      ((zerop sig)
       ;; infinity
       (if (zerop sign)
           #b0111110000000000
           #b1111110000000000))
      (t
       ;;NaN
       (logior (ash sign 15)
               (ash #x1f 10)
               (ldb (byte 10 13) sig)))))
  ;; clisp short-float is 1+8+16
  ;; 32bit lispworks 5+ is 1+8+??, lw4 only has double
  ;; not sure about others?
  #- (or mezzano ecl sbcl cmucl allegro ccl (and 64-bit lispworks))
  (progn float (error "Implementation not supported.")))

(declaim (ftype (function (T) (unsigned-byte 64)) double-float-bits))
(defun double-float-bits (float)
  #+abcl
  (logior (system::double-float-low-bits float)
          (ash (system::double-float-high-bits float) 32))
  #+allegro
  (multiple-value-bind (s3 s2 s1 s0) (excl:double-float-to-shorts float)
    (logior s0 (ash s1 16) (ash s2 32) (ash s3 48)))
  #+ccl
  (multiple-value-bind (high low) (ccl::double-float-bits float)
    (logior low (ash high 32)))
  #+clasp
  (ext:double-float-to-bits float)
  #+cmucl
  (ldb (byte 64 0)
   (logior (kernel:double-float-low-bits float)
           (ash (kernel:double-float-high-bits float) 32)))
  #+ecl-float-bit-translations
  (si:double-float-bits float)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
    (declare (optimize (speed 3) (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref 'double-float v 0) float)
          #+64-bit (sys:typed-aref '(unsigned-byte 64) v 0)
          #+(and 32-bit (not big-endian)) (logior (sys:typed-aref '(unsigned-byte 32) v 0)
                                                  (ash (sys:typed-aref '(unsigned-byte 32) v 4) 32))
          #+(and 32-bit big-endian) (logior (sys:typed-aref '(unsigned-byte 32) v 4)
                                            (ash (sys:typed-aref '(unsigned-byte 32) v 0) 32)))
  #+mezzano
  (mezzano.extensions:double-float-to-ieee-binary64 float)
  #+sbcl
  (ldb (byte 64 0)
       (logior (sb-kernel:double-float-low-bits float)
               (ash (sb-kernel:double-float-high-bits float) 32)))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn float (error "Implementation not supported.")))

(declaim (ftype (function (T) (unsigned-byte 128)) long-float-bits))
(defun long-float-bits (float)
  (declare (ignorable float))
  #+ecl-float-bit-translations
  (si:long-float-bits float)
  #-(or ecl-float-bit-translations)
  (error "Implementation not supported."))

(declaim (ftype (function (T) single-float) bits-single-float))
(defun bits-single-float (bits)
  #+abcl
  (system:make-single-float bits)
  #+allegro
  (excl:shorts-to-single-float (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
  #+ccl
  (ccl::host-single-float-from-unsigned-byte-32 bits)
  #+clasp
  (ext:bits-to-single-float bits)
  #+cmucl
  (flet ((s32 (x)
           (logior x (- (mask-field (byte 1 31) x))) ))
    (kernel:make-single-float (s32 bits)))
  #+ecl-float-bit-translations
  (si:bits-single-float bits)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 4)))
    (declare (optimize speed (float 0) (safety 0)))
    (declare (dynamic-extent v))
    (setf (sys:typed-aref '(unsigned-byte 32) v 0) bits)
    (sys:typed-aref 'single-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary32-to-single-float bits)
  #+sbcl
  (sb-kernel:make-single-float
   (sb-c::mask-signed-field 32 (the (unsigned-byte 32) bits)))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn bits (error "Implementation not supported.")))
  
(declaim (ftype (function (T) short-float) bits-short-float))
(defun bits-short-float (bits)
  (declare (ignorable bits))
  #+mezzano
  (mezzano.extensions:ieee-binary16-to-short-float bits)
  #+(or ecl sbcl cmucl allegro ccl (and 64-bit lispworks))
  (let ((sign (ldb (byte 1 15) bits))
        (exp (ldb (byte 5 10) bits))
        (sig (ldb (byte 10 0) bits)))
    (if (= exp 31)
        (cond
          ((not (zerop sig))
           ;; NaNs
           (bits-single-float
            (logior (ash sign 31)
                    (ash #xff 23)
                    ;; store in high-bit to preserve quiet/signalling
                    (ash sig 13))))
          ;; infinities
          ((zerop sign)
           single-float-positive-infinity)
          (t
           single-float-negative-infinity))
        (cond
          ((= 0 exp sig)
           ;; +- 0
           (if (zerop sign) 0s0 -0s0))
          ((zerop exp)
           ;; denormals -> single floats
           (let ((d (- 11 (integer-length sig))))
             (setf exp (- -14 d))
             (setf sig (ldb (byte 11 0) (ash sig (1+ d))))
             (bits-single-float
              (logior (ash sign 31)
                      (ash (+ exp 127) 23)
                      (ash sig #.(- 23 11))))))
          (t
           ;; normal numbers
           (bits-single-float
            (logior (ash sign 31)
                    (ash (+ exp #.(+ 127 -15)) 23)
                    (ash sig #.(- 23 10))))))))
  #-(or allegro ccl cmucl ecl mezzano sbcl (and 64-bit lispworks))
  (progn bits (error "Implementation not supported.")))

(declaim (ftype (function (T) double-float) bits-double-float))
(defun bits-double-float (bits)
  #+abcl
  (system:make-double-float bits)
  #+allegro
  (excl:shorts-to-double-float
   (ldb (byte 16 48) bits) (ldb (byte 16 32) bits) (ldb (byte 16 16) bits) (ldb (byte 16 0) bits))
  #+ccl
  (ccl::double-float-from-bits (ldb (byte 32 32) bits) (ldb (byte 32 0) bits))
  #+clasp
  (ext:bits-to-double-float bits)
  #+cmucl
  (flet ((s32 (x)
           (logior x (- (mask-field (byte 1 31) x))) ))
    (kernel:make-double-float (s32 (ldb (byte 32 32) bits))
                              (ldb (byte 32 0) bits)))
  #+ecl-float-bit-translations
  (si:bits-double-float bits)
  #+lispworks
  (let ((v (sys:make-typed-aref-vector 8)))
    (declare (optimize speed (float 0) (safety 0)))
    (declare (dynamic-extent v))
    #+64-bit (setf (sys:typed-aref '(unsigned-byte 64) v 0) bits)
    #+(and :32-bit (not :big-endian)) (setf (sys:typed-aref '(unsigned-byte 32) v 0) (ldb (byte 32 0) bits)
                                            (sys:typed-aref '(unsigned-byte 32) v 4) (ldb (byte 32 32) bits))
    #+(and :32-bit :big-endian) (setf (sys:typed-aref '(unsigned-byte 32) v 4) (ldb (byte 32 0) bits)
                                      (sys:typed-aref '(unsigned-byte 32) v 0) (ldb (byte 32 32) bits))
    (sys:typed-aref 'double-float v 0))
  #+mezzano
  (mezzano.extensions:ieee-binary64-to-double-float bits)
  #+sbcl
  (sb-kernel:make-double-float
   (sb-c::mask-signed-field 32 (ldb (byte 32 32) (the (unsigned-byte 64) bits)))
   (ldb (byte 32 0) bits))
  #-(or abcl allegro ccl clasp cmucl ecl-float-bit-translations lispworks mezzano sbcl)
  (progn bits (error "Implementation not supported.")))

(declaim (ftype (function (T) long-float) bits-long-float))
(defun bits-long-float (bits)
  (declare (ignorable bits))
  #+ecl-float-bit-translations
  (si:bits-long-float bits)
  #-(or ecl-float-bit-translations)
  (error "Implementation not supported."))
