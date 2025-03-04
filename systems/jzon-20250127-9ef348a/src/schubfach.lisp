;;
;; This implementation was ported from the Scala version in
;; jsoniter found here:
;;  <https://github.com/plokhotnyuk/jsoniter-scala/blob/master/jsoniter-scala-core/jvm/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/core/JsonWriter.scala>
;; which is in turn based on the Java version here:
;;   https://github.com/c4f7fcce9cb06515/Schubfach/blob/3c92d3c9b1fead540616c918cdfef432bca53dfa/todec/src/math/FloatToDecimal.java
;;
;; NOTE - There are many improvements to be made by properly
;;        defining numerical ranges of variables. The Java and Scala
;;        versions use signed types everywhere, and this can often
;;        get in the way of optimizations and code readability with
;;        CL's number system (hence the %int32 and such macros
;;
;;        At the time of writing, performance here is still at least
;;        5x the builtin writer on SBCL, with about 7x with (speed 3)
;;

(defpackage #:com.inuoe.jzon/schubfach
  (:use #:cl)
  (:export
    #:write-float
    #:write-double))

(in-package #:com.inuoe.jzon/schubfach)

(defmacro %int (size integer)
  (check-type size (integer 1))
  (let ((integer-sym (gensym (string '#:integer))))
    `(let ((,integer-sym (ldb (byte ,size 0) ,integer)))
      (logior (ldb (byte (1- ,size) 0) ,integer-sym)
              (- (mask-field (byte 1 (1- ,size)) ,integer-sym))))))

(defmacro %<< (size integer count)
  (check-type size (integer 1))
  `(%int ,size (ash ,integer (logand ,count (1- ,size)))))

(defmacro %>> (size integer count)
  (check-type size (integer 1))
  `(%int ,size (ash ,integer (- (logand ,count (1- ,size))))))

(defmacro %>>> (size integer count)
  (check-type size (integer 1))
  (let ((integer-sym (gensym (string '#:integer)))
        (count-sym (gensym (string '#:count))))
  `(let ((,integer-sym (ldb (byte ,size 0) ,integer))
         (,count-sym (logand ,count (1- ,size))))
    (%int ,size (mask-field (byte (- ,size ,count-sym) 0)
                              (ash ,integer-sym (- ,count-sym)))))))

(defmacro %int32 (integer)
  `(%int 32 ,integer))

(defmacro %<<32 (integer count)
  `(%<< 32 ,integer ,count))

(defmacro %>>32 (integer count)
  `(%>> 32 ,integer ,count))

(defmacro %>>>32 (integer count)
  `(%>>> 32 ,integer ,count))

(defmacro %int64 (integer)
  `(%int 64 ,integer))

(defmacro %<<64 (integer count)
  `(%<< 64 ,integer ,count))

(defmacro %>>64 (integer count)
  `(%>> 64 ,integer ,count))

(defmacro %>>>64 (integer count)
  `(%>>> 64 ,integer ,count))

(defparameter *%offsets* (make-array 65
                                     :element-type '(unsigned-byte 64)
                                     :initial-contents '(5088146770730811392 5088146770730811392 5088146770730811392 5088146770730811392
                                                         5088146770730811392 5088146770730811392 5088146770730811392 5088146770730811392
                                                         4889916394579099648 4889916394579099648 4889916394579099648 4610686018427387904
                                                         4610686018427387904 4610686018427387904 4610686018427387904 4323355642275676160
                                                         4323355642275676160 4323355642275676160 4035215266123964416 4035215266123964416
                                                         4035215266123964416 3746993889972252672 3746993889972252672 3746993889972252672
                                                         3746993889972252672 3458764413820540928 3458764413820540928 3458764413820540928
                                                         3170534127668829184 3170534127668829184 3170534127668829184 2882303760517117440
                                                         2882303760517117440 2882303760517117440 2882303760517117440 2594073385265405696
                                                         2594073385265405696 2594073385265405696 2305843009203693952 2305843009203693952
                                                         2305843009203693952 2017612633060982208 2017612633060982208 2017612633060982208
                                                         2017612633060982208 1729382256910170464 1729382256910170464 1729382256910170464
                                                         1441151880758548720 1441151880758548720 1441151880758548720 1152921504606845976
                                                         1152921504606845976 1152921504606845976 1152921504606845976 864691128455135132
                                                         864691128455135132  864691128455135132  576460752303423478  576460752303423478
                                                         576460752303423478  576460752303423478  576460752303423478  576460752303423478
                                                         576460752303423478)))

(defparameter *%digits* (let ((ds (make-array 100 :element-type '(unsigned-byte 16)))
                              (i 0)
                              (j 0))
                           (declare (type (integer 0) i j))
                           (loop :while (< j 10)
                                 :for k :of-type (integer 0) := 0
                                 :do (loop :while (< k 10)
                                           :do (setf (aref ds i)
                                                     (logior (ash (+ k (char-code #\0)) 8)
                                                             (ash (+ j (char-code #\0)) 0)))
                                               (incf i)
                                               (incf k))


                                     (incf j))
                           ds))

(defparameter *%gs* (let* ((gs (make-array 1234 :element-type '(signed-byte 64)))
                           (i 0)
                           (pow5 1))
                       (declare (type (integer 0 1235) i)
                                ;; var pow5 = BigInt(1)
                                (type (integer 1) pow5))
                       (loop :while (< i 650)
                             :do (let ((av (+ (ash pow5 (- (- (integer-length pow5) 126))) 1)))
                                   (setf (aref gs (- 648 i)) (logand (%int64 (ash av -63)) #x7FFFFFFFFFFFFFFF))
                                   (setf (aref gs (- 649 i)) (logand (%int64 av) #x7FFFFFFFFFFFFFFF))
                                   (setf pow5 (* pow5 5))
                                   (incf i 2)))
                       (setf pow5 5)
                       (loop :while (< i 1234)
                             :do (let ((inv (+ (truncate (ash 1 (+ (integer-length pow5) 125))
                                                         pow5)
                                               1)))
                                   (setf (aref gs i) (logand (%int64 (ash inv -63)) #x7FFFFFFFFFFFFFFF))
                                   (setf (aref gs (+ i 1)) (logand (%int64 inv) #x7FFFFFFFFFFFFFFF))
                                   (setf pow5 (* pow5 5))
                                   (incf i 2)))
                       gs))

(defmacro %multiply-high (x y)
  #+sbcl `(sb-kernel:%multiply-high ,x ,y)
  #-sbcl `(ldb (byte 64 64) (* ,x ,y)))

(declaim (inline %rop2))
(defun %rop2 (g cp)
  (declare (type (signed-byte 64) g)
           (type (signed-byte 32) cp))
  (the (values (signed-byte 32) &optional)
    (let ((x (%multiply-high g (%<<64 cp 32))))
      (logior (%int32 (%>>>64 (+ x) 31))
              (%>>>32 (- (%int32 x)) 31)))))

;;
;; https://lemire.me/blog/2021/06/03/computing-the-number-of-digits-of-an-integer-even-faster/
;;
(declaim (inline %digit-count))
(defun %digit-count (q0 &aux (offsets (load-time-value *%offsets*)))
  (declare (type (unsigned-byte 64) q0))
  (declare (type (simple-array (unsigned-byte 64) (65)) offsets))
  (%int32 (%>>64 (%int64 (+ (aref offsets (- 64 (integer-length q0)))
                                  q0))
                  58)))

(defun %write-fraction-digits (q p pos-lim buf ds)
  (declare (type (signed-byte 32) q p pos-lim)
           (type (or (simple-base-string 15)
                     (simple-base-string 24)) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values null &optional)
    (let ((q0 q)
          (pos p))
      (declare (type (signed-byte 32) q0))
      (loop :while (> pos pos-lim)
            :do (let* ((q1 (%int32 (%>>64 (* q0 1374389535) 37)))
                       (d (aref ds (- q0 (* q1 100)))))
                  (setf (char buf (- pos 1)) (code-char (ldb (byte 7 0) d)))
                  (setf (char buf (- pos 0)) (code-char (ldb (byte 7 8) d)))
                  (setf q0 q1)
                  (decf pos 2))))))

(defun %write-significant-fraction-digits32 (q p pos-lim buf ds)
  (declare (type (unsigned-byte 32) q p pos-lim)
           (type (or (simple-base-string 15)
                     (simple-base-string 24)) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let* ((q0 q)
           (q1 0)
           (pos p))
      (declare (type (signed-byte 32) q0))
      (loop :while (let ((qp (%int64 (* q0 1374389535))))
                     (setf q1 (%int32 (%>>64 qp 37)))
                     (zerop (logand qp #x1FC0000000)))
            :do (setf q0 q1)
                (setf pos (%int32 (- pos 2))))
      (let ((d (aref ds (- q0 (* q1 100)))))
        (setf (char buf (- pos 1)) (code-char (ldb (byte 7 0) d)))
        (setf (char buf (- pos 0)) (code-char (ldb (byte 7 8) d)))

        (let ((last-pos pos))
          (when (> d #x3039)
            (incf last-pos))
          (%write-fraction-digits q1 (- pos 2) pos-lim buf ds)
          last-pos)))))

(declaim (inline %write-2-digits))
(defun %write-2-digits (q0 pos buf ds)
  (declare (type (signed-byte 32) q0)
           (type (integer 0 22) pos)
           (type (or (simple-base-string 15)
                     (simple-base-string 24)) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let ((d (aref ds q0)))
      (setf (char buf (+ pos 0)) (code-char (ldb (byte 7 0) d)))
      (setf (char buf (+ pos 1)) (code-char (ldb (byte 7 8) d)))
      (+ pos 2))))

(defun %write-positive-int-digits (q p buf ds)
  (declare (type (unsigned-byte 31) q)
           (type (integer 0 23) p)
           (type (or (simple-base-string 15)
                     (simple-base-string 24)) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (let ((q0 q)
        (pos p))
    (declare (type (signed-byte 32) q0))
    (loop
      (setf pos (- pos 2))
      (when (< q0 100)
        (return))

      (let* ((q1 (%int32 (%>>64 (%int64 (* q0 1374389535)) 37)))
             (d (aref ds (- q0 (* q1 100)))))
        (setf (char buf (+ pos 0)) (code-char (ldb (byte 7 0) d)))
        (setf (char buf (+ pos 1)) (code-char (ldb (byte 7 8) d)))
        (setf q0 q1)))
    (if (< q0 10)
      (setf (char buf (+ pos 1)) (code-char (+ q0 (char-code #\0))))
      (let ((d (aref ds q0)))
        (setf (char buf (+ pos 0)) (code-char (ldb (byte 7 0) d)))
        (setf (char buf (+ pos 1)) (code-char (ldb (byte 7 8) d))))))
  (values))

(defmacro %single-float-bits (x)
  #-ecl
  `(the (unsigned-byte 32) (org.shirakumo.float-features:single-float-bits ,x))
  #+ecl
  (if (find-symbol (string '#:single-float-bits) '#:si)
    `(,(intern (string '#:single-float-bits) '#:si) ,x)
    (let ((tmp (gensym (string 'tmp))))
      `(ffi:with-foreign-object (,tmp :float)
        (setf (ffi:deref-pointer ,tmp :float) ,x)
        (ffi:deref-pointer ,tmp :uint32-t)))))

(defun %write-float (x buf
                      &aux
                      (pos 0)
                      (bits (%single-float-bits x))
                      (ds *%digits*)
                      (gs *%gs*))
  (declare (type single-float x)
           (type (simple-base-string 15) buf)
           (type (integer 0 14) pos)
           (type (unsigned-byte 32) bits)
           (type (simple-array (unsigned-byte 16) (100)) ds)
           (type (simple-array (signed-byte 64) (1234)) gs))
  (when (logbitp 31 bits)
    (setf (char buf pos) #\-)
    (incf pos))
  (cond
    ((zerop x)
      (setf (char buf (+ pos 0)) #\0)
      (setf (char buf (+ pos 1)) #\.)
      (setf (char buf (+ pos 2)) #\0)
      (+ pos 3))
    (t
      (let* ((ieee-exponent (ldb (byte 8 23) bits))
             (ieee-mantissa (ldb (byte 23 0) bits))
             (e2 (- ieee-exponent 150))
             (m2 (logior ieee-mantissa #x800000))
             (m10 0)
             (e10 0))
        (declare (type (signed-byte 32) e2))
        (declare (type (signed-byte 32) m10))
        (declare (type (signed-byte 32) e10))
        (cond
          ((zerop e2) (setf m10 m2))
          ((and (>= e2 -23) (<= e2 0) (zerop (logand (%<<32 m2 e2) e2)))
            (setf m10 (%>>32 m2 (- e2))))
          (t
            (let ((e10-corr 0)
                  (e2-corr 0)
                  (cbl-corr 2))
              (cond
                ((zerop ieee-exponent)
                  (setf e2 -149)
                  (setf m2 ieee-mantissa)
                  (when (< ieee-mantissa 8)
                    (setf m2 (* m2 10))
                    (setf e10-corr 1)))
                ((= ieee-exponent 255) (error "IllegalNumberError"))
                ((and (zerop ieee-mantissa) (> ieee-exponent 1))
                  (setf e2-corr 131007)
                  (setf cbl-corr 1)))

              (setf e10 (%>>32 (- (* e2 315653) e2-corr) 20))

              (let* ((g (%int64 (+ (aref gs (%<<32 (+ e10 324) 1)) 1)))
                     (h (+ (%>>32 (* (- e10) 108853) 15) e2 1))
                     (cb (%<<32 m2 2))
                     (vb-corr (- (logand m2 #x01) 1))
                     (vb (%rop2 g (%<<32 cb h)))
                     (vbl (+ (%rop2 g (%<<32 (- cb cbl-corr) h)) vb-corr))
                     (vbr (- (%rop2 g (%<<32 (+ cb 2) h)) vb-corr)))
                (when (or (< vb 400)
                          (progn
                            ;; divide a positive int by 40
                            (setf m10 (%int32 (%>>64 (* vb 107374183) 32)))
                            (let* ((vb40 (%int32 (* m10 40)))
                                   (diff (%int32 (- vbl vb40))))
                              (or (>= (logxor (%int32 (+ vb40 (- vbr) 40)) diff) 0)
                                  (progn
                                    (setf m10 (%int32 (+ m10 (%>>>32 (lognot diff) 31))))
                                    (setf e10 (%int32 (+ e10 1)))
                                    nil)))))
                  (setf m10 (%>>32 vb 2))
                  (let* ((vb4 (logand vb #xFFFFFFFC))
                         (diff (%int32 (- vbl vb4))))
                    (setf m10 (%int32
                                (+ m10
                                   (%>>>32
                                     (lognot
                                       (if (< (logxor (%int32 (+ vb4 (- vb4) 4)) diff) 0)
                                         diff
                                         (+ (logand vb #x3) (logand m10 #x1) -3)))
                                     31))))
                    (setf e10 (%int32 (- e10 e10-corr)))))))))

        (let ((len (%digit-count m10)))
          (setf e10 (%int32 (+ e10 len -1)))
          (cond
            ((or (< e10 -3) (>= e10 7))
              (let ((last-pos (%write-significant-fraction-digits32 m10 (+ pos len) pos buf ds)))
                (setf (char buf pos) (char buf (+ pos 1)))
                (setf (char buf (+ pos 1)) #\.)
                (setf pos
                  (cond
                    ((< last-pos (+ pos 3))
                      (setf (char buf last-pos) #\0)
                      (1+ last-pos))
                    (t last-pos)))
                (setf (char buf (+ pos 0)) #\e)
                (setf (char buf (+ pos 1)) #\-)
                (incf pos)
                (when (< e10 0)
                  (setf e10 (- e10))
                  (incf pos))
                (cond
                  ((< e10 10)
                    (setf (char buf pos) (code-char (+ e10 (char-code #\0))))
                    (+ pos 1))
                  (t (%write-2-digits e10 pos buf ds)))))
            ((< e10 0)
              (let ((dot-pos (+ pos 1)))
                (setf (char buf (+ pos 0)) #\0)
                (setf (char buf (+ pos 2)) #\0)
                (setf (char buf (+ pos 3)) #\0)
                (decf pos e10)
                (let ((last-pos (%write-significant-fraction-digits32 m10 (+ pos len) pos buf ds)))
                  (setf (char buf dot-pos) #\.)
                  last-pos)))
            ((< e10 (- len 1)) ;; + exp
              (let ((last-pos (%write-significant-fraction-digits32 m10 (+ pos len) pos buf ds)))
                (loop :repeat (1+ e10)
                      :do (setf (char buf pos) (char buf (+ pos 1)))
                          (incf pos))
                (setf (char buf pos) #\.)
                last-pos))
            (t
              (incf pos len)
              (%write-positive-int-digits m10 pos buf ds)
              (setf (char buf (+ pos 0)) #\.)
              (setf (char buf (+ pos 1)) #\0)
              (+ pos 2))))))))

(defun stringify-float (x &aux (buf (make-array 15 :element-type 'base-char)))
  (declare (dynamic-extent buf))
  (check-type x single-float)
  (let ((n (%write-float x buf)))
    (subseq buf 0 n)))

(defun write-float (x stream &aux (buf (make-array 15 :element-type 'base-char)))
  "Write the `single-float' `x' to `stream', which must be a character output stream."
  (declare (dynamic-extent buf))
  (check-type x single-float)
  (check-type stream stream)
  (let ((n (%write-float x buf)))
    (write-string buf stream :end n)))

(declaim (inline %rop3))
(defun %rop3 (g1 g0 cp)
  (declare (type (signed-byte 64) g1 g0 cp))
  (the (values (signed-byte 64) &optional)
    (let ((x (%int64 (+ (%multiply-high g0 cp) (%>>>64 (* g1 cp) 1)))))
      (logior (%int64 (+ (%multiply-high g1 cp) (%>>>64 x 63)))
              (%>>>64 (- (logand x #x7FFFFFFFFFFFFFFF)) 63)))))

(defun %write-significant-fraction-digits64 (q p pl buf ds)
  (declare (type (unsigned-byte 64) q)
           (type (integer 0 23) p)
           (type (signed-byte 32) pl)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let* ((q0 (%int32 q))
           (pos p)
           (pos-lim pl))
      (when (/= q0 q)
        (let* ((q1 (%int32 (%>>>64 (%multiply-high q 6189700196426901375) 25)))
               (r1 (%int32 (- q (* q1 100000000))))
               (posm8 (- pos 8)))
          (cond
            ((zerop r1)
              (setf q0 q1)
              (setf pos posm8))
            (t
              (%write-fraction-digits q1 posm8 pos-lim buf ds)
              (setf q0 r1)
              (setf pos-lim posm8)))))
      (%write-significant-fraction-digits32 q0 pos pos-lim buf ds))))

(declaim (inline %write-3-digits))
(defun %write-3-digits (q0 pos buf ds)
  (declare (type (signed-byte 32) q0)
           (type (integer 0 21) pos)
           (type (simple-base-string 24) buf)
           (type (simple-array (unsigned-byte 16) (100)) ds))
  (the (values (integer 0 24) &optional)
    (let* ((q1 (%>>32 (* q0 1311) 17))
           (d (aref ds (- q0 (* q1 100)))))
      (setf (char buf (+ pos 0)) (code-char (+ q1 (char-code #\0))))
      (setf (char buf (+ pos 1)) (code-char (ldb (byte 7 0) d)))
      (setf (char buf (+ pos 2)) (code-char (ldb (byte 7 8) d)))
      (+ pos 3))))

(defmacro %double-float-bits (x)
  #-ecl
  `(the (unsigned-byte 64) (org.shirakumo.float-features:double-float-bits ,x))
  #+ecl
  (if (find-symbol (string '#:double-float-bits) '#:si)
    `(,(intern (string '#:double-float-bits) '#:si) ,x)
    (let ((tmp (gensym (string 'tmp))))
      `(ffi:with-foreign-object (,tmp :double)
        (setf (ffi:deref-pointer ,tmp :double) ,x)
        (ffi:deref-pointer ,tmp :uint64-t)))))

(defun %write-double (x buf
                      &aux
                      (pos 0)
                      (bits (%double-float-bits x))
                      (ds (load-time-value *%digits*))
                      (gs (load-time-value *%gs*)))
  (declare (type double-float x)
           (type (simple-base-string 24) buf)
           (type (integer 0 23) pos)
           (type (simple-array (signed-byte 64) (1234)) gs))
  (when (logbitp 63 bits)
    (setf (char buf pos) #\-)
    (incf pos))
  (cond
    ((zerop x)
      (setf (char buf (+ pos 0)) #\0)
      (setf (char buf (+ pos 1)) #\.)
      (setf (char buf (+ pos 2)) #\0)
      (+ pos 3))
    (t
      (let* ((ieee-exponent (ldb (byte 11 52) bits))
             (ieee-mantissa (ldb (byte 52 0) bits))
             (e2 (- ieee-exponent 1075))
             (m2 (logior ieee-mantissa #x10000000000000))
             (m10 0)
             (e10 0))
        (declare (type (signed-byte 32) e2))
        (declare (type (signed-byte 64) m10))
        (declare (type (signed-byte 32) e10))
        (cond
          ((zerop e2) (setf m10 m2))
          ((and (>= e2 -52) (<= e2 0) (zerop (logand (%<<64 m2 e2) e2)))
            (setf m10 (%>>64 m2 (- e2))))
          (t
            (let ((e10-corr 0)
                  (e2-corr 0)
                  (cbl-corr 2))
              (cond
                ((zerop ieee-exponent)
                  (setf e2 -1074)
                  (setf m2 ieee-mantissa)
                  (when (< ieee-mantissa 3)
                    (setf m2 (* m2 10))
                    (setf e10-corr 1)))
                ((= ieee-exponent #x7FF) (error "IllegalNumberError"))
                ((and (zerop ieee-mantissa) (> ieee-exponent 1))
                  (setf e2-corr 131007)
                  (setf cbl-corr 1)))

              (setf e10 (%>>64 (- (* e2 315653) e2-corr) 20))

              (let* ((i (%<<32 (+ e10 324) 1))
                     (g1 (aref gs (+ i 0)))
                     (g0 (aref gs (+ i 1)))
                     (h (%int32 (+ (%>>32 (* (- e10) 108853) 15) e2 2)))
                     (cb (%<<64 m2 2))
                     (vb-corr (- (logand m2 #x01) 1))
                     (vb (%rop3 g1 g0 (%<<64 cb h)))
                     (vbl (%int64 (+ (%rop3 g1 g0 (%<<64 (- cb cbl-corr) h)) vb-corr)))
                     (vbr (%int64 (- (%rop3 g1 g0 (%<<64 (+ cb 2) h))        vb-corr))))
                (when (or (< vb 400)
                          (progn
                            ;; divide a positive int by 40
                            (setf m10 (%multiply-high vb 461168601842738792))
                            (let* ((vb40 (%int32 (* m10 40)))
                                   (diff (%int32 (- vbl vb40))))
                              (or (>= (logxor (+ (%int32 (- vb40 vbr)) 40) diff) 0)
                                  (progn
                                    (setf m10 (%int64 (+ m10 (%>>>32 (lognot diff) 31))))
                                    (setf e10 (%int32 (+ e10 1)))
                                    nil)))))
                  (setf m10 (%>>64 vb 2))
                  (let* ((vb4 (logand vb #xFFFFFFFFFFFFFFFC))
                         (diff (%int32 (- vbl vb4))))
                    (setf m10 (+ m10 (%>>>32
                                       (lognot
                                         (if (< (logxor (+ (%int32 (- vb4 vbr)) 4) diff) 0)
                                           diff
                                           (+ (logand vb #x3) (logand m10 #x1) -3)))
                                       31)))
                    (setf e10 (- e10 e10-corr))))))))
        (let ((len (%digit-count m10)))
          (setf e10 (%int32 (+ e10 len -1)))
          (cond
            ((or (< e10 -3) (>= e10 7))
              (let ((last-pos (%write-significant-fraction-digits64 m10 (+ pos len) pos buf ds)))
                (setf (char buf pos) (char buf (+ pos 1)))
                (setf (char buf (+ pos 1)) #\.)
                (setf pos
                  (cond
                    ((< last-pos (+ pos 3))
                      (setf (char buf last-pos) #\0)
                      (1+ last-pos))
                    (t last-pos)))
                (setf (char buf (+ pos 0)) #\e)
                (setf (char buf (+ pos 1)) #\-)
                (incf pos)
                (when (< e10 0)
                  (setf e10 (- e10))
                  (incf pos))
                (cond
                  ((< e10 10)
                    (setf (char buf pos) (code-char (+ e10 (char-code #\0))))
                    (+ pos 1))
                  ((< e10 100)
                    (%write-2-digits e10 pos buf ds))
                  (t (%write-3-digits e10 pos buf ds)))))
            ((< e10 0)
              (let ((dot-pos (+ pos 1)))
                (setf (char buf (+ pos 0)) #\0)
                (setf (char buf (+ pos 2)) #\0)
                (setf (char buf (+ pos 3)) #\0)
                (decf pos e10)
                (let ((last-pos (%write-significant-fraction-digits64 m10 (+ pos len) pos buf ds)))
                  (setf (char buf dot-pos) #\.)
                  last-pos)))
            ((< e10 (- len 1))
              (let ((last-pos (%write-significant-fraction-digits64 m10 (+ pos len) pos buf ds)))
                (loop :repeat (1+ e10)
                      :do (setf (char buf pos) (char buf (+ pos 1)))
                          (incf pos))
                (setf (char buf pos) #\.)
                last-pos))
            (t
              (incf pos len)
              (%write-positive-int-digits (%int32 m10) pos buf ds)
              (setf (char buf (+ pos 0)) #\.)
              (setf (char buf (+ pos 1)) #\0)
              (+ pos 2))))))))

(defun stringify-double (x &aux (buf (make-array 24 :element-type 'base-char)))
  (declare (dynamic-extent buf))
  (check-type x double-float)
  (let ((n (%write-double x buf)))
    (subseq buf 0 n)))

(defun write-double (x stream &aux (buf (make-array 24 :element-type 'base-char)))
  "Write the `double-float' `x' to `stream', which must be a character output stream."
  (declare (dynamic-extent buf))
  (check-type x double-float)
  (check-type stream stream)
  (let ((n (%write-double x buf)))
    (write-string buf stream :end n)))
