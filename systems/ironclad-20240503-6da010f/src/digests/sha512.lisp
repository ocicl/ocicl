;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;; sha512.lisp -- implementation of SHA-384/512 from NIST

(in-package :crypto)
(in-ironclad-readtable)

(define-digest-registers (sha384 :endian :big :size 8 :digest-registers 6)
  (a #xCBBB9D5DC1059ED8)
  (b #x629A292A367CD507)
  (c #x9159015A3070DD17)
  (d #x152FECD8F70E5939)
  (e #x67332667FFC00B31)
  (f #x8EB44A8768581511)
  (g #xDB0C2E0D64F98FA7)
  (h #x47B5481DBEFA4FA4))

(defconst +pristine-sha384-registers+ (initial-sha384-regs))

(define-digest-registers (sha512 :endian :big :size 8)
  (a #x6A09E667F3BCC908)
  (b #xBB67AE8584CAA73B)
  (c #x3C6EF372FE94F82B)
  (d #xA54FF53A5F1D36F1)
  (e #x510E527FADE682D1)
  (f #x9B05688C2B3E6C1F)
  (g #x1F83D9ABFB41BD6B)
  (h #x5BE0CD19137E2179))

(defconst +pristine-sha512-registers+ (initial-sha512-regs))

(defconst +sha512-round-constants+
#64@(#x428A2F98D728AE22 #x7137449123EF65CD #xB5C0FBCFEC4D3B2F #xE9B5DBA58189DBBC
#x3956C25BF348B538 #x59F111F1B605D019 #x923F82A4AF194F9B #xAB1C5ED5DA6D8118
#xD807AA98A3030242 #x12835B0145706FBE #x243185BE4EE4B28C #x550C7DC3D5FFB4E2
#x72BE5D74F27B896F #x80DEB1FE3B1696B1 #x9BDC06A725C71235 #xC19BF174CF692694
#xE49B69C19EF14AD2 #xEFBE4786384F25E3 #x0FC19DC68B8CD5B5 #x240CA1CC77AC9C65
#x2DE92C6F592B0275 #x4A7484AA6EA6E483 #x5CB0A9DCBD41FBD4 #x76F988DA831153B5
#x983E5152EE66DFAB #xA831C66D2DB43210 #xB00327C898FB213F #xBF597FC7BEEF0EE4
#xC6E00BF33DA88FC2 #xD5A79147930AA725 #x06CA6351E003826F #x142929670A0E6E70
#x27B70A8546D22FFC #x2E1B21385C26C926 #x4D2C6DFC5AC42AED #x53380D139D95B3DF
#x650A73548BAF63DE #x766A0ABB3C77B2A8 #x81C2C92E47EDAEE6 #x92722C851482353B
#xA2BFE8A14CF10364 #xA81A664BBC423001 #xC24B8B70D0F89791 #xC76C51A30654BE30
#xD192E819D6EF5218 #xD69906245565A910 #xF40E35855771202A #x106AA07032BBD1B8
#x19A4C116B8D2D0C8 #x1E376C085141AB53 #x2748774CDF8EEB99 #x34B0BCB5E19B48A8
#x391C0CB3C5C95A63 #x4ED8AA4AE3418ACB #x5B9CCA4F7763E373 #x682E6FF3D6B2B8A3
#x748F82EE5DEFB2FC #x78A5636F43172F60 #x84C87814A1F0AB72 #x8CC702081A6439EC
#x90BEFFFA23631E28 #xA4506CEBDE82BDE9 #xBEF9A3F7B2C67915 #xC67178F2E372532B
#xCA273ECEEA26619C #xD186B8C721C0C207 #xEADA7DD6CDE0EB1E #xF57D4F7FEE6ED178
#x06F067AA72176FBA #x0A637DC5A2C898A6 #x113F9804BEF90DAE #x1B710B35131C471B
#x28DB77F523047D84 #x32CAAB7B40C72493 #x3C9EBE0A15C9BEBC #x431D67C49C100D4C
#x4CC5D4BECB3E42B6 #x597F299CFC657E2A #x5FCB6FAB3AD6FAEC #x6C44198C4A475817))

(defun update-sha512-block (regs block)
  (declare (type sha512-regs regs))
  (declare (type (simple-array (unsigned-byte 64) (80)) block)
           #.(burn-baby-burn))
  (let ((a (sha512-regs-a regs)) (b (sha512-regs-b regs))
        (c (sha512-regs-c regs)) (d (sha512-regs-d regs))
        (e (sha512-regs-e regs)) (f (sha512-regs-f regs))
        (g (sha512-regs-g regs)) (h (sha512-regs-h regs)))
    (flet ((rho (x r1 r2 r3)
             (logxor (ror64 x r1) (ror64 x r2) (ror64 x r3))))
      ;; FIXME: Implement inline 64-bit rotates for x86-64 SBCL.
      ;; #+ironclad-fast-mod64-arithmetic
      ;; (declare (inline rho))
      (macrolet ((sha512-round (i a b c d e f g h)
                   `(let ((x (mod64+ (rho ,e 14 18 41)
                                     (mod64+ (logxor (logand ,e ,f)
                                                     (logandc1 ,e ,g))
                                             (mod64+ (aref block ,i)
                                                     (aref +sha512-round-constants+ ,i))))))
                      (setf ,d (mod64+ ,d (mod64+ ,h x))
                            ,h (mod64+ ,h
                                       (mod64+ x (mod64+ (rho ,a 28 34 39)
                                                         (logxor (logand ,a ,b)
                                                                 (logand ,a ,c)
                                                                 (logand ,b ,c)))))))))
        #.(let ((xvars (make-circular-list 'a 'b 'c 'd 'e 'f 'g 'h)))
            (loop for i from 0 below 80
                  for vars on xvars by #'(lambda (x) (nthcdr 7 x))
                 collect `(sha512-round ,i ,@(circular-list-subseq vars 0 8)) into forms
                 finally (return `(progn ,@forms))))
        #.(loop for slot in '(a b c d e f g h)
                collect (let ((regs-accessor (symbolicate '#:sha512-regs- slot)))
                          `(setf (,regs-accessor regs)
                            (mod64+ (,regs-accessor regs) ,slot))) into forms
                finally (return `(progn ,@forms)))))))

(defun sha512-expand-block (block)
  (declare (type (simple-array (unsigned-byte 64) (80)) block)
           #.(burn-baby-burn))
  (flet ((sigma (x r1 r2 r3)
           (logxor (ror64 x r1) (ror64 x r2) (ash x (- r3)))))
    #+ironclad-fast-mod64-arithmetic (declare (inline sigma))
    (loop for i from 16 below 80 do
         (setf (aref block i)
               (mod64+ (sigma (aref block (- i 2)) 19 61 6)
                       (mod64+ (aref block (- i 7))
                               (mod64+ (sigma (aref block (- i 15)) 1 8 7)
                                       (aref block (- i 16)))))))
    (values)))


;;; mid-level

(defstruct (sha512
             (:constructor %make-sha512-digest
              (&aux (buffer (make-array 128 :element-type '(unsigned-byte 8)))))
             (:copier nil)
             (:include mdx))
  (regs (initial-sha512-regs) :type sha512-regs :read-only t)
  (block (make-array 80 :element-type '(unsigned-byte 64)) :read-only t
         :type (simple-array (unsigned-byte 64) (80))))

(defstruct (sha384
             (:include sha512)
             (:constructor %make-sha384-digest
              (&aux (regs (initial-sha384-regs))
                    (buffer (make-array 128 :element-type '(unsigned-byte 8)))))
             (:copier nil))
  ;; No slots.
  )

(defmethod reinitialize-instance ((state sha512) &rest initargs)
  (declare (ignore initargs))
  ;; Some versions of Clozure CCL have a bug where the elements of
  ;; +PRISTINE-SHA512-REGISTERS+ are considered to be negative.  Force
  ;; the compiler to see them as positive.
  #+ccl
  (let ((regs (sha512-regs state)))
    (dotimes (i (length +pristine-sha512-registers+))
      (setf (aref regs i) (ldb (byte 64 0) (aref +pristine-sha512-registers+ i)))))
  #-ccl
  (replace (sha512-regs state) +pristine-sha512-registers+)
  (setf (sha512-amount state) 0
        (sha512-buffer-index state) 0)
  state)

(defmethod reinitialize-instance ((state sha384) &rest initargs)
  (declare (ignore initargs))
  ;; Some versions of Clozure CCL have a bug where the elements of
  ;; +PRISTINE-SHA384-REGISTERS+ are considered to be negative.  Force
  ;; the compiler to see them as positive.
  #+ccl
  (let ((regs (sha384-regs state)))
    (dotimes (i (length +pristine-sha384-registers+))
      (setf (aref regs i) (ldb (byte 64 0) (aref +pristine-sha384-registers+ i)))))
  #-ccl
  (replace (sha384-regs state) +pristine-sha384-registers+)
  (setf (sha384-amount state) 0
        (sha384-buffer-index state) 0)
  state)

(defmethod copy-digest ((state sha512) &optional copy)
  (check-type copy (or null sha512))
  (let ((copy (if copy
                  copy
                  (etypecase state
                    (sha384 (%make-sha384-digest))
                    (sha512 (%make-sha512-digest))))))
    (declare (type sha512 copy))
    (replace (sha512-regs copy) (sha512-regs state))
    (replace (sha512-buffer copy) (sha512-buffer state))
    (setf (sha512-amount copy) (sha512-amount state)
          (sha512-buffer-index copy) (sha512-buffer-index state))
    copy))

(define-digest-updater sha512
  (flet ((compress (state sequence offset)
           (let ((block (sha512-block state)))
             (fill-block-ub8-be/64 block sequence offset)
             (sha512-expand-block block)
             (update-sha512-block (sha512-regs state) block))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer ((sha512 64) (sha384 48))
  (let ((regs (sha512-regs state))
        (block (sha512-block state))
        (buffer (sha512-buffer state))
        (buffer-index (sha512-buffer-index state))
        (total-length (* 8 (sha512-amount state))))
    (declare (type sha512-regs regs)
             (type (integer 0 127) buffer-index)
             (type (simple-array (unsigned-byte 64) (80)) block)
             (type (simple-array (unsigned-byte 8) (128)) buffer))
    (setf (aref buffer buffer-index) #x80)
    (when (> buffer-index 111)
      (loop for index of-type (integer 0 128)
         from (1+ buffer-index) below 128
         do (setf (aref buffer index) #x00))
      (fill-block-ub8-be/64 block buffer 0)
      (sha512-expand-block block)
      (update-sha512-block regs block)
      (loop for index of-type (integer 0 16)
         from 0 below 16
         do (setf (aref block index) #x00000000)))
    (when (<= buffer-index 111)
      (loop for index of-type (integer 0 128)
         from (1+ buffer-index) below 128
         do (setf (aref buffer index) #x00))
      ;; copy the data to BLOCK prematurely
      (fill-block-ub8-be/64 block buffer 0))
    ;; fill in the remaining block data
    (setf (aref block 15) total-length)
    (sha512-expand-block block)
    (update-sha512-block regs block)
    (finalize-registers state regs)))

(defdigest sha512 :digest-length 64 :block-length 128)
(defdigest sha384 :digest-length 48 :block-length 128)
