;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- SBCL implementation
;;;

(in-package :static-vectors)

(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (foreign-funcall "memset" :pointer pointer :int value :size length :pointer)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (foreign-funcall "memcpy" :pointer dst-ptr :pointer src-ptr :size length :pointer)
  dst-ptr)

;;; We have to handle all the low-level bits including setting the array header
;;; and keeping around the info about the original pointer returned by the
;;; foreign allocator.
;;;
;;; It goes like this:
;;;
;;; 1. Compute the data size for the Lisp-visible memory (that means an extra #\Nul
;;;    at the end for strings)
;;; 2. Sum the data size, the SBCL header size, and our extra header size to get
;;;    the total foreign size required
;;; 3. Adjust the total foreign size to the required alignment, compute the header offset
;;;    and write the headers.
;;;
;;; Array layout:
;;;
;;;    +-------------------+
;;;    | Allocated address | <-- Original pointer
;;;    +-------------------+
;;;    | Start gap ...     | <-- For large alignments, there's a gap between
;;;    |                   |     the data block and the headers.
;;;    +-------------------+
;;;    | SV header         | <-- The offset from the original pointer (DWORD)
;;;    +-------------------+
;;;    | Lisp array header | <-- Array element-type and size (DWORD)
;;;    +-------------------+
;;;    | Lisp array data   | <-- Lisp-visible data
;;;    +-------------------+
;;;
;;; There's no end gap because when a alignment is requested,
;;; the requested size must also be a multiple of the alignment.

(defconstant +array-header-size+
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(declaim (inline vector-widetag-and-n-bytes))
(defun vector-widetag-and-n-bytes (type)
  "Returns the widetag and octet size of the upgraded array element type
for a given type specifier."
  (let ((upgraded-type (upgraded-array-element-type type)))
    (case upgraded-type
      ((nil t) (error "~A is not a specializable array element type" type))
      (t
       #+#.(cl:if (cl:find-symbol "%VECTOR-WIDETAG-AND-N-BITS" "SB-IMPL")
                  '(and) '(or))
       (sb-impl::%vector-widetag-and-n-bits type)
       #+#.(cl:if (cl:find-symbol "%VECTOR-WIDETAG-AND-N-BITS-SHIFT" "SB-IMPL")
                  '(and) '(or))
       (multiple-value-bind (widetag shift)
           (sb-impl::%vector-widetag-and-n-bits-shift type)
         (values widetag (expt 2 (- shift 3))))))))

(declaim (inline align))
(defun align (size boundary)
  (* boundary
     (ceiling size boundary)))

(declaim (inline %memalign))
(defun %memalign (size alignment)
  #+unix
  (with-foreign-object (box :pointer)
    (let ((errno (foreign-funcall "posix_memalign"
                                  :pointer box
                                  :size alignment
                                  :size size
                                  :int)))
      (when (not (zerop errno))
        (error "posix_memalign() returned error ~A" errno))
      (mem-ref box :pointer)))
  #-unix
  (progn
    (assert (= alignment 16))
    (foreign-alloc :char :count size)))

(defun %allocate-static-vector (length element-type alignment)
  (declare (type (unsigned-byte 16) alignment))
  (flet ((allocation-sizes (length widetag n-bytes)
           (values
            ;; We're allocating two headers: one for SBCL and
            ;; the other one for our bookkeeping.
            (align (* 2 +array-header-size+) alignment)
            ;; Align data size.
            (align
             (* (if (= widetag sb-vm:simple-character-string-widetag)
                    (1+ length)         ; for the final #\Nul
                    length)
                n-bytes)
             alignment))))
    (multiple-value-bind (widetag n-bytes)
        (vector-widetag-and-n-bytes element-type)
      (multiple-value-bind (header-size data-size)
          (allocation-sizes length widetag n-bytes)
        (let* ((total-size (+ header-size data-size))
               (foreign-block (%memalign total-size alignment))
               (data-offset header-size )
               (lisp-header-offset
                 (- data-offset +array-header-size+))
               (lisp-header-pointer
                 (inc-pointer foreign-block lisp-header-offset))
               (extra-header-offset
                 (- data-offset (* 2 +array-header-size+)))
               (extra-header-pointer
                 (inc-pointer foreign-block extra-header-offset)))
          ;; Write Lisp header: tag and length
          (setf (sb-sys:sap-ref-word lisp-header-pointer 0) widetag)
          (setf (sb-sys:sap-ref-word lisp-header-pointer sb-vm:n-word-bytes)
                (sb-vm:fixnumize length))
          ;; Save the relative position from the start of the foreign block
          (setf (sb-sys:sap-ref-word extra-header-pointer 0)
                (- data-offset (* 2 +array-header-size+)))
          ;; Instantiate Lisp object
          (sb-kernel:%make-lisp-obj (logior (pointer-address lisp-header-pointer)
                                            sb-vm:other-pointer-lowtag)))))))

(declaim (inline static-vector-address))
(defun static-vector-address (vector)
  "Return a foreign pointer to start of the Lisp VECTOR(including its header).
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (logandc2 (sb-kernel:get-lisp-obj-address vector)
            sb-vm:lowtag-mask))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (make-pointer (+ (static-vector-address vector)
                   +array-header-size+
                   offset)))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((extra-header-pointer
           (make-pointer (- (static-vector-address vector) +array-header-size+)))
         (start-offset
           (sb-sys:sap-ref-word extra-header-pointer 0)))
    (foreign-free (inc-pointer extra-header-pointer (- start-offset))))
  (values))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type ''(unsigned-byte 8))
                                 initial-contents initial-element)
                              &body body &environment env)
  "Bind PTR-VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignorable element-type initial-contents initial-element))
  (multiple-value-bind (real-element-type length type-spec)
      (canonicalize-args env element-type length)
    (let ((args (copy-list args)))
      (remf args :element-type)
      `(sb-sys:without-interrupts
         (let ((,var (make-static-vector ,length ,@args
                                         :element-type ,real-element-type)))
           (declare (type ,type-spec ,var))
           (unwind-protect
                (sb-sys:with-local-interrupts ,@body)
             (when ,var (free-static-vector ,var))))))))
