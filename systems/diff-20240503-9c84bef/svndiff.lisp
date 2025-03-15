;;;; svndiff.lisp - encoding copy/insert deltas in svndiff format
(in-package :diff)

(declaim (optimize (debug 3)))

(defun instruction-buffer-length (instlist)
  "Compute the length of the instruction buffer for a window containing
the instructions in INSTLIST."
  (reduce #'+ instlist :initial-value 0 :key #'instruction-length))

(defun new-data-buffer-length (instlist)
  "Compute the length of the new data buffer for a window containing
the instructions in INSTLIST."
  (reduce #'+ instlist :initial-value 0
                       :key #'(lambda (op)
                                (ecase (svndiff-op-kind op)
                                  (:new-data (svndiff-op-bytes op))
                                  ((:copy-source :copy-target) 0)))))

(defun write-svndiff-op (op target instruction-stream data-stream)
  (declare (type bytebuf target))
  (let ((kind (svndiff-op-kind op))
        (offset (svndiff-op-offset op))
        (bytes (svndiff-op-bytes op)))
    (flet ((encode-copy (insn-byte)
             (when (< bytes 64)
               (setf insn-byte (logior insn-byte bytes)))
             (write-byte insn-byte instruction-stream)
             (when (>= bytes 64)
               (write-svndiff-integer bytes instruction-stream))
             (write-svndiff-integer offset instruction-stream)))
      (ecase kind
        (:copy-target (encode-copy #x40))
        (:copy-source (encode-copy #x00))
        (:new-data
         (let ((insn-byte #x80))        ; first two bits `10'
           (when (< bytes 64)
             (setf insn-byte (logior insn-byte bytes)))
           (write-byte insn-byte instruction-stream)
           (when (>= bytes 64)
             (write-svndiff-integer bytes instruction-stream))
           (dotimes (i bytes)
             (write-byte (aref target (+ i offset))
                         data-stream))))))))

(defun construct-svndiff-window (source-offset source-len target-len
                                               ops target)
  (declare (type bytebuf target))
  (let* ((ops-buf-length (instruction-buffer-length ops))
         (data-buf-length (new-data-buffer-length ops))
         (ops-buffer (make-array ops-buf-length
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0))
         (data-buffer (make-array data-buf-length
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0))
         (ops-stream (make-instance 'byte-buffer-stream :buffer ops-buffer))
         (data-stream (make-instance 'byte-buffer-stream :buffer data-buffer)))
    (declare (type bytebuf ops-buffer data-buffer))
    (dolist (op ops)
      (write-svndiff-op op target ops-stream data-stream))
    (make-svndiff-window :source-offset source-offset
                         :source-len source-len
                         :target-len target-len
                         :ops ops-buffer
                         :new-data data-buffer)))

(defun svndiff-integer-length (num)
  (if (zerop num)
      1
      (nth-value 0 (ceiling (integer-length num) 7))))

(defun instruction-length (op)
  "Compute the number of bytes needed to represent OP when it is svndiff
encoded."
  (let ((kind (svndiff-op-kind op))
        (offset (svndiff-op-offset op))
        (bytes (svndiff-op-bytes op)))
    (ecase kind
      ((:copy-source :copy-target)
       (if (< bytes 64)
           (1+ (svndiff-integer-length offset))
           (+ 1
              (svndiff-integer-length offset)
              (svndiff-integer-length bytes))))
      (:new-data
       (if (< bytes 64)
           1
           (1+ (svndiff-integer-length bytes)))))))

(defun read-svndiff-integer (stream)
  "Reads a svndiff-encoded integer from STREAM."
  (let ((int 0)
        (byte (read-byte stream))
        (data-byte-spec (byte 7 0)))
    (setf int (ldb data-byte-spec byte))
    (loop while (> byte 127)
          do (setf byte (read-byte stream)
                   int (logior (ash int 7)
                               (ldb data-byte-spec byte))))
    int))

(defun write-svndiff-integer (integer stream)
  "Writes the non-negative INTEGER to STREAM using svndiff encoding."
  (when (zerop integer)
    (write-byte #x00 stream)
    (return-from write-svndiff-integer (values)))
  (let ((blocks (svndiff-integer-length integer)))
    (loop for i downfrom blocks above 0
          do (let ((part (ldb (byte 7 (* 7 (1- i))) integer)))
               (assert (< part 128))
               (unless (= i 1)
                 (setf part (logior #x80 part)))
               (write-byte part stream)))
    (values)))

(defun read-svndiff-window (stream)
  "Reads a svndiff window from STREAM."
  (let ((source-offset (read-svndiff-integer stream))
        (source-len (read-svndiff-integer stream))
        (target-len (read-svndiff-integer stream))
        (instrs-len (read-svndiff-integer stream))
        (new-data-len (read-svndiff-integer stream)))
    (let ((instr-bytes (make-array instrs-len
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0))
          (new-data (make-array new-data-len
                                :element-type '(unsigned-byte 8)
                                :initial-element 0))
          (bytes-read 0))
      (declare (type bytebuf instr-bytes new-data))
      (setf bytes-read (read-sequence instr-bytes stream))
      (unless (= bytes-read instrs-len)
        (error "Could not read instructions for svndiff window."))
      (setf bytes-read (read-sequence new-data stream))
      (unless (= bytes-read new-data-len)
        (error "Could not read new data for svndiff window."))
      (make-svndiff-window :source-offset source-offset
                           :source-len source-len
                           :target-len target-len
                           :ops instr-bytes
                           :new-data new-data))))

(defun write-svndiff-window (window stream)
  "Writes the svndiff window WINDOW to STREAM."
  (let ((source-offset (svndiff-window-source-offset window))
        (source-len (svndiff-window-source-len window))
        (target-len (svndiff-window-target-len window))
        (instr-bytes (svndiff-window-ops window))
        (new-data (svndiff-window-new-data window)))
    (declare (type (simple-array (unsigned-byte 8) (*)) instr-bytes new-data))
    (write-svndiff-integer source-offset stream)
    (write-svndiff-integer source-len stream)
    (write-svndiff-integer target-len stream)
    (write-svndiff-integer (length instr-bytes) stream)
    (write-svndiff-integer (length new-data) stream)
    (write-sequence instr-bytes stream)
    (write-sequence new-data stream)
    (values)))

(defun write-svndiff (windows stream)
  "Writes a svndiff document to STREAM using the information in WINDOWS,
which is a list of SVNDIFF-WINDOWs."
  ;; "SVN\0"
  (write-byte #x83 stream)
  (write-byte #x86 stream)
  (write-byte #x78 stream)
  (write-byte #x00 stream)
  (dolist (window windows)
    (write-svndiff-window window stream)))


;;; high level driver for the whole shebang

(defun compare-files (source-filename target-filename)
  "Return a list of svndiff windows describing the differences between
SOURCE-FILENAME and TARGET-FILENAME."
  (with-binary-file (source-stream source-filename :input)
    (with-binary-file (target-stream target-filename :input)
      (let ((context (make-instance 'vdelta-context))
            (windows nil))
        (do* ((buffer (buffer context))
              (source-offset 0)
              (source-length (read-sequence buffer source-stream
                                            :start 0 :end +buffer-size+)
                             (read-sequence buffer source-stream
                                            :start 0 :end +buffer-size+))
              (target-length (read-sequence buffer target-stream
                                            :start source-length)
                             (read-sequence buffer target-stream
                                            :start source-length)))
             ((zerop target-length) (nreverse windows))
          (clrhash (table context))
          (setf (source-length context) source-length
                (target-start context) source-length
                (target-length context) (- target-length source-length))
          (initialize-match-table context)
          (let* ((ops (calculate-svndiff-ops context)))
            (push ops windows))
          (incf source-offset source-length))))))

#|
;;; vcdiff encoding stuff

;;; managing the cache.  these algorithms are taken directly from the RFC,
;;; with small adjustments from the C-style used therein
(defclass address-cache ()
  ((near-cache :initarg :near-cache :reader near-cache)
   (next-near-slot :initform 0 :reader next-near-slot)
   (same-cache :initarg :same-cache :reader same-cache)))

(defun make-address-cache (near-size same-size)
  (let ((near-cache (make-array near-size :initial-element 0))
        (same-cache (make-array (* 256 same-size :initial-element 0))))
    (make-instance 'address-cache
                   :near-cache near-cache :same-cache same-cache)))

(defun update-cache (address-cache address)
  (let ((near-cache-length (length (near-cache address-cache)))
        (same-cache-length (length (same-cache address-cache))))
    (when (plusp near-cache-length)
      (setf (aref (near-cache address-cache) (next-near-slot address-cache))
            address)
      (setf (next-near-slot address-cache)
            (truncate (1+ (next-near-slot address-cache)) near-cache-length)))
    (when (plusp same-cache-length)
      (setf (aref (same-cache address-cache)
                  (nth-value 1 (truncate address same-cache-length)))
            address))
    (values)))

;;; from the RFC: "attempt to find the address mode that yields the
;;; smallest integer value for the encoded address value, thereby
;;; minimizing the encoded size of the address.  the RFC goes on to
;;; note that this best address value is "local" and suggests that more
;;; complex schemes might be able to do better.
(defun encode-address (address-cache address here-p)
  (let ((best-encoded-address address)
        ;; I think when the RFC uses `here', it means `target', and
        ;; when it uses `self', it means `source'.  check this to
        ;; make sure our intuition is right.  why don't they use
        ;; self-descriptive terms?  and can't we already figure this
        ;; out in an earlier phase?
        (best-mode (if here-p :vcd-here :vcd-self)))
    (dotimes (i (length (near-cache address-cache)))
      (let ((d (- address (aref (near-cache address-cache) i))))
        (when (and (not (minusp d)) (< d best-encoded-address))
          (setf best-encoded-address d
                ;; ugh
                best-mode (+ i 2)))))
    (let ((d (nth-value 1 (truncate address
                                    (* 256 (length (same-cache address-cache)))))))
      (when (and (plusp (length (same-cache address-cache)))
                 (= addr
                    (aref (same-cache address-cache) d)))
        (setf best-encoded-address d
              best-mode (+ (length (near-cache address-cache)) 2 (truncate d 256))))
      (update-cache address-cache address)
      (values best-encoded-address best-mode))))

(defun decode-address (address-cache address mode)
  (let ((decoded-address 0))
    (cond
      ((eq mode :vcd-self) (setf decoded-address
                                 (fetch-copy-address-integer #|wherever|#)))
      ((eq mode :vcd-here) (setf decoded-address
                                 (- address (fetch-copy-address-integer #|wherever|#))))
      ((=
        )))))
|#