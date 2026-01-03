;;; utils.lisp --- Utility functions for pure-tls
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>

(in-package #:pure-tls)

;;;; Type Definitions

(deftype octet ()
  "An unsigned 8-bit byte."
  '(unsigned-byte 8))

(deftype octet-vector (&optional size)
  "A simple array of octets."
  `(simple-array octet (,size)))

;;;; Byte Vector Construction

(defun make-octet-vector (size &key (initial-element 0))
  "Create a new octet vector of the specified SIZE."
  (make-array size :element-type 'octet :initial-element initial-element))

(defun octet-vector (&rest octets)
  "Create an octet vector from the given OCTETS."
  (make-array (length octets)
              :element-type 'octet
              :initial-contents octets))

(defun concat-octet-vectors (&rest vectors)
  "Concatenate multiple octet vectors into a single vector."
  (let* ((total-length (reduce #'+ vectors :key #'length))
         (result (make-octet-vector total-length))
         (offset 0))
    (dolist (vec vectors result)
      (replace result vec :start1 offset)
      (incf offset (length vec)))))

;;;; Integer Encoding/Decoding (Big-Endian)

(defun encode-uint16 (value)
  "Encode a 16-bit unsigned integer as a 2-byte big-endian octet vector."
  (octet-vector (ldb (byte 8 8) value)
                (ldb (byte 8 0) value)))

(defun encode-uint24 (value)
  "Encode a 24-bit unsigned integer as a 3-byte big-endian octet vector."
  (octet-vector (ldb (byte 8 16) value)
                (ldb (byte 8 8) value)
                (ldb (byte 8 0) value)))

(defun encode-uint32 (value)
  "Encode a 32-bit unsigned integer as a 4-byte big-endian octet vector."
  (octet-vector (ldb (byte 8 24) value)
                (ldb (byte 8 16) value)
                (ldb (byte 8 8) value)
                (ldb (byte 8 0) value)))

(defun decode-uint16 (vector &optional (offset 0))
  "Decode a 16-bit unsigned integer from big-endian bytes."
  (logior (ash (aref vector offset) 8)
          (aref vector (1+ offset))))

(defun decode-uint24 (vector &optional (offset 0))
  "Decode a 24-bit unsigned integer from big-endian bytes."
  (logior (ash (aref vector offset) 16)
          (ash (aref vector (1+ offset)) 8)
          (aref vector (+ offset 2))))

(defun decode-uint32 (vector &optional (offset 0))
  "Decode a 32-bit unsigned integer from big-endian bytes."
  (logior (ash (aref vector offset) 24)
          (ash (aref vector (1+ offset)) 16)
          (ash (aref vector (+ offset 2)) 8)
          (aref vector (+ offset 3))))

;;;; Variable-Length Integer Encoding (TLS style)

(defun encode-length-prefixed (data prefix-bytes)
  "Encode DATA with a PREFIX-BYTES length prefix."
  (let ((len (length data)))
    (ecase prefix-bytes
      (1 (concat-octet-vectors (octet-vector len) data))
      (2 (concat-octet-vectors (encode-uint16 len) data))
      (3 (concat-octet-vectors (encode-uint24 len) data)))))

;;;; Buffer Reading Utilities

(defstruct (tls-buffer (:constructor make-tls-buffer (data)))
  "A buffer for reading TLS data with position tracking."
  (data (make-octet-vector 0) :type octet-vector)
  (position 0 :type fixnum))

(defun buffer-remaining (buffer)
  "Return the number of bytes remaining in BUFFER."
  (- (length (tls-buffer-data buffer))
     (tls-buffer-position buffer)))

(defun buffer-read-octet (buffer)
  "Read a single octet from BUFFER."
  (when (zerop (buffer-remaining buffer))
    (error 'tls-decode-error :message "Unexpected end of buffer"))
  (prog1 (aref (tls-buffer-data buffer) (tls-buffer-position buffer))
    (incf (tls-buffer-position buffer))))

(defun buffer-read-octets (buffer count)
  "Read COUNT octets from BUFFER as a new octet vector."
  (when (< (buffer-remaining buffer) count)
    (error 'tls-decode-error
           :message (format nil "Expected ~D bytes but only ~D remaining"
                            count (buffer-remaining buffer))))
  (let ((result (make-octet-vector count))
        (pos (tls-buffer-position buffer)))
    (replace result (tls-buffer-data buffer) :start2 pos :end2 (+ pos count))
    (incf (tls-buffer-position buffer) count)
    result))

(defun buffer-read-uint16 (buffer)
  "Read a 16-bit big-endian unsigned integer from BUFFER."
  (decode-uint16 (buffer-read-octets buffer 2)))

(defun buffer-read-uint24 (buffer)
  "Read a 24-bit big-endian unsigned integer from BUFFER."
  (decode-uint24 (buffer-read-octets buffer 3)))

(defun buffer-read-uint32 (buffer)
  "Read a 32-bit big-endian unsigned integer from BUFFER."
  (decode-uint32 (buffer-read-octets buffer 4)))

(defun buffer-read-vector8 (buffer)
  "Read a 1-byte length-prefixed vector from BUFFER."
  (let ((len (buffer-read-octet buffer)))
    (buffer-read-octets buffer len)))

(defun buffer-read-vector16 (buffer)
  "Read a 2-byte length-prefixed vector from BUFFER."
  (let ((len (buffer-read-uint16 buffer)))
    (buffer-read-octets buffer len)))

(defun buffer-read-vector24 (buffer)
  "Read a 3-byte length-prefixed vector from BUFFER."
  (let ((len (buffer-read-uint24 buffer)))
    (buffer-read-octets buffer len)))

;;;; Buffer Writing Utilities

(defstruct (tls-write-buffer (:constructor make-tls-write-buffer ()))
  "A growable buffer for writing TLS data."
  (chunks nil :type list))

(defun write-buffer-append (buffer data)
  "Append DATA (octet vector) to BUFFER."
  (push data (tls-write-buffer-chunks buffer)))

(defun write-buffer-append-octet (buffer octet)
  "Append a single OCTET to BUFFER."
  (write-buffer-append buffer (octet-vector octet)))

(defun write-buffer-append-uint16 (buffer value)
  "Append a 16-bit big-endian VALUE to BUFFER."
  (write-buffer-append buffer (encode-uint16 value)))

(defun write-buffer-append-uint24 (buffer value)
  "Append a 24-bit big-endian VALUE to BUFFER."
  (write-buffer-append buffer (encode-uint24 value)))

(defun write-buffer-append-uint32 (buffer value)
  "Append a 32-bit big-endian VALUE to BUFFER."
  (write-buffer-append buffer (encode-uint32 value)))

(defun write-buffer-append-vector8 (buffer data)
  "Append DATA with a 1-byte length prefix to BUFFER."
  (write-buffer-append-octet buffer (length data))
  (write-buffer-append buffer data))

(defun write-buffer-append-vector16 (buffer data)
  "Append DATA with a 2-byte length prefix to BUFFER."
  (write-buffer-append-uint16 buffer (length data))
  (write-buffer-append buffer data))

(defun write-buffer-append-vector24 (buffer data)
  "Append DATA with a 3-byte length prefix to BUFFER."
  (write-buffer-append-uint24 buffer (length data))
  (write-buffer-append buffer data))

(defun write-buffer-contents (buffer)
  "Return the complete contents of BUFFER as an octet vector."
  (apply #'concat-octet-vectors (reverse (tls-write-buffer-chunks buffer))))

;;;; Random Bytes

(defun random-bytes (count)
  "Generate COUNT cryptographically random bytes."
  (ironclad:random-data count))

;;;; Hex Encoding (for debugging)

(defun octets-to-hex (octets)
  "Convert an octet vector to a hexadecimal string."
  (with-output-to-string (s)
    (loop for octet across octets
          do (format s "~2,'0x" octet))))

(defun hex-to-octets (hex-string)
  "Convert a hexadecimal string to an octet vector."
  (let* ((len (/ (length hex-string) 2))
         (result (make-octet-vector len)))
    (loop for i from 0 below len
          for j from 0 by 2
          do (setf (aref result i)
                   (parse-integer hex-string :start j :end (+ j 2) :radix 16)))
    result))

;;;; Constant-Time Comparison
;;;
;;; Use Ironclad's constant-time-equal for side-channel resistance.
;;; This avoids early returns and compares all elements to prevent timing attacks.

(defun constant-time-equal (a b)
  "Compare two octet vectors in constant time to prevent timing attacks.
   Delegates to Ironclad's implementation for proper side-channel resistance."
  (ironclad:constant-time-equal a b))

;;;; Secret Zeroization
;;;
;;; Wipe sensitive data from memory to reduce exposure window.
;;; Note: In a GC'd runtime, this is best-effort - the GC may have
;;; already copied the data elsewhere. For highest security, consider
;;; keeping secrets in foreign memory that can be mlock'd and wiped.

(defun zeroize (vector)
  "Overwrite VECTOR with zeros to clear sensitive data.
   Returns VECTOR for convenience in cleanup chains.

   IMPORTANT: This is best-effort in a GC'd runtime. The data may have
   been copied by the GC before zeroization. For critical secrets,
   consider using foreign memory."
  (when vector
    (fill vector 0))
  vector)

(defmacro with-zeroized-vector ((var init-form) &body body)
  "Execute BODY with VAR bound to INIT-FORM, then zeroize VAR.
   Ensures zeroization even if BODY signals an error."
  `(let ((,var ,init-form))
     (unwind-protect
          (progn ,@body)
       (zeroize ,var))))

;;;; String Encoding

(defun string-to-octets (string)
  "Convert a string to an octet vector using UTF-8 encoding."
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun octets-to-string (octets)
  "Convert an octet vector to a string using UTF-8 encoding."
  (flexi-streams:octets-to-string octets :external-format :utf-8))

;;;; Environment Variables

(defun get-environment-variable (name)
  "Get an environment variable value, or NIL if not set.
   Portable across multiple Common Lisp implementations."
  #+sbcl (sb-ext:posix-getenv name)
  #+ccl (ccl:getenv name)
  #+ecl (ext:getenv name)
  #+clisp (ext:getenv name)
  #+allegro (sys:getenv name)
  #+lispworks (lispworks:environment-variable name)
  #+abcl (ext:getenv name)
  #+cmucl (cdr (assoc name ext:*environment-list* :test #'string=))
  #+clasp (ext:getenv name)
  #-(or sbcl ccl ecl clisp allegro lispworks abcl cmucl clasp)
  (error "get-environment-variable not implemented for this Lisp"))

;;;; XOR Operations

(defun xor-octets (a b)
  "XOR two octet vectors of the same length."
  (assert (= (length a) (length b)))
  (let ((result (make-octet-vector (length a))))
    (dotimes (i (length a) result)
      (setf (aref result i)
            (logxor (aref a i) (aref b i))))))

;;;; Integer/Octet Conversions

(defun octets-to-integer (octets)
  "Convert an octet vector to an integer (big-endian)."
  (let ((result 0))
    (loop for octet across octets
          do (setf result (logior (ash result 8) octet)))
    result))

(defun integer-to-octets (integer &optional (min-length 0))
  "Convert an integer to an octet vector (big-endian).
   If MIN-LENGTH is specified, pad with leading zeros if necessary."
  (if (zerop integer)
      (make-array (max 1 min-length) :element-type 'octet :initial-element 0)
      (let ((octets nil))
        (loop while (plusp integer)
              do (push (ldb (byte 8 0) integer) octets)
                 (setf integer (ash integer -8)))
        (let* ((octet-vec (coerce octets '(vector (unsigned-byte 8))))
               (result (make-array (max (length octet-vec) min-length)
                                   :element-type 'octet
                                   :initial-element 0)))
          (replace result octet-vec :start1 (- (length result) (length octet-vec)))
          result))))
