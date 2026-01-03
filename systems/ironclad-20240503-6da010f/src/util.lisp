;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; util.lisp -- functions that come in handy in crypto applications

(in-package :crypto)

(declaim (inline byte-array-to-hex-string
                 hex-string-to-byte-array
                 ascii-string-to-byte-array))

(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector)
           (type fixnum start)
           (type (or null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length vector)))
         (length (- end start))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (ecase element-type
                          ;; so that the compiler optimization can jump in
                          (base-char (make-string (* length 2)
                                                  :element-type 'base-char))
                          (character (make-string (* length 2)
                                                  :element-type 'character)))
       for i from start below end
       for j from 0 below (* length 2) by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref hexdigits (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref hexdigits (ldb (byte 4 0) byte))))
       finally (return string))))

(defun hex-string-to-byte-array (string &key (start 0) (end nil))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (declare (type string string))
  (let* ((end (or end (length string)))
         (length (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
    (flet ((char-to-digit (char)
             (or (position char "0123456789abcdef" :test #'char-equal)
                 (error 'ironclad-error
                        :format-control "~A is not a hex digit"
                        :format-arguments (list char)))))
      (loop for i from 0
            for j from start below end by 2
            do (setf (aref key i)
                     (+ (* (char-to-digit (char string j)) 16)
                        (char-to-digit (char string (1+ j)))))
         finally (return key)))))

(defun ascii-string-to-byte-array (string &key (start 0) end)
  "Convert STRING to a (VECTOR (UNSIGNED-BYTE 8)).  It is an error if
STRING contains any character whose CHAR-CODE is greater than 255."
  (declare (type string string)
           (type fixnum start)
           (type (or null fixnum) end)
           (optimize (speed 3) (safety 1)))
  (let* ((length (length string))
         (vec (make-array length :element-type '(unsigned-byte 8)))
         (end (or end length)))
    (loop for i from start below end do
          (let ((byte (char-code (char string i))))
            (unless (< byte 256)
              (error 'ironclad-error
                     :format-control "~A is not an ASCII character"
                     :format-arguments (list (char string i))))
            (setf (aref vec i) byte))
          finally (return vec))))

(declaim (notinline byte-array-to-hex-string
                    hex-string-to-byte-array
                    ascii-string-to-byte-array))

(defun constant-time-equal (data1 data2)
  "Returns T if the elements in DATA1 and DATA2 are identical, NIL otherwise.
All the elements of DATA1 and DATA2 are compared to prevent timing attacks."
  (declare (type (simple-array (unsigned-byte 8) (*)) data1 data2)
           (optimize (speed 3)))
  (let ((res (if (= (length data1) (length data2)) 0 1)))
    (declare (type (unsigned-byte 8) res))
    (loop for d1 across data1
          for d2 across data2
          do (setf res (logior res (logxor d1 d2))))
    (zerop res)))
