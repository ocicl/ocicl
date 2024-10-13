;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defun read-number-from-buffer (buffer &key (start 0) end (radix 10))
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (declare (type (integer 2 36) radix))
  (let ((end (or (position-if #'(lambda (b)
                                  ;; For BSD tar, a number can end with
                                  ;; a space or a null byte.
                                  (or (= b +ascii-space+) (zerop b)))
                              buffer :start start :end end)
                 end
                 (length buffer))))
    ;; GNU tar permits storing numbers as binary; a binary number is
    ;; indicated by starting the field with #x80.
    (if (= (aref buffer start) #x80)
        (loop for i from (1- end) downto (1+ start)
              for base = 1 then (* base 256)
              sum (* (aref buffer i) base))
        (loop for i from (1- end) downto start
              for base = 1 then (* base radix)
              sum (let ((byte (aref buffer i)))
                    (cond
                      ((<= +ascii-zero+ byte +ascii-nine+)
                       (* base (- byte +ascii-zero+)))
                      ((<= +ascii-a+ byte +ascii-z+)
                       (* base (+ 10 (- byte +ascii-a+))))
                      (t (error "Invalid byte: ~A in ~A"
                                byte (subseq buffer start end)))))))))

(defun write-number-to-buffer (number buffer
                               &key (start 0) end (radix 10) nullp)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (declare (type (integer 2 36) radix))
  (let ((end (let ((dend (or end (length buffer))))
               (if nullp
                   (1- dend)
                   dend))))
    (loop for i from (1- end) downto start
          do (multiple-value-bind (quo rem) (truncate number radix)
               (setf number quo)
               (setf (aref buffer i)
                     (cond
                       ((<= 0 rem 9) (+ rem +ascii-zero+))
                       ((<= 10 rem 36) (+ (- rem 10) +ascii-a+))
                       (t (error "Don't know how to encode ~A" rem))))))
    (values)))

(defun read-bytevec-from-buffer (buffer &key (start 0) end nullp)
  (let ((end (if nullp
                 (or (position 0 buffer :start start :end end) end)
                 end)))
    (subseq buffer start end)))
