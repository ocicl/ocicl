;;;; This is part of tar-file. See README.org and LICENSE for more information.

(in-package #:tar-file)

(defparameter *default-header-encoding* :latin-1
  "The default encoding to use for strings read from/written to tar
headers. Must be recognized by Babel.")

(defun bytevec-to-string (bytevec &optional encoding)
  (babel:octets-to-string bytevec :encoding (or encoding *default-header-encoding*)))
(defun string-to-bytevec (string &optional encoding)
  (babel:string-to-octets string :encoding (or encoding *default-header-encoding*)))

(defun round-up-to-tar-block (num)
  (* (ceiling num +tar-n-block-bytes+) +tar-n-block-bytes+))

(defun tar-checksum-guts (header-type block start transform-fun)
  (declare (type (simple-array (unsigned-byte 8) (*)) block))
  (let* ((end (+ start +tar-n-block-bytes+))
         (checksum-offset (field-offset header-type 'checksum))
         (checksum-start (+ start checksum-offset))
         (checksum-end (+ start checksum-offset
                          (field-length header-type 'checksum))))
    (loop for i from start below end
          sum (if (or (< i checksum-start) (<= checksum-end i))
                  (funcall transform-fun (aref block i))
                  +ascii-space+))))

(defun compute-checksum-for-tar-header (header-type block start)
  (tar-checksum-guts header-type block start #'identity))

(defun compute-old-checksum-for-tar-header (header-type block start)
  (tar-checksum-guts header-type block start #'(lambda (b) (if (< b 128) b (- b 256)))))

(defun tar-block-checksum-matches-p (header-type block checksum start)
  (let ((sum (compute-checksum-for-tar-header header-type block start)))
    (if (= sum checksum)
        t
        ;; try the older, signed arithmetic way
        (let ((signed-sum (compute-old-checksum-for-tar-header header-type block start)))
          (values (= signed-sum checksum) sum)))))

(defun null-block-p (block start)
  (declare (type (simple-array (unsigned-byte 8) (*)) block))
  (null (position-if-not #'zerop block
			             :start start :end (+ start +tar-n-block-bytes+))))
