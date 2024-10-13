(in-package #:tar-file-test)

(defun write-test-file (s byte block-size)
  (write-byte byte s)
  (write-sequence (make-array (1- block-size) :element-type '(unsigned-byte 8)
                                              :initial-element 0)
                  s))

(para:define-test blocked-output-stream
  (dolist (block-size '(512 1024 2048))
    (uiop:with-temporary-file (:stream raw-stream :pathname pn
                               :element-type '(unsigned-byte 8))
      (with-open-stream (s (make-instance 'tar-file::blocked-output-stream
                                          :stream raw-stream
                                          :block-size block-size))
        (write-byte (char-code #\a) s))
      :close-stream
      (with-open-file (raw-stream pn
                                  :element-type '(unsigned-byte 8))
        (para:is = block-size (file-length raw-stream))
        (para:is = (char-code #\a) (read-byte raw-stream))
        (let ((seq (make-array block-size :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
          (read-sequence seq raw-stream)
          (para:true (every #'zerop seq)))))))

(para:define-test blocked-input-stream
  (uiop:with-temporary-file (:stream raw-stream :pathname pn
                             :element-type '(unsigned-byte 8))
    (write-test-file raw-stream (char-code #\a) 1024)
    :close-stream
    (with-open-file (raw-stream pn
                                :element-type '(unsigned-byte 8))
      (with-open-stream (s (make-instance 'tar-file::blocked-input-stream
                                          :stream raw-stream
                                          :block-size 1024))
        (para:is = (char-code #\a) (read-byte s))
        (let ((seq (make-array 1024 :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
          (para:is = (1- 1024) (read-sequence seq s))
          (para:true (every #'zerop seq)))
        (para:is eql :eof (read-byte s nil :eof))))))

(para:define-test blocked-input-stream-set-file-position
  (uiop:with-temporary-file (:stream raw-stream :pathname pn
                             :element-type '(unsigned-byte 8))
    (write-test-file raw-stream (char-code #\a) 1024)
    :close-stream
    (with-open-file (raw-stream pn
                                :element-type '(unsigned-byte 8))
      (with-open-stream (s (make-instance 'tar-file::blocked-input-stream
                                          :stream raw-stream
                                          :block-size 1024))
        (para:is = (char-code #\a) (read-byte s))
        (file-position s 0)
        (para:is = (char-code #\a) (read-byte s))
        (let ((seq (make-array 1024 :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
          (para:is = (1- 1024) (read-sequence seq s))
          (para:true (every #'zerop seq)))
        (para:is eql :eof (read-byte s nil :eof))))))

(para:define-test blocked-input-stream-set-file-position-2
  (uiop:with-temporary-file (:stream raw-stream :pathname pn
                             :element-type '(unsigned-byte 8))
    (write-test-file raw-stream (char-code #\a) 1024)
    :close-stream
    (with-open-file (raw-stream pn
                                :element-type '(unsigned-byte 8))
      (with-open-stream (s (make-instance 'tar-file::blocked-input-stream
                                          :stream raw-stream
                                          :block-size 512))
        (para:is = (char-code #\a) (read-byte s))
        (let ((seq (make-array 1024 :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
          (para:is = (1- 1024) (read-sequence seq s))
          (para:true (every #'zerop seq)))
        (para:is eql :eof (read-byte s nil :eof))
        (file-position s 0)
        (para:is = (char-code #\a) (read-byte s))))))

(para:define-test jumping-across-blocks
  (uiop:with-temporary-file (:stream raw-stream :pathname pn
                             :element-type '(unsigned-byte 8))
    (write-sequence (make-array 1024 :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                    raw-stream)
    (write-sequence (make-array 1024 :element-type '(unsigned-byte 8)
                                     :initial-element 1)
                    raw-stream)
    :close-stream
    (with-open-file (raw-stream pn
                                :element-type '(unsigned-byte 8))
      (with-open-stream (s (make-instance 'tar-file::blocked-input-stream
                                          :stream raw-stream
                                          :block-size 1024))
        (let ((seq (make-array 512 :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
          (read-sequence seq s)
          (para:true (every #'zerop seq))
          (read-sequence seq s)
          (para:true (every #'zerop seq))
          (read-sequence seq s)
          (para:true (every (lambda (x) (= x 1)) seq))
          (file-position s 512)
          (read-sequence seq s)
          (para:true (every #'zerop seq))
          (read-sequence seq s)
          (para:true (every (lambda (x) (= x 1)) seq)))))))

(para:define-test bounded-stream-jumping-across-blocks
  (uiop:with-temporary-file (:stream raw-stream :pathname pn
                             :element-type '(unsigned-byte 8))
    (write-sequence (make-array 1024 :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                    raw-stream)
    (write-sequence (make-array 1024 :element-type '(unsigned-byte 8)
                                     :initial-element 1)
                    raw-stream)
    :close-stream
    (with-open-file (raw-stream pn
                                :element-type '(unsigned-byte 8))
      (with-open-stream (s (make-instance 'tar-file::blocked-input-stream
                                          :stream raw-stream
                                          :block-size 1024))
        (let ((seq-1 (make-array 512 :element-type '(unsigned-byte 8)
                                     :initial-element 0))
              (seq-2 (make-array 1024 :element-type '(unsigned-byte 8)
                                      :initial-element 0)))

          (read-sequence seq-1 s)
          (para:true (every #'zerop seq-1))
          (with-open-stream (bounded-stream (tar-file::make-bounded-stream s 1024))
            (read-sequence seq-2 bounded-stream)
            (para:true (every #'zerop (subseq seq-2 0 512)))
            (para:true (every (lambda (x) (= x 1)) (subseq seq-2 512))))
          (with-open-stream (bounded-stream (tar-file::make-bounded-stream s 1024 512))
            (read-sequence seq-2 bounded-stream)
            (para:true (every #'zerop (subseq seq-2 0 512)))
            (para:true (every (lambda (x) (= x 1)) (subseq seq-2 512))))

          (read-sequence seq-1 s)
          (para:true (every (lambda (x) (= x 1)) seq-1)))))))
