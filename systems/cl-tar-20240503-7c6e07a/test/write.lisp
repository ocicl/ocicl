(in-package #:tar-test)

(para:define-test write-ustar
  (uiop:with-temporary-file (:stream s :pathname pn
                             :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:ustar-archive)
      (write-a.txt a)
      (write-a-symlink.txt a)
      (write-a-hardlink.txt a)
      (write-fifo a)
      (write-sda1 a)
      (write-tty0 a))
    :close-stream
    (files-equal pn (asdf:system-relative-pathname
                     :tar "test/ustar.tar"))))

(para:define-test write-ustar-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (uiop:with-temporary-file (:stream s :pathname pn
                                :type "tar.gz"
                                :element-type '(unsigned-byte 8))
       (tar:with-open-archive (a s :direction :output :type 'tar:ustar-archive)
         (write-a.txt a)
         (write-a-symlink.txt a)
         (write-a-hardlink.txt a)
         (write-fifo a)
         (write-sda1 a)
         (write-tty0 a))
       :close-stream
       (tar:with-open-archive (a pn)
         (let (entry)
           ;; First entry is a plain file, with the contents "Hello, world!"
           (setf entry (tar:read-entry a))
           (read-a.txt :ustar entry)

           ;; Next is the a-symlink.txt -> a.txt symlink
           (setf entry (tar:read-entry a))
           (read-a-symlink.txt :ustar entry)

           ;; Next is the a-hardlink.txt -> a.txt hardlink
           (setf entry (tar:read-entry a))
           (read-a-hardlink.txt :ustar entry)

           ;; fifo
           (setf entry (tar:read-entry a))
           (read-fifo :ustar entry)

           ;; sda1
           (setf entry (tar:read-entry a))
           (read-sda1 :ustar entry)

           ;; tty0
           (setf entry (tar:read-entry a))
           (read-tty0 :ustar entry)

           ;; End of tar-file
           (setf entry (tar:read-entry a))
           (para:true (null entry))))))))

(para:define-test write-pax
  (uiop:with-temporary-file (:stream s :pathname pn
                             :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (write-a.txt a)
      (write-a-symlink.txt a)
      (write-a-hardlink.txt a)
      (write-fifo a)
      (write-sda1 a)
      (write-tty0 a))
    :close-stream
    (files-equal pn (asdf:system-relative-pathname
                     :tar "test/pax.tar"))))

(para:define-test write-pax-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (uiop:with-temporary-file (:stream s :pathname pn
                                :type "tar.gz"
                                :element-type '(unsigned-byte 8))
       (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
         (write-a.txt a)
         (write-a-symlink.txt a)
         (write-a-hardlink.txt a)
         (write-fifo a)
         (write-sda1 a)
         (write-tty0 a))
       :close-stream
       (tar:with-open-archive (a pn)
         (let (entry)
           (setf entry (tar:read-entry a))
           (read-a.txt :pax entry)

           (setf entry (tar:read-entry a))
           (read-a-symlink.txt :pax entry)

           (setf entry (tar:read-entry a))
           (read-a-hardlink.txt :pax entry)

           (setf entry (tar:read-entry a))
           (read-fifo :pax entry)

           (setf entry (tar:read-entry a))
           (read-sda1 :pax entry)

           (setf entry (tar:read-entry a))
           (read-tty0 :pax entry)

           ;; End of tar-file
           (setf entry (tar:read-entry a))
           (para:true (null entry))))))))

(para:define-test write-v7
  (uiop:with-temporary-file (:stream s :pathname pn
                             :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type :v7)
      (write-a.txt a)
      (write-a-symlink.txt a)
      (write-a-hardlink.txt a))
    :close-stream
    (files-equal pn (asdf:system-relative-pathname
                     :tar "test/v7.tar"))))

(para:define-test write-v7-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (uiop:with-temporary-file (:stream s :pathname pn
                                :type "tar.gz"
                                :element-type '(unsigned-byte 8))
       (tar:with-open-archive (a s :direction :output :type :v7)
         (write-a.txt a)
         (write-a-symlink.txt a)
         (write-a-hardlink.txt a))
       :close-stream
       (tar:with-open-archive (a pn)
         (let ((entry))
           (setf entry (tar:read-entry a))
           (read-a.txt :v7 entry)

           ;; Next is the a-symlink.txt -> a.txt symlink
           (setf entry (tar:read-entry a))
           (read-a-symlink.txt :v7 entry)

           ;; Next is the a-hardlink.txt -> a.txt hardlink
           (setf entry (tar:read-entry a))
           (read-a-hardlink.txt :v7 entry)

           ;; End of tar-file
           (setf entry (tar:read-entry a))
           (para:true (null entry))))))))
