(in-package #:tar-file-test)

(para:define-test write-ustar
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar" :direction :output
                             :element-type '(unsigned-byte 8)
                             :keep *keep-written-tar-files*)
    (tar-file:with-open-tar-file (a s
                                    :direction :output
                                    :type 'tar-file:ustar-tar-file)

      (write-a.txt :ustar a)
      (write-a-symlink.txt :ustar a)
      (write-a-hardlink.txt :ustar a)
      (write-fifo :ustar a)
      (write-sda1 :ustar a)
      (write-tty0 :ustar a))
    :close-stream
    (files-equal (asdf:system-relative-pathname :tar-file "test/ustar.tar")
                 pn)))

(para:define-test write-ustar-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (uiop:with-temporary-file (:stream s :pathname pn :type "tar.gz" :direction :output
                                :element-type '(unsigned-byte 8)
                                :keep *keep-written-tar-files*)
       (tar-file:with-open-tar-file (a s
                                     :direction :output
                                     :type 'tar-file:ustar-tar-file)

         (write-a.txt :ustar a)
         (write-a-symlink.txt :ustar a)
         (write-a-hardlink.txt :ustar a)
         (write-fifo :ustar a)
         (write-sda1 :ustar a)
         (write-tty0 :ustar a))
       :close-stream
       (tar-file:with-open-tar-file (a pn)
         (let (entry)
           ;; First entry is a plain file, with the contents "Hello, world!"
           (setf entry (tar-file:read-entry a))
           (read-a.txt :ustar entry)

           ;; Next is the a-symlink.txt -> a.txt symlink
           (setf entry (tar-file:read-entry a))
           (read-a-symlink.txt :ustar entry)

           ;; Next is the a-hardlink.txt -> a.txt hardlink
           (setf entry (tar-file:read-entry a))
           (read-a-hardlink.txt :ustar entry)

           ;; fifo
           (setf entry (tar-file:read-entry a))
           (read-fifo :ustar entry)

           ;; sda1
           (setf entry (tar-file:read-entry a))
           (read-sda1 :ustar entry)

           ;; tty0
           (setf entry (tar-file:read-entry a))
           (read-tty0 :ustar entry)

           ;; End of tar-file
           (setf entry (tar-file:read-entry a))
           (para:true (null entry))))))))

;; For some reason, GNU tar writes v7 tar-files with all zeroes in the devmajor
;; and devminor fields. We don't do that (yet?) So this test always fails.
(para:define-test write-v7
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar" :direction :output
                             :element-type '(unsigned-byte 8)
                             :keep *keep-written-tar-files*)
    (tar-file:with-open-tar-file (a s
                                    :direction :output
                                    :type 'tar-file:v7-tar-file)

      (write-a.txt :v7 a)
      (write-a-symlink.txt :v7 a)
      (write-a-hardlink.txt :v7 a))
    :close-stream
    (para:skip "GNU tar writes v7 tar-files with all zeros in the devmajor and devminor fields??"
      (files-equal (asdf:system-relative-pathname :tar-file "test/v7.tar")
                   pn))))
