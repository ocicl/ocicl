(in-package #:tar-test)

(para:define-test read-ustar
  (tar:with-open-archive (a (asdf:system-relative-pathname
                             :tar "test/ustar.tar"))
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
      (para:true (null entry)))))

(para:define-test read-ustar-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar:with-open-archive (a (asdf:system-relative-pathname
                                :tar "test/ustar.tar.gz"))
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
         (para:true (null entry)))))))

(para:define-test read-pax
  (tar:with-open-archive (a (asdf:system-relative-pathname
                             :tar "test/pax.tar"))
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
      (para:true (null entry)))))

(para:define-test read-pax-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar:with-open-archive (a (asdf:system-relative-pathname
                                :tar "test/pax.tar.gz"))
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
         (para:true (null entry)))))))

(para:define-test read-gnu
  (tar:with-open-archive (a (asdf:system-relative-pathname
                              :tar "test/gnu.tar"))
    (let (entry)
      ;; First entry is a plain file, with the contents "Hello, world!"
      (setf entry (tar:read-entry a))
      (read-a.txt :gnu entry)

      ;; Next is the a-symlink.txt -> a.txt symlink
      (setf entry (tar:read-entry a))
      (read-a-symlink.txt :gnu entry)

      ;; Next is the a-hardlink.txt -> a.txt hardlink
      (setf entry (tar:read-entry a))
      (read-a-hardlink.txt :gnu entry)

      ;; fifo
      (setf entry (tar:read-entry a))
      (read-fifo :gnu entry)

      ;; sda1
      (setf entry (tar:read-entry a))
      (read-sda1 :gnu entry)

      ;; tty0
      (setf entry (tar:read-entry a))
      (read-tty0 :gnu entry)

      ;; End of tar-file
      (setf entry (tar:read-entry a))
      (para:true (null entry)))))

(para:define-test read-gnu-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar:with-open-archive (a (asdf:system-relative-pathname
                                :tar "test/gnu.tar.gz"))
       (let (entry)
         ;; First entry is a plain file, with the contents "Hello, world!"
         (setf entry (tar:read-entry a))
         (read-a.txt :gnu entry)

         ;; Next is the a-symlink.txt -> a.txt symlink
         (setf entry (tar:read-entry a))
         (read-a-symlink.txt :gnu entry)

         ;; Next is the a-hardlink.txt -> a.txt hardlink
         (setf entry (tar:read-entry a))
         (read-a-hardlink.txt :gnu entry)

         ;; fifo
         (setf entry (tar:read-entry a))
         (read-fifo :gnu entry)

         ;; sda1
         (setf entry (tar:read-entry a))
         (read-sda1 :gnu entry)

         ;; tty0
         (setf entry (tar:read-entry a))
         (read-tty0 :gnu entry)

         ;; End of tar-file
         (setf entry (tar:read-entry a))
         (para:true (null entry)))))))

(para:define-test read-v7
  (tar:with-open-archive (a (asdf:system-relative-pathname
                              :tar "test/v7.tar"))
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
      (para:true (null entry)))))

(para:define-test read-v7-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar:with-open-archive (a (asdf:system-relative-pathname
                                :tar "test/v7.tar.gz"))
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
         (para:true (null entry)))))))
