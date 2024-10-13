(in-package #:tar-file-test)

(para:define-test read-ustar
  (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                   :tar-file "test/ustar.tar"))
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
      (para:true (null entry)))))

(para:define-test read-ustar-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                      :tar-file "test/ustar.tar.gz"))
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
         (para:true (null entry)))))))

(para:define-test read-pax
  (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                   :tar-file "test/pax.tar"))
    (let (entry)
      (setf entry (tar-file:read-entry a))
      (read-a.txt-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-a.txt :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-a-symlink.txt-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-a-symlink.txt :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-a-hardlink.txt-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-a-hardlink.txt :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-fifo-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-fifo :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-sda1-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-sda1 :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-tty0-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-tty0 :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-sparse.txt-attr :pax entry)

      (setf entry (tar-file:read-entry a))
      (read-sparse.txt :pax entry)

      ;; End of tar-file
      (setf entry (tar-file:read-entry a))
      (para:true (null entry)))))

(para:define-test read-pax-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                      :tar-file "test/pax.tar.gz"))
       (let (entry)
         (setf entry (tar-file:read-entry a))
         (read-a.txt-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-a.txt :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-a-symlink.txt-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-a-symlink.txt :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-a-hardlink.txt-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-a-hardlink.txt :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-fifo-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-fifo :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-sda1-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-sda1 :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-tty0-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-tty0 :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-sparse.txt-attr :pax entry)

         (setf entry (tar-file:read-entry a))
         (read-sparse.txt :pax entry)

         ;; End of tar-file
         (setf entry (tar-file:read-entry a))
         (para:true (null entry)))))))

(para:define-test read-gnu
  (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                   :tar-file "test/gnu.tar"))
    (let (entry)
      ;; First entry is a plain file, with the contents "Hello, world!"
      (setf entry (tar-file:read-entry a))
      (read-a.txt :gnu entry)

      ;; Next is the a-symlink.txt -> a.txt symlink
      (setf entry (tar-file:read-entry a))
      (read-a-symlink.txt :gnu entry)

      ;; Next is the a-hardlink.txt -> a.txt hardlink
      (setf entry (tar-file:read-entry a))
      (read-a-hardlink.txt :gnu entry)

      ;; fifo
      (setf entry (tar-file:read-entry a))
      (read-fifo :gnu entry)

      ;; sda1
      (setf entry (tar-file:read-entry a))
      (read-sda1 :gnu entry)

      ;; tty0
      (setf entry (tar-file:read-entry a))
      (read-tty0 :gnu entry)

      ;; sparse.txt
      (setf entry (tar-file:read-entry a))
      (read-sparse.txt :gnu entry)

      ;; End of tar-file
      (setf entry (tar-file:read-entry a))
      (para:true (null entry)))))

(para:define-test read-gnu-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                      :tar-file "test/gnu.tar.gz"))
       (let (entry)
         ;; First entry is a plain file, with the contents "Hello, world!"
         (setf entry (tar-file:read-entry a))
         (read-a.txt :gnu entry)

         ;; Next is the a-symlink.txt -> a.txt symlink
         (setf entry (tar-file:read-entry a))
         (read-a-symlink.txt :gnu entry)

         ;; Next is the a-hardlink.txt -> a.txt hardlink
         (setf entry (tar-file:read-entry a))
         (read-a-hardlink.txt :gnu entry)

         ;; fifo
         (setf entry (tar-file:read-entry a))
         (read-fifo :gnu entry)

         ;; sda1
         (setf entry (tar-file:read-entry a))
         (read-sda1 :gnu entry)

         ;; tty0
         (setf entry (tar-file:read-entry a))
         (read-tty0 :gnu entry)

         ;; sparse.txt
         (setf entry (tar-file:read-entry a))
         (read-sparse.txt :gnu entry)

         ;; End of tar-file
         (setf entry (tar-file:read-entry a))
         (para:true (null entry)))))))

(para:define-test read-v7
  (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                   :tar-file "test/v7.tar"))
    (let (entry)
      ;; First entry is a plain file, with the contents "Hello, world!"
      (setf entry (tar-file:read-entry a))
      (read-a.txt :v7 entry)

      ;; Next is the a-symlink.txt -> a.txt symlink
      (setf entry (tar-file:read-entry a))
      (read-a-symlink.txt :v7 entry)

      ;; Next is the a-hardlink.txt -> a.txt hardlink
      (setf entry (tar-file:read-entry a))
      (read-a-hardlink.txt :v7 entry)

      ;; End of tar-file
      (setf entry (tar-file:read-entry a))
      (para:true (null entry)))))

(para:define-test read-v7-gzip
  (para:skip-on (abcl) "decompressing stream doesn't work on ABCL."
    (para:finish
     (tar-file:with-open-tar-file (a (asdf:system-relative-pathname
                                      :tar-file "test/v7.tar.gz"))
       (let (entry)
         ;; First entry is a plain file, with the contents "Hello, world!"
         (setf entry (tar-file:read-entry a))
         (read-a.txt :v7 entry)

         ;; Next is the a-symlink.txt -> a.txt symlink
         (setf entry (tar-file:read-entry a))
         (read-a-symlink.txt :v7 entry)

         ;; Next is the a-hardlink.txt -> a.txt hardlink
         (setf entry (tar-file:read-entry a))
         (read-a-hardlink.txt :v7 entry)

         ;; End of tar-file
         (setf entry (tar-file:read-entry a))
         (para:true (null entry)))))))
