(in-package #:tar-create-test)

(para:define-test create-pax
  (with-temp-dir ()
    (nix:mkdir (merge-pathnames "test/") (logior nix:s-irwxu
                                                 #-windows nix:s-irgrp #-windows nix:s-ixgrp
                                                 #-windows nix:s-iroth #-windows nix:s-ixoth))
    (with-open-file (s "test/a.txt" :direction :output)
      (write-string "Hello, world!
"
                    s))
    (osicat:make-link "test/a-symlink.txt" :target "a.txt")
    (osicat:make-link "test/a-hardlink.txt" :target "a.txt" :hard t)

    (uiop:with-temporary-file (:stream s :pathname pn :element-type '(unsigned-byte 8))
      (tar:with-open-archive (a s :direction :output :type :pax)
        (tar-create:create-archive a "test/" :recursep t))
      :close-stream
      (tar:with-open-archive (a pn :direction :input)
        (let (entry)

          (setf entry (tar:read-entry a))
          (para:true (typep entry 'tar:directory-entry))
          (para:is equal "test/" (tar:name entry))

          (setf entry (tar:read-entry a))
          (para:true (typep entry 'tar:file-entry))
          (para:is equal "test/a-hardlink.txt" (tar:name entry))

          (setf entry (tar:read-entry a))
          (para:true (typep entry 'tar:symbolic-link-entry))
          (para:is equal "test/a-symlink.txt" (tar:name entry))
          (para:is equal "a.txt" (tar:linkname entry))

          (setf entry (tar:read-entry a))
          (para:true (typep entry 'tar:hard-link-entry))
          (para:is equal "test/a.txt" (tar:name entry))
          (para:is equal "test/a-hardlink.txt" (tar:linkname entry))

          (setf entry (tar:read-entry a))
          (para:true (null entry)))))))
