(in-package #:tar-extract-test)

(para:define-test extract-v7
  (with-temp-dir ()
    (tar:with-open-archive (a (asdf:system-relative-pathname
                               :tar "test/v7.tar"))
      (tar-extract:extract-archive a :symbolic-links #+windows :dereference #-windows t
                                     :hard-links #+windows :dereference #-windows t)
      (para:is equal
               "Hello, world!
"
               (uiop:read-file-string (merge-pathnames "a.txt")))
      #-windows
      (para:is eql
               :symbolic-link
               (osicat:file-kind (merge-pathnames "a-symlink.txt")))
      #-windows
      (para:is equal
               "a.txt"
               (nix:readlink (merge-pathnames "a-symlink.txt")))
      (para:is equal
               "Hello, world!
"
               (uiop:read-file-string (merge-pathnames "a-symlink.txt")))
      (para:is equal
               "Hello, world!
"
               (uiop:read-file-string (merge-pathnames "a-hardlink.txt"))))))

(para:define-test extract-ustar-1
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:file-entry
                                        :name "a.txt"
                                        :size 14
                                        :data "Hello, world!
"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read)
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15)))
      (tar:write-entry a (make-instance 'tar:symbolic-link-entry
                                        :name "a-symlink.txt"
                                        :linkname "a.txt"
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 20)
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write :user-exec
                                                :group-read :group-write :group-exec
                                                :other-read :other-write :other-exec)))
      (tar:write-entry a (make-instance 'tar:hard-link-entry
                                        :name "a-hardlink.txt"
                                        :linkname "a.txt"
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15)
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read))))
    :close-stream
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :symbolic-links #+windows :dereference #-windows t
                                       :hard-links #+windows :dereference #-windows t)
        (para:is equal
                 "Hello, world!
"
                 (uiop:read-file-string (merge-pathnames "a.txt")))
        #-windows
        (para:is eql
                 :symbolic-link
                 (osicat:file-kind (merge-pathnames "a-symlink.txt")))
        #-windows
        (para:is equal
                 "a.txt"
                 (nix:readlink (merge-pathnames "a-symlink.txt")))
        (para:is equal
                 "Hello, world!
"
                 (uiop:read-file-string (merge-pathnames "a-symlink.txt")))
        (para:is equal
                 "Hello, world!
"
                 (uiop:read-file-string (merge-pathnames "a-hardlink.txt")))))))

(para:define-test extract-ustar-2
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:directory-entry
                                        :name "dir/"
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 10)
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write :user-exec
                                                :other-read :other-exec
                                                :group-read :group-exec)))
      (tar:write-entry a (make-instance 'tar:file-entry
                                        :name "dir/a.txt"
                                        :size 14
                                        :data "Hello, world!
"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read)
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15)))
      (tar:write-entry a (make-instance 'tar:symbolic-link-entry
                                        :name "dir/a-symlink.txt"
                                        :linkname "a.txt"
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 20)
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write :user-exec
                                                :group-read :group-write :group-exec
                                                :other-read :other-write :other-exec)))
      (tar:write-entry a (make-instance 'tar:hard-link-entry
                                        :name "dir/a-hardlink.txt"
                                        :linkname "dir/a.txt"
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15)
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read))))
    :close-stream
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :symbolic-links #+windows :dereference #-windows t
                                       :hard-links #+windows :dereference #-windows t)
        (para:is equal
                 "Hello, world!
"
                 (uiop:read-file-string (merge-pathnames "dir/a.txt")))
        #-windows
        (para:is eql
                 :symbolic-link
                 (osicat:file-kind (merge-pathnames "dir/a-symlink.txt")))
        #-windows
        (para:is equal
                 "a.txt"
                 (nix:readlink (merge-pathnames "dir/a-symlink.txt")))
        (para:is equal
                 "Hello, world!
"
                 (uiop:read-file-string (merge-pathnames "dir/a-symlink.txt")))
        (para:is equal
                 "Hello, world!
"
                 (uiop:read-file-string (merge-pathnames "dir/a-hardlink.txt")))

        #-windows
        (progn
          (para:is = 2000 (nix:stat-mtime (nix:stat (merge-pathnames "dir/a.txt"))))
          (para:is = 15 (nix:stat-mtime-nsec (nix:stat (merge-pathnames "dir/a.txt"))))
          (para:is = 2000 (nix:stat-mtime (nix:stat (merge-pathnames "dir/a-hardlink.txt"))))
          (para:is = 15 (nix:stat-mtime-nsec (nix:stat (merge-pathnames "dir/a-hardlink.txt"))))
          (para:is = 2000 (nix:stat-mtime (nix:stat (merge-pathnames "dir/"))))
          (para:is = 10 (nix:stat-mtime-nsec (nix:stat (merge-pathnames "dir/")))))))))
