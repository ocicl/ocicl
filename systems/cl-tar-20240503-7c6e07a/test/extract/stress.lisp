(in-package #:tar-extract-test)

(para:define-test absolute-pathnames
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:file-entry
                                        :name "/a.txt"
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
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15))))
    :close-stream
    (with-temp-dir ()
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a))
          'tar-extract:entry-name-is-absolute-error))

    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :absolute-pathnames :skip))
      (para:false (probe-file (merge-pathnames "a.txt"))))

    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :absolute-pathnames :relativize))
      (para:true (probe-file (merge-pathnames "a.txt"))))))

(para:define-test dot-dot-pathnames
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:file-entry
                                        :name "dir/../a.txt"
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
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15))))
    :close-stream
    (with-temp-dir ()
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a))
          'tar-extract:entry-name-contains-..-error))

    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :dot-dot :skip))
      (para:false (probe-file (merge-pathnames "a.txt")))
      (para:false (probe-file (merge-pathnames "dir/a.txt"))))

    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :dot-dot :back))
      (para:true (probe-file (merge-pathnames "a.txt"))))))

(para:define-test strip-components
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:directory-entry
                                        :name "dir/"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read)
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15)))
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
                                        :mtime (local-time:unix-to-timestamp 2000 :nsec 15))))
    :close-stream
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :strip-components 1))
      (para:true (probe-file (merge-pathnames "a.txt"))))))

(para:define-test if-newer-exists
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
                                        :mtime (local-time:now))))
    :close-stream
    (sleep 1)
    (with-temp-dir ()
      (with-open-file (s "a.txt" :direction :output)
        (write-string "existing" s))
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a))
          'file-error))
    (with-temp-dir ()
      (with-open-file (s "a.txt" :direction :output)
        (write-string "existing" s))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-newer-exists :keep))
      (para:is equal "existing" (uiop:read-file-string "a.txt")))
    (with-temp-dir ()
      (with-open-file (s "a.txt" :direction :output)
        (write-string "existing" s))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-newer-exists :supersede))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "a.txt")))))

(para:define-test if-exists
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
                                        :mtime (local-time:timestamp+ (local-time:now) 1 :day))))
    :close-stream
    (with-temp-dir ()
      (with-open-file (s "a.txt" :direction :output)
        (write-string "existing" s))
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a))
          'file-error))
    (with-temp-dir ()
      (with-open-file (s "a.txt" :direction :output)
        (write-string "existing" s))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-exists :keep))
      (para:is equal "existing" (uiop:read-file-string "a.txt")))
    (with-temp-dir ()
      (with-open-file (s "a.txt" :direction :output)
        (write-string "existing" s))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-exists :supersede))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "a.txt")))))

#-windows
(para:define-test if-directory-symbolic-link-exists
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
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
                                        :mtime (local-time:timestamp+ (local-time:now) 1 :day))))
    :close-stream
    (with-temp-dir ()
      (nix:mkdir (merge-pathnames "real/") nix:s-irwxu)
      (nix:symlink "real" (merge-pathnames "dir"))
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a))
          'tar-extract:extraction-through-symbolic-link-error))
    (with-temp-dir ()
      (nix:mkdir (merge-pathnames "real/") nix:s-irwxu)
      (nix:symlink "real" (merge-pathnames "dir"))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-directory-symbolic-link-exists :follow))
      (para:is equal "real" (nix:readlink (merge-pathnames "dir")))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a.txt")))
    (with-temp-dir ()
      (nix:mkdir (merge-pathnames "real/") nix:s-irwxu)
      (nix:symlink "real" (merge-pathnames "dir"))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-directory-symbolic-link-exists :supersede))
      (para:is eql :directory (osicat:file-kind (merge-pathnames "dir")
                                                :follow-symlinks (uiop:os-windows-p)))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a.txt")))))

#-windows
(para:define-test if-symbolic-link-exists
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
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
                                        :mtime (local-time:timestamp+ (local-time:now) 1 :day))))
    :close-stream
    (with-temp-dir ()
      (nix:mkdir (merge-pathnames "dir/") nix:s-irwxu)
      (nix:symlink "b.txt" (merge-pathnames "dir/a.txt"))
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a))
          'tar-extract:extraction-through-symbolic-link-error))
    (with-temp-dir ()
      (nix:mkdir (merge-pathnames "dir/") nix:s-irwxu)
      (nix:symlink "b.txt" (merge-pathnames "dir/a.txt"))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-symbolic-link-exists :follow))
      (para:is equal "b.txt" (nix:readlink (merge-pathnames "dir/a.txt")))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a.txt"))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/b.txt")))
    (with-temp-dir ()
      (nix:mkdir (merge-pathnames "dir/") nix:s-irwxu)
      (nix:symlink "b.txt" (merge-pathnames "dir/a.txt"))
      (with-open-file (s (merge-pathnames "dir/b.txt")
                         :direction :output)
        (write-string "b" s))
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :if-symbolic-link-exists :supersede))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a.txt"))
      (para:is equal "b"
               (uiop:read-file-string "dir/b.txt")))))

(para:define-test symbolic-links
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
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
                                        :mtime (local-time:now)))
      (tar:write-entry a (make-instance 'tar:symbolic-link-entry
                                        :name "dir/a-symlink.txt"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write :user-exec
                                                :group-read :group-write :group-exec
                                                :other-read :other-write :other-exec)
                                        :linkname "a.txt"
                                        :mtime (local-time:now))))
    :close-stream
    #-windows
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a))
      (para:is equal "a.txt" (nix:readlink (merge-pathnames "dir/a-symlink.txt")))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a-symlink.txt")))
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :symbolic-links :skip))
      (para:false (probe-file (merge-pathnames "dir/a-symlink.txt"))))
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :symbolic-links :dereference))
      (para:is eql :regular-file (osicat:file-kind (merge-pathnames "dir/a-symlink.txt")
                                                   :follow-symlinks (uiop:os-windows-p)))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a-symlink.txt")))))

(para:define-test hard-links
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
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
                                        :mtime (local-time:now)))
      (tar:write-entry a (make-instance 'tar:hard-link-entry
                                        :name "dir/a-hardlink.txt"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write :user-exec
                                                :group-read :group-write :group-exec
                                                :other-read :other-write :other-exec)
                                        :linkname "dir/a.txt"
                                        :mtime (local-time:now))))
    :close-stream
    #-windows
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a-hardlink.txt"))
      (para:is = 2 (nix:stat-nlink (nix:stat (merge-pathnames "dir/a-hardlink.txt")))))
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :hard-links :skip))
      (para:false (probe-file (merge-pathnames "dir/a-hardlink.txt"))))
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :hard-links :dereference))
      (para:is eql :regular-file (osicat:file-kind (merge-pathnames "dir/a-hardlink.txt")
                                                   :follow-symlinks (uiop:os-windows-p)))
      #-windows
      (para:is = 1 (nix:stat-nlink (nix:stat (merge-pathnames "dir/a-hardlink.txt"))))
      (para:is equal "Hello, world!
"
               (uiop:read-file-string "dir/a-hardlink.txt")))))

(para:define-test character-devices
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:character-device-entry
                                        :name "tty0"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read)
                                        :mtime (local-time:now)
                                        :devmajor 4
                                        :devminor 0)))
    :close-stream
    (with-temp-dir ()
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a :character-devices :error))
          'tar-extract:extract-character-device-entry-error))
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :character-devices :skip))
      (para:false (probe-file "tty0")))))

(para:define-test block-devices
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:block-device-entry
                                        :name "sda1"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read)
                                        :mtime (local-time:now)
                                        :devmajor 8
                                        :devminor 1)))
    :close-stream
    (with-temp-dir ()
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a :block-devices :error))
          'tar-extract:extract-block-device-entry-error))
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a :block-devices :skip))
      (para:false (probe-file "sda1")))))

(para:define-test fifos
  (uiop:with-temporary-file (:stream s :pathname pn :type "tar"
                             :element-type '(unsigned-byte 8))
    (tar:with-open-archive (a s :direction :output :type 'tar:pax-archive)
      (tar:write-entry a (make-instance 'tar:fifo-entry
                                        :name "fifo"
                                        :uname "root"
                                        :gname "root"
                                        :uid 0
                                        :gid 0
                                        :mode '(:user-read :user-write
                                                :group-read
                                                :other-read)
                                        :mtime (local-time:now))))
    :close-stream
    (with-temp-dir ()
      (para:fail
          (tar:with-open-archive (a pn)
            (tar-extract:extract-archive a :fifos :error))
          'tar-extract:extract-fifo-entry-error))
    #-windows
    (with-temp-dir ()
      (tar:with-open-archive (a pn)
        (tar-extract:extract-archive a))
      (para:is eql :pipe (osicat:file-kind (merge-pathnames "fifo"))))))
