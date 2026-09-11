;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- lfp_spawn(3) and its minions
;;;

(in-package :iolib/os)

(defsyscall (lfp-spawn "lfp_spawn")
    (:int :restart t)
  (pid          :pointer)
  (path         :string)
  (arguments    :pointer)
  (environment  :pointer)
  (file-actions :pointer)
  (attributes   :pointer))

(defsyscall (lfp-spawnp "lfp_spawnp")
    (:int :restart t)
  (pid          :pointer)
  (file         :string)
  (arguments    :pointer)
  (environment  :pointer)
  (file-actions :pointer)
  (attributes   :pointer))

(defsyscall (lfp-spawnattr-init
             "lfp_spawnattr_init")
    :int
  (attributes :pointer))

(defsyscall (lfp-spawnattr-destroy
             "lfp_spawnattr_destroy")
    :int
  (attributes :pointer))

(defsyscall (lfp-spawnattr-setflags
             "lfp_spawnattr_setflags")
    :int
  (attributes :pointer)
  (flags      :uint32))

(defsyscall (lfp-spawnattr-setsigmask
             "lfp_spawnattr_setsigmask")
    :int
  (attributes :pointer)
  (sigmask    :pointer))

(defsyscall (lfp-spawnattr-setsigdefault
             "lfp_spawnattr_setsigdefault")
    :int
  (attributes :pointer)
  (sigdefault :pointer))

(defsyscall (lfp-spawnattr-setpgroup
             "lfp_spawnattr_setpgroup")
    :int
  (attributes :pointer)
  (pgroup     pid-t))

(defsyscall (lfp-spawnattr-setsid
             "lfp_spawnattr_setsid")
    :int
  (attributes :pointer))

(defsyscall (lfp-spawnattr-setctty
             "lfp_spawnattr_setctty")
    :int
  (attributes :pointer)
  (pts        sstring))

(defsyscall (lfp-spawnattr-setcwd
             "lfp_spawnattr_setcwd")
    :int
  (attributes :pointer)
  (path       sstring))

(defsyscall (lfp-spawnattr-setuid
             "lfp_spawnattr_setuid")
    :int
  (attributes :pointer)
  (uid        uid-t))

(defsyscall (lfp-spawnattr-setgid
             "lfp_spawnattr_setgid")
    :int
  (attributes :pointer)
  (gid        gid-t))

;; (defsyscall (lfp-spawnattr-getschedpolicy
;;              "lfp_spawnattr_getschedpolicy")
;;     :int
;;   (attributes  :pointer)
;;   (schedpolicy :pointer))

;; (defsyscall (lfp-spawnattr-setschedpolicy
;;              "lfp_spawnattr_setschedpolicy")
;;     :int
;;   (attributes  :pointer)
;;   (schedpolicy :pointer))

;; (defsyscall (lfp-spawnattr-getschedparam
;;              "lfp_spawnattr_getschedparam")
;;     :int
;;   (attributes :pointer)
;;   (schedparam :pointer))

;; (defsyscall (lfp-spawnattr-setschedparam
;;              "lfp_spawnattr_setschedparam")
;;     :int
;;   (attributes :pointer)
;;   (schedparam :pointer))

(defsyscall (lfp-spawn-file-actions-init
             "lfp_spawn_file_actions_init")
    :int
  (file-actions :pointer))

(defsyscall (lfp-spawn-file-actions-destroy
             "lfp_spawn_file_actions_destroy")
    :int
  (file-actions :pointer))

(defsyscall (lfp-spawn-file-actions-addopen
             "lfp_spawn_file_actions_addopen")
    :int
  (file-actions :pointer)
  (fd           :int)
  (path         :string)
  (flags        :int)
  (mode         mode-t))

(defsyscall (lfp-spawn-file-actions-addclose
             "lfp_spawn_file_actions_addclose")
    :int
  (file-actions :pointer)
  (fd           :int))

(defsyscall (lfp-spawn-file-actions-adddup2
             "lfp_spawn_file_actions_adddup2")
    :int
  (file-actions :pointer)
  (fd           :int)
  (newfd        :int))
