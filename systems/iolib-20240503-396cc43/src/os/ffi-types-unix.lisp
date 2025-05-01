;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Foreign types related to lfp_spawn(3)
;;;

(in-package :iolib/os)

(pkg-config-cflags "libfixposix")

(include "lfp.h")

(constant (+stdin+  "STDIN_FILENO"))
(constant (+stdout+ "STDOUT_FILENO"))
(constant (+stderr+ "STDERR_FILENO"))

(cstruct lfp-spawnattr-t "lfp_spawnattr_t")

(cstruct lfp-spawn-file-actions-t "lfp_spawn_file_actions_t")

(constant (lfp-spawn-setsigmask    "LFP_SPAWN_SETSIGMASK"))
(constant (lfp-spawn-setsigdefault "LFP_SPAWN_SETSIGDEFAULT"))
(constant (lfp-spawn-setpgroup     "LFP_SPAWN_SETPGROUP"))
(constant (lfp-spawn-resetids      "LFP_SPAWN_RESETIDS"))
(constant (lfp-spawn-setuid        "LFP_SPAWN_SETUID"))
(constant (lfp-spawn-setgid        "LFP_SPAWN_SETGID"))
(constant (lfp-spawn-usevfork      "LFP_SPAWN_USEVFORK"))
;; (constant (lfp-spawn-setschedparam "LFP_SPAWN_SETSCHEDPARAM"))
;; (constant (lfp-spawn-setscheduler  "LFP_SPAWN_SETSCHEDULER"))
