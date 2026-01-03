;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)


(defun list-all-kdfs ()
  (copy-list '(:argon2i :argon2d :bcrypt :bcrypt-pbkdf
               :hmac-kdf :pbkdf1 :pbkdf2 :scrypt-kdf)))

(defun make-kdf (kind &key digest
                      (n 4096) (r 8) (p 2)
                      (block-count 10000) additional-key additional-data)
  ;; PBKDF1, at least, will do stricter checking; this is good enough for now.
  "digest is used for pbkdf1 and pbkdf2.
N, p, and r are cost factors for scrypt.
block-count, additional-key and additional-data are parameters for
argon2"
  (case (massage-symbol kind)
    (pbkdf1
     (let ((digest-name (massage-symbol digest)))
       (unless (digestp digest-name)
         (error 'unsupported-digest :name digest))
       (make-instance 'pbkdf1 :digest digest-name)))
    (pbkdf2
     (let ((digest-name (massage-symbol digest)))
       (unless (digestp digest-name)
         (error 'unsupported-digest :name digest))
       (make-instance 'pbkdf2 :digest digest-name)))
    (hmac-kdf
     (let ((digest-name (massage-symbol digest)))
       (unless (digestp digest-name)
         (error 'unsupported-digest :name digest))
       (make-instance 'hmac-kdf :digest digest-name :info additional-data)))
    (scrypt-kdf
     (when (or (<= n 1)
               (not (zerop (logand n (1- n))))
               (>= (* r p) (expt 2 30)))
       (error 'unsupported-scrypt-cost-factors :n n :r r :p p))
     (make-instance 'scrypt-kdf :n n :r r :p p))
    (argon2i
     (when (< block-count 8)
       (error 'unsupported-argon2-parameters))
     (make-instance 'argon2i
                    :block-count block-count
                    :additional-key additional-key
                    :additional-data additional-data))
    (argon2d
     (when (< block-count 8)
       (error 'unsupported-argon2-parameters))
     (make-instance 'argon2d
                    :block-count block-count
                    :additional-key additional-key
                    :additional-data additional-data))
    (bcrypt
     (make-instance 'bcrypt))
    (bcrypt-pbkdf
     (make-instance 'bcrypt-pbkdf))
    (t
     (error 'unsupported-kdf :kdf kind))))
