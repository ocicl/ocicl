;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto)

(defun make-random-salt (&optional (size 16))
  "Generate a byte vector of SIZE (default 16) random bytes, suitable
for use as a password salt."
  (random-data size))

(defun pbkdf2-hash-password (password &key (salt (make-random-salt))
                                           (digest 'sha256)
                                           (iterations 1000))
  "Given a PASSWORD as a byte vector, a SALT as a byte
vector (MAKE-RANDOM-SALT is called to generate a random salt if none
is provided), a digest function (SHA256 by default), and a number of
iterations (1000), returns the PBKDF2-derived hash of the
password (byte vector) as the first value, and the SALT (byte vector)
as the second value."
  (values (pbkdf2-derive-key digest password salt iterations (digest-length digest))
          salt))

(defun pbkdf2-hash-password-to-combined-string (password &key
                                                (salt (make-random-salt))
                                                (digest 'sha256)
                                                (iterations 1000))
  "Given a PASSWORD byte vector, a SALT as a byte vector (MAKE-RANDOM-SALT
is called to generate a random salt if none is provided), a digest
function (SHA256 by default), and a number of iterations (1000),
returns the salt and PBKDF2-derived hash of the password encoded in a
single ASCII string, suitable for use with PBKDF2-CHECK-PASSWORD."
  (with-standard-io-syntax
    (format nil "PBKDF2$~a:~a$~a$~a" digest iterations
            (byte-array-to-hex-string salt)
            (byte-array-to-hex-string
             (pbkdf2-hash-password password :iterations iterations
                                            :salt salt :digest digest)))))

(defun pbkdf2-check-password (password combined-salt-and-digest)
  "Given a PASSWORD byte vector and a combined salt and digest string
produced by PBKDF2-HASH-PASSWORD-TO-COMBINED-STRING, checks whether
the password is valid."
  ;; can we have a dependency on regular expressions, please?
  (let* ((positions (loop with start = 0 repeat 3 collect
                         (setf start (position #\$ combined-salt-and-digest
                                               :start (1+ start)))))
         (digest-separator-position
          (position #\: combined-salt-and-digest :start (first positions))))
    (constant-time-equal
     (pbkdf2-hash-password
      password
      :digest (find-symbol (subseq combined-salt-and-digest
                                   (1+ (first positions))
                                   digest-separator-position)
                           '#:ironclad)
      :iterations (parse-integer combined-salt-and-digest
                                 :start (1+ digest-separator-position)
                                 :end (second positions))
      :salt (hex-string-to-byte-array combined-salt-and-digest
                                      :start (1+ (second positions))
                                      :end (third positions)))
     (hex-string-to-byte-array combined-salt-and-digest
                               :start (1+ (third positions))))))
