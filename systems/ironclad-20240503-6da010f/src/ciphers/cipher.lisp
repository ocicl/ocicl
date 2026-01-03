;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; cipher.lisp -- generic functions for symmetric encryption

(in-package :crypto)

(defclass cipher ()
  ((mode :initarg :mode :accessor mode)
   (mode-name :reader mode-name)
   (initialized-p :initform nil :accessor initialized-p)))

;;; Block ciphers are denoted by the use of the {8,16,32,64,128}-byte-block-mixin.
(defclass stream-cipher (cipher)
  ())

(defmethod encrypt ((cipher cipher) plaintext ciphertext &key (plaintext-start 0) plaintext-end (ciphertext-start 0) handle-final-block &allow-other-keys)
  (check-type plaintext vector)
  (let ((plaintext-end (or plaintext-end (length plaintext))))
    (funcall (slot-value (mode cipher) 'encrypt-function)
             plaintext ciphertext
             plaintext-start plaintext-end ciphertext-start
             handle-final-block)))

(defmethod decrypt ((cipher cipher) ciphertext plaintext &key (ciphertext-start 0) ciphertext-end (plaintext-start 0) handle-final-block &allow-other-keys)
  (check-type ciphertext vector)
  (let ((ciphertext-end (or ciphertext-end (length ciphertext))))
    (funcall (slot-value (mode cipher) 'decrypt-function)
             ciphertext plaintext
             ciphertext-start ciphertext-end
             plaintext-start handle-final-block)))

(defun encrypt-in-place (cipher text &key (start 0) end)
  (encrypt cipher text text
           :plaintext-start start :plaintext-end end
           :ciphertext-start start))

(defun decrypt-in-place (cipher text &key (start 0) end)
  (decrypt cipher text text
           :ciphertext-start start :ciphertext-end end
           :plaintext-start start))


;;; utilities for wordwise fetches and stores

;;; we attempt to make this as efficient as possible.  the first check we
;;; do is to see whether or not the range
;;; [INITIAL-OFFSET, INITIAL-OFFSET + BLOCK-SIZE) is within the bounds of
;;; the array.  if not, then we do the fetches as normal.  if so, then we
;;; can either (DECLARE (SAFETY 0)) to avoid the bounds-checking on the
;;; fetches, or we can do full-word fetches if INITIAL-OFFSET is
;;; word-addressable and the implementation supports it.
#+nil
(defmacro with-words (((&rest word-vars) array initial-offset
                       &key (size 4) (big-endian t))
                      &body body)
  (let ((ref-sym (ubref-fun-name (* size 8) big-endian))
        (n-bytes (* (length word-vars) size)))
    (flet ((generate-fetches (n-fetches)
             (loop for offset from 0 by size below (* n-fetches size)
                   collect `(,ref-sym ,array (+ ,initial-offset ,offset)))))
      `(multiple-value-bind ,word-vars (let ((length (length ,array)))
                                         (cond
                                           ((<= ,initial-offset (- length ,n-bytes))
                                            ,(if (and (member :sbcl *features*)
                                                      (= size 4)
                                                      (or (and big-endian (member :big-endian *features*))
                                                          (and (not big-endian) (member :little-endian *features*))))
                                                 `(if (logtest ,initial-offset (1- ,size))
                                                      ;; do FETCH-UB* way
                                                      (locally (declare (optimize (safety 0)))
                                                        (values ,@(generate-fetches (length word-vars))))
                                                      (let ((word-offset (truncate ,initial-offset 4)))
                                                        (values
                                                         ,@(loop for offset from 0 below (length word-vars)
                                                              collect `(sb-kernel:%vector-raw-bits ,array (+ word-offset ,offset))))))
                                                  `(locally (declare (optimize (safety 0)))
                                                     (values ,@(generate-fetches (length word-vars))))))
                                           (t
                                            (values ,@(generate-fetches (length word-vars))))))
         (declare (type (unsigned-byte ,(* size 8)) ,@word-vars))
         (macrolet ((store-words (buffer buffer-offset &rest word-vars)
                      (loop for word-var in word-vars
                         for offset from 0 by ,size
                         collect `(setf (,',ref-sym ,buffer (+ ,buffer-offset ,offset)) ,word-var)
                         into stores
                         finally (return `(progn ,@stores)))))
           ,@body)))))

(defmacro with-words (((&rest word-vars) array initial-offset
                       &key (size 4) (big-endian t))
                      &body body)
  (let ((ref-sym (ubref-fun-name (* size 8) big-endian)))
    (loop for word-var in word-vars
          for offset from 0 by size
          collect `(,word-var (,ref-sym ,array (+ ,initial-offset ,offset)))
          into let-bindings
          finally (return `(macrolet ((store-words (buffer buffer-offset &rest word-vars)
                                       (loop for word-var in word-vars
                                             for offset from 0 by ,size
                                             collect `(setf (,',ref-sym ,buffer (+ ,buffer-offset ,offset)) ,word-var)
                                             into stores
                                             finally (return `(progn ,@stores)))))
                             (let ,let-bindings
                               (declare (type (unsigned-byte ,(* size 8)) ,@word-vars))
                               ,@body))))))


;;; mixins for dispatching

(defclass 8-byte-block-mixin ()
  ())

(defclass 16-byte-block-mixin ()
  ())

(defclass 32-byte-block-mixin ()
  ())

(defclass 64-byte-block-mixin ()
  ())

(defclass 128-byte-block-mixin ()
  ())


;;; defining ciphers

;;; the idea behind this is that one only has to implement encryption
;;; and decryption of a block for a particular cipher (and perhaps
;;; some key generation) and then "define" the cipher with some
;;; parameters.  necessary interface functions will be auto-generated
;;; with this macro.

;;; possible things to go in INITARGS
;;;
;;; * (:encrypt-function #'cipher-encrypt-block)
;;; * (:decrypt-function #'cipher-decrypt-block)
;;; * (:key-length (:fixed &rest lengths))
;;; * (:key-length (:variable low high increment))
;;; * (:constructor #'create-cipher-context)
(defmacro defcipher (name &rest initargs)
  (%defcipher name initargs))

;;; KLUDGE: we add the blocksize to these two forms so that we can declare
;;; the type of the *-START parameters correctly.  That is, good Lisp
;;; implementations will see that references into the plaintext and
;;; ciphertext can never overflow into bignum land; shorter code should
;;; then be generated.  This is a kludge, however, because we're putting
;;; the blocksize in three different places: once in the encryptor, once
;;; in the decryptor, and once in the DEFCIPHER form.  It would be nice
;;; if there was one single place to put everything.
(defmacro define-block-encryptor (algorithm blocksize &body body)
  `(defun ,(symbolicate algorithm '#:-encrypt-block)
    (context plaintext plaintext-start ciphertext ciphertext-start)
    (declare (optimize (speed 3) (debug 0) (space 0)))
    (declare (type simple-octet-vector plaintext ciphertext)
     (type (integer 0 ,(- array-dimension-limit blocksize))
      plaintext-start ciphertext-start))
    ,@body))

(defmacro define-block-decryptor (algorithm blocksize &body body)
  `(defun ,(symbolicate algorithm '#:-decrypt-block)
    (context ciphertext ciphertext-start plaintext plaintext-start)
    (declare (optimize (speed 3) (debug 0) (space 0)))
    (declare (type simple-octet-vector ciphertext plaintext)
     (type (integer 0 ,(- array-dimension-limit blocksize))
      ciphertext-start plaintext-start))
    ,@body))

(defmacro define-stream-cryptor (algorithm &body body)
  `(defun ,(symbolicate algorithm '#:-crypt)
       (context plaintext plaintext-start ciphertext ciphertext-start length)
     (declare (optimize (speed 3) (debug 0) (space 0)))
     (declare (type simple-octet-vector plaintext ciphertext))
     (declare (type index plaintext-start ciphertext-start length))
     ,@body))

;; Catch various errors.
(defmethod verify-key (cipher key)
  ;; check the key first
  (when (null key)
    (error 'key-not-supplied :cipher cipher))
  (unless (typep key '(vector (unsigned-byte 8)))
    (error 'type-error :datum key :expected-type '(vector (unsigned-byte 8))))
  ;; hmmm, the key looks OK.  what about the cipher?
  (unless (member cipher (list-all-ciphers))
    (error 'unsupported-cipher :name cipher)))

(defmethod schedule-key :before ((cipher cipher) key)
  (verify-key cipher key))

;;; introspection
(defclass cipher-info ()
  ((class-name :reader %class-name :initarg :class-name)
   (name :reader cipher :initarg :cipher)
   (block-length :reader %block-length :initarg :block-length)
   (key-lengths :reader %key-lengths :initarg :key-lengths)))

(defmethod print-object ((object cipher-info) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (cipher object))))

(defun %find-cipher (name)
  (and (symbolp name)
       (let ((name (massage-symbol name)))
         (and name (get name '%cipher-info)))))

(defun (setf %find-cipher) (cipher-info name)
  (setf (get (massage-symbol name) '%cipher-info) cipher-info))

(defmethod key-lengths (cipher)
  (let ((cipher-info (%find-cipher cipher)))
    (and cipher-info (%key-lengths cipher-info))))

(defmethod key-lengths ((cipher cipher))
  (key-lengths (class-name (class-of cipher))))

(defmethod block-length ((cipher symbol))
  (let ((cipher-info (%find-cipher (massage-symbol cipher))))
    (and cipher-info (%block-length cipher-info))))

(defmethod block-length ((cipher cipher))
  (block-length (class-name (class-of cipher))))

(defmethod block-length ((cipher 8-byte-block-mixin))
  8)

(defmethod block-length ((cipher 16-byte-block-mixin))
  16)

(defmethod block-length ((cipher 32-byte-block-mixin))
  32)

(defmethod block-length ((cipher 64-byte-block-mixin))
  64)

(defmethod block-length ((cipher 128-byte-block-mixin))
  128)

(defun list-all-ciphers ()
  (loop for symbol being each external-symbol of (find-package :ironclad)
     if (%find-cipher symbol)
     collect (intern (symbol-name symbol) :keyword) into ciphers
     finally (return (sort ciphers #'string<))))

(defun cipher-supported-p (name)
  "Return T if the cipher NAME is supported as an argument to MAKE-CIPHER."
  (not (null (%find-cipher name))))

(defun acceptable-key-lengths* (key-length-spec)
  (ecase (car key-length-spec)
    (:fixed (loop for length in (cdr key-length-spec)
               collect `(= length ,length) into forms
               finally (return `(or ,@forms))))
    (:variable (destructuring-bind (low high increment) (cdr key-length-spec)
                 (if (= increment 1)
                     `(<= ,low length ,high)
                     ;; Punt.  It'd be a weird cipher implemented otherwise.
                     (error 'ironclad-error :format-control "Need to implement the (/= INCREMENT 1) case"))))))

(defun acceptable-key-lengths (key-length-spec)
  (ecase (car key-length-spec)
    (:fixed (cdr key-length-spec))
    (:variable (destructuring-bind (low high increment) (cdr key-length-spec)
                 (loop for i from low to high by increment
                       collect i)))))

(defun generate-key-verifier-methods (name key-length-spec)
  (let ((acceptable-key-lengths (acceptable-key-lengths key-length-spec)))
    `(defmethod verify-key ((cipher ,name) (key vector))
      (check-type key (array (unsigned-byte 8) (*)))
      (let ((length (length key)))
        (cond
          (,(acceptable-key-lengths* key-length-spec) (copy-seq key))
          (t (error 'invalid-key-length
                    :cipher ',name
                    :accepted-lengths ',acceptable-key-lengths)))))))

(defun generate-common-cipher-methods (name block-length key-length-spec)
  `(progn
     ;; make sure we pass in valid keys
     ,(generate-key-verifier-methods name key-length-spec)
     (setf (%find-cipher ',name)
           (make-instance 'cipher-info
                          :class-name ',name
                          :cipher ',name
                          :block-length ,block-length
                          :key-lengths ',(acceptable-key-lengths key-length-spec)))))

(defun generate-block-cipher-forms (name key-length-spec
                                    encrypt-function decrypt-function)
  (declare (ignorable key-length-spec))
  `(progn
     (defmethod encrypt-function ((cipher ,name))
       #',encrypt-function)
     (defmethod decrypt-function ((cipher ,name))
       #',decrypt-function)))

(defun generate-stream-cipher-forms (name key-length-spec crypt-function)
  (declare (ignorable key-length-spec))
  `(progn
     (defmethod encrypt-function ((cipher ,name))
       #',crypt-function)
     (defmethod decrypt-function ((cipher ,name))
       #',crypt-function)))

(defun %defcipher (name initargs)
  (let ((encrypt-function nil)
        (decrypt-function nil)
        (crypt-function nil)
        (block-length nil)
        (mode :block)
        (key-length-spec nil)
        (constructor nil))
    (declare (ignorable constructor))
    (loop for (arg value) in initargs
          do (case arg
               (:encrypt-function
                (if (not encrypt-function)
                    (setf encrypt-function value)
                    (error 'ironclad-error :format-control "Specified :ENCRYPT-FUNCTION multiple times.")))
               (:decrypt-function
                (if (not decrypt-function)
                    (setf decrypt-function value)
                    (error 'ironclad-error :format-control "Specified :DECRYPT-FUNCTION multiple times.")))
               (:crypt-function
                (if (not crypt-function)
                    (setf crypt-function value)
                    (error 'ironclad-error :format-control "Specified :CRYPT-FUNCTION multiple times.")))
               (:mode
                (setf mode value))
               (:block-length
                (cond
                  (block-length
                   (error 'ironclad-error :format-control "Specified :BLOCK-LENGTH multiple times."))
                  ((or (not (integerp value))
                       (not (plusp value)))
                   (error 'ironclad-error :format-control ":BLOCK-LENGTH must be a positive, integral number."))
                  (t
                   (setf block-length value))))
               (:key-length
                (cond
                  (key-length-spec
                   (error 'ironclad-error :format-control "Specified :KEY-LENGTH multiple times."))
                  ((not (consp value))
                   (error 'ironclad-error :format-control ":KEY-LENGTH value must be a list."))
                  ((and (not (eq :fixed (car value)))
                        (not (eq :variable (car value))))
                   (error 'ironclad-error :format-control "First element of :KEY-LENGTH spec must be either :FIXED or :VARIABLE."))
                  ((eq :fixed (car value))
                   (if (and (cdr value)
                            (every #'integerp (cdr value))
                            (every #'plusp (cdr value)))
                       (setf key-length-spec value)
                       ;;; FIXME: better error message
                       (error 'ironclad-error :format-control "bad :FIXED specification for :KEY-LENGTH.")))
                  ((eq :variable (car value))
                   (if (and (null (nthcdr 4 value))
                            (every #'integerp (cdr value))
                            (every #'plusp (cdr value))
                            (< (cadr value) (caddr value)))
                       (setf key-length-spec value)
                       (error 'ironclad-error :format-control "bad :VARIABLE specification for :KEY-LENGTH."))))))
          finally (cond
                    ((and (eq mode :block) key-length-spec encrypt-function decrypt-function)
                     (return
                       `(progn
                          ,(generate-common-cipher-methods name block-length key-length-spec)
                          ,(generate-block-cipher-forms name key-length-spec
                                                        encrypt-function decrypt-function))))
                    ((and (eq mode :stream) crypt-function key-length-spec)
                     (return
                       `(progn
                          ,(generate-common-cipher-methods name 1 key-length-spec)
                          ,(generate-stream-cipher-forms name key-length-spec crypt-function))))
                    (t
                     (error 'ironclad-error :format-control "Didn't specify all required fields for DEFCIPHER"))))))
