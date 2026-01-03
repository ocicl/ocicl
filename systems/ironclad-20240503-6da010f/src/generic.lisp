;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; generic.lisp -- generic function definitions

(in-package :crypto)


;;; Authenticated encryption

(defgeneric process-associated-data (mode data &key start end)
  (:documentation "Update the internal state of MODE with the contents of DATA
between START and END so that they are taken into consideration in the
authentication tag."))

(defgeneric produce-tag (mode &key tag tag-start)
  (:documentation "Return the authentication tag of the data processed by MODE
so far. If TAG is provided, the computed tag will be placed into TAG starting at
TAG-START."))


;;; Ciphers

(defgeneric verify-key (cipher key)
  (:documentation "Return T if KEY is a valid encryption key for CIPHER."))

(defgeneric schedule-key (cipher key)
  (:documentation "Schedule KEY for CIPHER, filling CIPHER with any
round keys, etc. needed for encryption and decryption."))

(defgeneric key-lengths (cipher)
  (:documentation "Return a list of possible lengths of a key for
CIPHER.  CIPHER may either be a cipher name as accepted by
MAKE-CIPHER or a cipher object as returned by MAKE-CIPHER.  NIL
is returned if CIPHER does not name a known cipher or is not a
cipher object."))

(defgeneric block-length (cipher)
  (:documentation "Return the number of bytes in an encryption or
decryption block for CIPHER.  CIPHER may either be a cipher name
as accepted by MAKE-CIPHER or a cipher object as returned by
MAKE-CIPHER.  NIL is returned if CIPHER does not name a known
cipher or is not a cipher object."))

(defgeneric encrypted-message-length (cipher mode length &optional handle-final-block)
  (:documentation "Return the length a message of LENGTH would be if it
were to be encrypted (decrypted) with CIPHER in MODE.  HANDLE-FINAL-BLOCK
indicates whether we are encrypting up to and including the final block
 (so that short blocks may be taken into account, if applicable).

Note that this computation may involve MODE's state."))

(defgeneric mode-crypt-functions (cipher mode)
  (:documentation "Returns two functions that perform encryption and
decryption, respectively, with CIPHER in MODE.  The lambda list of each
function is (IN OUT IN-START IN-END OUT-START HANDLE-FINAL-BLOCK).
HANDLE-FINAL-BLOCK is as in ENCRYPT and DECRYPT; the remaining parameters
should be self-explanatory.  Each function, when called, returns two values:
the number of octets processed from IN and the number of octets processed
from OUT.  Note that for some cipher modes, IN and OUT may be different."))

(defgeneric valid-mode-for-cipher-p (cipher mode))

(defgeneric encrypt (cipher plaintext ciphertext &key plaintext-start plaintext-end ciphertext-start handle-final-block &allow-other-keys)
  (:documentation "Encrypt the data in PLAINTEXT between PLAINTEXT-START and
PLAINTEXT-END according to CIPHER. Places the encrypted data in
CIPHERTEXT, beginning at CIPHERTEXT-START. Less data than
(- PLAINTEXT-END PLAINTEXT-START) may be encrypted, depending on the
alignment constraints of CIPHER and the amount of space available in
CIPHERTEXT."))

(defgeneric decrypt (cipher ciphertext plaintext &key ciphertext-start ciphertext-end plaintext-start handle-final-block &allow-other-keys)
  (:documentation "Decrypt the data in CIPHERTEXT between CIPHERTEXT-START and
CIPHERTEXT-END according to CIPHER. Places the decrypted data in
PLAINTEXT, beginning at PLAINTEXT-START. Less data than
(- CIPHERTEXT-END CIPHERTEXT-START) may be decrypted, depending on the
alignment constraints of CIPHER and the amount of space available in
PLAINTEXT."))


;;; Digests

(defgeneric digest-file (digest-spec pathname &rest args &key buffer start end digest digest-start)
  (:documentation "Return the digest of the contents of the file named by
PATHNAME using the algorithm DIGEST-NAME.

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START. DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)). An error
will be signaled if there is insufficient room in DIGEST.

If BUFFER is provided, the portion of BUFFER between START and END will be used
to hold data read from the stream."))

(defgeneric digest-stream (digest-spec stream &rest args &key buffer start end digest digest-start)
  (:documentation "Return the digest of the contents of STREAM using the algorithm
DIGEST-NAME.  STREAM-ELEMENT-TYPE of STREAM should be (UNSIGNED-BYTE 8).

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST.

If BUFFER is provided, the portion of BUFFER between START and END will
be used to hold data read from the stream."))

(defgeneric digest-sequence (digest-spec sequence &rest args &key start end digest digest-start)
  (:documentation "Return the digest of the subsequence of SEQUENCE
specified by START and END using the algorithm DIGEST-SPEC.
SEQUENCE can be any vector with an element-type of (UNSIGNED-BYTE 8).

If DIGEST is provided, the digest will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST."))

(defgeneric copy-digest (digester &optional copy)
  (:documentation "Return a copy of DIGESTER.  If COPY is not NIL, it
should be of the same type as DIGESTER and will receive the copied data,
rather than creating a new object.  The copy is a deep copy, not a
shallow copy as might be returned by COPY-STRUCTURE."))

(defgeneric update-digest (digester thing &key &allow-other-keys)
  (:documentation "Update the internal state of DIGESTER with THING.
The exact method is determined by the type of THING."))

(defgeneric produce-digest (digester &key digest digest-start)
  (:documentation "Return the hash of the data processed by
DIGESTER so far.

If DIGEST is provided, the hash will be placed into DIGEST starting at
DIGEST-START.  DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST."))

(defgeneric digest-length (digest)
  (:documentation "Return the number of bytes in a digest generated by DIGEST."))


;;; Key derivation functions

(defgeneric derive-key (kdf passphrase salt iteration-count key-length)
  (:documentation "Given a key derivation function object (produced by
MAKE-KDF), a PASSWORD, a SALT and an ITERATION-COUNT, return the password digest
as a byte array of length KEY-LENGTH."))


;;; Message authentication codes

(defgeneric update-mac (mac thing &key &allow-other-keys)
  (:documentation "Update the internal state of MAC with THING.
The exact method is determined by the type of THING."))

(defgeneric produce-mac (mac &key digest digest-start)
  (:documentation "Return the hash of the data processed by
MAC so far.

If DIGEST is provided, the hash will be placed into DIGEST starting at
DIGEST-START. DIGEST must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).
An error will be signaled if there is insufficient room in DIGEST."))


;;; Padding

(defgeneric add-padding-bytes (padding text start block-offset block-size)
  (:documentation "Add padding to the block in TEXT beginning at position
START.  Padding is done according to PADDING and assumes that text
prior to BLOCK-OFFSET is user-supplied.

This function assumes that the portion of TEXT from START to
 (+ START BLOCK-SIZE) is writable."))

(defgeneric count-padding-bytes (padding text start block-size)
  (:documentation "Return the number of bytes of padding in the block in
TEXT beginning at START.  The padding algorithm used for the block is
PADDING."))


;;; Pseudo random number generators

(defgeneric make-prng (name &key seed)
  (:documentation "Create a new NAME-type random number generator,
  seeding it from SEED.  If SEED is a pathname or namestring, read data
  from the indicated file; if it is sequence of bytes, use those bytes
  directly; if it is :RANDOM then read from /dev/random; if it
  is :URANDOM then read from /dev/urandom; if it is NIL then the
  generator is not seeded."))

(defgeneric prng-random-data (num-bytes prng)
  (:documentation "Generate NUM-BYTES bytes using PRNG"))

(defgeneric prng-reseed (seed prng)
  (:documentation "Reseed PRNG with SEED; SEED must
  be (PRNG-SEED-LENGTH PRNG) bytes long.")
  (:method (seed prng) (declare (ignorable seed prng))))

(defgeneric prng-seed-length (prng)
  (:documentation "Length of seed required by PRNG-RESEED.")
  (:method (prng) (declare (ignorable prng)) 0))


;;; Public key cryptography

(defgeneric make-public-key (kind &key &allow-other-keys)
  (:documentation "Return a public key of KIND, initialized according to
the specified keyword arguments."))

(defgeneric destructure-public-key (public-key)
  (:documentation "Return a plist containing the elements of a PUBLIC-KEY."))

(defgeneric make-private-key (kind &key &allow-other-keys)
  (:documentation "Return a private key of KIND, initialized according to
the specified keyword arguments."))

(defgeneric destructure-private-key (private-key)
  (:documentation "Return a plist containing the elements of a PRIVATE-KEY."))

(defgeneric generate-key-pair (kind &key num-bits &allow-other-keys)
  (:documentation "Generate a new key pair. The first returned
value is the secret key, the second value is the public key. If KIND
is :RSA or :DSA, NUM-BITS must be specified. If /kind/ is :ELGAMAL,
NUM-BITS must be specified unless COMPATIBLE-WITH-KEY is specified."))

(defgeneric make-signature (kind &key &allow-other-keys)
  (:documentation "Build the octet vector representing a signature
from its elements."))

(defgeneric destructure-signature (kind signature)
  (:documentation "Return a plist containing the elements of a SIGNATURE."))

(defgeneric generate-signature-nonce (key message &optional parameters)
  (:documentation "Generate a one-time use number for a signature (the k
parameter in DSA or ECDSA signatures)."))

(defgeneric sign-message (key message &key start end &allow-other-keys)
  (:documentation "Produce a key-specific signature of MESSAGE; MESSAGE is a
(VECTOR (UNSIGNED-BYTE 8)).  START and END bound the extent of the
message."))

(defgeneric verify-signature (key message signature &key start end &allow-other-keys)
  (:documentation "Verify that SIGNATURE is the signature of MESSAGE using
KEY.  START and END bound the extent of the message."))

(defgeneric make-message (kind &key &allow-other-keys)
  (:documentation "Build the octet vector representing a message
from its elements."))

(defgeneric destructure-message (kind message)
  (:documentation "Return a plist containing the elements of
an encrypted MESSAGE."))

(defgeneric encrypt-message (cipher-or-key message &key start end &allow-other-keys)
  (:documentation "Encrypt a MESSAGE with a CIPHER or a public KEY. START and
END bound the extent of the message. Returns a fresh octet vector."))

(defgeneric decrypt-message (cipher-or-key message &key start end n-bits &allow-other-keys)
  (:documentation "Decrypt a MESSAGE with a CIPHER or a private KEY. START and
END bound the extent of the message. Returns a fresh octet vector. N-BITS can be
used to indicate the expected size of the decrypted message."))

(defgeneric diffie-hellman (private-key public-key)
  (:documentation "Compute a shared secret using Alice's PRIVATE-KEY and Bob's PUBLIC-KEY"))
