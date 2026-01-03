;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; make-cipher.lisp -- all the machinery necessary for MAKE-CIPHER

(in-package :crypto)


;;; Validity of modes for ciphers.

(defmethod valid-mode-for-cipher-p (cipher mode)
  nil)

(defun valid-mode-for-block-cipher-p (mode)
  (member mode '(:ecb :cbc :ofb :cfb :cfb8 :ctr
                 ecb cbc ofb cfb cfb8 ctr)))

(defmethod valid-mode-for-cipher-p ((cipher 128-byte-block-mixin) mode)
  (valid-mode-for-block-cipher-p mode))

(defmethod valid-mode-for-cipher-p ((cipher 64-byte-block-mixin) mode)
  (valid-mode-for-block-cipher-p mode))

(defmethod valid-mode-for-cipher-p ((cipher 32-byte-block-mixin) mode)
  (valid-mode-for-block-cipher-p mode))

(defmethod valid-mode-for-cipher-p ((cipher 16-byte-block-mixin) mode)
  (valid-mode-for-block-cipher-p mode))

(defmethod valid-mode-for-cipher-p ((cipher 8-byte-block-mixin) mode)
  (valid-mode-for-block-cipher-p mode))

(defmethod valid-mode-for-cipher-p ((cipher stream-cipher) mode)
  (or (eq mode :stream) (eq mode 'stream)))

(defun make-mode-for-cipher (cipher mode &optional initialization-vector padding)
  (let ((block-length (block-length cipher)))
    (flet ((make-extended-mode (mode-class)
             (declare (ignorable mode-class))
             (unless initialization-vector
               (error 'initialization-vector-not-supplied
                      :mode mode))
             (unless (typep initialization-vector '(vector (unsigned-byte 8)))
               (error 'type-error
                      :datum initialization-vector
                      :expected-type '(vector (unsigned-byte 8))))
             (unless (= (length initialization-vector) block-length)
               (error 'invalid-initialization-vector
                      :cipher (class-name (class-of cipher))
                      :block-length block-length))
             (make-instance mode-class
                            :initialization-vector (copy-seq initialization-vector)
                            :padding padding
                            :cipher cipher)))
    (case mode
      ((:ecb ecb)
       (make-instance 'ecb-mode :cipher cipher :padding padding))
      ((:cbc cbc)
       (make-extended-mode 'cbc-mode))
      ((:ofb ofb)
       (make-extended-mode 'ofb-mode))
      ((:cfb cfb)
       (make-extended-mode 'cfb-mode))
      ((:cfb8 cfb8)
       (make-extended-mode 'cfb8-mode))
      ((:ctr ctr)
       (make-extended-mode 'ctr-mode))
      ((:stream stream)
       (make-instance 'stream-mode :cipher cipher))
      (t
       (error 'unsupported-mode :mode mode))))))


;;; CLOS methods.

;;; This is where all the work gets done.
(defmethod shared-initialize :after ((cipher cipher) slot-names
                                     &rest initargs
                                     &key (key nil key-p) (mode nil mode-p)
                                       (padding nil padding-p)
                                       (initialization-vector nil iv-p)
                                     &allow-other-keys)
  (declare (ignorable padding padding-p iv-p initargs))
  ;; We always want to check that we have a valid key when we initialize
  ;; a cipher (what good is an unkeyed cipher?).  We want to check for
  ;; a valid key upon reinitialization only if one has been provided.
  (when (or (not (initialized-p cipher)) key-p)
    (schedule-key cipher key))
  ;; Check that the mode is valid for the cipher we are initializing.
  (when (and (or (not (initialized-p cipher)) mode-p)
             (not (valid-mode-for-cipher-p cipher mode)))
    ;; FIXME: (CLASS-NAME (CLASS-OF ...)) is not quite right.
    (error 'unsupported-mode :mode mode :cipher (class-name (class-of cipher))))
  (when (and iv-p
             (not mode-p))
    (setq mode (mode-name cipher)))
  (when (or mode-p iv-p padding-p)
    (setf (slot-value cipher 'mode-name) mode)
    (let ((mode-instance (make-mode-for-cipher cipher mode initialization-vector padding)))
      (setf (mode cipher) mode-instance)))
  cipher)

(defmethod initialize-instance :after ((cipher cipher)
                                       &rest initargs
                                       &key key mode padding tweak
                                       initialization-vector
                                       &allow-other-keys)
  (declare (ignore key mode padding initialization-vector initargs tweak))
  (setf (initialized-p cipher) t)
  cipher)

(defun %block-cipher-p (info)
  (not (= (%block-length info) 1)))

(defun find-cipher-or-lose (name)
  (let ((cipher-info (%find-cipher name)))
    (unless cipher-info
      (error 'unsupported-cipher :name name))
    cipher-info))

(defun validate-parameters-for-cipher-info (cipher-info mode padding)
  (cond
    ((%block-cipher-p cipher-info)
     ;; Block cipher.
     (when (or (eq mode 'stream) (eq mode :stream))
       (error 'unsupported-mode :cipher (cipher cipher-info) :mode mode)))
    (t
     ;; Stream cipher.
     (unless (or (eq mode 'stream) (eq mode :stream))
       (error 'unsupported-mode :cipher (cipher cipher-info) :mode mode))
     (when padding
       (error 'ironclad-error :format-control "padding is not supported for stream ciphers"))))
  cipher-info)

(defun make-cipher (name &key key mode initialization-vector padding tweak)
  "Return a cipher object using algorithm NAME with KEY in the
specified MODE.  If MODE requires an initialization vector, it
must be provided as INITIALIZATION-VECTOR; otherwise, the
INITIALIZATION-VECTOR argument is ignored.  If the cipher can
can use a tweak, it can be provided with the TWEAK argument."
  (let ((cipher-info (find-cipher-or-lose name)))
    (validate-parameters-for-cipher-info cipher-info mode padding)
    (make-instance (%class-name cipher-info) :key key :mode mode
                   :initialization-vector initialization-vector
                   :padding padding
                   :tweak tweak)))

;;; Many implementations can optimize MAKE-INSTANCE of a constant class
;;; name; try to enable that optimization by converting MAKE-CIPHER to
;;; such a form.
(define-compiler-macro make-cipher (&whole form &environment env
                                           name
                                           &rest keys
                                           &key key mode initialization-vector padding tweak &allow-other-keys)
  (declare (ignore env keys))
  (cond
   ((or (keywordp name)
        (and (quotationp name) (symbolp name)))
    (let ((cipher-info (ignore-errors
                         (validate-parameters-for-cipher-info
                          (find-cipher-or-lose (unquote name))
                          (unquote mode)
                          padding))))
      (if cipher-info
          `(make-instance ',(%class-name cipher-info)
                          :key ,key :mode ,mode
                          :initialization-vector ,initialization-vector
                          :padding ,padding
                          :tweak ,tweak)
          form)))
   (t form)))
