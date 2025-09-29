(cl:in-package #:eclector.reader)

;;; Token Reading

(defmethod read-token
    ((client t) (input-stream t) (eof-error-p t) (eof-value t))
  (with-token-info (push-char (start-escape end-escape) finalize)
    (labels ((handle-char (char escapep)
               (declare (ignore escapep))
               (push-char char))
             (unterminated-single-escape (escape-char)
               (%recoverable-reader-error
                input-stream 'unterminated-single-escape-in-symbol
                :escape-char escape-char :report 'use-partial-symbol))
             (unterminated-multiple-escape (delimiter)
               (%recoverable-reader-error
                input-stream 'unterminated-multiple-escape-in-symbol
                :delimiter delimiter :report 'use-partial-symbol))
             (terminate-token ()
               (return-from read-token
                 (cond ((state-value client '*read-suppress*)
                        (note-skipped-input client input-stream
                                            (or *skip-reason* '*read-suppress*))
                        nil)
                       (t
                        (multiple-value-bind (token escape-ranges) (finalize)
                          (interpret-token client input-stream token escape-ranges)))))))
      (let ((readtable (state-value client 'cl:*readtable*)))
        (token-state-machine
         input-stream readtable handle-char start-escape end-escape
         unterminated-single-escape unterminated-multiple-escape
         terminate-token)))))

;;; Constituent traits
;;;
;;; Based on Table 2-8 in 2.1.4.2 Constituent Traits

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun trait->index (trait)
    (ecase trait
      (:invalid                 #b00000001)
      (:alphabetic              #b00000010)
      (:alphadigit              #b00000100)
      ((:plus-sign :minus-sign) #b00001000)
      ((:dot :decimal-point)    #b00010000)
      (:package-marker          #b00100000)
      (:ratio-marker            #b01000000)
      ((:short-float-exponent-marker :single-float-exponent-marker
        :double-float-exponent-marker :long-float-exponent-marker
        :float-exponent-marker)
       #b10000000)))

  (declaim (type (simple-array (unsigned-byte 8) 1) +constituent-traits+))
  (defvar +constituent-traits+
    (let* ((raw '((#\Backspace . :invalid)
                  (#\Tab       . :invalid)
                  (#\Newline   . :invalid)    (#\+          . (:alphabetic :plus-sign))
                  (#\Linefeed  . :invalid)    (#\-          . (:alphabetic :minus-sign))
                  (#\Page      . :invalid)    (#\.          . (:alphabetic :dot :decimal-point))
                  (#\Return    . :invalid)    (#\/          . (:alphabetic :ratio-marker))
                  (#\Space     . :invalid)    (#\:          . :package-marker)
                  (#\Rubout    . :invalid)    ("0123456789" . :alphadigit)
                  (#\!         . :alphabetic) ("Aa"         . :alphadigit)
                  (#\"         . :alphabetic) ("Bb"         . :alphadigit)
                  (#\#         . :alphabetic) ("Cc"         . :alphadigit)
                  (#\$         . :alphabetic) ("Dd"         . (:alphadigit :double-float-exponent-marker))
                  (#\%         . :alphabetic) ("Ee"         . (:alphadigit :float-exponent-marker))
                  (#\&         . :alphabetic) ("Ff"         . (:alphadigit :single-float-exponent-marker))
                  (#\'         . :alphabetic) ("Gg"         . :alphadigit)
                  (#\(         . :alphabetic) ("Hh"         . :alphadigit)
                  (#\)         . :alphabetic) ("Ii"         . :alphadigit)
                  (#\*         . :alphabetic) ("Jj"         . :alphadigit)
                  (#\,         . :alphabetic) ("Kk"         . :alphadigit)
                  (#\;         . :alphabetic) ("Ll"         . (:alphadigit :long-float-exponent-marker))
                  (#\<         . :alphabetic) ("Mm"         . :alphadigit)
                  (#\=         . :alphabetic) ("Nn"         . :alphadigit)
                  (#\>         . :alphabetic) ("Oo"         . :alphadigit)
                  (#\?         . :alphabetic) ("Pp"         . :alphadigit)
                  (#\@         . :alphabetic) ("Qq"         . :alphadigit)
                  (#\[         . :alphabetic) ("Rr"         . :alphadigit)
                  (#\\         . :alphabetic) ("Ss"         . (:alphadigit :short-float-exponent-marker))
                  (#\]         . :alphabetic) ("Tt"         . :alphadigit)
                  (#\^         . :alphabetic) ("Uu"         . :alphadigit)
                  (#\_         . :alphabetic) ("Vv"         . :alphadigit)
                  (#\`         . :alphabetic) ("Ww"         . :alphadigit)
                  (#\|         . :alphabetic) ("Xx"         . :alphadigit)
                  (#\~         . :alphabetic) ("Yy"         . :alphadigit)
                  ("{}"        . :alphabetic) ("Zz"         . :alphadigit)))
           (table (make-array 0 :adjustable t)))
      (loop for (characters . traits) in raw
            do (loop for character in (typecase characters
                                        (character (list characters))
                                        (string (coerce characters 'list)))
                     for code = (char-code character)
                     for pattern = (reduce #'logior (alexandria:ensure-list traits)
                                           :key #'trait->index)
                     do (adjust-array table (max (length table) (1+ code))
                                      :initial-element 0)
                        (setf (aref table code) pattern)))
      (make-array (length table) :element-type '(unsigned-byte 8)
                                 :adjustable nil
                                 :initial-contents table))))

(macrolet ((define-trait-predicate (trait)
             (let ((name (alexandria:symbolicate '#:char- trait '#:-p)))
               `(progn
                  (declaim (inline ,name))
                  (defun ,name (char)
                    (let ((code (char-code char)))
                      (when (< code ,(length +constituent-traits+))
                        (logtest ,(trait->index trait)
                                 (aref +constituent-traits+ code)))))))))
  (define-trait-predicate :invalid)
  (define-trait-predicate :float-exponent-marker))

;;; Token interpretation

(declaim (inline reader-float-format))
(defun reader-float-format (client &optional exponent-marker)
  (ecase exponent-marker
    ((nil #\e #\E)
     (let ((default-format (state-value client '*read-default-float-format*)))
       (case default-format
         (single-float 'single-float)
         (short-float 'short-float)
         (double-float 'double-float)
         (long-float 'long-float)
         ;; *READ-DEFAULT-FLOAT-FORMAT* may be some other type
         ;; specifier which the implementation chooses to allow.
         (t
          (if (subtypep default-format 'float)
              default-format
              (values nil default-format))))))
    ((#\f #\F) 'single-float)
    ((#\s #\S) 'short-float)
    ((#\d #\D) 'double-float)
    ((#\l #\L) 'long-float)))

(defmacro with-accumulators ((&rest specs) &body body)
  (loop for (name base) in specs
        for variable = (gensym (string name))
        collect `(,variable 0) into variables
        collect `(function ,name) into function-names
        collect `(,name (&optional char invalidatep)
                   (cond ((null char)
                          ,variable)
                         ((null ,variable)
                          nil)
                         (t
                          (let ((digit (digit-char-p char ,base)))
                            (cond ((not (null digit))
                                   (setf ,variable (+ (* ,variable ,base) digit)))
                                  (invalidatep
                                   (setf ,variable nil))
                                  (t
                                   nil))))))
          into functions
        finally (return `(let ,variables
                           (flet ,functions
                             (declare (dynamic-extent ,@function-names))
                             ,@body)))))

(defmethod interpret-token
    ((client t) (input-stream t) (token t) (escape-ranges t))
  (let ((read-base (state-value client '*read-base*))
        (readtable (state-value client 'cl:*readtable*)))
    (convert-according-to-readtable-case readtable token escape-ranges)
    (let* ((length (length token))
           (remaining-escape-ranges escape-ranges)
           (escape-range (first remaining-escape-ranges))
           (sign 1)
           (decimal-exponent 0)
           (exponent-sign 1)
           (exponent-marker nil)
           (position-package-marker-1 nil)
           (position-package-marker-2 nil)
           (index -1))
      (declare (type (or (eql -1) array-index) index))
      (with-accumulators ((decimal-mantissa 10)
                          (numerator* read-base) (denominator* read-base)
                          (exponent 10))
        ;; The NEXT function and the NEXT-COND macro handle fetching the
        ;; next character and returning a symbol and going to tag SYMBOL
        ;; in case of an escape and as the default successor state.
        (flet ((next ()
                 (incf index)
                 (if (= length index)
                     (values nil nil)
                     (values (aref token index)
                             (update-escape-ranges
                              index escape-range remaining-escape-ranges))))
               (symbol ()
                 (multiple-value-bind (token
                                       position-package-marker-1
                                       position-package-marker-2)
                     (check-symbol-token client input-stream token escape-ranges
                                         position-package-marker-1
                                         position-package-marker-2)
                   (values (interpret-symbol-token client input-stream token
                                                   position-package-marker-1
                                                   position-package-marker-2))))
               (make-float (exponentp)
                 (multiple-value-bind (type default-format)
                     (reader-float-format client exponent-marker)
                   (when (null type)
                     (multiple-value-bind (offset length)
                         (if exponentp
                             (values
                              (- (+ 1 (numeric-token-length (exponent))))
                              1)
                             (values 0 0))
                       (%recoverable-reader-error
                        input-stream 'invalid-default-float-format
                        :position-offset offset :length length
                        :exponent-marker exponent-marker
                        :float-format default-format
                        :report 'use-replacement-float-format))
                     (setf type 'single-float))
                   (let ((magnitude (* (decimal-mantissa)
                                       (expt 10 (- (if exponentp
                                                       (* exponent-sign (exponent))
                                                       0)
                                                   decimal-exponent)))))
                     (* sign (coerce magnitude type))))))
          (macrolet ((next-cond ((char-var &optional return-symbol-if-eoi
                                                     (colon-go-symbol t))
                                 &body clauses)
                       (alexandria:with-unique-names (escapep-var)
                         `(multiple-value-bind (,char-var ,escapep-var) (next)
                            (cond ,@(when return-symbol-if-eoi
                                      `(((null ,char-var)
                                         (return-from interpret-token (symbol)))))
                                  ((and ,char-var
                                        (not ,escapep-var)
                                        (char-invalid-p ,char-var))
                                   (%recoverable-reader-error
                                    input-stream 'invalid-constituent-character
                                    :position-offset -1
                                    :token (string ,char-var)
                                    :report 'replace-invalid-character)
                                   (setf (aref token index) #\_)
                                   (go symbol))
                                  (,escapep-var (go symbol))
                                  ,@(when colon-go-symbol
                                      `(((eql ,char-var #\:)
                                         (setf position-package-marker-1 index)
                                         (go symbol))))
                                  ,@clauses
                                  (t (go symbol)))))))
            (tagbody
             start
               ;; If we have a token of length 0, it must be a symbol in
               ;; the current package.
               (next-cond (char t)
                 ((eql char #\.)
                  (go dot))
                 ((not (null escape-ranges))
                  ;; Cannot be a potential number according to HyperSpec
                  ;; section 2.3.1.1.1 (Escape Characters and Potential
                  ;; Numbers).
                  (go symbol))
                 ((eql char #\+)
                  (go sign))
                 ((eql char #\-)
                  (setf sign -1)
                  (go sign))
                 ((decimal-mantissa char)
                  (numerator* char t)
                  (go decimal-integer))
                 ((numerator* char)
                  (go integer)))
             sign ; We have a sign, i.e., #\+ or #\-.
               ;; If a sign is all we have, it is a symbol.
               (next-cond (char t)
                 ((decimal-mantissa char)
                  (numerator* char t)
                  (go decimal-integer))
                 ((numerator* char)
                  (go integer))
                 ((eql char #\.)
                  (go sign-dot)))
             dot
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token
                    (if (null escape-ranges)
                        *consing-dot*
                        (symbol))))
                 ((not (null escape-ranges))
                  ;; Cannot be a potential number according to HyperSpec
                  ;; section 2.3.1.1.1 (Escape Characters and Potential
                  ;; Numbers).
                  (go symbol))
                 ((eql char #\.)
                  (go maybe-too-many-dots))
                 ((decimal-mantissa char)
                  (incf decimal-exponent)
                  (go float-no-exponent)))
             maybe-too-many-dots
               ;; According to HyperSpec section 2.3.3 (The Consing Dot),
               ;; a token consisting solely of multiple dots (more than
               ;; one dot, no escapes) is illegal.
               (next-cond (char)
                 ((not char)
                  (%recoverable-reader-error input-stream 'too-many-dots
                                             :position-offset (- (length token))
                                             :report 'treat-as-escaped)
                  (push (cons 0 (length token)) escape-ranges)
                  (return-from interpret-token (symbol)))
                 ((eql char #\.)
                  (go maybe-too-many-dots)))
             sign-dot ; sign decimal-point
               ;; If all we have is a sign followed by a dot, it must be a
               ;; symbol in the current package.
               (next-cond (char t)
                 ((decimal-mantissa char)
                  (incf decimal-exponent)
                  (go float-no-exponent)))
             decimal-integer ; [sign] decimal-digit+
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token
                    (alexandria:if-let ((value (numerator*)))
                      (* sign value)
                      (symbol))))
                 ((eql char #\.)
                  (go decimal-integer-final))
                 ((decimal-mantissa char)
                  (numerator* char t)
                  (go decimal-integer))
                 ((numerator* char)
                  (go integer))
                 ((eql char #\/)
                  (go ratio-start))
                 ((char-float-exponent-marker-p char)
                  (setf exponent-marker char)
                  (go float-exponent-start)))
             decimal-integer-final ; [sign] decimal-digit+ decimal-point
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token (* sign (decimal-mantissa))))
                 ((decimal-mantissa char)
                  (incf decimal-exponent)
                  (go float-no-exponent))
                 ((char-float-exponent-marker-p char)
                  (setf exponent-marker char)
                  (go float-exponent-start)))
             integer ; [sign] digit+ (At least one digit is not decimal)
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token (* sign (numerator*))))
                 ((numerator* char)
                  (go integer))
                 ((eql char #\/)
                  (go ratio-start)))
             ratio-start ; [sign] digit+ /
               (next-cond (char t)
                 ((denominator* char)
                  (go ratio)))
             ratio ; [sign] digit+ / digit+
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token
                    (alexandria:if-let ((numerator (numerator*)))
                      (let ((denominator (denominator*)))
                        (when (zerop denominator)
                          (%recoverable-reader-error
                           input-stream 'zero-denominator
                           :position-offset -1 :report 'replace-invalid-digit)
                          (setf denominator 1))
                        (* sign (/ numerator denominator)))
                      (symbol))))
                 ((denominator* char)
                  (go ratio)))
             float-no-exponent
               ;; [sign] decimal-digit* decimal-point decimal-digit+
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token (make-float nil)))
                 ((decimal-mantissa char)
                  (incf decimal-exponent)
                  (go float-no-exponent))
                 ((char-float-exponent-marker-p char)
                  (setf exponent-marker char)
                  (go float-exponent-start)))
             float-exponent-start
               ;; [sign] decimal-digit+ exponent-marker
               ;; or
               ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker
               (next-cond (char t)
                 ((eq char #\+)
                  (go float-exponent-sign))
                 ((eq char #\-)
                  (setf exponent-sign -1)
                  (go float-exponent-sign))
                 ((exponent char)
                  (go float-exponent)))
             float-exponent-sign
               ;; [sign] decimal-digit+ exponent-marker sign
               ;; or
               ;; [sign] decimal-digit* decimal-point decimal-digit+ exponent-marker sign
               (next-cond (char t)
                 ((exponent char)
                  (go float-exponent)))
             float-exponent
               ;; [sign] decimal-digit+ exponent-marker [sign] digit+
               ;; or
               ;; [sign] decimal-digit* decimal-point decimal-digit+
               ;; exponent-marker [sign] digit+
               (next-cond (char)
                 ((not char)
                  (return-from interpret-token (make-float t)))
                 ((exponent char)
                  (go float-exponent)))
             symbol
               ;; a sequence of characters denoting a valid symbol name,
               ;; except that the last character might be a package
               ;; marker.
               (next-cond (char t nil)
                 ((eq char #\:)
                  (cond ((null position-package-marker-1)
                         (setf position-package-marker-1 index))
                        ((null position-package-marker-2)
                         (setf position-package-marker-2 index))
                        (t
                         (%recoverable-reader-error
                          input-stream 'symbol-can-have-at-most-two-package-markers
                          :position-offset (- (- (length token) index)) :length 1
                          :token token :report 'treat-as-escaped)))
                  (go symbol))))))))))

;;; Symbol token interpretation

(defmethod interpret-symbol-token ((client t)
                                   (input-stream t)
                                   (token t)
                                   (position-package-marker-1 t)
                                   (position-package-marker-2 t))
  (let ((package-markers-end (or position-package-marker-2
                                 position-package-marker-1)))
    (flet ((interpret (package symbol internp)
             (interpret-symbol client input-stream package symbol internp)))
      (cond ((null position-package-marker-1)
             (interpret :current token t))
            ((zerop position-package-marker-1)
             ;; We use PACKAGE-MARKERS-END so we can handle ::foo
             ;; which can happen when recovering from errors.
             (interpret :keyword
                        (subseq token (1+ package-markers-end))
                        t))
            ((not (null position-package-marker-2))
             (interpret (subseq token 0 position-package-marker-1)
                        (subseq token (1+ position-package-marker-2))
                        t))
            (t
             (interpret (subseq token 0 position-package-marker-1)
                        (subseq token (1+ position-package-marker-1))
                        nil))))))

(defmethod check-symbol-token ((client t)
                               (input-stream t)
                               (token t)
                               (escape-ranges t)
                               (position-package-marker-1 t)
                               (position-package-marker-2 t))
  (let ((length (length token)))
    (cond ;; This signals an error for ":" and "::" but accepts ":||". "::" is
          ;; handled via TWO-PACKAGE-MARKERS-MUST-NOT-BE-FIRST.
          ((and (not escape-ranges)
                (case length
                  (1 (eql position-package-marker-1 0))
                  (2 (eql position-package-marker-2 1))))
           (%recoverable-reader-error
            input-stream 'symbol-name-must-not-be-only-package-markers
            :position-offset (- (length token))
            :token token :report 'treat-as-escaped)
           (setf position-package-marker-1 nil
                 position-package-marker-2 nil))
          ;; When there are two package markers, they must be adjacent and not
          ;; at the beginning of the token.
          ((and position-package-marker-2
                (/= position-package-marker-1
                    (1- position-package-marker-2)))
           (%recoverable-reader-error
            input-stream 'two-package-markers-must-be-adjacent
            :position-offset (+ (- (length token)) position-package-marker-1)
            :length (1+ (- position-package-marker-2
                           position-package-marker-1))
            :token token :report 'treat-as-escaped)
           (setf position-package-marker-2 nil))
          ((and position-package-marker-2
                (= position-package-marker-1 0))
           (%recoverable-reader-error
            input-stream 'two-package-markers-must-not-be-first
            :position-offset (- (length token)) :length 2
            :token token :report 'treat-as-escaped))
          ;; The symbol token must not end with a package marker (or
          ;; two package markers).
          ((let ((package-markers-end (or position-package-marker-2
                                          position-package-marker-1)))
             (and package-markers-end
                  (= package-markers-end (1- length))
                  (not (find length escape-ranges :test #'= :key #'car))))
           (%recoverable-reader-error
            input-stream 'symbol-name-must-not-end-with-package-marker
            :position-offset -1
            :token token :report 'treat-as-escaped)
           (setf position-package-marker-1 nil
                 position-package-marker-2 nil))))
  (values token position-package-marker-1 position-package-marker-2))

(defmethod interpret-symbol ((client t)
                             (input-stream t)
                             (package-indicator null)
                             (symbol-name t)
                             (internp t))
  (make-symbol symbol-name))

;;; INTERPRET-SYMBOL for interned symbols
;;;
;;; Since many things can go wrong here, reporting of and recovering
;;; from individual errors is handled in separate functions.

(defun accept-symbol ()
  (declare (notinline read))
  (let ((stream *query-io*))
    (format stream "Enter symbol (not evaluated): ")
    (finish-output stream)
    (list (read stream))))

(defun accept-package-name ()
  (let ((stream *query-io*))
    (format stream "Enter package name (case-sensitive, not evaluated): ")
    (finish-output stream)
    (list (read-line stream))))

(defun package-does-not-exist (input-stream package-indicator symbol-name internp)
  (restart-case
      (let ((package-length (length package-indicator))
            (symbol-length (length symbol-name)))
        (%reader-error input-stream 'package-does-not-exist
                       :position-offset (- (+ package-length ; inaccurate when escapes are present
                                              (if internp 2 1)
                                              symbol-length))
                       :length package-length
                       :package-name package-indicator))
    (recover ()
      :report (lambda (stream)
                (format-recovery-report stream 'use-uninterned-symbol
                                        package-indicator symbol-name))
      (values (make-symbol symbol-name) 'symbol))
    (use-value (symbol)
      :report (lambda (stream)
                (format-recovery-report stream 'use-replacement-symbol
                                        package-indicator symbol-name))
      :interactive accept-symbol
      (values symbol 'symbol))
    (use-package (package)
      :report (lambda (stream)
                (format-recovery-report stream 'use-replacement-package
                                        package-indicator))
      :interactive accept-package-name
      (values package (if (stringp package) :package-name 'package)))))

(defun symbol-does-not-exist (input-stream package symbol-name)
  (restart-case
      (%reader-error input-stream 'symbol-does-not-exist
                     :position-offset (- (length symbol-name))
                     :package package :symbol-name symbol-name)
    (recover ()
      :report (lambda (stream)
                (format-recovery-report stream 'inject-nil))
      nil)
    (use-value (symbol)
      :report (lambda (stream)
                (format-recovery-report stream 'use-replacement-symbol
                                        package symbol-name))
      :interactive accept-symbol
      symbol)
    (intern ()
      :report (lambda (stream)
                (format-recovery-report stream 'intern package symbol-name))
      (intern symbol-name package))))

(defun symbol-is-not-external (input-stream package symbol-name symbol)
  (restart-case
      (%reader-error input-stream 'symbol-is-not-external
                     :position-offset (- (length symbol-name))
                     :package package :symbol-name symbol-name)
    (recover ()
      :report (lambda (stream)
                (format-recovery-report stream 'use-anyway
                                        package symbol-name))
      symbol)
    (use-value (symbol)
      :report (lambda (stream)
                (format-recovery-report stream 'use-replacement-symbol
                                        package symbol-name))
      :interactive accept-symbol
      symbol)
    (use-anyway ()
      :report (lambda (stream)
                (format-recovery-report stream 'use-anyway
                                        package symbol-name))
      symbol)))

(defmethod interpret-symbol ((client t)
                             (input-stream t)
                             (package-indicator t)
                             (symbol-name t)
                             (internp t))
  (prog (package symbol)
   package
     (setf package (case package-indicator
                     (:current (state-value client '*package*))
                     (:keyword (find-package "KEYWORD"))
                     (t        (or (find-package package-indicator)
                                   (multiple-value-bind (value kind)
                                       (package-does-not-exist
                                        input-stream
                                        package-indicator symbol-name internp)
                                     (ecase kind
                                       (symbol
                                        (return value))
                                       (package
                                        value)
                                       (:package-name
                                        (setf package-indicator value)
                                        (go package))))))))
   symbol
     (setf symbol (if internp
                      (intern symbol-name package)
                      (multiple-value-bind (symbol status)
                          (find-symbol symbol-name package)
                        (cond ((null status)
                               (symbol-does-not-exist
                                input-stream package symbol-name))
                              ((eq status :internal)
                               (symbol-is-not-external
                                input-stream package symbol-name symbol))
                              (t
                               symbol)))))
   done
     (return symbol)))
