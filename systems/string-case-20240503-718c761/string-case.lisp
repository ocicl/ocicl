;;; Implementing an efficient string= case in Common Lisp
;;;
;;; 2015-11-15: Defknown don't have explicit-check in SBCL 1.3.0
;;;  Remove the declaration.  It's never useful the way we use
;;;  numeric-char=.
;;;
;;; 2015-11-15: Make this a real ASDF system for Xach
;;;  I copied the system definition from Quicklisp and mangled as
;;;  necessary.
;;;
;;; 2010-06-30: Tiny bugfix
;;;  Widen the type declarations inside cases to allow vectors that
;;;  have a length that's shorter than the total size (due to fill-
;;;  pointers).

;;;
;;;# Introduction
;;;
;;; In `<http://neverfriday.com/blog/?p=10>', OMouse asks how
;;; best to implement a `string= case' (in Scheme). I noted that
;;; naively iterating through the cases with `string=' at runtime
;;; is suboptimal. Seeing the problem as a simplistic pattern
;;; matching one makes an efficient solution obvious.
;;; Note that, unlike Haskell, both Scheme and CL have random-
;;; access on strings in O(1), something which I exploit to
;;; generate better code.
;;;
;;; This is also a pbook.el file (the pdf can be found at
;;; `<http://www.discontinuity.info/~pkhuong/string-case.pdf>' ).
;;; I'm new at this not-quite-illiterate programming thing, so
;;; please bear with me (: I'm also looking for comments on the
;;; formatting. I'm particularly iffy with the way keywords look
;;; like. It just looks really fuzzy when you're not really zoomed
;;; in (or reading it on paper).

;;; I usually don't use packages for throw-away code, but this looks
;;; like it could be useful to someone.

(cl:defpackage #:string-case
  (:use    #:cl #+sbcl #:sb-c #+sbcl #:sb-vm)
  (:export #:string-case))

(cl:in-package #:string-case)

;;;# Some utility code

(defun split (list &key (test 'eql) (key 'identity))
  "Splits input list into sublists of elements
   whose elements are all such that (key element)
   are all test.
   It's assumed that test and key form an equality class.
   (This is similar to groupBy)"
  (when list
    (let* ((lists ())
           (cur-list (list (first list)))
           (cur-key  (funcall key (first list))))
      (dolist (elt (rest list) (nreverse (cons (nreverse cur-list)
                                               lists)))
        (let ((new-key (funcall key elt)))
          (if (funcall test cur-key new-key)
              (push elt cur-list)
              (progn
                (push (nreverse cur-list) lists)
                (setf cur-list (list elt)
                      cur-key  new-key))))))))

(defun iota (n)
  (loop for i below n collect i))

(defun hash-table->list (table &key (keep-keys t) (keep-values t))
  "Saves the keys and/or values in table to a list.
   As with hash table iterating functions, there is no
   implicit ordering."
  (let ((list ()))
    (maphash (cond ((and keep-keys
                         keep-values)
                    (lambda (k v)
                      (push (cons k v) list)))
                   (keep-keys
                    (lambda (k v)
                      (declare (ignore v))
                      (push k list)))
                   (keep-values
                    (lambda (k v)
                      (declare (ignore k))
                      (push v list))))
             table)
    list))

(defun all-equal (list &key (key 'identity) (test 'eql))
  (if (or (null list)
          (null (rest list)))
      t
      (let ((first-key (funcall key (first list))))
        (every (lambda (element)
                 (funcall test first-key
                          (funcall key element)))
               (rest list)))))

(defun split-at (list n)
  "Split list in k lists of n elements (or less for the last list)"
  (declare (type (and fixnum (integer (0))) n))
  (let ((lists    '())
        (cur-list '())
        (counter  0))
    (declare (type (and fixnum unsigned-byte) counter))
    (dolist (elt list (nreverse (if cur-list
                                    (cons (nreverse cur-list)
                                          lists)
                                    lists)))
      (push elt cur-list)
      (when (= (incf counter) n)
        (push (nreverse cur-list) lists)
        (setf cur-list '()
              counter   0)))))

;;;# The string matching compiler per se
;;;
;;; I use special variables here because I find that
;;; preferable to introducing noise everywhere to thread
;;; these values through all the calls, especially
;;; when `*no-match-form*' is only used at the very end.

(defparameter *input-string* nil
  "Symbol of the variable holding the input string")

(defparameter *no-match-form* nil
  "Form to insert when no match is found.")

;;; The basic idea of the pattern matching process here is
;;; to first discriminate with the input string's length;
;;; once that is done, it is very easy to safely use random
;;; access until only one candidate string (pattern) remains.
;;; However, even if we determine that only one case might be
;;; a candidate, it might still be possible for another string
;;; (not in the set of cases) to match the criteria. So we also
;;; have to make sure that *all* the indices match. A simple
;;; way to do this would be to emit the remaining checks at the
;;; every end, when only one candidate is left. However, that
;;; can result in a lot of duplicate code, and some useless
;;; work on mismatches. Instead, the code generator always
;;; tries to find (new) indices for which all the candidates
;;; left in the branch share the same character, and then emits
;;; a guard, checking the character at that index as soon as possible.

;;; In my experience, there are two main problems when writing
;;; pattern matchers: how to decide what to test for at each
;;; fork, and how to ensure the code won't explode exponentially.
;;; Luckily, for our rather restricted pattern language (equality
;;; on strings), patterns can't overlap, and it's possible to guarantee
;;; that no candidate will ever be possible in both branches of a
;;; fork.

;;; Due to the the latter guarantee, we have a simple fitness
;;; measure for tests: simply maximising the number of
;;; candidates in the smallest branch will make our search tree
;;; as balanced as possible. Of course, we don't know whether
;;; the subtrees will be balanced too, but I don't think it'll
;;; be much of an issue.

;;; Note that, if we had access, whether via annotations or profiling,
;;; to the probability of each case, the situation would be very
;;; different. In fact, on a pipelined machine where branch
;;; mispredictions are expensive, an unbalanced tree will yield
;;; better expected runtimes. There was a very interesting and rather
;;; sophisticated Google lecture on that topic on Google video
;;; (the speaker used markov chains to model dynamic predictors,
;;; for example), but I can't seem to find the URL.

;;; TODO: Find bounds on the size of the code!

(defun find-best-split (strings to-check)
  "Iterate over all the indices left to check to find
   which index (and which character) to test for equality
   with, keeping the ones which result in the most balanced
   split."
  (flet ((evaluate-split (i char)
           "Just count all the matches and mismatches"
           (let ((=  0)
                 (/= 0))
             (dolist (string strings (min = /=))
               (if (eql (aref string i) char)
                   (incf =)
                   (incf /=)))))
         (uniquify-chars (chars)
           "Only keep one copy of each char in the list"
           (mapcar 'first (split (sort chars 'char<)))))
    (let ((best-split 0)            ; maximise size of smallest branch
          (best-posn  nil)
          (best-char  nil))
      (dolist (i to-check (values best-posn best-char))
        (dolist (char (uniquify-chars (mapcar (lambda (string)
                                                (aref string i))
                                              strings)))
          (let ((Z (evaluate-split i char)))
            (when (> Z best-split)
              (setf best-split Z
                    best-posn  i
                    best-char  char))))))))

;;; We sometimes have to execute sequences of checks for
;;; equality. The natural way to express this is via a
;;; sequence of checks, wrapped in an `and'. However, that
;;; translates to a sequence of conditional branches, predicated
;;; on very short computations. On (not so) modern architectures,
;;; it'll be faster to coalesce a sequence of such checks together
;;; as straightline code (e.g. via `or' of `xor'), and only branch
;;; at the very end. The code doesn't become much more complex,
;;; and benchmarks have shown it to be beneficial (giving a speed
;;; up of 2-5% for both predictable and unpredictable workloads,
;;; on a Core 2).

;;; Benchmarks (and experience) have shown that, instead of executing
;;; a cascade of comparison/conditional branch, it's slightly
;;; faster, both for predictable and unpredictable workloads,
;;; to `or' together a bunch of comparisons (e.g. `xor'). On a Core 2
;;; processor, it seems that doing so for sequences of around 4
;;; comparisons is the sweetspot. On perfectly predictable input,
;;; aborting early (on the first check) saves as much time as
;;; the 4 test/conditional branch add, compared to a sequence of
;;; `xor' and `or'. 

;;; Numeric char= abstracts out the xor check, and, on SBCL,
;;; is replaced by a short assembly sequence when the first
;;; argument is a constant. The declared return type is then
;;; wider than strictly necessary making it fit in a machine
;;; register, but not as a fixnum ensures that the compiler
;;; won't repeatedly convert the values to fixnums, when all
;;; we'll do is `or' them together and check for zero-ness.
;;; This function is the only place where the macro isn't
;;; generic over the elements stored in the cases. It shouldn't
;;; be too hard to implement a numeric-eql, which would
;;; restore genericity to the macro, while keeping the 
;;; speed-up.

#- (and sbcl (or x86 x86-64))
(declaim (inline numeric-char=)
         (ftype (function (character character)
                          (values (and unsigned-byte fixnum)))
                numeric-char=))
(defun numeric-char= (x y)
  (declare (type character x y))
  (logxor (char-code x)
          (char-code y)))

#+ (and sbcl (or x86 x86-64))
(progn
  (defknown numeric-char= (character character)
      (unsigned-byte #. (1- sb-vm:n-machine-word-bits))
      (movable foldable flushable))

  (define-vop (numeric-char=)
    (:args (x :scs (sb-vm::character-reg sb-vm::character-stack)
              :target r
              :load-if (not (location= x r))))
    (:info y)
    (:arg-types (:constant character) character)
    (:results (r :scs (sb-vm::unsigned-reg)
                 :load-if (not (location= x r))))
    (:result-types sb-vm::unsigned-num)
    (:translate numeric-char=)
    (:policy :fast-safe)
    (:note "inline constant numeric-char=")
    (:generator 1
       (move r x)
       (sb-vm::inst #:xor r (char-code y)))))

;;; At each step, we may be able to find positions for which
;;; there can only be one character. If we emit the check for
;;; these positions as soon as possible, we avoid duplicating
;;; potentially a lot of code. Since benchmarks have shown
;;; it to be useful, this function implements the checks
;;; as a series of (zerop (logior (numeric-char= ...)...)),
;;; if there is more than one such check to emit.

(defun emit-common-checks (strings to-check)
  (labels ((emit-char= (pairs)
             (mapcar (lambda (pair)
                       (destructuring-bind (posn . char)
                           pair
                         `(numeric-char= ,char
                                         (aref ,*input-string* ,posn))))
                     pairs))
           (emit-checking-form (common-chars)
             (when common-chars
               (let ((common-chars (sort common-chars '< :key 'car)))
                 #+ (and) `(and ,@(mapcar
                                   (lambda (chunk)
                                     (if (null (rest chunk))
                                         (destructuring-bind ((posn . char))
                                             chunk
                                           `(eql ,char
                                                 (aref ,*input-string* ,posn)))
                                         `(zerop
                                           (logior ,@(emit-char= chunk)))))
                                   (split-at common-chars 4)))
                 #+ (or) `(and ,@(mapcar
                                  (lambda (pair)
                                    (destructuring-bind (posn . char)
                                        pair
                                      `(eql ,char
                                            (aref ,*input-string* ,posn))))
                                  common-chars))))))
    (let ((common-chars  ())
          (left-to-check ()))
      (dolist (posn to-check (values (emit-checking-form common-chars)
                                     (nreverse           left-to-check)))
        (if (all-equal strings :key (lambda (string)
                                      (aref string posn)))
            (push (cons posn (aref (first strings) posn))
                  common-chars)
            (push posn left-to-check))))))

;;; The driving function: First, emit any test that is
;;; common to all the candidates. If there's only one
;;; candidate, then we just have to execute the body;
;;; if not, we look for the `best' test and emit the
;;; corresponding code: execute the test, and recurse
;;; on the candidates that match the test and on those
;;; that don't.

(defun make-search-tree (strings bodies to-check)
  (multiple-value-bind (guard to-check)
      (emit-common-checks strings to-check)
    (if (null (rest strings))
        (progn
          (assert (null to-check)) ; there shouldn't be anything left to check
          (if guard
              `(if ,guard
                   (progn ,@(first bodies))
                   ,*no-match-form*)
              `(progn ,@(first bodies))))
        (multiple-value-bind (posn char)
            (find-best-split strings to-check)
          (assert posn) ; this can only happen if all strings are equal
          (let ((=strings  ())
                (=bodies   ())
                (/=strings ())
                (/=bodies  ()))
            (loop
               for string in strings
               for body   in bodies
               do (if (eql char (aref string posn))
                      (progn
                        (push string =strings)
                        (push body   =bodies))
                      (progn
                        (push string /=strings)
                        (push body   /=bodies))))
            (let ((tree `(if (eql ,char (aref ,*input-string* ,posn))
                             ,(make-search-tree  =strings   =bodies
                                                 (remove posn to-check))
                             ,(make-search-tree /=strings /=bodies
                                                to-check))))
              (if guard
                  `(if ,guard
                       ,tree
                       ,*no-match-form*)
                  tree)))))))

;;; Finally, we can glue it all together.
;;; To recapitulate, first, dispatch on string
;;; length, then execute a search tree for the
;;; few candidates left, and finally make sure
;;; the input string actually matches the one 
;;; candidate left at the leaf.

(defun emit-string-case (cases input-var no-match)
  (flet ((case-string-length (x)
           (length (first x))))
    (let ((*input-string*  input-var)
          (*no-match-form* no-match)
          (cases-lists     (split (sort cases '<
                                        :key #'case-string-length)
                                  :key #'case-string-length)))
      `(locally (declare (type vector ,input-var))
         (case (length ,input-var)
           ,@(loop for cases in cases-lists
                   for length = (case-string-length (first cases))
                   collect `((,length)
                             ;; arrays with fill pointers expose the total length
                             ;; in their type, not the position of the fill-pointer.
                             ;; The type below only applies to simple-arrays.
                             (locally (declare (type (or (not simple-array)
                                                         (simple-array * (,length)))
                                                     ,input-var))
                               ,(make-search-tree (mapcar 'first cases)
                                                  (mapcar 'rest  cases)
                                                  (iota length)))))
           (t ,no-match))))))

;;; Just wrapping the previous function in a macro,
;;; and adding some error checking (the rest of the code
;;; just assumes there won't be duplicate patterns).
;;; Note how we use a local function instead of passing
;;; the default form directly. This can save a lot on
;;; code size, especially when the default form is
;;; large.

(defmacro string-case ((string &key (default '(error "No match")))
                       &body cases)
  "(string-case (string &key default)
     case*)
   case ::= string form*
          | t      form*
   Where t is the default case."
  (let ((cases-table (make-hash-table :test 'equal)))
    "Error checking cruft"
    (dolist (case cases)
      (assert (typep case '(cons (or string (eql t)))))
      (let ((other-case (gethash (first case) cases-table)))
        (if other-case
            (warn "Duplicate string-case cases: ~A -> ~A or ~A~%"
                  (first case)
                  (rest other-case)
                  (rest case))
            (setf (gethash (first case) cases-table)
                  (rest case)))))
    (let ((input-var    (gensym "INPUT"))
          (default-fn   (gensym "ON-ERROR"))
          (default-body (gethash t cases-table (list default))))
      `(let ((,input-var ,string))
         (flet ((,default-fn ()
                  ,@default-body))
           ,(emit-string-case (progn
                                (remhash t cases-table)
                                (hash-table->list cases-table))
                              input-var
                              `(,default-fn)))))))

;;; Demo output (downcased):
#+bad #+reader #+hack

(macroexpand-1 '(string-case ("foobar")
                 ("" 'empty)
                 ("foo" 'foo)
                 ("fob" 'fob)
                 ("foobar" 'hit)
                 (t     'default)))
=>
(let ((#:input2056 "foobar"))
  (flet ((#:on-error2057 ()
           'default))
    (case (length #:input2056)
      ((0) (locally (declare (type (array * (0)) #:input2056))
             (progn 'empty)))
      ((3)
       (locally (declare (type (array * (3)) #:input2056))
        (if (and (zerop (logior (numeric-char= #\f (aref #:input2056 0))
                                (numeric-char= #\o (aref #:input2056 1)))))
            (if (eql #\b (aref #:input2056 2))
                (progn 'fob)
                (if (eql #\o (aref #:input2056 2))
                    (progn 'foo)
                    (#:on-error2057)))
         (#:on-error2057))))
      ((6) (locally (declare (type (array * (6)) #:input2056))
             (if (and (zerop (logior (numeric-char= #\f (aref #:input2056 0))
                                     (numeric-char= #\o (aref #:input2056 1))
                                     (numeric-char= #\o (aref #:input2056 2))
                                     (numeric-char= #\b (aref #:input2056 3))))
                      (zerop (logior (numeric-char= #\a (aref #:input2056 4))
                                     (numeric-char= #\r (aref #:input2056 5)))))
                 (progn 'hit)
                 (#:on-error2057))))
      (t (#:on-error2057)))))

;;; A disassembly of the output on SBCL/x86-64
#+nil
(disassemble (lambda (x)
               (declare (optimize speed)
                        (type simple-base-string x))
               (string-case (x)
                 ("" 'empty)
                 ("foo" 'foo)
                 ("fob" 'fob)
                 ("foobar" 'hit)
                 (t     'default))))

; 0302B9C0:       488B4AF9         MOV RCX, [RDX-7]           ; no-arg-parsing entry point
;      9C4:       4885C9           TEST RCX, RCX
;      9C7:       7513             JNE L1
;      9C9:       488B1560FFFFFF   MOV RDX, [RIP-160]         ; 'EMPTY
;      9D0: L0:   488D65F0         LEA RSP, [RBP-16]
;      9D4:       F8               CLC
;      9D5:       488B6DF8         MOV RBP, [RBP-8]
;      9D9:       C20800           RET 8
;      9DC: L1:   4883F918         CMP RCX, 24
;      9E0:       7468             JEQ L4
;      9E2:       4883F930         CMP RCX, 48
;      9E6:       7405             JEQ L2
;      9E8:       E9AA000000       JMP L7
;      9ED: L2:   480FB64201       MOVZX RAX, BYTE PTR [RDX+1]
;      9F2:       4883F066         XOR RAX, 102
;      9F6:       488BC8           MOV RCX, RAX
;      9F9:       480FB64202       MOVZX RAX, BYTE PTR [RDX+2]
;      9FE:       4883F06F         XOR RAX, 111
;      A02:       4809C1           OR RCX, RAX
;      A05:       480FB64203       MOVZX RAX, BYTE PTR [RDX+3]
;      A0A:       4883F06F         XOR RAX, 111
;      A0E:       4809C1           OR RCX, RAX
;      A11:       480FB64204       MOVZX RAX, BYTE PTR [RDX+4]
;      A16:       4883F062         XOR RAX, 98
;      A1A:       4809C1           OR RCX, RAX
;      A1D:       4885C9           TEST RCX, RCX
;      A20:       7526             JNE L3
;      A22:       480FB64205       MOVZX RAX, BYTE PTR [RDX+5]
;      A27:       4883F061         XOR RAX, 97
;      A2B:       488BC8           MOV RCX, RAX
;      A2E:       480FB64206       MOVZX RAX, BYTE PTR [RDX+6]
;      A33:       4883F072         XOR RAX, 114
;      A37:       4809C1           OR RCX, RAX
;      A3A:       4885C9           TEST RCX, RCX
;      A3D:       7509             JNE L3
;      A3F:       488B15F2FEFFFF   MOV RDX, [RIP-270]         ; 'HIT
;      A46:       EB88             JMP L0
;      A48: L3:   EB4D             JMP L7
;      A4A: L4:   480FB64201       MOVZX RAX, BYTE PTR [RDX+1]
;      A4F:       4883F066         XOR RAX, 102
;      A53:       488BC8           MOV RCX, RAX
;      A56:       480FB64202       MOVZX RAX, BYTE PTR [RDX+2]
;      A5B:       4883F06F         XOR RAX, 111
;      A5F:       4809C1           OR RCX, RAX
;      A62:       4885C9           TEST RCX, RCX
;      A65:       7402             JEQ L5
;      A67:       EB2E             JMP L7
;      A69: L5:   480FB64203       MOVZX RAX, BYTE PTR [RDX+3]
;      A6E:       4883F862         CMP RAX, 98
;      A72:       750C             JNE L6
;      A74:       488B15C5FEFFFF   MOV RDX, [RIP-315]         ; 'FOB
;      A7B:       E950FFFFFF       JMP L0
;      A80: L6:   480FB64203       MOVZX RAX, BYTE PTR [RDX+3]
;      A85:       4883F86F         CMP RAX, 111
;      A89:       750C             JNE L7
;      A8B:       488B15B6FEFFFF   MOV RDX, [RIP-330]         ; 'FOO
;      A92:       E939FFFFFF       JMP L0
;      A97: L7:   488B15B2FEFFFF   MOV RDX, [RIP-334]         ; 'DEFAULT
                                                              ; no-arg-parsing entry point
;      A9E:       488D65F0         LEA RSP, [RBP-16]
;      AA2:       F8               CLC
;      AA3:       488B6DF8         MOV RBP, [RBP-8]
;      AA7:       C20800           RET 8
;      AAA:       90               NOP
;      AAB:       90               NOP
;      AAC:       90               NOP
;      AAD:       90               NOP
;      AAE:       90               NOP
;      AAF:       90               NOP
;      AB0:       0F0B0A           BREAK 10                   ; error trap
;      AB3:       02               BYTE #X02
;      AB4:       18               BYTE #X18                  ; INVALID-ARG-COUNT-ERROR
;      AB5:       4E               BYTE #X4E                  ; RCX
;      AB6:       0F0B0A           BREAK 10                   ; error trap
;      AB9:       02               BYTE #X02
;      ABA:       34               BYTE #X34                  ; OBJECT-NOT-SIMPLE-BASE-STRING-ERROR
;      ABB:       8F               BYTE #X8F                  ; RDX


;;; Some noise to guide pbook's generation.
;; Local Variables:
;; pbook-author:      "Paul Khuong"
;; pbook-include-toc: nil
;; pbook-style:       article
;; pbook-monochrome:  t
;; End:

#+nil
(defmacro with-timing ((total-iters subiters) &body forms)
  (let ((_thunk (gensym "THUNK"))
        (iters  (ceiling total-iters subiters)))
    `(flet ((,_thunk ()
              ,@forms))
       (let ((min sb-ext:double-float-positive-infinity)
             (sum 0d0)
             (max 0d0))
         (declare (type double-float min sum max))
         (loop repeat ,iters
            do (multiple-value-bind (_ begin/sec begin/us)
                   (sb-unix:unix-fast-getrusage sb-unix:rusage_self)
                 (declare (ignore _))
                 (loop repeat ,subiters
                    do (,_thunk))
                 (multiple-value-bind (_ end/sec end/us)
                     (sb-unix:unix-fast-getrusage sb-unix:rusage_self)
                   (declare (ignore _))
                   (let ((time (+ (float  (- end/sec begin/sec) 0d0)
                                  (* 1d-6 (- end/us begin/us)))))
                     (setf min (min time min)
                           sum (+   time sum)
                           max (max time max))
                     (values))))
            finally (return (values min
                                    (/ sum ,iters)
                                    max)))))))
