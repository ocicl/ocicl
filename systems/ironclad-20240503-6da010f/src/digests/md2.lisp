;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
;;;; md2.lisp -- the MD2 message digest algorithm from RFC 1319

(in-package :crypto)
(in-ironclad-readtable)

(defconst +md2-permutation+
#8@(41 46 67 201 162 216 124 1 61 54 84 161 236 240 6
  19 98 167 5 243 192 199 115 140 152 147 43 217 188
  76 130 202 30 155 87 60 253 212 224 22 103 66 111 24
  138 23 229 18 190 78 196 214 218 158 222 73 160 251
  245 142 187 47 238 122 169 104 121 145 21 178 7 63
  148 194 16 137 11 34 95 33 128 127 93 154 90 144 50
  39 53 62 204 231 191 247 151 3 255 25 48 179 72 165
  181 209 215 94 146 42 172 86 170 198 79 184 56 210
  150 164 125 182 118 252 107 226 156 116 4 241 69 157
  112 89 100 113 135 32 134 91 207 101 230 45 168 2 27
  96 37 173 174 176 185 246 28 70 97 105 52 64 126 15
  85 71 163 35 221 81 175 58 195 92 249 206 186 197
  234 38 44 83 13 110 133 40 132 9 211 223 205 244 65
  129 77 82 106 220 55 200 108 193 171 250 36 225 123
  8 12 189 177 74 120 136 149 139 227 99 232 109 233
  203 213 254 59 0 29 57 242 239 183 14 102 88 208 228
  166 119 114 248 235 117 75 10 49 68 80 180 143 237
  31 26 219 153 141 51 159 17 131 20))

(eval-when (:compile-toplevel)
(defmacro stateref (regs i) `(aref ,regs (+ ,i 0)))
(defmacro blockref (regs i) `(aref ,regs (+ ,i 16)))
(defmacro workref (regs i) `(aref ,regs (+ ,i 32)))
) ; EVAL-WHEN

(defun update-md2-regs (regs buffer offset checksum)
  (declare (type (simple-array (unsigned-byte 8) (48)) regs)
           (type (simple-array (unsigned-byte 8) (16)) checksum)
           (type simple-octet-vector buffer)
           #.(burn-baby-burn))
  (let ((x 0))
    (declare (type (unsigned-byte 8) x))
    ;; save original input and prepare encryption block
    (dotimes (i 16)
      (setf (workref regs i)
            (logxor (stateref regs i) (aref buffer (+ i offset)))
            (blockref regs i) (aref buffer (+ i offset))))
    ;; encrypt block
    (dotimes (i 18)
      (dotimes (j 48)
        (setf x (logxor (aref +md2-permutation+ x) (aref regs j))
              (aref regs j) x))
      (setf x (mod (+ x i) 256)))
    ;; update checksum
    (setf x (aref checksum 15))
    (dotimes (i 16)
      (setf x (logxor (aref checksum i)
                      (aref +md2-permutation+
                            (logxor (aref buffer (+ i offset)) x)))
            (aref checksum i) x))))

(declaim (inline md2-regs-digest))
(defun md2-regs-digest (regs buffer start)
  (declare (type (simple-array (unsigned-byte 8) (48)) regs)
           #.(burn-baby-burn))
  (flet ((stuff-registers (buffer start)
           (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
           (dotimes (i 16 buffer)
             (setf (aref buffer (+ start i)) (stateref regs i)))))
    (declare (inline stuff-registers))
    (cond
      (buffer
       (stuff-registers buffer start))
      (t
       (stuff-registers (make-array 16 :element-type '(unsigned-byte 8)
                                    :initial-element 0) 0)))))

(defstruct (md2
             (:constructor %make-md2-digest
              (&aux (buffer (make-array 16 :element-type '(unsigned-byte 8)
                                        :initial-element 0))))
             (:constructor %make-md2-state
                           (regs checksum buffer buffer-index))
             (:copier nil)
             (:include mdx))
  (regs (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (48)) :read-only t)
  (checksum (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (16)) :read-only t))

(defmethod reinitialize-instance ((state md2) &rest initargs)
  (declare (ignore initargs))
  (fill (md2-regs state) 0)
  (fill (md2-checksum state) 0)
  (setf (md2-buffer-index state) 0)
  state)

(defmethod copy-digest ((state md2) &optional copy)
  (check-type copy (or null md2))
  (cond
    (copy
     (replace (md2-regs copy) (md2-regs state))
     (replace (md2-checksum copy) (md2-checksum state))
     (replace (md2-buffer copy) (md2-buffer state))
     (setf (md2-buffer-index copy) (md2-buffer-index state))
     copy)
    (t
     (%make-md2-state (copy-seq (md2-regs state))
                      (copy-seq (md2-checksum state))
                      (copy-seq (md2-buffer state))
                      (md2-buffer-index state)))))

(define-digest-updater md2
  (flet ((compress (state sequence offset)
           (update-md2-regs (md2-regs state)
                            sequence offset
                            (md2-checksum state))))
    (declare (dynamic-extent #'compress))
    (declare (notinline mdx-updater))
    (mdx-updater state #'compress sequence start end)))

(define-digest-finalizer (md2 16)
  (let* ((regs (md2-regs state))
         (checksum (md2-checksum state))
         (buffer (md2-buffer state))
         (buffer-index (md2-buffer-index state))
         (pad-amount (- 16 buffer-index)))
    ;; pad with appropriate padding
    (dotimes (i pad-amount)
      (setf (aref buffer (+ buffer-index i)) pad-amount))
    (update-md2-regs regs buffer 0 checksum)
    ;; extend the message with the checksum
    (dotimes (i 16)
      (setf (aref buffer i) (aref checksum i)))
    (update-md2-regs regs buffer 0 checksum)
    (finalize-registers state regs)))

(defdigest md2 :digest-length 16 :block-length 16)
