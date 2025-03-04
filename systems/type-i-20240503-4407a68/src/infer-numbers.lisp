(in-package :type-i)

;;; integers

(defpattern =? (type pattern)
  `(or (list '= '? (and (type ,type) ,pattern))
       (list '= (and (type ,type) ,pattern) '?)))

(define-inference-rule =? (test)
  (let ((res
         (match test
           ;; rationals
           ((=? integer n)
            `(typep ? '(integer ,n ,n)))
           ((=? ratio n)
            `(typep ? '(ratio ,n ,n)))
           ;; floats
           ((=? double-float n)
            `(typep ? '(double-float ,n ,n)))
           ((=? single-float n)
            `(typep ? '(single-float ,n ,n)))
           ((=? short-float n)
            `(typep ? '(short-float ,n ,n)))
           ((=? long-float n)
            `(typep ? '(long-float ,n ,n)))
           ;; superclasses
           ((=? float n)
            `(typep ? '(float ,n ,n)))
           ((=? rational n)
            `(typep ? '(rational ,n ,n)))
           ((=? real n)
            `(typep ? '(real ,n ,n))))))
    (when res
      (list res))))

(defpattern <?< (type lower upper)
  `(or (list '< (and (type ,type) ,lower) '? (and (type ,type) ,upper))
       (list '> (and (type ,type) ,upper) '? (and (type ,type) ,lower))
       (and (or (list '< (and (type ,type) ,lower) '?)
                (list '> '? (and (type ,type) ,lower)))
            (<> ,upper '*))
       (and (or (list '< '? (and (type ,type) ,upper))
                (list '> (and (type ,type) ,upper) '?))
            (<> ,lower '*))))

(define-inference-rule <?< (test)
  (let ((res
         (match test
           ;; rationals
           ((<?< integer low high)
            `(typep ? '(integer ,low ,high)))
           ((<?< ratio low high)
            `(typep ? '(ratio ,low ,high)))
           ;; floats
           ((<?< double-float low high)
            `(typep ? '(double-float ,low ,high)))
           ((<?< single-float low high)
            `(typep ? '(single-float ,low ,high)))
           ((<?< short-float low high)
            `(typep ? '(short-float ,low ,high)))
           ((<?< long-float low high)
            `(typep ? '(long-float ,low ,high)))
           ;; superclasses
           ((<?< float low high)
            `(typep ? '(float ,low ,high)))
           ((<?< rational low high)
            `(typep ? '(rational ,low ,high)))
           ((<?< real low high)
            `(typep ? '(real ,low ,high))))))
    (when res
      (list res))))

