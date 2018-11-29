#lang racket
(provide def len average pow even? square)
(provide csv-take)
(provide mapmap build-mlist mlist-ref)

(define-syntax def
  (syntax-rules ()
    [(def id1 id2 ...)
     (define id1 id2 ...)]))

; 若x多于reader中剩余的行数，那么就会把剩余所有的行都提取出来
; 此时行数小于x
; 若reader已经没有数据，那么直接返回'()
(def (csv-take reader x)
  (def (iter count_)
    (let ([row (reader)])
      (if (or (= count_ 0) (null? row))
        null
        (cons row (iter (- count_ 1))))))
  (iter x))

(def len length)
(def (mapmap proc arg)
  (map (λ (lst) (map proc lst))
       arg))
(def (build-mlist n proc)
  (def (iter i res)
    (if (= i 0)
      res
      (iter (i . - . 1) (mcons (proc (i . - . 1)) res))))
  (iter n null))
(def (mlist-ref mlst n)
  (if (= n 0)
    (mcar mlst)
    (mlist-ref (mcdr mlst) (n . - . 1))))

; math {{{

(def (average lst) (/ (apply + lst) (len lst)))

(def (even? x)
  (= (remainder x 2) 0))

(def (square x) (* x x))

(def (pow b p)
  (def (iter res a n)
    (if (= n 0)
      res
      (if (even? n)
        (iter res (square a) (/ n 2))
        (iter (* res a) a (- n 1)))))
  (iter 1 b p))
; }}}
