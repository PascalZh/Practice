#lang racket
(provide def len mapmap average pow even? square)
(provide csv-take)

(define-syntax def
  (syntax-rules ()
    [(def id1 id2 ...)
     (define id1 id2 ...)]))

(def (csv-take reader x)
  (def (iter lst count_)
    (if (= count_ 0)
      lst
      (iter (cons (reader) lst) (count_ . - . 1))))
  (reverse (iter null x)))

(def len length)
(def (mapmap proc arg)
  (map (λ (lst) (map proc lst))
       arg))

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
