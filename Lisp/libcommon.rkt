#lang racket
(provide def len average pow even? square)

(define-syntax def
  (syntax-rules ()
    [(def id1 id2 ...)
     (define id1 id2 ...)]))

(def len length)
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
