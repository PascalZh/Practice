#lang racket
(provide def len average)

(define-syntax def
  (syntax-rules ()
    [(def id1 id2 ...)
     (define id1 id2 ...)]))

(def len length)
(def (average lst) (/ (apply + lst) (len lst)))
