#lang racket
(provide def len)

(define-syntax def
  (syntax-rules ()
    [(def id1 id2 ...)
     (define id1 id2 ...)]))

(def len length)
