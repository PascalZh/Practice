#lang racket
(require racket/date)
(provide def len average pow even? square)
(provide csv-take sh)
(provide map2d build-mlist mlist-ref)
(provide LOGI LOGW LOGE)

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

(def (sh cmd)
  (with-output-to-string (λ () (system cmd))))

(def len length)
(def (map2d proc arg)
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

(def (LOG_ level tag message)
  (begin
    (display-time-ln)
    (display level) (display tag) (display ":")
    (println message)))
(def (LOGI tag message)
  (LOG_ "(I)" tag message))
(def (LOGW tag message)
  (LOG_ "(W)" tag message))
(def (LOGE tag message)
  (LOG_ "(E)" tag message))

(def (display-time)
  (def d (current-date))
  (date-display-format 'iso-8601)
  (display (date->string d)) (display " ")
  (display (string-append
             (number->string (date-hour d))
             ":"
             (number->string (date-minute d))
             ;":"
             ;(number->string (date-second d))
             )))
(def (display-time-ln) (display-time) (displayln ""))

; math {{{

(def (average lst) (/ (apply + lst) (len lst)))

(def (square x) (* x x))

; fast power
(def (pow b p)
  (def (iter res a n)
    (if (= n 0)
      res
      (if (even? n)
        (iter res (square a) (/ n 2))
        (iter (* res a) a (- n 1)))))
  (iter 1 b p))

; }}}
