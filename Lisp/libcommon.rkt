#lang racket
(require racket/date)
(provide def len average pow even? square)
(provide csv-take sh)
(provide mapmap build-mlist mlist-ref)
(provide LOGI LOGW LOGE)

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

(def (sh cmd)
  (with-output-to-string (λ () (system cmd))))

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

(def (LOGI tag message)
  (display-time)(displayln "")
  (display "(I)")(display tag)(display ":")
  (println message))

(def (LOGW tag message)
  (display-time)(displayln "")
  (display "(W)")(display tag)(display ":")
  (println message))

(def (LOGE tag message)
  (display-time)(displayln "")
  (display "(E)")(display tag)(display ":")
  (println message))

(def (display-time)
  (def d (current-date))
  (date-display-format 'iso-8601)
  (display (date->string d)) (display " ")
  (display (string-append
             (number->string (date-hour d))
             ":"
             (number->string (date-minute d))
             ":"
             (number->string (date-second d)))))

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
