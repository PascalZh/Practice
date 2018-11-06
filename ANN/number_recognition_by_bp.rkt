#!/usr/bin/env racket
#lang racket
(require "../Lisp/libcommon.rkt")
(require "bp_ann.rkt")

(require csv-reading)
; (start-workflow samples labels t-samples t-labels) {{{
(def (start-workflow samples labels t-samples t-labels)
  ; t- affix refer to test-
  (display "train sample numbers: ")
  (displayln (len samples))
  (display "train label numbers: ")
  (displayln (len labels))

  (check-input samples labels)
  (check-input t-samples t-labels)

  (let* ([num-input (len (car samples))]
         [num-output (len (car labels))]
         [ntw-struct (calc-ntw-struct num-input num-output)])
    (def lr 0.6)
    (def α 0.1)
    (set-α α)
    (displayln "network neuron numbers per layer (input layer on the far left): ")
    (displayln ntw-struct)
    (display "learning rate: ") (displayln lr)
    (display "inertia term (momentum): ") (displayln α)
    (newline) (displayln "train starting...") (newline) 

    (def ntw (make-network ntw-struct))
    (def trained-ntw 0)
    (set-train-times 1)
    (def cnt 100)
    (def (iter ntw_ i)
     (if (= i 0)
         null
         (let* ([s-l (map (λ (s l) (cons s l)) samples labels) ]
                [shuffled-s-l (shuffle s-l)]
                [shuffled-samples (map car shuffled-s-l)]
                [shuffled-labels (map cdr shuffled-s-l)])
           (display "No. ")
           (displayln (- cnt i))
           (set! trained-ntw (train shuffled-samples shuffled-labels ntw_
                                    #:learning-rate lr))
           (analyze-result t-samples t-labels trained-ntw)
           (displayln trained-ntw)
           (iter trained-ntw (- i 1))
           )))
    (iter ntw cnt)))
; }}}

(def (mapmap proc l)
     (map (λ (l_) (map proc l_))
          l))
; (preproc-data data) {{{
(def (preproc-data data)
     (let* ([balanced (mapmap (λ (x) (- x 0.5)) data)]
            [average (map (λ (record)
                             (+ (/ (apply + record) (len record)) 0.0))
                          data)])
       data))
; }}}
; (csv->data str) {{{
(def make-data-reader
     (make-csv-reader-maker
       '((separator-chars            #\,)
         (strip-leading-whitespace?  . #t)
         (strip-trailing-whitespace? . #t))))
(def (csv->data str)
     (csv-map (lambda (row)
                (map string->number row))
              (make-data-reader (open-input-file str))))
; }}}
; (csv->label str) {{{
(def (csv->label str)
     (def (iter one i res)
          (if (= i 0)
              (reverse res)
              (if (= (- 10 one) i)
                  (iter one (- i 1) (cons 1 res))
                  (iter one (- i 1) (cons 0 res)))))
     (csv-map (lambda (row)
                (iter (string->number (first row)) 10 null))
              (open-input-file str)))
; }}}


;(def train-images (csv->data "./dataset/train-images-weak.csv"))
;(def train-labels (csv->label "./dataset/train-labels-weak.csv"))
;(def t-images (csv->data "./dataset/test-images-weak.csv"))
;(def t-labels (csv->label "./dataset/test-labels-weak.csv"))

(def train-images (csv->data "./dataset/train-images.csv"))
(def train-labels (csv->label "./dataset/train-labels.csv"))
(def t-images (csv->data "./dataset/test-images.csv"))
(def t-labels (csv->label "./dataset/test-labels.csv"))

; this is for debug
;(def train-images (csv->data "./foo.csv"))
;(def train-labels (csv->label "./fool.csv"))
;(def t-images (csv->data "./foo.csv"))
;(def t-labels (csv->label "./fool.csv"))


; (check-input samples labels) {{{
(def (check-input samples labels)
     (cond [(not (= (len samples) (len labels)))
            (error "sample and label size does not match!")]))
; }}}

; (calc-ntw-struct n-i n-o) {{{
(def (calc-ntw-struct n-i n-o)
     (list n-i
           (inexact->exact (round (+ 5
                                     (sqrt (* n-i n-o)))))
           n-o)
     ;(list n-i 400 n-o)
     )
; }}}

; (analyze-result ntw test-samples test-labels) {{{
(def (analyze-result test-samples test-labels ntw)
     (newline)
     (displayln "train finished...")
     (newline) (displayln "test starting...") (newline)
     (let* ([outputs (map (λ (input) (final-output (apply-network input ntw)))
                          test-samples)]
            [result (map (λ (output t)
                            (let* ([max-output (apply max output)]
                                   [norm-output (map (λ (x) (if (= x max-output) 1 0))
                                                     output)])
                              (if (equal? norm-output t)
                                  1
                                  0)))
                         outputs
                         test-labels)]
            [success-rate (average result)])
       (display "success rate: ") (display success-rate) (display " (")
       (display (exact->inexact success-rate)) (displayln ")"))

     (displayln "test finished")
     (void))
; }}}

; this function needs to be modified when the training has been modified.
(def (hour->times h)
     (inexact->exact (round
                       (/ (* h 60 60)
                          (/ 64 1000.0)))))

(module* main #f
         (start-workflow train-images train-labels t-images t-labels))
