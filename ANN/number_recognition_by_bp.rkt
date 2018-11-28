#!/usr/bin/env racket
#lang racket
(require csv-reading)
(require "../Lisp/libcommon.rkt")
(require "bp_ann.rkt")

; (start-workflow samples labels t-samples t-labels) {{{
(def (start-workflow samples labels t-samples t-labels)
  ; t- affix refer to test-

  (let* ([num-input (* 28 28)]
         [num-output 10]
         [ntw-struct (calc-ntw-struct num-input num-output)])
    (def lr 0.6)
    (set-batch-size 50)
    (set-train-times 5)
    (displayln "network neuron numbers per layer (input layer on the far left): ")
    (displayln ntw-struct)
    (display "learning rate: ") (displayln lr)
    (newline) (displayln "train starting...") (newline) 

    (def ntw (make-network ntw-struct))
    (set! ntw (train samples labels ntw #:learning-rate lr))
    ;(analyze-result t-samples t-labels ntw)

    (void)))
; }}}

(def make-data-reader
     (make-csv-reader-maker
       '((separator-chars            #\,)
         (strip-leading-whitespace?  . #t)
         (strip-trailing-whitespace? . #t))))
(def (row->label row)
  (def (iter num i res)
    (if (= i 0)
      (reverse res)
      (if (= (- 10 num) i)
        (iter num (- i 1) (cons 1 res))
        (iter num (- i 1) (cons 0 res)))))
  (iter (string->number (first row) 10 null)))

(def train-images "./dataset/train-images.csv")
(def train-labels "./dataset/train-labels.csv")
(def t-images "./dataset/test-images.csv")
(def t-labels "./dataset/test-labels.csv")

(def (calc-ntw-struct n-i n-o)
  (list n-i
        (inexact->exact (round (+ 5
                                  (sqrt (* n-i n-o)))))
        n-o))

; (analyze-result ntw test-samples test-labels) {{{
(def (analyze-result test-samples test-labels ntw)
  (set! test-samples (csv->list test-samples))
  (set! test-labels (csv-map row->label test-labels))
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

(module* main #f
  (start-workflow train-images train-labels t-images t-labels))
