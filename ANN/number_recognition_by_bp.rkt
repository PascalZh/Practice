#!/usr/bin/env racket
#lang racket
(require csv-reading)
(require "../Lisp/libcommon.rkt")
(require "bp_ann.rkt")

(def (start-workflow samples labels t-samples t-labels)
  ; t- affix refer to test-

  (let* ([num-input (* 28 28)]
         [num-output 10]
         [ntw-struct (calc-ntw-struct num-input num-output)])
    (def lr 0.1)
    (set-batch-size 60)
    (set-train-times 3)
    (displayln "network neuron numbers per layer (input layer on the far left): ")
    (displayln ntw-struct)
    (display "learning rate: ") (displayln lr)
    (displayln "train starting...")

    (def ntw (make-network ntw-struct))
    (set! ntw (train samples labels ntw #:learning-rate lr))
    (analyze-result (csv->data t-samples) (csv->label t-labels) ntw)

    (void)))

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

; (csv->data str) {{{
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

(def train-images "./dataset/train-images.csv")
(def train-labels "./dataset/train-labels.csv")
(def t-images "./dataset/test-images.csv")
(def t-labels "./dataset/test-labels.csv")

(def (calc-ntw-struct n-i n-o)
  (list n-i
        (inexact->exact (round (+ 5
                                  (sqrt (+ n-i n-o)))))
        n-o))

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

(module* main #f
  (start-workflow train-images train-labels t-images t-labels))
