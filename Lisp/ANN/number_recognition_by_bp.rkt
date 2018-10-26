#!/usr/bin/env racket
#lang racket
(require "../libcommon.rkt")
(require "bp_ann.rkt")

(require csv-reading)

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
; (get_training_sample) {{{
(def (csv->data str)
  (csv-map (lambda (row)
             (map string->number row))
           (open-input-file str)))
; }}}

(def train-images (preproc-data (csv->data "./dataset/train-images-weak.csv")))
(def train-labels (csv->data "./dataset/train-labels-weak.csv"))
(def t-images (preproc-data (csv->data "./dataset/test-images-weak.csv")))
(def t-labels (csv->data "./dataset/test-labels-weak.csv"))

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
         [num-output 1]
         [ntw-struct (calc-ntw-struct num-input num-output)])
    (display "input neuron numbers: ")
    (displayln num-input)
    (display "output neuron numbers: ")
    (displayln num-output)
    (displayln "train starting...")
    (newline) (newline)

    (define ntw (make-network ntw-struct))
    ;(displayln ntw)
    ;(displayln ntw-struct)
    ;(displayln (len ntw))
    ;(displayln samples)
    ;(displayln (map car labels))
    ;(sleep 1)
    ;(displayln (apply-network (car samples) ntw))
    (define trained-ntw (train ntw samples (map car labels)
                               #:act-f activity-function
                               #:learning-rate 0.1))
    (analyze-result trained-ntw t-samples t-labels)
    ))
; }}}

; (check-input samples labels) {{{
(def (check-input samples labels)
  (cond [(not (= (len samples) (len labels)))
         (error "sample and label size does not match!")]))
; }}}

; (calc-ntw-struct n-i n-o) {{{
(def (calc-ntw-struct n-i n-o)
  (list n-i
        (inexact->exact (round (/ n-i 2.0)))
        n-o))
; }}}

; (analyze-result ntw test-samples test-labels) {{{
(def (analyze-result ntw test-samples test-labels)
  (newline) (newline)
  (displayln "train finished..."))
; }}}

(define (activity-function z) (* (sigmoid z)))
(def (main)
  (start-workflow train-images train-labels t-images t-labels))
(main)
