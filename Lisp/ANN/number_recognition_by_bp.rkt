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
; (csv->data str) {{{
(def (csv->data str)
  (def (logic-not x)
    (if (eq? x "1")
      0
      1))
  (csv-map (lambda (row)
             (map (λ (x) (logic-not x)) row))
           (open-input-file str)))
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


(def train-images (preproc-data (csv->data "./dataset/train-images-weak.csv")))
(def train-labels (csv->label "./dataset/train-labels-weak.csv"))
(def t-images (preproc-data (csv->data "./dataset/test-images-weak.csv")))
(def t-labels (csv->label "./dataset/test-labels-weak.csv"))

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
    (displayln "network neuron numbers per layer (input layer on the far left): ")
    (displayln ntw-struct)
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
    (define trained-ntw (train samples labels ntw
                               #:learning-rate 0.6))
    (analyze-result t-samples t-labels trained-ntw)
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
        (inexact->exact (round (+ 5
                                  (sqrt (* n-i n-o)))))
        n-o))
; }}}

; (analyze-result ntw test-samples test-labels) {{{
(def (analyze-result test-samples test-labels ntw)
  (newline)
  (displayln "train finished...")
  (newline) (displayln "test starting...") (newline)
  (let ([outputs (map (λ (input) (final-output (apply-network input ntw)))
                      test-samples)])
    (map (λ (output t)
            (map (λ (ot tr) (display ot) (display "\t\t") (displayln tr))
          output t) (newline))
         outputs
         test-labels))

  (displayln "test finished")
  (void))
; }}}
(def (hour->times h)
  (inexact->exact (round
                    (/ (* h 60 60)
                       (/ 64 1000.0)))))

(module* main #f
  (set-train-times 200)
  (set-show-err 200)
  (start-workflow train-images train-labels t-images t-labels))
