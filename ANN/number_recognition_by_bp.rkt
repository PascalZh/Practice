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
    (def lr 0.3)
    (def bs 2)
    (set-batch-size bs)
    (set-train-times 100)
    (displayln "network neuron numbers per layer (input layer on the far left): ")
    (displayln ntw-struct)
    (display "learning rate: ") (displayln lr)
    (display "batch size: ") (displayln bs)
    (displayln "train starting...")

    (def ntw (make-network ntw-struct))
    (set! ntw (train samples labels ntw #:learning-rate lr))))

(def train-images "./dataset/train-images.csv")
(def train-labels "./dataset/train-labels.csv")
(def t-images "./dataset/test-images.csv")
(def t-labels "./dataset/test-labels.csv")

(def (calc-ntw-struct n-i n-o)
  (list n-i
        (inexact->exact (round (+ 5
                                  (sqrt (+ n-i n-o)))))
        n-o)
  ;(list n-i (round (/ n-i 2)) n-o)
  (list n-i 200 n-o)
  )

(module* main #f
  (start-workflow train-images train-labels t-images t-labels))
