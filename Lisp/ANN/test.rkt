#!/usr/bin/env racket
#lang racket
(require "../libcommon.rkt")
(require csv-reading)
(require "./bp_ann.rkt")

(def a 1)
a
(def (r x) x)
(r 2)
(len (list 1 2 3))
(displayln (csv->list "train-labels.csv"))

(def ntw (make-network '(2 2 1)))
ntw
(ntw-node ntw 2 1)
(ntw-node ntw 3 1)
(def output (apply-network '(1 0) ntw))
output
