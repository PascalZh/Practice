#!/usr/bin/env racket
#lang racket
(require "../Lisp/libcommon.rkt")
(require csv-reading)
(require "./bp_ann.rkt")

;(def a 1)
;a
;(def (r x) x)
;(r 2)
;(len (list 1 2 3))
;(displayln (csv->list "train-labels.csv"))

;(def ntw (make-network '(2 2 1)))
;ntw
;(ntw-node ntw 2 1)
;(ntw-node ntw 3 1)
;(def output (apply-network '(1 0) ntw))
;output
;(def ntw '(bp1  ((0.5 0.5))))
;ntw
;(def ntw2 (make-network '(2 2 1)))
;ntw2
;(def ntw1 (add-network (cdr ntw) (cdr ntw2)))
;ntw1
;(def i '((1 0)))
;(def o '((1)))
;(def res (apply-network '(1 2) ntw))
;res
;(def trained-ntw (train i o ntw #:learning-rate 0.6))
;(displayln trained-ntw)
;(displayln (apply-network (car i) trained-ntw))

(def make-data-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
(def reader (make-data-reader (open-input-file "foo.csv")))
(csv-take reader 2)
(csv-take reader 4)