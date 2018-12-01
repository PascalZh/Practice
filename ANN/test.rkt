#!/usr/bin/env racket
#lang racket

(require racket/fasl)
(require csv-reading)
(require "bp_ann.rkt")
(require "../Lisp/libcommon.rkt")


;(define ntws (with-input-from-file
               ;"ntws0"
               ;(λ () (fasl->s-exp (current-input-port)))))

;(map (λ (ntw) (map (λ (node) (println (apply + node)))(last ntw)) (displayln "**********")) ntws)

;(println (len ntws))
;(display (map (λ (l1 l2)
                ;(map (λ (n1 n2)(map - n1 n2))
                     ;l1 l2))
              ;(car ntws)
              ;(cadr ntws)))

(define (analyze-result test-samples test-labels ntw)
  (displayln "test starting...") (displayln (len test-samples))(newline)
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
    (displayln (take result 10))
    (display "success rate: ") (display success-rate) (display " (")
    (display (exact->inexact success-rate)) (displayln ")"))

  (displayln "test finished")
  (void))
(define (final-output o)
  (first (reverse o)))
(define (apply-network input network #:act-f [af sigmoid])
  (def (iter ntw res)
    (if (null? ntw)
      (reverse res)
      (iter (cdr ntw)
            (cons
              (map af
                   (map (λ (node) (dot-prod node (car res)))
                        (car ntw)))
              res))))
  (if (not (pair? (car network)))
    (iter (cdr network ) (list (map af input)))
    (iter network (list (map af input)))))
(def sigmoid (λ (z) (/ 1 (+ 1 (exp (- 0.0 z))))))
; dot-prod {{{
(def (dot-prod l1 l2)
  (when (not (= (len l1) (len l2)))
    (displayln "dot-prod: l1 and l2 not aligned")
    (displayln "l1: ")
    (displayln l1)
    (displayln "l2: ")
    (displayln l2))
  (foldl +
         0
         (map * l1 l2)))
; }}}
(def make-data-reader
  (make-csv-reader-maker
    '((separator-chars            #\,)
      (strip-leading-whitespace?  . #t)
      (strip-trailing-whitespace? . #t))))
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


(analyze-result (csv->data  "./dataset/test-images.csv") (csv->label  "./dataset/test-labels.csv") (make-network '(784 33 10)))
