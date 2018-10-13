#! /usr/bin/env racket
#lang racket
(define nil '())
(define len length)
; access list by index
(define (at l i) (if (equal? i 0) (car l) (at (cdr l) (- i 1))))
(define (loc l i j) (at (at l i) j))

;; 每个神经元都假定与前一层的全部神经元相连
;; 构造出来的网络实际上是权值(w)

; 以list表示权值
; 构建网络
(define (make-network num-of-first . nums)
  (define (make-w number)
    (define (generate-w) 0)
    (define (iter n res)
      (if (= n 0) res (iter (- n 1) (cons (generate-w) res))))
    (reverse (iter number nil)))
  (define (iter nums res)
    (define (iter_ num-of-w num-of-nodes res)
      (if (= num-of-nodes 0)
          res
          (iter_ num-of-w (- num-of-nodes 1) (cons (make-w num-of-w) res))))
    (if (eq? (cdr nums) nil)
        res
        (iter (cdr nums) (cons (iter_ (car nums) (cadr nums) nil) res))))
  (cons 'bp1 (reverse (iter (cons num-of-first nums) nil))))

(define (type? obj type)
  (eq? (car obj) type))

; 神经元的层从左到右从0开始编号，每一层从上到下从0开始编号
; 权值记为w由该层编号，神经元编号，上层神经元编号共同确定
(define (access-w network layer num prev-num)
  (cond [(type? network 'bp1)
         (at (loc (cdr network) layer num) prev-num)]))

; af: activity function
(define (apply-network input network af)
  (define (iter ntw res)
    (if (null? (cdr ntw))
        (reverse res)
        (iter (cdr ntw)
              (cons (map af
                         (map (lambda (l) (dot-prod l res))
                              (car ntw)))
                    res))))
  (reverse (iter network (map af input))))

; alpha指的是学习率
; t表示真实值
(define train
  (lambda (input t network
           #:alpha [alpha 1]
           #:activity-func [af sigmoid]
           #:precision [p 0.01])
    (cond [(type? network 'bp1) (train-bp1 input t (cdr network)
                                           #:alpha alpha
                                           #:activity-func af
                                           #:precision p)])))

; 根据wikipedia的公式而写
(define (dot-prod) l1 l2)
(define (train-bp1 input t network
                   #:alpha a
                   #:activity-func af
                   #:precision p)
  (define (iter o ntw)
    (define (get-delta o ntw))
    (define (get-delta-w)
      (map (lambda (lst)
             (map (lambda (elem lst-o)
                    (map (lambda (elem-o)
                           (* elem elem-o))
                         lst-o))
                  lst
                  o))
           (get-delta o ntw)))
    (define (new-network network)
      (map (lambda (layer d-layer)
             (map (lambda (lst d-lst)
                    (map +
                         lst
                         d-lst))
                  layer
                  d-layer))
           network
           (get-delta-w)))
    (if ((- (caar o) t) . < . p)
        ntw
        (let ([new-ntw (new-network network)])
          (iter (apply-network input new-ntw af) new-ntw))))
  (iter (apply-network input network af) network))

(define sigmoid (lambda (z) (/ 1 (+ 1 (exp (- 0 z))))))
