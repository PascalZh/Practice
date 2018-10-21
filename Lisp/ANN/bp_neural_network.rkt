#! /usr/bin/env racket
#lang racket

(provide test)

(provide make-network)
(provide apply-network)
(provide train)

(provide transpose)
(provide sigmoid)
(provide dot-prod)
(provide scale)

;; 每个神经元都假定与前一层的全部神经元相连
;; 构造出来的网络实际上是权值(w)

; 以list表示权值
; 构建网络 {{{
(define (make-network num-of-first . nums)
  (define (make-w number)
    (define generate-w random)
    (define (iter n res)
      (if (= n 0) res (iter (- n 1) (cons (generate-w) res))))
    (reverse (iter number null)))
  (define (iter nums res)
    (define (iter_ num-of-w num-of-nodes res)
      (if (= num-of-nodes 0)
          res
          (iter_ num-of-w (- num-of-nodes 1) (cons (make-w num-of-w) res))))
    (if (eq? (cdr nums) null)
        res
        (iter (cdr nums) (cons (iter_ (car nums) (cadr nums) null) res))))
  (cons 'bp1 (reverse (iter (cons num-of-first nums) null))))
; }}}

(define (type? obj type)
  (eq? (car obj) type))

; apply-network {{{
; 神经元的层从左到右从0开始编号，每一层从上到下从0开始编号
; 权值记为w由该层编号，神经元编号，上层神经元编号共同确定
; 输入层不会经过激活函数处理

; af: activity function
(define (apply-network input network #:act-f [af sigmoid])
  (define (iter ntw res)
    (if (null? ntw)
        (reverse res)
        (iter (cdr ntw)
              (cons (map af
                         (map (lambda (l) (dot-prod l (car res)))
                              (car ntw)))
                    res))))
  (if (not (pair? (car network)))
      (iter (cdr network ) (list input))
      (iter network (list input))))
; }}}

; alpha指的是学习率
; t表示真实值
; input 应该为二维的训练对象
(define train
  (lambda (input t network
                 #:learning_rate [alpha 1]
                 #:act-f [af sigmoid]
                 #:precision [p 0.000000000001])
    (cond [(type? network 'bp1) (train-bp1 input t (cdr network)
                                           #:learning_rate alpha
                                           #:act-f af
                                           #:precision p)])))

; train {{{
; 根据wikipedia的公式而写
; 输出层应该只有一个neuron

(define (train-bp1 input t network
                   #:learning_rate a
                   #:act-f af
                   #:precision p)

  (define (get-delta o-nh t_ ntw-nh) ; -nh represents no head
      (let ([layer-o (car o-nh)])
        (if (null? ntw-nh)
          (cons (map (lambda (elem-o)
                 (* (- elem-o t_)  ; use sigmoid function
                    elem-o
                    (- 1 elem-o)))
               layer-o)
               null)
          (let ([layer-ntw (car ntw-nh)])
            (cons (map (lambda (elem-o elem-ntw)
                 (* elem-o
                    (- 1 elem-o)
                    (dot-prod
                      elem-ntw
                      (first (get-delta (cdr o-nh) t_ (cdr ntw-nh))))))
               layer-o
               (transpose layer-ntw))
                 (get-delta (cdr o-nh) t_ (cdr ntw-nh)))))))

  (define (get-delta-w o t_ ntw)
    ;(display-n (get-delta (remove-head o) (remove-head ntw)))
    (map (lambda (lst-delta lst-o)
           (map (lambda (elem-delta)
                  (scale (* -1 a elem-delta) lst-o))
                lst-delta))
         (get-delta (remove-head o) t_ (remove-head ntw))
         (remove-tail o)))

  (define (new-network o t_ ntw)
    (add-network network (get-delta-w o t_ ntw)))

  (define (iter_ i_ t_ ntw)
    (define (output o_) (car (first (reverse o_))))
    (let* ([o (apply-network i_ ntw)]
           [new-ntw (new-network o t_ ntw)]
           [new-o (apply-network i_ new-ntw)])
      (if ((abs (- (output o) (output new-o) )) . < . p)
          ntw
          (begin
            (newline)
            (display "ntw:")
            (display-n new-ntw)
            (display "output:")
            (display-n (output new-o))
            (display "t")
            (display-n t_)
            (newline)
            (iter_ i_ t_ new-ntw)))))

  (define (iter i_ t_ ntw)
    (if (null? i_)
        ntw
        (iter (cdr i_) (cdr t_)
              (iter_ (car i_) (car t_) ntw))))

  (cons 'bp1 (iter input t network)))
; }}}

(define sigmoid (lambda (z) (/ 1 (+ 1 (exp (- 0 z))))))
(define (dot-prod l1 l2)
  (foldl +
         0
         (map * l1 l2)))
(define (transpose lst-2d)
  (if (null? (car lst-2d))
      null
      (cons (map first lst-2d)
            (transpose (map rest lst-2d)))))
(define (add-network ntw1 ntw2)
  (map (lambda (layer d-layer)
         (map (lambda (lst d-lst)
                (map +
                     lst
                     d-lst))
              layer
              d-layer))
       ntw1
       ntw2))
(define (scale k lst)
  (map (lambda (x) (* k x)) lst))
(define remove-head cdr)
(define (remove-tail lst)
  (reverse (rest (reverse lst))))

(define (test)

  ;(define ntw (make-network 2 3 1))
  ;(display ntw)
  ;(newline)

  ;(define i_ '(0
  ;0
  ;0
  ;0
  ;0
  ;0
  ;0
  ;0
  ;0
  ;))
  ;(define i1 (map (lambda (x) (random)) i_))
  ;(define i2 (map (lambda (x) (random)) i_))
  ;(define input (map (lambda (x1 x2) (list x1 x2)) i1 i2))
  ;(define t (map (lambda (x1 x2) (sqrt (+ (* x1 x1) (* x2 x2)))) i1 i2))

  (define ntw '(bp1 ((0.2 0.8) (-0.7 -0.5)) ((0.3 0.5))))
  ;(define ntw (make-network 2 2 1))
  (display ntw)
  (define input '((0.3 -0.7)))
  (define t '(0.1))

  (train input t ntw #:learning_rate 0.6)

  ;(define output (apply-network (list 0.9 0.1) ntw-t))
  ;(display output)
  ;(newline)
  ;(define output1 (apply-network (list 0.3 0.5) ntw-t))
  ;(display output1)
  ;(newline)
  ;(define output2 (apply-network (list 0.1 0.3) ntw-t))
  ;(display output2)
  ;(newline)
  )

(define (display-n obj)
  (display obj)
  (newline))

(test)
