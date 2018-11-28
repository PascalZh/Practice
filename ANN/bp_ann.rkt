#! /usr/bin/env racket
#lang racket
(require "../Lisp/libcommon.rkt")
(require "csv-reading")

(provide make-network apply-network final-output
         (contract-out [train (path? path? list? . -> . list?)]))
(provide set-train-times)
(provide set-α set-batch-size)
(provide ntw-layer ntw-node ntw-w add-network)
(provide transpose sigmoid dot-prod scale remove-head remove-tail)

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

; train-bp1 {{{
; 根据wikipedia的公式而写
; 输出层应该只有一个neuron

(def (train-bp1 input t network
                #:learning-rate η
                #:act-f af
                #:precision p)

  ; get-δ {{{
  (def (get-δ o-nh t_ ntw-nh) ; -nh represents no head
    (let ([layer-o (car o-nh)])
      (if (null? ntw-nh)                    ; output layer
        (cons (map (λ (elem-o elem-t)
                     (* (- elem-o elem-t)  ; use sigmoid function
                        elem-o
                        (- 1 elem-o)))
                   layer-o
                   t_)
              null)
        (let ([layer-ntw (car ntw-nh)]
              [next-δ (get-δ (cdr o-nh) t_ (cdr ntw-nh))])     ; inner layer
          (cons (map (λ (elem-o elem-ntw)
                       (* elem-o
                          (- 1 elem-o)
                          (dot-prod
                            elem-ntw
                            (first next-δ))))
                     layer-o
                     (transpose layer-ntw))
                next-δ)))))
  ; }}}

  ; get-Δw {{{
  (def former-Δw #f)
  (def (get-Δw o t_ ntw)
    ;(displayln (get-Δw (remove-head o) t_ (remove-head ntw)))
    (if (not (pair? former-Δw))
      (map (λ (layer-δ lst-o)
             (map (λ (elem-δ)
                    (scale (* -1 η elem-δ) lst-o))
                  layer-δ))
           (get-δ (remove-head o) t_ (remove-head ntw))
           (remove-tail o))

      (map (λ (layer-δ lst-o layer-former-Δw)
             (map (λ (elem-δ node-former-Δw)
                    (map +
                         (scale (1 . - . α)
                                (scale (* -1 η elem-δ) lst-o))
                         (scale α node-former-Δw)))
                  layer-δ
                  layer-former-Δw))
           (get-δ (remove-head o) t_ (remove-head ntw))
           (remove-tail o)
           former-Δw)
      ))
  ; }}}

  ; new-network {{{
  ; t_ should be numbers; o should be result of apply-network
  ; ntw is a mpair, [a,b) is the range of i_ and t_ that will be used.]
  (def (new-network i_ t_ ntw a b)
    (let* ([_i (take (take-right i_ a) (- b a))]
           [_t (take (take-right t_ a) (- b a))]
           [o (apply-network _i network #:act-f af)]
           [Δw (get-Δw o _t network)])
      (set-mcar! ntw Δw)))
  ; }}}

  ; analyze-error {{{
  (def (analyze-error i_ t_)
    (let
      ([mean-error  ; errr = 1/2 * (y - t)^2
         (average
           (map (λ (input true-value)
                  (let ([output (final-output (apply-network input ntw))])
                    (apply + (map loss-func output true-value))))
                i_
                t_))]
       [abs-error null])
      (display "Train No. ") (display (+ 1 (- old-max-train-times max-train-times)))
      (display "\tmean error: ") (displayln mean-error)))
  ; }}}

  ; multi-threaded
  (def tmp-ntw (mcons #f (mcons #f (mcons #f (mcons #f null)))))
  (def (train-batch i_ t_)
    (def b1 (round (/ batchsize 4)))
    (def b2 (round (* 2 (/ batchsize 4))))
    (def b3 (round (* 3 (/ batchsize 4))))
    (def thd0 (thread (λ () (new-network i_ t_ tmp-ntw 0 b1))))
    (def thd1 (thread (λ () (new-network i_ t_ (mcdr tmp-ntw) b1 b2))))
    (def thd2 (thread (λ () (new-network i_ t_ (mcdr (mcdr tmp-ntw)) b2 b3))))
    (def thd3 (thread (λ () (new-network i_ t_ (mcdr (mcdr (mcdr tmp-ntw))) b3 batch-size))))
    (thread-wait thd0)(thread-wait thd1)(thread-wait thd2)(thread-wait thd3)
    (set! network (add-network
                    network
                    (add-network
                      (mcar tmp-ntw)
                      (add-network
                        (mcadr tmp-ntw)
                        (add-network (mcaddr tmp-ntw) (mcadddr tmp-ntw))))))
    (analyze-error i_ t_))
  
  (def old-max-train-times max-train-times)
  (def (loop)
    (if (= max-train-times 0)
      null
      (begin
        (def reader-i (make-data-reader (open-input-file input)))
        (def reader-t (make-data-reader (open-input-file t)))
        (let ([i_ (csv-take reader-i batch-size)]
              [t_ (map row->label (csv-take reader-t batch-size))])
          (when (< (len i_) batch-size)
            (set! max-train-times (- max-train-times 1))
            (set! batch-size (len i_)))
          (if (< batch-size 4)
            null
            (train-batch i_ t_))))))
  (loop)
  network)

; }}}

; network API {{{

(def max-train-times 1000000000)
; decide how many mean error during the training will be show
(def show-err 100) 
(def show-part-num 100)
(def α 0.5)
(def batch-size 50)
;; 每个神经元都假定与前一层的全部神经元相连
;; 构造出来的网络实际上是权值(w)
(def (set-train-times n) (set! max-train-times n))
;(def (set-show-err n) (set! show-err n))
(def (set-α α_) (set! α α_))
(def (set-batch-size x) (set! batch-size x))

(def (ntw-layer ntw n-layer)
  (if (eq? (car ntw) 'bp1)
    (list-ref (cdr ntw) (- n-layer 2))
    (list-ref ntw (- n-layer 2))))

(def (ntw-node ntw n-layer n)
  (list-ref (ntw-layer ntw n-layer)
            (- n 1)))

(def (ntw-w ntw n-layer n prev-n)
  (list-ref (ntw-node ntw n-layer n) prev-n))

(def (final-output o)
  (first (reverse o)))
; }}}

; 构建网络 {{{
; 以list表示权值
(def (make-network nums)
  (def (make-w number)
    (def generate-w random)
    (def (iter n res)
      (if (= n 0) res (iter (- n 1) (cons (generate-w) res))))
    (reverse (iter number null)))

  (def (iter nums res)

    (def (iter_ num-of-w num-of-nodes res)
      (if (= num-of-nodes 0)
        res
        (iter_ num-of-w (- num-of-nodes 1) (cons (make-w num-of-w) res))))

    (if (eq? (cdr nums) null)
      res
      (iter (cdr nums)
            (cons
              (let ([layer (map (λ (node)
                                  (let ([sum (apply + node)])
                                    (map (λ (elem) (/ elem sum)) node)))
                                (iter_ (car nums) (cadr nums) null))])
                layer)
              res))))
  (cons 'bp1 (reverse (iter nums null))))

(def (type? obj type)
  (eq? (car obj) type))
; }}}

; apply-network {{{
; ReturnValue: 返回的是每个节点的输出值
; 神经元的层从左到右从0开始编号，每一层从上到下从0开始编号
; 权值记为w由该层编号，神经元编号，上层神经元编号共同确定

; af: activity function
(def (apply-network input network #:act-f [af sigmoid])
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

; }}}

; train {{{
; alpha指的是学习率
; t表示真实值向量
; input 应该为二维的list，即训练对象
(def train
  (λ (input t network
            #:learning-rate [alpha 0.9]
            #:act-f [af sigmoid]
            #:precision [p 0.000000000001])
    (cond [(type? network 'bp1) (train-bp1 input t (cdr network)
                                           #:learning-rate alpha
                                           #:act-f af
                                           #:precision p)])))

; }}}

; common {{{

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
(def (transpose lst-2d)
  (if (null? (car lst-2d))
    null
    (cons (map first lst-2d)
          (transpose (map rest lst-2d)))))
(def (add-network ntw1 ntw2)
  (map (λ (layer d-layer)
         (map (λ (lst d-lst)
                (map +
                     lst
                     d-lst))
              layer
              d-layer))
       ntw1
       ntw2))
(def (scale k lst)
  (map (λ (x) (* k x)) lst))
(def remove-head cdr)
(def (remove-tail lst)
  (reverse (rest (reverse lst))))
(def (repeat x n)
  (if (= n 0)
    null
    (cons x (repeat x (- n 1)))))
(def (loss-func y t)
  (/ (square (- y t))
     2))
(def (square x) (* x x))
; }}}
