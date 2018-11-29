#! /usr/bin/env racket
#lang racket
(require "../Lisp/libcommon.rkt")
(require racket/future)
(require csv-reading)

(provide make-network apply-network final-output
         (contract-out [train (->* (string? string? list?) (#:learning-rate number?
                                                        #:act-f procedure?
                                                        #:precision number?) list?)]))
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
  (iter (string->number (first row)) 10 null))

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

  ; t_ should be numbers; o should be result of apply-network
  ; ntw is a mpair, [a,b) is the range of i_ and t_ that will be used.]
  (def (new-network i_ t_ ind a b)
    ;(display a) (display ", ") (displayln b)
    (let* ([_i (take (list-tail i_ a) (- b a))]
           [_t (take (list-tail t_ a) (- b a))]
           [sum-Δw (foldl add-network #f
                          (map (λ (i__ t__)
                                 (get-Δw (apply-network i__ network #:act-f af)
                                         t__ network))
                               _i
                               _t))])
      ;(display sum-Δw)
      (vector-set! batch-delat-w ind sum-Δw)))

  ; analyze-error {{{
  (def (analyze-error i_ t_)
    (let
      ([mean-error  ; err = average(1/2 * (yi - ti)^2), i = 0,1,...,9
         (average
           (map (λ (input true-value)
                  (let ([output (final-output (apply-network input network))])
                    ;(map (λ (o__ t__) (when (= t__ 1) (displayln (apply max output))(display o__) (display "\t") (displayln t__))) output true-value)
                    ;(displayln "*************************")
                    (average (map loss-func output true-value))))
                i_
                t_))]
       [abs-error (average
           (map (λ (input true-value)
                  (let ([output (final-output (apply-network input network))])
                    (average (map (λ (y t) (abs (- y t))) output true-value))))
                i_
                t_))])
      (display "Train No. ") (display (+ 1 (- old-max-train-times max-train-times)))
      (display "\tmean error: ") (display mean-error)
      (display " abs error: ") (displayln abs-error)))
  ; }}}

  ; multi-threaded
  (def core-num (processor-count))
  (display "using ") (display core-num) (displayln " cores of cpu...")
  (def batch-delat-w (make-vector core-num))
  (def (train-batch i_ t_)
    ;(displayln (take t_ 3))
    ;(def (init-threads num)
      ;(if (= num 0)
        ;null
        ;(cons (thread (λ () (new-network i_ t_ (- num 1)
                                         ;(round (* (/ (- num 1) core-num)
                                                   ;batch-size))
                                         ;(round (* (/ num core-num)
                                                   ;batch-size)))))
              ;(init-threads (- num 1)))))
    ;(def (wait-threads l)
      ;(if (null? l)
        ;null
        ;(begin (thread-wait (car l)) (wait-threads (cdr l)))))
    ;(wait-threads (init-threads core-num))
    (for/async ([ind (build-list core-num values)])
               (new-network i_ t_ ind
                            (round (* (/ ind core-num)
                                      batch-size))
                            (round (* (/ (+ ind 1) core-num)
                                      batch-size))))
    (set! network (foldl add-network network (vector->list batch-delat-w)))
    (analyze-error i_ t_))

  (def old-max-train-times max-train-times)
  (def (loop)
    (if (= max-train-times 0)
      null
      (let* ([reader-t (make-data-reader (open-input-file t))]
             [reader-i (make-data-reader (open-input-file input))])
        (def (iter)
          (let ([i_raw (csv-take reader-i batch-size)]
                [t_raw (csv-take reader-t batch-size)])
            (if (< (len i_raw) batch-size)
              (begin
                (set! max-train-times (- max-train-times 1)))
              (let ([i_ (mapmap string->number i_raw)]
                    [t_ (map row->label t_raw)])
                (train-batch i_ t_)
                (iter)
                ))))
        (iter)
        (analyze-result (csv->data  "./dataset/test-images.csv")
                        (csv->label "./dataset/test-labels.csv") network)
        (loop))))
  (loop)
  network)

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

; 定义#f为0权值网络
(def (add-network ntw1 ntw2)
  (cond [(not ntw1) ntw2]
        [(not ntw2) ntw1]
        [else
          (map (λ (layer d-layer)
                 (map (λ (lst d-lst)
                        (map +
                             lst
                             d-lst))
                      layer
                      d-layer))
               ntw1
               ntw2)]))
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
