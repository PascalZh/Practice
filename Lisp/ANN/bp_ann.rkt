#! /usr/bin/env racket
#lang racket
(require "../libcommon.rkt")

(provide make-network apply-network final-output train)
(provide set-show-err set-train-times)
(provide set-α set-min-part)
(provide ntw-layer ntw-node ntw-w add-network)
(provide transpose sigmoid dot-prod scale remove-head remove-tail)

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
  (def former-Δw 0)
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
  (def (new-network o t_ ntw)
    ;(displayln (get o t_ ntw))
    ;(displayln o)
    ;(displayln t_)
    ;(displayln ntw)
    (let ([Δw (get-Δw o t_ ntw)])
      (set! former-Δw Δw)
      ;(displayln ntw)
      ;(displayln (caaar   Δw ))
      ;(newline)
      ;(displayln (add-network network Δw))
      ;(sleep 3)
      (add-network ntw Δw))) ;!!!I spent 2 days to find that I write (add-network network Δw) mistakenly!!!
  ; }}}

  ; analyze-error {{{
  (def (analyze-error i_ t_ ntw count_)
    (let
      ([mean-error
         (average
           (map (λ (input true-value)
                  (let ([output (final-output (apply-network input ntw))])
                    ;(map (λ (ot tr) (display ot) (display "\t\t") (displayln tr))
                    ;output true-value)
                    ;(newline)
                    (apply + (map loss-func output true-value))))
                i_
                t_))])
      (when (= (remainder count_ (quotient max-train-times show-err))
               0)
        (display "Part No. ")
        (display count_)
        (display " (")
        (display (/ (round (* 10000 (/ count_ len_part))) 100.0))
        (display "%)")
        (display "\tmean error: ")
        (displayln mean-error)
        ;(displayln i_)
        ;(displayln t_)
        )
      mean-error))
  ; }}}

  ; iter {{{
  ; train one time for all inputs
  (def (iter i_ t_ ntw)
    ;(displayln ntw)
    ;(sleep 1)
    (if (null? i_)
      ntw
      (iter (cdr i_) (cdr t_)
            (new-network (apply-network (car i_) ntw #:act-f af)
                         (car t_)
                         ntw))))
  ; }}}



  ; (loop-part i-p t-p ntw count_part) {{{
  (def former-m-e 0)
  (def (loop-part i-p t-p ntw count_part)
    (let ([i_ (car i-p)]
          [t_ (car t-p)])
      (if (null? i-p)
        ntw
        (let ([mean-error (analyze-error i_ t_ ntw count_part)])
          (if (< (abs (- mean-error former-m-e)) p)
            ntw
            (begin 
              (set! former-m-e mean-error)
              (set! former-Δw 0)
              (loop-part (cdr i-p) (cdr t-p) (iter i_ t_ ntw) (+ count_part 1))))))))
  ; }}}


  ; (loop i-p t-p ntw count_) {{{
  (def (loop i-p t-p ntw count_)
    (if (= count_ max-train-times)
      ntw
      (begin
        (display "Train No. ") (displayln count_)
        (loop i-p t-p (loop-part i-p t-p ntw 0) (+ count_ 1)))))
  ; }}}

  ; (part d){{{
  (def (part d)
    (def (iter d_ res)
      (if (< (len d_) (* min-part 1.3))
        (cons d_ res)
        (iter (drop d_ min-part) (cons (take d_ min-part) res))))
    (iter d null))
  ; }}}

  ; (preprocess input t ntw) {{{
  (def len_part 0)
  (def (preprocess input t ntw)
    (def i-p (part input))
    (def t-p (part t))
    (set! len_part (len i-p))
    (display "data have been parted into ")
    (display len_part) (displayln " parts.") (newline)
    ;(def (iter i_ t_ ntw)
    ;(if (null? i_)
    ;ntw
    ;(iter (cdr i_) (cdr t_)
    ;(loop (car i_) (car t_) ntw 0))))
    ;(iter i-p t-p network)
    (loop i-p t-p ntw 0))
  ; }}}

  (cons 'bp1 (preprocess input t network)))
; }}}

; network API {{{

(def max-train-times 1000000000)
; decide how many mean error during the training will be show
(def show-err 10000000) 
(def α 0.5)
(def min-part 50)
;; 每个神经元都假定与前一层的全部神经元相连
;; 构造出来的网络实际上是权值(w)
(def (set-train-times n) (set! max-train-times n))
(def (set-show-err n) (set! show-err n))
(def (set-α α_) (set! α α_))
(def (set-min-part x) (set! min-part x))

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
