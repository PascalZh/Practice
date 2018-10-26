#! /usr/bin/env racket
#lang racket
(require "../libcommon.rkt")

(provide make-network apply-network final-output train)
(provide ntw-layer ntw-node ntw-w)
(provide transpose sigmoid dot-prod scale)


(def max-train-times 100000)
; decide how many mean error during the training will be show
(def num-of-show-m-err 100000) 
;; 每个神经元都假定与前一层的全部神经元相连
;; 构造出来的网络实际上是权值(w)

(def (ntw-layer ntw n-layer)
  (if (eq? (car ntw) 'bp1)
    (list-ref (cdr ntw) (- n-layer 2))
    (list-ref ntw (- n-layer 2))))

(def (ntw-node ntw n-layer n)
  (list-ref (ntw-layer ntw n-layer)
            (- n 1)))

(def (ntw-w ntw n-layer n prev-n)
  (list-ref (ntw-node ntw n-layer n) prev-n))


; 以list表示权值
; 构建网络 {{{
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

; (final-output o) {{{
(def (final-output o)
  (first (reverse o)))
; }}}

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

; train-bp1 {{{
; 根据wikipedia的公式而写
; 输出层应该只有一个neuron

(def (train-bp1 input t network
                #:learning-rate a
                #:act-f af
                #:precision p)

  ; get-delta {{{
  (def (get-delta o-nh t_ ntw-nh) ; -nh represents no head
    (let ([layer-o (car o-nh)])
      (if (null? ntw-nh)                    ; output layer
        (cons (map (λ (elem-o elem-t)
                      (* (- elem-o elem-t)  ; use sigmoid function
                         elem-o
                         (- 1 elem-o)))
                   layer-o
                   t_)
              null)
        (let ([layer-ntw (car ntw-nh)])     ; inner layer
          (cons (map (λ (elem-o elem-ntw)
                        (* elem-o
                           (- 1 elem-o)
                           (dot-prod
                             elem-ntw
                             (first (get-delta (cdr o-nh) t_ (cdr ntw-nh))))))
                     layer-o
                     (transpose layer-ntw))
                (get-delta (cdr o-nh) t_ (cdr ntw-nh)))))))
  ; }}}

  ; get-delta-w {{{
  (def (get-delta-w o t_ ntw)
    ;(displayln (get-delta (remove-head o) t_ (remove-head ntw)))
    (map (λ (lst-delta lst-o)
            (map (λ (elem-delta)
                    (scale (* -1 a elem-delta) lst-o))
                 lst-delta))
         (get-delta (remove-head o) t_ (remove-head ntw))
         (remove-tail o)))
  ; }}}

  ; new-network {{{
  ; t_ should be numbers; o should be result of apply-network
  (def (new-network o t_ ntw)
    ;(displayln (get-delta-w o t_ ntw))
    ;(displayln o)
    ;(displayln t_)
    ;(displayln ntw)
    (let ([delta-w (get-delta-w o t_ ntw)])
      ;(displayln (ntw-w delta-w 3 1 1))
      ;(newline)
      (add-network network delta-w)))
  ; }}}

  ; analyze-error {{{
  (def (analyze-error i_ t_ ntw count_)
    (let*
      ([mean-error
         (average
           (map (λ (input true-value)
                   (let ([output (final-output (apply-network input ntw))])
                     ;(displayln output)
                     (apply + (map loss-func output true-value))))
                i_
                t_))])
      (when (= (remainder count_ (quotient max-train-times num-of-show-m-err))
               0)
        (display "No. ")
        (display count_)
        (display "\t\tmean error: ")
        (displayln mean-error)
        ;(displayln i_)
        ;(displayln t_)
        )
      mean-error))
  ; }}}

  ; iter {{{
  ; train one time for all inputs
  (def (iter i_ t_ ntw)
    ;(displayln (ntw-node ntw 3 1))
    (if (null? i_)
      ntw
      (iter (cdr i_) (cdr t_)
            (new-network (apply-network (car i_) ntw #:act-f af)
                         (car t_)
                         ntw))))
  ; }}}


  (def (loop i_ t_ ntw count_)
    ;(displayln "loop")
    (if (= count_ max-train-times)
      ntw
      (let ([mean-error (analyze-error i_ t_ ntw count_)])
        (if (< mean-error p)
          ntw
          (loop i_ t_ (iter i_ t_ ntw) (+ count_ 1))))))

  (cons 'bp1 (loop input t network 0)))
; }}}


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
