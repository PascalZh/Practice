#!/usr/bin/env racket
#lang racket/gui
(require "libcommon.rkt")

(provide set-Oxy set-size set-grid-size set-colors refresh-maze get-canvas)
(provide make-colors colors-ref colors-set!)

(def (paint-maze canvas dc)
  (def (iter i)
    (def (iter_ j)
      (if (= j 0)
        null
        (let ([x (+ Ox
                    (* grid-width (- i 1))
                    grid-border)]
              [y (+ Oy
                    (* grid-height (- j 1))
                    grid-border)]
              [color (colors-ref colors (- i 1) (- j 1))])
          (send dc set-brush color'solid)
          (send dc draw-rectangle
                x y
                (- grid-width grid-border)
                (- grid-height grid-border))
          (iter_ (- j 1)))))
    (if (= i 0)
      null
      (begin
        (iter_ H)
        (iter (- i 1)))))

  (send dc set-brush "green" 'solid)
  (send dc draw-rectangle
        Ox Oy
        maze-width
        maze-height)

  (iter W))

(def H 4)
(def W 4)
(def Ox 10)
(def Oy 11)
(def grid-width 40)
(def grid-height 40)
(def grid-border (/ grid-width 10))

(def maze-width (+ grid-border (* grid-width W)))
(def maze-height (+ grid-border (* grid-height H)))

(def (set-Oxy x y)
  (set! Ox x)
  (set! Oy y))

(def (set-size w h) (set! W w) (set! H h)
  (set! colors (make-colors W H))
  (set! maze-width (+ grid-border (* grid-width W)))
  (set! maze-height (+ grid-border (* grid-height H)))
  )
(def (set-grid-size w h) (set! grid-width w) (set! grid-height h)
  (set! maze-width (+ grid-border (* grid-width W)))
  (set! maze-height (+ grid-border (* grid-height H)))
  )
; assign color for every grid

(def (make-colors w h)
  (build-mlist W (λ (x) (build-mlist H (λ (x) "blue")))))
(def colors (make-colors W H))
(def (colors-ref clrs x y)
  (mlist-ref (mlist-ref clrs x) y))

(def set-colors
  (case-lambda
    [(clrs) (cond [(mpair? clrs) (set! colors clrs)])]
    [(x y color) (colors-set! colors x y color)]))

(def (colors-set! clrs x y color)
  (def (iter l i)
    (def (iter_ l_ j)
      (if (= j 0)
        (set-mcar! l_ color)
        (iter_ (mcdr l_) (j . - . 1))))
    (if (= i 0)
      (iter_ (mcar l) y)
      (iter (mcdr l) (i . - . 1))))
  (iter clrs x))
(def canvas 0)
(def (get-canvas) canvas)
(def (set-canvas cvs)
  (set! canvas cvs))
(def (refresh-maze)
  (send canvas refresh))

(module+ test
  (require rackunit)
  (displayln colors)
  (colors-set! colors 1 0 "red")
  (displayln colors)
  (displayln (colors-ref colors 1 0))
  )

(module* main #f
  (define my-canvas%
    (class canvas% ; The base class is canvas%
      ; Define overriding method to handle mouse events
      (define/override (on-event event)
                       (when (send event button-down? 'left)
                         (let* ([x (send event get-x)]
                                [y (send event get-y)]
                                [i (quotient (- x Ox) grid-width)]
                                [j (quotient (- y Oy) grid-height)]
                                [x_ (+ Ox (+ (* grid-width i)
                                             grid-border))]
                                [y_ (+ Oy (+ (* grid-height j)
                                             grid-border))])
                           (when (and (> (remainder (- x Ox) grid-width)
                                         grid-border)
                                      (> (remainder (- y Oy) grid-height)
                                         grid-border)
                                      (< i W)
                                      (< j H))
                             (send dc set-brush "black" 'solid)
                             (send dc draw-rectangle
                                   x_ y_
                                   (- grid-width grid-border)
                                   (- grid-height grid-border)))))
                       )
      ; Call the superclass init, passing on all init args
      (super-new)))
  (def frame (new frame% [label "Maze"]
                  [width 500]
                  [height 600]))
  (set-canvas (new my-canvas% [parent frame]
                   ;[min-width (+ maze-width Ox)]
                   ;[min-height (+ maze-height Oy)]
                   [paint-callback paint-maze]))
  (def dc (send canvas get-dc))
  (new button% [parent frame]
       [label "随机使一个格子变黑"]
       [callback (lambda (button event)
                   (let ([x (+ Ox (+ (* grid-width (random W))
                                     grid-border))]
                         [y (+ Oy (+ (* grid-height (random H))
                                     grid-border))])
                     (send dc set-brush "black" 'solid)
                     (send dc draw-rectangle
                           x y
                           (- grid-width grid-border)
                           (- grid-height grid-border)))
                   )])
  (new button% [parent frame]
       [label "恢复"]
       [callback (λ (button event)
                   (send canvas refresh))])

  (set-size 5 5)
  (set-Oxy 10 10)
  (colors-set! colors 0 0 "red")
  (colors-set! colors 3 0 "red")
  (colors-set! colors 0 3 "red")
  (colors-set! colors 3 3 "red")
  (send frame show #t)
  )
