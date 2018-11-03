#!/usr/bin/env racket
#lang racket/gui
(require "libcommon.rkt")

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
                    grid-border)])
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

  (send dc set-brush "red" 'solid)
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
                [width (+ maze-width 100)]
                [height (+ maze-height 100)]))
(def canvas  (new my-canvas% [parent frame]
                  [min-width (+ maze-width Ox)]
                  [min-height (+ maze-height Oy)]
                  [paint-callback paint-maze]))
(def dc (send canvas get-dc))
(new button% [parent frame]
     [label "Turn Black"]
     ; Callback procedure for a button click:
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

(def mouse-event (new mouse-event% [event-type 'left-up]))
(send canvas on-event mouse-event)
(send frame show #t)
)
