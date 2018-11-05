#!/usr/bin/env racket
#lang racket/gui
(require "libcommon.rkt")

(provide show-maze)
(provide set-origin set-size set-grid-size set-colors
         set-background get-canvas)
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

  (send dc set-brush background'solid)
  (send dc draw-rectangle
        Ox Oy
        maze-width
        maze-height)

  (iter W))

(def H 10)          (def W 10)
(def Ox 10)         (def Oy 11)
(def grid-width 30) (def grid-height 30)
(def grid-border (/ grid-width 10))

(def maze-width (+ grid-border (* grid-width W)))
(def maze-height (+ grid-border (* grid-height H)))

(def (make-colors w h)
  (build-mlist W (λ (x) (build-mlist H (λ (x) "blue")))))

; assign color for every grid
(def colors (make-colors W H))
(def background "cyan")

(def (set-origin x y)
  (set! Ox x)
  (set! Oy y)
  (refresh-maze))

(def (set-size w h) (set! W w) (set! H h)
  (set! colors (make-colors W H))
  (set! maze-width (+ grid-border (* grid-width W)))
  (set! maze-height (+ grid-border (* grid-height H)))
  (refresh-maze))

(def (set-grid-size w h) (set! grid-width w) (set! grid-height h)
  (set! maze-width (+ grid-border (* grid-width W)))
  (set! maze-height (+ grid-border (* grid-height H)))
  (refresh-maze))

(def set-colors
  (case-lambda
    [(clrs) (cond [(mpair? clrs)
                   (set! colors clrs)
                   (refresh-maze)])]
    [(x y color) (colors-set! colors x y color)
                 (refresh-maze)]))

(def (set-background bg) (set! background bg) (refresh-maze))

(def (colors-ref clrs x y)
  (mlist-ref (mlist-ref clrs x) y))

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
  (when (not (eq? canvas 0))
    (send canvas refresh)))

; test {{{
(module+ test
  (require rackunit)
  (displayln colors)
  (colors-set! colors 1 0 "red")
  (displayln colors)
  (displayln (colors-ref colors 1 0))
  )
; }}}

(def (show-maze)
  (def frame (new frame% [label "Maze"]
                  [width (+ Ox Ox maze-width)]
                  [height (+ Oy Oy maze-height)]))
  (set-canvas (new canvas% [parent frame]
                   [paint-callback paint-maze]))
  (send frame show #t))


(def (racket-print x)
  (if (void? x)
    null
    (begin
      (display "Racket ]=> ")
      (displayln x))))

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

; use this function to communicate with other process
(def (maze-driver-loop)
  (let ([input (begin (display ">>") (read))])
    (if (eof-object? input)
      null
      (begin
        (cond [(pair? input)
               (with-handlers ([any/c (λ (msg) (racket-print msg))])
                 (racket-print (eval input ns)))]
              [else (println input)])
        (maze-driver-loop)))))

(module* main #f
  (show-maze)
  (thread maze-driver-loop))
