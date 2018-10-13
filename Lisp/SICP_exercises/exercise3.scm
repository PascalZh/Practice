;; The most practice codes of the chapter 3 have been deleted owing to a rm.
;; Dying to rewrite it!!!
;; so I give up =_=

;; Streams as signals
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

; Exercise 3.73
(define (RC R C dt)
  (lambda (i v0)
    (add-stream (scale-stream i R)
                (integral (scale-stream i (/ 1 C)) v0 dt))))

; Exercise 3.74
(define sign-change-detector (lambda (x y) x))
(define sense-data (cons-stream 1 sense-data))
(define zero-crossings (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

; Exercise 3.76
(define (smooth s)
  (stream-map (lambda (x1 x2) (/ (+ x1 x2) 2))
              (cons-stream 0 s)
              s))

(define (make-zero-crossings in-s smooth)
  (stream-map sign-change-detector (smooth in-s) (cons-stream 0 (smooth in-s))))

; Exercise 3.77
(define (integral delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
      the-empty-stream
      (integral (delay (stream-cdr integrand))
                (+ (* dt (stream-car integrand))
                   initial-value)
                dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; Exericise 3.79
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; Exercise 3.80
(define (RLC R L C dt)
  (define ()))

;; Common functions
(define (scale-stream s k)
  (stream-map (lambda (x) (* x k)) s))
(define (add-stream s1 s2)
  (stream-map + s1 s2))
