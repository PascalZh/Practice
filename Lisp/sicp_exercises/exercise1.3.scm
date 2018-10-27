; common procedures

(define (pow b p)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (iter res a n)
    (if (= n 0)
        res
        (if (even? n)
          (iter res (square a) (/ n 2))
          (iter (* res a) a (- n 1)))))
  (iter 1 b p))

(define (average a b)
  (/ (+ a b) 2))

(define (cube x)
  (* x (* x x)))

; >>>>>>>>>>>>>>>>>>>>>>
; fixed point procedure
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

; golden ratio
(define (golden-ratio-f x)
  (+ 1 (/ 1 x)))

; sovle this equation(with fixed point): x^x = 1000
(define (g x) (/ (log 1000) (log x)))
; with average damping
(define (g-a-d x) (/ (+ x (/ (log 1000) (log x))) 2))

; Exercise 1.37
; infinite continued fraction

;(define (cont-frac n d k)
  ;(newline)
  ;(display d)
  ;(let ((next-d (+ 1 (/ 1 d)))
        ;(next-n 1))
    ;(if (= k 1)
      ;(/ next-n next-d)
      ;(cont-frac next-n next-d (- k 1)))))

; iterative procedure:
(define (cont-frac-v1 n d k)
  (define (loop result term)
    (if (= term 0)
      result
      (loop (/ (n term)
               (+ (d term) result))
               (- term 1))))
  (loop 0 k))
; recursive procedure:
(define (cont-frac-v2 n d k)
  (define (cont-frac-rec i)
    (if (> i k)
      0
      (/ (n i)
         (+ (d i) (cont-frac-rec i+1)))))
  (cont-frac-rec 1))
(define (cont-frac n d k)
  (cont-frac-v1 n d k))

; Exercise 1.38
; generate 1, 2, 1, 1, 4, 1, 1, 6, 1
(define (d-v1 i)
  (if (= (remainder i 3) 2)
    (* (/ (+ i 1) 3) 2)
    1))

; Exercise 1.39
(define (tan-cf x k)
  (define (cf n d k)
    (define (loop result term)
      (if (= term 0)
        result
        (loop (/ (n term)
                 (- (d term) result)); - instead of +, pay attention!
              (- term 1))))
    (loop 0 k))
  (cf (lambda (i)
        (if (= i 1)
          x
          (square x)))
      (lambda (i)
        (- (* 2.0 i) 1))
      k))

; Exercise 1.40
(define (deriv g)
 (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)
(define (newtons-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43
(define (repeated f cnt)
  (if (= cnt 1)
    f
    (compose f (repeated f (- cnt 1)))))
    ; Why don't use (lambda (x) (f (repeated f (- cnt 1))))?
    ; Look at it! There is no 'x' in the function!
    ; Instead, we use compose to avoid this.

; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Exercise 1.45
(define (average-damp f)
  (lambda (x) (/ (+ x (f x))
                 2.0)))

(define (nth-root x n)
  (define (log2 a)
    (/ (log a) (log 2)))
  ((fixed-point ((repeated average-damp (floor (log2 n)))
                 (lambda (y)
                   (/ x (pow y (- n 1)))))
                1.0)))

; Exercise 1.46
(define (iterative-improve improve good-enough?)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve improve good-enough?) (improve guess)))))

; Iterative-improve version sqrt function:
(define (sqrt-v2 n)
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x))
       tolerance))
  ((iterative-improve (lambda (y) (improve y n))
                      (lambda (y) (good-enough? y n)))
   1.0))
; Iterative-improve version sqrt function:
(define (fixed-point-v2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve improve
                      (lambda (x) (close-enough? x (improve x))))
   1.0))
