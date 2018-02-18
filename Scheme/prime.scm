(define (smallest-divisor n) (find-divisor n 2))

(define (square n)
  (* n n))
(define (divides? a b)
  (= (remainder a b) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next test-divisor)
  (cond ((= test-divisor 2) 3)
        (else (+ test-divisor 2))))

; check whether a number is prime using Fermat test.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1)))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test-Carmichael-numbers n)
  (define (try-it a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (try-it (+ a 1)))
          (else true)))
  (try-it 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((> a b) 0)
        ((fast-prime? a 1) (timed-prime-test a) (search-for-primes (+ a 1) b))
        (else (search-for-primes (+ a 1) b))))

