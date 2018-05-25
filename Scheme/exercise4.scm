; Exercise4.22
(define (analyze-let exp)
  (define (vars rt p)
    (if (null? p)
      rt
      (vars (cons (caar p) rt) (cdr p))))
  (define (exps rt p)
    (if (null? p)
      rt
      (exps (cons (cdar p) rt) (cdr p))))
  (cons (cons 'lambda (cons (list (vars '() (cadr exp))) (cddr exp)))
        (exps '() (cadr exp))))

; Exercise4.35
(define (require p) (if (not p) (amb)))
(define (an-integer-between i j)
  (require (<= i j))
  (amb i (an-integer-between (+ i 1) j)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
