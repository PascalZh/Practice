; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Common
(define nil '())
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise2.5
(define (exp base n)
  (define (iter x result)
    (if (> x n)
      result
      (iter (+ x 1) (* result base))))
  (iter 1 1))

; Assuming b^p is the divisor of z, get the biggest p.
(define (get-exp-of-divisor z base)
  (define (iter x)
    (if (= 0 (remainder z (exp base (+ x 1))))
      (iter (+ x 1))
      x))
  (iter 1))

; Here are my-cons, my-car, and my-cdr.
(define (my-cons a b)
  (* (exp 2 a) (exp 3 b)))
(define (my-car z)
  (get-exp-of-divisor z 2))
(define (my-cdr z)
  (get-exp-of-divisor z 3))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.17
(define (last-pair _list)
  (if (null? (cdr _list))
    (car _list)
    (last-pair (cdr _list))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.18
(define (reverse _list)
  (if (null? (cdr _list))
    _list
    (append (reverse (cdr _list))
            (cons (car _list) nil))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.19
(define (cc amount coin-values)
  (define (no-more? c-v)
    (null? c-v))
  (define (except-first-denomination c-v)
    (cdr c-v))
  (define (first-denomination c-v)
    (car c-v))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values))
                 coin-values)))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.19
; Q:How write functions that have arbitrary numbers of parameters?
; A:As following.
(define (same-parity first . others)
  (define (iter _list result remainder-val)
    (if (null? _list)
      result
      (iter (cdr _list)
            (if (= (remainder (car _list) 2) remainder-val)
                (append result (list (car _list)))
                result)
            remainder-val)))
  (iter others (list first) (remainder first 2)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.23
(define (for-each proc _list)
  (cond ((not (null? _list))
    (proc (car _list))
    (for-each proc (cdr _list)))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.27
(define (deep-reverse _list)
  (let ((car-list
        (if (pair? (car _list))
          (deep-reverse (car _list))
          (car _list))))
    (if (null? (cdr _list))
      _list
      (append (deep-reverse (cdr _list))
              (cons car-list nil)))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.28
(define (fringe _list)
  (define (iter l result)
    (if (null? l)
      result
      (iter (cdr l)
            (if (pair? (car l))
              (iter (car l) result)
              (cons (car l) result)))))
  (reverse (iter _list nil)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.28
;(define (square-tree _tree)
  ;(map (lambda (sub-tree)
         ;(if (pair? sub-tree)
           ;(square-tree sub-tree)
           ;(square sub-tree)))
       ;_tree))
; New version of procedure "square-tree" with procedure "tree-map".
(define (tree-map proc _tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       _tree))
(define (square-tree _tree)
  (tree-map square _tree))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (sub-set)
                          (append (list (car s)) sub-set))
                        rest)))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.32
(define (accumulate op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (map p seq)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              seq))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length seq)
  (accumulate (lambda (x y) (+ 1 y))
              0
              seq))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              (reverse coefficient-sequence)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.35
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (node)
                     (cond ((null? node) 0)
                           ((pair? node)
                            (count-leaves node))
                           (else 1)))
                   t)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0
              (accumulate-n * 1 (list v w))))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (accumulate
                       (lambda (x y)
                         (cons (dot-product x v) y))
                       nil
                       cols))
         m)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.39
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

; Some insights:
; Fold-right is working in the same direction with forming list using cons.
; But reverse need to "cons" in reverse order.
; So reverse-using-fold-right have to use append
; However fold-left is in accordance with reverse, so it just uses cons.
(define (reverse-using-fold-right _l)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil
              _l))
(define (reverse-using-fold-left _l)
  (fold-left (lambda (x y)
               (cons y x))
             nil
             _l))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Some problems I meet.
(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)    ; why use flat map here?
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))));
             s)))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
; Assuming pass (1 2 3) into permutations
; If we use map,
; firstly, map procedure to 1, we get ((1 2 3) (1 3 2)) to replace 1
; So we turn (1 2 3) into:
; (((1 2 3) (1 3 2)) 2 3)
; Apparently, there are some redundant parentheses.
; It will even increases with the recursion.
; So use flatmap to remove unnecessary parentheses.

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.40
(define (enumerate-interval a b)
  (if (> a b)
    nil
    (cons a (enumerate-interval (+ a 1) b))))
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (prime? x)
  (define (test divisor)
    (cond ((> (square divisor) x) true)
          ((= 0 (remainder x divisor)) false)
          (else (test (+ divisor 1)))))
  (test 2))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.41
(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j)
             (map (lambda (k) (list k j i))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
(define (ordered-triples-sum n s)
  (filter (lambda (triple) (= s (accumulate + 0 triple)))
          (unique-triples n)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.42
(define (queens board-size)
  (define (e-b-iter n)
    (if (= n 0)
      nil
      (cons 0 (e-b-iter (- n 1)))))
  (define empty-board
    (e-b-iter board-size))

  (define (safe? k positions)
    (define (at pos _k)
      (if (= _k 1)
        (car pos)
        (at (cdr pos) (- _k 1))))
    (define (iter n) ; check whether n-th columns is safe.
      (cond ((= n 0) true)
            ((or (= (at positions n) (at positions k))
                 (= (+ n (at positions n))
                    (+ k (at positions k)))
                 (= (- n (at positions n))
                    (- k (at positions k))))
             false)
            (else (iter (- n 1)))))
    (iter (- k 1)))

  (define (adjoin-position new-row k rest-of-queens)
    (define (iter _k _list)
      (if (= _k 1)
        (cons new-row (cdr _list))
        (cons (car _list) (iter (- _k 1)
                                (cdr _list)))))
    (iter k rest-of-queens))

  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.46~2.47
(define (make-vect xcor ycor) (cons xcor ycor))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))
(define (add-vect a b) (cons (+ (car a) (car b))
                             (+ (cdr a) (cdr b))))
(define (sub-vect a b) (cons (- (car a) (car b))
                             (- (cdr a) (cdr b))))
(define (scale-vect k vect) (cons (* k (car vect)) (* k (cdr vect))))

(define (make-frame origin edge1 edge2) (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (cdr (cdr frame)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.48~2.49
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; a
(define (pt-outline-of-frame frame)
  (let ((origin-vect (origin-frame frame))
        (edge1-vect (edge1-frame frame))
        (edge2-vect (edge2-frame frame)))
    (define segment-list (list
                           (make-segment origin-vect (add-vect origin-vect edge1-vect))
                           (make-segment origin-vect (add-vect origin-vect edge2-vect))
                           (make-segment (add-vect origin-vect edge1-vect) (add-vect (add-vect origin-vect edge2-vect) edge1-vect))
                           (make-segment (add-vect origin-vect edge2-vect) (add-vect (add-vect origin-vect edge2-vect) edge1-vect))))
    ((segments->painter segment-list) frame)))

; b
; ...
; not done...

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.50
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))
; rotate painters counterclockwise by 180 degrees and 270 degrees
(define (cc-rotate180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))
(define (cc-rotate270 painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.51
(define (below p1 p2)
  (let ((painter-below
          (transform-painter
            p1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            (make-vect 0.0 0.5)))
        (painter-above
          (transform-painter
            p2
            (make-vect 0.0 0.5)
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
    (lambda (frame)
      (painter-below frame)
      (painter-above frame))))
(define (below p1 p2)
  (rotate270 (beside (rotate90 p1) (rotate90 p2))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.54
(define (equal? a b)
  (cond ((and (not (pair? a))
              (not (pair? b)))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else false)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.56~2.57
(define (deriv _exp _var)
  (cond ((number? _exp) 0)
        ((variable? _exp) (if (same-variable? _exp _var) 1 0))
        ((sum? _exp) (make-sum (deriv (addend _exp) _var)
                               (deriv (augend _exp) _var)))
        ((product? _exp)
         (make-sum
           (make-product (multiplier _exp)
                         (deriv (multiplicand _exp) _var))
           (make-product (deriv (multiplier _exp) _var)
                         (multiplicand _exp))))
        ((exponentiation? _exp)
         (make-product
           (exponent _exp)
           (make-product
             (make-exponentiation (base _exp)
                                  (make-sum (exponent _exp) -1))
             (deriv (base _exp) _var))))
        (else
          (error "unknown expression type: DERIV" _exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? _exp _num)
  (and (number? _exp) (= _exp _num)))

; derivative of sum
;(define (make-sum a1 a2)
  ;(cond ((=number? a1 0) a2)
        ;((=number? a2 0) a1)
        ;((and (number? a1) (number? a2))
         ;(+ a1 a2))
        ;(else (list '+ a1 a2))))
;(define (sum? x) (and (pair? x) (eq? (car x) '+)))
;(define (addend s) (cadr s))
;;(define (augend s) (caddr s))
;(define (augend s)
  ;(accumulate make-sum 0 (cddr s)))

; derivative of product
;(define (make-product m1 m2)
  ;(cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ;((=number? m1 1) m2)
        ;((=number? m2 1) m1)
        ;((and (number? m1) (number? m2)) (* m1 m2))
        ;(else (list '* m1 m2))))
;(define (product? x) (and (pair? x) (eq? (car x) '*)))
;(define (multiplier p) (cadr p))
;;(define (multiplicand p) (caddr p))
;(define (multiplicand p)
  ;(accumulate make-product 1 (cddr p)))

; derivative of exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 1) base)
        ((=number? exponent 0) 1)
        ((=number? base 1) 1)
        (else (list '** base exponent))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.58
; ordinary mathematical notation version of deriv
; however, the result is steel full of parentheses that contain two arguments.
; a is too simple, all you need to do is just change the position of + in the list.
; b solution:
(define (sum? expr)
  (eq? '+ (smallest-op expr)))
(define (product? expr)
  (eq? '* (smallest-op expr)))
; smallest-op searches an expression for the lowest-precedence operator.
(define (smallest-op expr)
  (accumulate (lambda (a b)
                (if (operator? a)
                  (min-precedence a b)
                  b))
              'maxop
              expr))
(define *precedence-table*
  '( (maxop . 10000)
     (minop . -10000)
     (+ . 0)
     (* . 1) ))
(define (operator? x)
  (define (loop op-pair)
    (cond ((null? op-pair) #f)
          ((eq? x (caar op-pair)) #t)
          (else (loop (cdr op-pair)))))
  (loop *precedence-table*))
(define (min-precedence a b)
  (if (precedence<? a b)
    a
    b))
(define (precedence<? a b)
  (< (precedence a) (precedence b)))
(define (precedence op)
  (define (loop op-pair)
    (cond ((null? op-pair)
           (error "Operator not defined -- PRECEDENCE:" op))
          ((eq? op (caar op-pair))
           (cdar op-pair))
          (else
            (loop (cdr op-pair)))))
  (loop *precedence-table*))
(define (prefix sym _list)
  (if (or (null? _list) (eq? sym (car _list)))
    '()
    (cons (car _list) (prefix sym (cdr _list)))))

; add
(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
    (if (singleton? a)
      (car a)
      a)))
(define (singleton? a)
  (and (pair? a) (= (length a) 1)))

(define (addend expr)
  (let ((a (prefix '+ expr)))
    (if (singleton? a)
      (car a)
      a)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

; multiply
(define (multiplier expr)
  (let ((m (prefix '* expr)))
    (if (singleton? m)
      (car m)
      m)))

(define (multiplicand expr)
  (let ((m (cdr (memq '* expr))))
    (if (singleton? m)
      (car m)
      m)))

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Representing Sets
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (if (element-of-set? x set) set (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.59~2.60
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.61~2.62
(define (adjoin-set x set)
  (define (iter _x _set result)
    (cond ((null? _set) (append result (list _x)))
          ((= _x (car _set)) (append result _set))
          ((< _x (car _set)) (append result (cons _x _set)))
          (else (iter _x (cdr _set) (cons (car _set) result)))))
  (iter x set nil))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
            ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
            ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.63~2.64
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
    nil
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))
(define  (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree nil))

; The first one is of O(nlog n) complexity(append takes O(n) time complexity), by contrast, the second one is of O(n) complexity.
; So we choose the second.
(define tree->list tree->list-2)

(define (list->tree elements) ; ordered list
  (car (partial-tree elements (length elements))))
; partial-tree takes as arguments an integer n and list of at least n elements and return a cons, which the car of it is a balanced
; binary tree formed with first n elements of the list; the cdr of it is a list containing remaining elements.
(define (partial-tree elts n)
  (if (= n 0)
    (cons nil elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>
; Huffman Code
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? node) (eq? (car node) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

; The procedures following are called generic procedures.
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      nil
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE_BRANCH" bit))))

; Huffman algorithm requires and a set.
; We will represent a set of leaves and trees as a list of elements.
; arranged in increasing order of weight.
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    nil
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.68
(define (exists? x _list)
  (cond ((null? _list) false)
        ((eq? x (car _list)) true)
        (else (exists? x (cdr _list)))))
(define (encode message tree)
  (define (encode-symbol symbol tree)
    (let ((left (left-branch tree)) (right (right-branch tree)))
      (cond ((leaf? tree) nil)
            ((exists? symbol (symbols left)) (cons '0 (encode-symbol symbol left)))
            ((exists? symbol (symbols right)) (cons '1 (encode-symbol symbol right)))
            (else (error "symbol is not in the Huffman tree: ENCODE-SYMBOL" symbol)))))
  (if (null? message)
    nil
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.69
(define (generate-huffman-tree pairs)
  (define (successive-merge _list)
    (if (null? (cdr _list))
      (car _list)
      (let ((first (car _list)) (second (cadr _list)) (remainings (cddr _list)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      remainings)))))
  (successive-merge (make-leaf-set pairs)))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Generic Operation
;(define (apply-generic op . args)
  ;(let ((type-tags (map type-tag args)))
    ;(let ((proc (get op type-tags)))
      ;(if proc
        ;(apply proc (map contents args))
        ;(error "No method for these types: APPLY-GENERIC" (list op type-tags))))))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))
          (error "No method for these types: APPLY-GENERIC" (list op type-tags)))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-rational x y) ((get 'make 'rational) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-scheme-number x) ((get 'make 'scheme-number) x))

(define (install-rectangular-package)
  ;;internel procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  (define (=zero? x) (= (numer x) 0))
  ;; interface to rest of system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (real-part z2))))
  (define (sub z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (real-part z2))))
  (define (mul z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (define (=zero? x)
    (and (= (real-part x) 0) (= (imag-part x) 0)))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div z1 z2))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.78~2.80
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Wrong datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Wrong datum -- CONTENTS" datum))))

(define (equ? x1 x2) (apply-generic 'equ? x1 x2))

(define (=zero? x) (apply-generic '=zero? x))

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; the last part of this chapter

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
; Exercise 2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1))
        (list (the-empty-termlist) L1)
        (let ((new-c (div (coeff t1) (coeff t2)))
              (new-o (- (order t1) (order t2))))
          (let ((rest-of-result
                  (div-terms (sub-terms L1
                                        (mul-terms L2
                                                   (list (make-term new-o new-r)))))))
            (list (adjoin-term (make-term new-o new-c)
                               (car rest-of-result))
                  (cadr rest-of-result))))))))
