;Exercise4.22
;(define (let? exp) (tagged-list? exp 'let))
;(define (analyze-let exp)
  ;(define (vars rt p)
    ;(if (null? p)
      ;rt
      ;(vars (cons (caar p) rt) (cdr p))))
  ;(define (exps rt p)
    ;(if (null? p)
      ;rt
      ;(exps (cons (cadar p) rt) (cdr p))))
  ;(analyze (cons (cons 'lambda (cons (vars '() (cadr exp)) (cddr exp)))
        ;(exps '() (cadr exp)))))

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

; Query Evaluator
;(define input-prompt ";;; Query input:")
;(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
             (stream-map
               (lambda (frame)
                 (instantiate
                   q
                   frame
                   (lambda (v f)
                     (contract-question-mark v))))
               (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

; Scheme Evaluator

;; save the primitive apply
(define apply-in-underlying-scheme apply)
;; redefine (error "string")
;; My version of error will stop the driver-loop and return to
;; original mit-scheme.
(define (error str . remaining)
	(newline)
	(display "ERROR =_=: ") (display str) (display-list remaining))
(define (display-list l)
	(if (null? l)
			(RESTART 1)
			(begin (display (car l)) (display " ")
						 (display-list (cdr l)))))

;; eval and analyze
(define (eval exp env) ((analyze exp) env))
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
				((let? exp) (analyze-let exp))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

;; all kinds of analyze {{{
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                    (cproc env)
                    (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env))
             aprocs)))))
;; }}}

;; predicate and implementation {{{
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))


(define (variable? exp) (symbol? exp))


(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp) (if (symbol? (cadr exp)) (cadr exp) (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ;parameters
                 (cddr exp))))  ;body


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (cons 'begin seq))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (cond-clauses exp) (cdr exp))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        (else (make-begin seq))))

(define (let? exp) (tagged-list? exp 'let))
(define (analyze-let exp)
  (define (vars rt p)
    (if (null? p)
      rt
      (vars (cons (caar p) rt) (cdr p))))
  (define (exps rt p)
    (if (null? p)
      rt
      (exps (cons (cadar p) rt) (cdr p))))
  (analyze (cons (cons 'lambda (cons (vars '() (cadr exp)) (cddr exp)))
        (exps '() (cadr exp)))))
;; }}}

;; apply and procedure {{{
;; apply
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
            (procedure-parameters proc)
            args
            (procedure-environment proc))))
        (else
          (error "Unknown procedure type: EXECUTE-APPLICATION"
                 proc))))
(define (application?  exp) (pair? exp))

;; procedure

(define (tagged-list? exp tag)
  (if (pair? exp)
			(eq? (car exp) tag)
			false))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; primitive procedures

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
				(list '* *)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
;; }}}


;; Enviroments and define {{{
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
	(set-car! frame (cons var (frame-variables frame)))
	(set-cdr! frame (cons val (frame-values frame))))

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
			(cons (make-frame vars vals) base-env)
			(if (< (length vars) (length vals))
					(error "Too many arguments supplied" vars vals)
					(error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars)
						 (env-loop (enclosing-environment env)))
						((eq? var (car vars)) (car vals))
						(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
				(error "Unbound variable" var)
				(let ((frame (first-frame env)))
					(scan (frame-variables frame)
								(frame-values frame)))))
	(env-loop env))

(define (set-variable-value! var val env)
	(define (env-loop env)
	  (define (scan vars vals)
			(cond ((null? vars)
						 (env-loop (enclosing-environment env)))
						((eq? var (car vars)) (set-car! vals val))
						(else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
				(error "Unbound variable: SET!" var)
				(let ((frame (first-frame env)))
					(scan (frame-variables frame)
								(frame-values frame)))))
	(env-loop env))

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond ((null? vars)
						 (add-binding-to-frame! var val frame))
						((eq? var (car vars)) (set-car! vals val))
						(else (scan (cdr vars) (cdr vals)))))
		(scan (frame-variables frame) (frame-values frame))))
;; }}}


;; Setup the environment and driver loop

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (prompt-for-input string)
  (newline) (newline) (display string)
  (newline) (display "(='_'=) >>"))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
		(display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(driver-loop)
