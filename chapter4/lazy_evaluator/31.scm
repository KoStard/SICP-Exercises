(load "./chapter4/lazy_evaluator/base.scm")


(define (delay-it exp env)
  (list 'thunk 'non-memo exp env))

(define (delay-it-memo exp env)
  (list 'thunk 'memo exp env))

(define (thunk-type thunk) (cadr thunk))
(define (memo-thunk? obj) (and (thunk? obj) (eq? (thunk-type obj) 'memo)))
(define (thunk-exp thunk) (caddr thunk))
(define (thunk-env thunk) (cadddr thunk))

(define (force-it obj)
  (cond ((memo-thunk? obj)
            (let ((result (actual-value
                                (thunk-exp obj)
                                (thunk-env obj))))
                (set-car! obj 'evaluated-thunk)
                (set-car! (cdr obj) result)  ; replace exp with its value
                (set-cdr! (cdr obj) '())     ; forget unneeded env
                result)
        )
        ((thunk? obj)
            (actual-value (thunk-exp obj) (thunk-env obj))
        )
        ((evaluated-thunk? obj)
            (thunk-value obj))
        (else obj)))

(define (lazy? parameter)
    (and (pair? parameter) (eq? (cadr parameter) 'lazy))
)

(define (lazy-memo? parameter)
    (and (pair? parameter) (eq? (cadr parameter) 'lazy-memo))
)

(define (lazy-parameter-name parameter) (car parameter))

(define (parameter-name parameter)
    (if (or (lazy? parameter) (lazy-memo? parameter)) (lazy-parameter-name parameter) parameter)
)

(define (procedure-parameters p)
    (map parameter-name (cadr p))
)

(define (raw-procedure-parameters p) (cadr p))

(define (list-of-compound-procedure-args parameters exps env)
    (if (no-operands? exps) '()
        (cons 
            (cond 
                ((lazy? (car parameters)) (delay-it (first-operand exps) env))
                ((lazy-memo? (car parameters)) (delay-it-memo (first-operand exps) env))
                (else (actual-value (first-operand exps) env))
            )
            (list-of-compound-procedure-args (cdr parameters) (rest-operands exps) env)
        )
    )
)

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-compound-procedure-args (raw-procedure-parameters procedure) arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(run-all
    '(
        (define (p1 x)
            (set! x (cons x '(2)))
            x
        )

        (define (p2 x)
            (define (p (e lazy))
                e
                x
            )
            (p (set! x (cons x '(2))))
        )

        ; (display (p1 2)) (newline)
        (display (p2 2)) (newline)
    )
)
