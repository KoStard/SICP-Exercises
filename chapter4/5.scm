(load "./chapter4/4.macro.scm")


(define (install-cond) 
    (define tag 'cond)

    (define (cond? exp) (tagged-list? exp 'cond))
    (define (cond-clauses exp) (cdr exp))
    (define (cond-else-clause? clause)
        (eq? (cond-predicate clause) 'else))
    (define (cond-recipient-clause? clause)
        (and (= (length clause) 3) (eq? (cadr clause) '=>))
    )
    (define (cond-recipient clause) (caddr clause))
    (define (cond-predicate clause) (car clause))
    (define (cond-actions clause) (cdr clause))
    (define (cond->if exp env)
        (expand-clauses (cond-clauses exp) env))

    (define (expand-clauses clauses env)
        (if (null? clauses)
            'false                          ; no else clause
            (let ((first (car clauses))
                    (rest (cdr clauses)))
                (cond (
                    (cond-else-clause? first)
                        (if (null? rest)
                            (sequence->exp (cond-actions first))
                            (error "ELSE clause isn't last -- COND->IF"
                                clauses)))
                    ((cond-recipient-clause? first)
                        (let ((val (evl (cond-predicate first) env)))
                            (make-if val
                                (make-application (cond-recipient first) (list val))
                                (expand-clauses rest env))
                        )
                    )
                    (else 
                        (make-if (cond-predicate first)
                            (sequence->exp (cond-actions first))
                            (expand-clauses rest env))
                    )
                ))))

    (define (eval-cond exp env) (evl (cond->if exp env) env))

    (put-checker tag cond?)
    (put-evaluator tag eval-cond)
)

(install-cond)

(define (make-application proc args)
    (cons proc args)
)
