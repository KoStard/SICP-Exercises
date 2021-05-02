; Implement require as a built-in special form

(load "./chapter4/non-deterministic/ch4-ambeval.scm")

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
    (let ((pproc (analyze (require-predicate exp))))
        (lambda (env succeed fail) 
            (pproc 
                env
                (lambda (val fail-predicate)
                    (if (not val)
                        (fail-predicate)
                        (succeed 'ok fail-predicate)
                    )
                )
                fail
            )
        )
    )
)