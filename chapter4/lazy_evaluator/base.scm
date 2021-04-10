(load "./chapter4/lazy_evaluator/ch4-leval.scm")

(define the-global-environment (setup-environment))

(define (run-exp exp)
    (eval exp the-global-environment)
)

(define (run-all seq)
    (run-exp (car seq))
    (if (not (null? (cdr seq))) 
        (run-all (cdr seq))
    ))