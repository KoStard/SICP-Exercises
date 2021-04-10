(load "./chapter4/lazy_evaluator/ch4-leval.scm")

(define the-global-environment (setup-environment))
; (driver-loop)

(define (run-exp exp)
    (eval exp the-global-environment)
)

(define (run-all seq)
    (run-exp (car seq))
    (if (not (null? (cdr seq))) 
        (run-all (cdr seq))
    ))

(run-all
    '(
        (define count 0)
        (define (id x) (set! count (+ count 1)) x)
        ; The define doesn't use thucks, so it's value is evaluated
        ; But because the value is application of a compound procedure, it's arguments are 
        ; being delayed. Because of this first id is evaluated, the count is changed to 1, 
        ; but only when the value of w is requested by display, the second w is getting executed
        ; and hence the count gets updated to 2.
        (define w (id (id 10)))
        (display count) (newline)
        (display w) (newline)
        (display count) (newline)
    )
)