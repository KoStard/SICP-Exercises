(load "./chapter4/lazy_evaluator/base.scm")


; With this the p2 is starting to work too.
; (define (eval-sequence exps env)
;     (cond ((last-exp? exps) (eval (first-exp exps) env))
;             (else (actual-value (first-exp exps) env)
;                 (eval-sequence (rest-exps exps) env))))

(run-all
    '(
        (define (p1 x)
            (set! x (cons x '(2)))
            x
        )

        (define (p2 x)
            (define (p e)
                e
                x
            )
            (p (set! x (cons x '(2))))
        )

        (display (p1 2)) (newline)
        (display (p2 2)) (newline)
    )
)
