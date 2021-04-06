;;; Evaluating from left to right
(define (list-of-values-lr exps env)
    (if (no-operands? exps)
        '()
        (let ((f (evl (first-operand exps) env))
            (r (list-of-values (rest-operands exps) env))
        )
            (cons f r)
        )
))


;;; Evaluating from right to left
(define (list-of-values-lr exps env)
    (if (no-operands? exps)
        '()
        (let ((r (list-of-values (rest-operands exps) env))
            (f (evl (first-operand exps) env))
        )
            (cons f r)
        )
))