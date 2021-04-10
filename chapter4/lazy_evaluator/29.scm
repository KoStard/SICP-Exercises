; If the evaluator is memoizing, then the count would be 1, as x would not be executed twice.
; If not, then it would be 2.

(load "./chapter4/lazy_evaluator/base.scm")

(run-all
    '(
        (define count 0)
        (define (id x) (set! count (+ count 1)) x)
        (define (square x) (* x x))
        ; (display (square (id 10))) (newline)
        ; (display count) (newline)  ; The result is 1, which is showing that our evaluator is memoizing
    )
)

(run-all
    '(
        (define (mult x y)
            (if (= y 1)
                x
                (+ (mult x (- y 1)) x)
            )
        )
        ; (display (mult (id 10) 10)) (newline)
        ; ; It's again printing 1, but if we use non-memoizing force-it implementation, then would get 10
        ; (display count)
    )
)

(run-all
    '(
        (define (fibonacci-with-x x n)
            (cond 
                ((= n 0) 0)
                ((= n 1) x)
                (else (+ (fibonacci-with-x x (- n 1)) (fibonacci-with-x x (- n 2))))
            )
        )
        ; If the lazy evaluator is not memoizing thunks, then the count will be equal to the fibonacci
        ; value, because id would be evaluated for every leaf of the fibonacci "tree". This would
        ; be significantly slower with such evaluator.
        (display (fibonacci-with-x (id 1) 10)) (newline)
        (display count)
    )
)