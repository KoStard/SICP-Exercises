(load "./chapter3/60.scm")

;;; S has to have const term = 1
(define (invert-unit-series S)
    (define X (cons-stream 1 (negate (mul-series (stream-cdr S) X))))
    X
)

(define (sum-first-n series n)
    (define (sum-first-n-iter series n current)
        (if (> n 0)
            (sum-first-n-iter (stream-cdr series) (- n 1) (+ current (stream-car series)))
            current
        )
    )
    (sum-first-n-iter series n 0.0)
)

;;; (display-first-n (mul-series cosine-series (invert-unit-series cosine-series)) 10)
