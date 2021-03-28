(load "./chapter3/62.scm")

(define (stream-limit stream tolerance)
    (let ((s0 (stream-car stream)) (s1 (stream-car (stream-cdr stream))))
        (if (< (abs (- s1 s0)) tolerance)
            s1
            (stream-limit (stream-cdr stream) tolerance)
        )
    )
)

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
            (stream-map (lambda (guess) (sqrt-improve guess x)) guesses))
    )
    guesses
)

(define (sqrt-improve guess x)
    (/ (+ (/ x guess) guess) 2.0)
)


(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance)
)

;;; (display (sqrt 2 0.00000001)) (newline)