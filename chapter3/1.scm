;;; Implement accumulator

(define (make-accumulator sum)
    (lambda (x) 
        (set! sum (+ sum x))
        sum
    )
)


(define A (make-accumulator 5))
(display (A 10)) (newline)
(display (A 10)) (newline)