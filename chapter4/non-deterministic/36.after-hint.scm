(define (square x) (* x x))

(define (round-to-integer x) (inexact->exact x))

(define (perfect-square? x)
    (= x (square (round-to-integer (sqrt x))))
)

(define (all-pythagorean-triples)
    (let ((j (an-integer-from 1)))
        (let ((i (an-integer-between 1 (- j 1))))
            (let ((k-square (+ (square i) (square j))))
                (require (perfect-square? k-square))
                (list i j (sqrt k-square))
            )
        )
    )
)