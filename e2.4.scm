(define (conss x y)
    (lambda (m) (m x y))
)

(define (cars z)
    (z (lambda (x y) x))
)

(define (cdrs z)
    (z (lambda (x y) y))
)

; Trying out


(let (
    (z (conss 4 5))
    )
    (display (cars z))
    (newline)
    (display (cdrs z))
    (newline)
    )
