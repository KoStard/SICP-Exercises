(define (reverse_recurs l c)
    (if (null? l)
        c
        (reverse_recurs (cdr l) (cons (car l) c))
    )
)


(define (reverse_custom l)
    (reverse_recurs l '())
)


(display (reverse_custom (list 1 2)))
(newline)
