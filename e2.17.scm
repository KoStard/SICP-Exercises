(define (last_pair l)
    (if (null? l)
        '()
        (if (null? (cdr l))
            l
            (last_pair (cdr l))
        )
    )
)


(display (last_pair (list 23 72 149 34)))
(newline)
