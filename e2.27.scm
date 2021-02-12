; implementing deep-reverse

(define (_deep_reverse_internal l current)
    (if (null? l)
        current
        (let ((left (car l)))
            (if (pair? left)
                (_deep_reverse_internal (cdr l) (cons (deep_reverse left) current))
                (_deep_reverse_internal (cdr l) (cons left current))
            )
        )
    )
)

(define (deep_reverse l)
    (_deep_reverse_internal l '())
)


(display (deep_reverse (list (list 1 2 (list 5 6)) (list 3 4))))
(newline)
