(define (for_each f items)
    (if (null? items)
        '()
        (begin
            (f (car items))
            (for_each f (cdr items)))
    )
)


; (for_each (lambda (x) (display x) (newline)) (list 5 6 78 9 123))
