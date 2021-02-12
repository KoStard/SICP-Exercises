(define (square_list items)
    (if (null? items)
        '()
        (cons (square (car items)) (square_list (cdr items)))
    )
)


(define (square_list_2 items)
    (map square items)
)


(display (square_list (list 1 2 3 4 5 6 7)))
(newline)
(display (square_list_2 (list 1 2 3 4 5 6 7)))
(newline)
