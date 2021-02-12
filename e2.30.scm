(define (square_tree tree)
    (cond ((null? tree) '())
        ((pair? tree) (cons (square_tree (car tree)) (square_tree (cdr tree))))
        (else (square tree))
    )
)


(display (square_tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
