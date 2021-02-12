(define (square_tree tree)
    (map (lambda (item)
        (if (pair? item) (square_tree item) (square item))) tree)
)

(display (square_tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
