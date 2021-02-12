(define (tree-map f tree)
    (map (lambda (item)
        (if (pair? item) (tree-map f item) (f item))) tree)
)

(define (square_tree tree) (tree-map square tree))

(display (square_tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))
