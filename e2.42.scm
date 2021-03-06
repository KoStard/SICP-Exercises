(load "./flatmap.scm")
(load "./enumerate_interval.scm")

(define empty_board '())

; returning the row numbers for columns from right to left
(define (queens board_size)
    (define (queen_cols k)
        (if (= k 0)
            (list empty_board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest_of_queens)
                        (map (lambda (new_row)
                            (adjoin_position new_row k rest_of_queens))
                            (enumerate_interval 1 board_size)))
                    (queen_cols (- k 1))))
        )
    )
    (queen_cols board_size)
)


(define (adjoin_position row column queens)
    (cons row queens)
)

(define (safe? column positions)
    (null? (filter (lambda (e) (= e (car positions))) (cdr positions)))
)


(queens 8)
