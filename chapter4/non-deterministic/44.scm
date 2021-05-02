(load "./chapter4/non-deterministic/base-for-running.scm")

(run-all '(
    (define (make-queen row col) (cons row col))
    (define (queen-row queen) (car queen))
    (define (queen-col queen) (cdr queen))
    (define (check? q1 q2)
        (not 
            (or 
                (= (queen-row q1) (queen-row q2))
                (= (queen-col q1) (queen-col q2))
                (= (abs (- (queen-row q1) (queen-row q2))) (abs (- (queen-col q1) (queen-col q2))))
            )
        )
    )
    (define (is-it-safe? queens new-queen)
        (if (null? queens)
            true
            (and (check? (car queens) new-queen) (is-it-safe? (cdr queens) new-queen))
        )
    )
    (define (eight-queens-puzzle)
        (define (internal-loop queens col)
            (let ((queen (make-queen (amb 0 1 2 3 4 5 6 7) col)))
                (require (is-it-safe? queens queen))
                (if (= col 7)
                    (cons queen queens)
                    (internal-loop (cons queen queens) (+ col 1))
                )
            )
        )
        (internal-loop '() 0)
    )
    (eight-queens-puzzle)
    try-again
))