(load "./e2.33.scm")

(define (count_leaves t)
    (accumulate + (if (or (pair? t) (null? t)) 0 1) (map count_leaves (if (pair? t) t '())))
)


(define x (cons (list 1 2) (list 3 4)))
(display (count_leaves x))
(newline)
(display (count_leaves (list x x)))
(newline)
