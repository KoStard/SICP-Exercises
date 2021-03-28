(load "./chapter3/12.scm")

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x
)

(define x (list 'a 'b 'c))
(display (last-pair (make-cycle x)))