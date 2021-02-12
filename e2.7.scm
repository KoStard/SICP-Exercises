(define (add_interval x y)
    (make_interval (+ (lower_bound x) (lower_bound y))
                   (+ (upper_bound x) (upper_bound y))
    )
)

(define (substract_interval x y)
    (add_interval x (make_interval (- (upper_bound y)) (- (lower_bound y)) ))
)


(define (multiply_interval x y)
    (let (
        (p1 (* (lower_bound x) (lower_bound y)))
        (p2 (* (lower_bound x) (upper_bound y)))
        (p3 (* (upper_bound x) (lower_bound y)))
        (p4 (* (upper_bound x) (upper_bound y)))
        )
        (make_interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4)
        )
        )
)


(define (divide_interval x y)
    (if (and (<= (lower_bound y) 0) (>= (upper_bound y) 0))
        (raise "Can't divide by interval that spans zero")
        (multiply_interval x
            (make_interval (/ 1.0 (upper_bound y))
                        (/ 1.0 (lower_bound y))
            ))
    )
)


(define (make_interval a b)
    (cons a b)
)

(define upper_bound cdr)
(define lower_bound car)

(define (print_interval interval)
    (display "[")
    (display (lower_bound interval))
    (display ":")
    (display (upper_bound interval))
    (display "]")
    (newline)
)


(define (width interval)
    (/ (- (upper_bound interval) (lower_bound interval)) 2.0)
)


; (print_interval (divide_interval (make_interval 3 4) (make_interval 1 6)))
