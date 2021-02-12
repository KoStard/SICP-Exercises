(load "./e2.12.scm")


; assumption that the percentages are small and the numbers are positive


(define (multiply_small_percentage p1 p2)
    (- (* (+ 1 p1) (+ 1 p2)) 1)
)

(define (multiply_interval_with_small_percentage x y)
    (make_center_percent (* (center x) (center y)) (multiply_small_percentage (percentage x) (percentage y)))
)

(define (print_interval_percentage interval)
    (display "(")
    (display (center interval))
    (display " ")
    (display (percentage interval))
    (display "perc)")
    (newline)
)


; (print_interval_percentage (multiply_interval_with_small_percentage (make_center_percent 5 0.02) (make_center_percent 9 0.01)))
