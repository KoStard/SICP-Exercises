(load "./e2.13.scm")

(define (par1 r1 r2)
    (divide_interval (multiply_interval r1 r2)
                    (add_interval r1 r2))
)


(define (par2 r1 r2)
    (let ((one (make_interval 1 1)))
        (divide_interval one
            (add_interval (divide_interval one r1) (divide_interval one r2))
        )
    )
)


; So we can see the difference, this needs further investigation
; (0.500002000002 0.0029999920000238307perc)
; (0.49999999999999994 0.001000000000000001perc)
; (0.5000043750098437 0.0044999737501707155perc)
; (0.4999998749997187 0.0015000007500018963perc)
(let (
    (A (make_center_percent 1 .001))
    (B (make_center_percent 1 .002))
    )
    (print_interval_percentage (par1 A A))
    (print_interval_percentage (par2 A A))
    (print_interval (par1 A A))
    (print_interval (par2 A A))
    ; (print_interval_percentage (par1 A B))
    ; (print_interval_percentage (par2 A B))
    ; (print_interval (multiply_interval A A))
    )
