(load "./e2.13.scm")

(define (par1 r1 r2)
    (add_interval (divide_interval r2 r1) (divide_interval r1 r2))
)


(define (par2 r1 r2)
    (divide_interval (add_interval r1 r2) (multiply_interval r1 r2))
)

(let (
    (A (make_center_percent 1 .001))
    (B (make_center_percent 1 .002))
    )
    ; (print_interval (par1 A A))
    ; (print_interval (par2 A A))
    (print_interval (divide_interval A B))
    (print_interval (divide_interval B A))
    )
