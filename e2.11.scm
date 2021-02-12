(load "./e2.7.scm")


(define (multiply_interval x y)
    (let (
        (a1 (lower_bound x))
        (b1 (upper_bound x))
        (a2 (lower_bound y))
        (b2 (upper_bound y))
        )
        (let (
            (na1 (negative? a1))
            (nb1 (negative? b1))
            (na2 (negative? a2))
            (nb2 (negative? b2))
            )
            (cond
                ((and na2 nb2 na1 nb1)
                    (make_interval (* b1 b2) (* a1 a2)))
                ((and na2 nb2 na1)
                    (make_interval (* b1 a2) (* a1 a2)))
                ((and na2 nb2)
                    (make_interval (* b1 a2) (* a1 b2)))
                ((and na2 na1 nb1)
                    (make_interval (* a1 b2) (* a1 a2)))
                ((and na2 na1)
                    (make_interval (min (* a1 b2) (* a2 b1)) (max (* a1 a2) (* b2 b1))))
                ((and na2)
                    (make_interval (* b1 a2) (* b1 b2)))
                ((and na1 nb1)
                    (make_interval (* a1 b2) (* b1 a2)))
                ((and na1)
                    (make_interval (* a1 b2) (* b1 b2)))
                (else
                    (make_interval (* a1 a2) (* b1 b2)))
                )
        )
        )
)


; Checking all cases
; (print_interval (multiply_interval (make_interval -6 -5) (make_interval -8 -4)))
; (print_interval (multiply_interval (make_interval -6 5) (make_interval -8 -4)))
; (print_interval (multiply_interval (make_interval 1 5) (make_interval -8 -4)))
; (print_interval (multiply_interval (make_interval -6 -5) (make_interval -8 4)))
; (print_interval (multiply_interval (make_interval -6 5) (make_interval -8 4)))
; (print_interval (multiply_interval (make_interval -6 5) (make_interval -8 10)))
; (print_interval (multiply_interval (make_interval 1 5) (make_interval -8 4)))
; (print_interval (multiply_interval (make_interval -6 -5) (make_interval 2 4)))
; (print_interval (multiply_interval (make_interval -6 5) (make_interval 2 4)))
; (print_interval (multiply_interval (make_interval 1 5) (make_interval 2 4)))
