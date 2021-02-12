(load "./segments.scm")
(load "./frame.scm")
(load "./e2.50.scm")
(load "./e2.49.scm")
(load "./e2.45.scm")

(define wave
    (segments->painter (list
        (make_segment (make_vect 0.45 1) (make_vect 0.4 0.85))
        (make_segment (make_vect 0.4 0.85) (make_vect 0.45 0.7))
        (make_segment (make_vect 0.45 0.7) (make_vect 0.3 0.71))
        (make_segment (make_vect 0.3 0.71) (make_vect 0.15 0.65))
        (make_segment (make_vect 0.15 0.65) (make_vect 0 0.85))

        (make_segment (make_vect 0.55 1) (make_vect 0.6 0.85))
        (make_segment (make_vect 0.6 0.85) (make_vect 0.55 0.7))
        (make_segment (make_vect 0.55 0.7) (make_vect 0.75 0.7))
        (make_segment (make_vect 0.75 0.7) (make_vect 1 0.3))

        (make_segment (make_vect 0 0.75) (make_vect 0.15 0.5))
        (make_segment (make_vect 0.15 0.5) (make_vect 0.3 0.65))
        (make_segment (make_vect 0.3 0.65) (make_vect 0.35 0.55))
        (make_segment (make_vect 0.35 0.55) (make_vect 0.2 0))

        (make_segment (make_vect 1 0.2) (make_vect 0.55 0.6))
        (make_segment (make_vect 0.55 0.6) (make_vect 0.7 0))

        (make_segment (make_vect 0.6 0) (make_vect 0.5 0.4))
        (make_segment (make_vect 0.5 0.4) (make_vect 0.4 0))

        (make_segment (make_vect 0.475 0.825) (make_vect 0.48 0.8))
        (make_segment (make_vect 0.48 0.8) (make_vect 0.52 0.8))
        (make_segment (make_vect 0.52 0.8) (make_vect 0.525 0.825))
    ))
)

; (test wave)


(define (corner_split painter n)
    (if (= n 0)
        painter
        (let ((up (up_split painter (- n 1)))
            (right (right_split painter (- n 1))))
            (let ((top_left up)
                (bottom_right right)
                (corner (corner_split painter (- n 1))))
                (beside (below painter top_left)
                        (below bottom_right corner))
                )
            )
    )
)


; (test (corner_split wave 5))

(define (square_limit painter n)
    (let ((part (corner_split painter n)))
        ((square_of_four flip_vert rotate180 identity flip_horiz) part)
    )
)


(test (square_limit wave 4))
