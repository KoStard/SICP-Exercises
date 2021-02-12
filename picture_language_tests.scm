(load "./frame.scm")
(load "./e2.49.scm")
(load "./e2.45.scm")
(load "./e2.50.scm")

(define (corner_split painter n)
    (if (= n 0)
        painter
        (let ((up (up_split painter (- n 1)))
            (right (right_split painter (- n 1))))
            (let ((top_left (beside up up))
                (bottom_right (below right right))
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
        (let ((half (below (flip_vert part) part)))
            (beside (flip_horiz half) half)
        )
    )
)


(test (square_limit wave 4))
