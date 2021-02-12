; Use this command to run - using python to visualize the results
; gosh e2.49.scm | python3 visualize_points.py

(load "./segments.scm")
; (load "./e2.45.scm")
(load "./frame.scm")
(load "./e2.50.scm")


; outline of designated frame
(define outline
    (segments->painter (list (make_segment (make_vect 0 0) (make_vect 1 0))
                                (make_segment (make_vect 1 0) (make_vect 1 1))
                                (make_segment (make_vect 1 1) (make_vect 0 1))
                                (make_segment (make_vect 0 1) (make_vect 0 0))
    ))
)


; outline of designated frame
(define outline
    (segments->painter (list (make_segment (make_vect 0 0) (make_vect 1 0))
                                (make_segment (make_vect 1 0) (make_vect 1 1))
                                (make_segment (make_vect 1 1) (make_vect 0 1))
                                (make_segment (make_vect 0 1) (make_vect 0 0))
    ))
)


(define draw_x
    (segments->painter (list (make_segment (make_vect 0 0) (make_vect 1 1))
                                (make_segment (make_vect 1 0) (make_vect 0 1))
    ))
)



(define draw_diamond_shape
    (segments->painter (list (make_segment (make_vect 0.5 0) (make_vect 0 0.5))
                                (make_segment (make_vect 0 0.5) (make_vect 0.5 1))
                                (make_segment (make_vect 0.5 1) (make_vect 1 0.5))
                                (make_segment (make_vect 1 0.5) (make_vect 0.5 0))
    ))
)



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
    ))
)



; ((beside wave (rotate270 wave)) (make_frame (make_vect 0 0) (make_vect 1 0) (make_vect 0 1)))
