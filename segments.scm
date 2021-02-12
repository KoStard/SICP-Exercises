(load "./vectors.scm")
(load "./for_each.scm")
(load "./frame.scm")

(define (make_segment start_vec end_vec)
    (list start_vec end_vec)
)

(define (start_segment segment)
    (car segment)
)

(define (end_segment segment)
    (cadr segment)
)

(define (segments->painter segments_list)
    (lambda (frame)
        (for_each
            (lambda (segment)
                (draw_line
                    ((frame_coord_map frame) (start_segment segment))
                    ((frame_coord_map frame) (end_segment segment)))
            )
            segments_list)
    )
)
