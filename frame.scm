(load "./vectors.scm")
(load "./below.scm")

(define (make_frame origin edge1 edge2)
    (list origin edge1 edge2)
)

(define (draw_line from to) (display from) (display " : ") (display to) (newline))

(define get_origin car)
(define get_edge1 cadr)
(define get_edge2 caddr)

(define (frame_coord_map frame)
    (lambda (v)
        (add_vect
            (get_origin frame)
            (add_vect (scale_vect (get_edge1 frame) (xcor_vect v))
                    (scale_vect (get_edge2 frame) (ycor_vect v))))
    )
)

(define (transform_painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame_coord_map frame)))
            (let ((new_origin (m origin)))
                (painter
                    (make_frame new_origin
                            (sub_vect (m corner1) new_origin)
                            (sub_vect (m corner2) new_origin)))
            )
        )
    )
)

(define (flip_vert painter)
    (transform_painter painter
            (make_vect 0.0 1.0)
            (make_vect 1.0 1.0)
            (make_vect 0.0 0.0))
)

(define (shrink_to_upper_right painter)
    (transform_painter painter
            (make_vect 0.5 0.5)
            (make_vect 1.0 0.5)
            (make_vect 0.5 1.0))
)

(define (rotate90 painter)
    (transform_painter painter
            (make_vect 1.0 0.0)
            (make_vect 1.0 1.0)
            (make_vect 0.0 0.0))
)

(define (squash_inwards painter)
    (transform_painter painter
            (make_vect 0.0 0.0)
            (make_vect 0.65 0.35)
            (make_vect 0.35 0.65))
)

(define (beside painter1 painter2)
    (let ((split_point (make_vect 0.5 0.0)))
        (let ((paint_left
                (transform_painter painter1
                    (make_vect 0.0 0.0)
                    split_point
                    (make_vect 0.0 1.0)))
                (paint_right
                    (transform_painter painter2
                        split_point
                        (make_vect 1.0 0.0)
                        (make_vect 0.5 1.0))))
            (lambda (frame)
                (paint_left frame)
                (paint_right frame)
            )
        )
    )
)


(define (test painter) (painter (make_frame (make_vect 0 0) (make_vect 1 0) (make_vect 0 1))))


(define (square_of_four tl tr bl br)
    (lambda (painter)
        (let ((top (beside (tl painter) (tr painter)))
                (bottom (beside (bl painter) (br painter))))
            (below bottom top)
        )
    )
)
