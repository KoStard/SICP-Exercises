(load "./vectors.scm")
(load "./2.47.a.scm")

(define (frame_coord_map frame)
    (lambda (v)
        (add_vect
                (get_origin frame)
                (add_vect (scale_vect (get_edge1 frame) (xcor_vect v))
                            (scale_vect (get_edge2 frame) (ycor_vect v))))
    )
)
