(load "./frame.scm")

(define (flip_horiz painter)
    (transform_painter painter
            (make_vect 1.0 0.0)
            (make_vect 0.0 0.0)
            (make_vect 1.0 1.0))
)

(define (rotate180 painter)
    (transform_painter painter
            (make_vect 1.0 1.0)
            (make_vect 0.0 1.0)
            (make_vect 1.0 0.0))
)

(define (rotate270 painter)
    (transform_painter painter
            (make_vect 0.0 1.0)
            (make_vect 0.0 0.0)
            (make_vect 1.0 1.0))
)
