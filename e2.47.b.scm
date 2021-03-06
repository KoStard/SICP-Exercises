(load "./e2.46.scm")

(define (make_frame origin edge1 edge2)
    (list origin (cons edge1 edge2))
)

(define get_origin car)
(define get_edge1 caadr)
(define get_edge2 cdadr)

(display (get_origin (make_frame (make_vect 2 3) (make_vect -1 2) (make_vect 4 5)))) (newline)
(display (get_edge1 (make_frame (make_vect 2 3) (make_vect -1 2) (make_vect 4 5)))) (newline)
(display (get_edge2 (make_frame (make_vect 2 3) (make_vect -1 2) (make_vect 4 5)))) (newline)
