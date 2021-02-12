; from e2.51.scm
; (load "./frame.scm")
; (load "./e2.49.scm")

(define (below a b)
    (let ((ap (transform_painter a (make_vect 0.0 0.0)
                         (make_vect 1.0 0.0)
                         (make_vect 0.0 0.5)))
          (bp (transform_painter b (make_vect 0.0 0.5)
                               (make_vect 1.0 0.5)
                               (make_vect 0.0 1.0)))
        )
        (lambda (frame)
            (ap frame)
            (bp frame)
        )
        )
)

; ((below wave wave) (make_frame (make_vect 0 0) (make_vect 1 0) (make_vect 0 1)))
