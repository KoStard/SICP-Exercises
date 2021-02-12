(define (make_vect x y)
    (list x y)
)

(define xcor_vect car)
(define ycor_vect cadr)

(define (add_vect v1 v2)
    (make_vect (+ (xcor_vect v1) (xcor_vect v2))
                (+ (ycor_vect v1) (ycor_vect v2)))
)

(define (sub_vect v1 v2)
    (make_vect (- (xcor_vect v1) (xcor_vect v2))
                (- (ycor_vect v1) (ycor_vect v2)))
)

(define (scale_vect v s)
    (make_vect (* s (xcor_vect v))
                (* s (ycor_vect v)))
)


; (display (scale_vect (make_vect 5 7) 4))
