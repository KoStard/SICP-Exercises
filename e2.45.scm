(load "./frame.scm")
(load "./e2.49.scm")

(define (split f s)
    (define (split_internal painter n)
        (if (= n 0)
            painter
            (let ((smaller (split_internal painter (- n 1))))
                (f painter (s smaller smaller))
            )
        )
    )
    split_internal
)

(define right_split (split beside below))
(define up_split (split below beside))


; (test (right_split wave 5))
