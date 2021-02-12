(load "./frame.scm")
(load "./e2.49.scm")

(define (up_split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up_split painter (- n 1))))
            (below painter (beside smaller smaller))
        )
    )
)

(test (up_split wave 5))
