(load "./e2.61.scm")

(define (union_set set1 set2)
    (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let ((x (car set1))
                (y (car set2))
            )
                (cond 
                    ((= x y) (cons x (union_set (cdr set1) (cdr set2))))
                    ((> x y) (cons y (union_set set1 (cdr set2))))
                    ((< x y) (cons x (union_set (cdr set1) set2)))
                )
            )
        )
    )
)

;;; (display (union_set '(1 3 5 7 8 9) '(1 2 3 4 5 6 8 10)))