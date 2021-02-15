;;; Representing the sets as ordered lists
;;; Assumes that the input will be ordered set too

(define (element_of_set? x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element_of_set? x (cdr set)))
    )
)

(define (intersection_set set1 set2)
    (if ((or (null? set1) (null? set2)) '())
        (let ((x (car set1)) (y (car set2)))
            (cond 
                ((= x y) (cons x (intersection_set (cdr set1) (cdr set2))))
                ((> x y) (intersection_set set1 (cdr set2)))
                ((< x y) (intersection_set (cdr set1) set2))
            )
        )
    )
)

(define (adjoin_set x set)
    (cond ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin_set x (cdr set))))
        ((< x (car set)) (cons x set))
    )
)

;;; (display (adjoin_set 4 '(1 3 5 7)))