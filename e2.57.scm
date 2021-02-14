(load "./e2.56.3.scm")

(define (make_sum a1 . a2l)
    (if (null? (cdr a2l))  ;;; Checking if there was only one element passed
        (let ((a2 (car a2l)))
            (cond 
                ((=number? a1 0) a2)
                ((=number? a2 0) a1)
                ((and (number? a1) (number? a2)) (+ a1 a2))
                (else (list '+ a1 a2))
            )
        )
        (cond 
            ((=number? a1 0) (apply make_sum a2l))
            (else (make_sum a1 (apply make_sum a2l)))
        )
    )
)

(define (make_product m1 . m2l) 
    (if (null? (cdr m2l))  ;;; Checking if there was only one element passed
        (let ((m2 (car m2l)))
            (cond 
                ((or (=number? m1 0) (=number? m2 0)) 0)
                ((=number? m1 1) m2)
                ((=number? m2 1) m1)
                ((and (number? m1) (number? m2)) (* m1 m2))
                (else (list '* m1 m2))
            )
        )
        (cond 
            ((=number? m1 0) 0)
            ((=number? m1 1) (apply make_product m2l))
            (else (make_product m1 (apply make_product m2l)))
        )
    )
)

(define (augend s) 
    (if (> (length s) 3)
        (apply make_sum (cddr s))
        (caddr s)
    )
)

(define (multiplicand p) 
    (if (> (length p) 3)
        (apply make_product (cddr p))
        (caddr p)
    )
)

(display (deriv '(* (* x y) (+ x 3)) 'x))  (newline)   ; This will be equivalent to the next line
(display (deriv '(* x y (+ x 3)) 'x))  (newline)