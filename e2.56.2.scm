;;; Improving the make-sum and make-product

(load "./e2.56.scm")

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

(define (make_sum a1 a2) 
    (cond 
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))
    )
)

(define (make_product m1 m2) 
    (cond 
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
    )
)


;;; (display (deriv '(+ x 3) 'x))  (newline)
;;; (display (deriv '(* x y) 'x))  (newline)
;;; (display (deriv '(* (* x y) (+ x 3)) 'x))  (newline)
