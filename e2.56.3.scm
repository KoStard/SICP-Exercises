;;; Here we solve the problem
(load "./e2.56.2.scm")

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same_variable? exp var) 1 0))
        ((sum? exp)
            (make_sum (deriv (addend exp) var)
                    (deriv (augend exp) var))
        )
        ((product? exp)
            (make_sum (make_product (multiplier exp) (deriv (multiplicand exp) var))
                    (make_product (multiplicand exp) (deriv (multiplier exp) var)))
        )
        ((exponentiation? exp)
            (make_product (make_product (exponent exp) (make_exponentiation (base exp) (- (exponent exp) 1))) (deriv (base exp) var)))
        (else
            (error "unknown expression type -- DERIV" exp))
    )
)


(define (make_exponentiation b e)
    (cond 
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))
    )
)

(define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**))
)

(define (base exp)
    (cadr exp)
)

(define (exponent exp)
    (caddr exp)
)


;;; (display (deriv '(** (+ x y) 10) 'x))
