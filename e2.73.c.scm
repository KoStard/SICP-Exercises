(load "./e2.56.3.scm")
(load "./e2.73.scm")

(define (exponentiation_deriv_package)
    (define (deriv_product oprs var)
        (make_product (make_product (cadr oprs) (make_exponentiation (car oprs) (- (cadr oprs) 1))) (deriv (car oprs) var))
    )
    (put 'deriv '** deriv_product)
)
(exponentiation_deriv_package)

(display (deriv '(** (+ x y) 10) 'x))