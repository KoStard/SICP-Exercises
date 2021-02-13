;;; derivatives
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
        (else
            (error "unknown expression type -- DERIV" exp))
    )
)

;;; Should not be executed on the operations
(define (variable? x) (symbol? x))

(define (same_variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make_sum a1 a2) (list '+ a1 a2))
(define (make_product m1 m2) (list '* m1 m2))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))



;;; Testing, this is awesome :) 
;;; (display (deriv '(+ x 3) 'x))  (newline)
;;; (display (deriv '(* x y) 'x))  (newline)
;;; (display (deriv '(* (* x y) (+ x 3)) 'x))  (newline)
