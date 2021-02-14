;;; Now we have to handle cases when unnecessary parentheses are removed
;;; multiplication happens before addition
;;; This can be represented as:
;;; (...1 + ...2) = (...1 + (...2))
;;; (a * b * c * d + ...2) = ((((a * b) * c) * d) + ...2)

(load "./e2.58.a.scm")

(define (compound_expression? exp)
    (> (length exp) 3)
)

(define (second_exp exp)
    (cadddr exp)
)

(define (first_exp exp)
    (cadr exp)
)

(define (add_first_skipped_parenthesis exp)
    (cond ((not (compound_expression? exp)) exp)
        ((eq? (first_exp exp) '+) (make_sum (car exp) (cddr exp)))
        ((eq? (first_exp exp) '*) (cons (make_product (car exp) (caddr exp)) (cdddr exp)))
        (else (error "add_skipped_parenthesis not implemented for this scenario" exp))
        )
)

(define (sum? x) 
    (let ((exp_with_first_skipped_par (add_all_parenthesis x)))
        (and (pair? exp_with_first_skipped_par) (eq? (first_exp exp_with_first_skipped_par) '+))
    )
)

(define (product? x)
    (let ((exp_with_first_skipped_par (add_all_parenthesis x)))
        (and (pair? exp_with_first_skipped_par) (eq? (first_exp exp_with_first_skipped_par) '*))
    )
)


(define (addend s) (car (add_all_parenthesis s)))
(define (augend s) (caddr (add_all_parenthesis s)))

(define (multiplier p) (car (add_all_parenthesis p)))
(define (multiplicand p) (caddr (add_all_parenthesis p)))

(define (add_all_parenthesis exp)
    (if (compound_expression? exp)
        (add_all_parenthesis (add_first_skipped_parenthesis exp))
        exp
    )
)


(display (deriv '(x * x * x + x) 'x)) (newline)

(display (add_all_parenthesis '(x * x * x + x))) (newline)
