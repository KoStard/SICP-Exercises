;;; Implement deriv procedure using data-directed dispatch

(load "./e2.56.scm")
(load "./e2.54.scm")

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same_variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))
    )
)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


;;; Maybe this is not a good implementation of put and get, but it seems to be working1
(define implementation_mapping_table '(()))

(define (put generic_name implementation_key implementation)
    (set-cdr! implementation_mapping_table 
        (cons 
            (list (list generic_name implementation_key) implementation)
            (cdr implementation_mapping_table)
        )
    )
)

(define (get generic_name implementation_key)
    (define (get_internal table)
        (if (null? table)
            false
            (if (equal? (caar table) (list generic_name implementation_key))
                (cadar table)
                (get_internal (cdr table))
            )
        )
    )
    (get_internal (cdr implementation_mapping_table))
)

;;; number? and variable? can't be data-directed dispatched, because they don't have type tag, which in other cases is the type

(define (attach_tag type_tag contents)
    (cons type_tag contents)
)

(define (type_tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE_TAG" datum)
    )
)

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)
    )
)

(define (sum_deriv_package)
    (define (deriv_sum oprs var)
        (make_sum (deriv (car oprs) var)
                  (deriv (cadr oprs) var))
    )
    (put 'deriv '+ deriv_sum)
)

(define (product_deriv_package)
    (define (deriv_product oprs var)
        (make_sum (make_product (car oprs) (deriv (cadr oprs) var))
                    (make_product (cadr oprs) (deriv (car oprs) var)))
    )
    (put 'deriv '* deriv_product)
)

(sum_deriv_package)
(product_deriv_package)

;;; (display (deriv '(+ x 3) 'x))  (newline)
;;; (display (deriv '(* x y) 'x))  (newline)
;;; (display (deriv '(* (* x y) (+ x 3)) 'x))  (newline)


;;; For subtask d, we would just need to change the installation part.