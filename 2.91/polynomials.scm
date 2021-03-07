;;; The polynomials don't depend on the actual implementation of the terms

(define (install-polynomial-package)
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (same-variable? v1 v2)
        (and (variable? v1) (variable? v2) (eq? v1 v2)))
    (define (variable? x) (symbol? x))

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                        (add (term-list p1)
                            (term-list p2)))
            (error "Polys not in same var -- ADD-POLY"
                    (list p1 p2))))
    
    (define (negate p)
        (make-poly (variable p) (negate_obj (term-list p)))
    )
    (define (sub-poly p1 p2)
        (add-poly p1 (negate p2))
    )
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                        (mul (term-list p1)
                            (term-list p2)))
            (error "Polys not in same var -- MUL-POLY"
                    (list p1 p2))))
    (define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                        (div (term-list p1)
                            (term-list p2)))
            (error "Polys not in same var -- DIV-POLY"
                    (list p1 p2))))

    (define (polynomial_zero? pol)
        (terms_zero? (term-list pol))
    )

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'div '(polynomial polynomial) (lambda (p1 p2) (tag (div-poly p1 p2))))
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
    (put '=zero? '(polynomial) polynomial_zero?)
    'done)

(define (make_polynomial var terms)
    ((get 'make 'polynomial) var terms)
)