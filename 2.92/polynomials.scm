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
            (add-poly (change-variable-internal p1 (get-final-variable (variable p1) (variable p2)))
                    (change-variable-internal p2 (get-final-variable (variable p1) (variable p2))) )
            ))
    
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
            (mul-poly (change-variable-internal p1 (get-final-variable (variable p1) (variable p2)))
                    (change-variable-internal p2 (get-final-variable (variable p1) (variable p2))) )
            ))
    (define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                        (div (term-list p1)
                            (term-list p2)))
            (div-poly (change-variable-internal p1 (get-final-variable (variable p1) (variable p2)))
                    (change-variable-internal p2 (get-final-variable (variable p1) (variable p2))) )
            ))

    (define (polynomial_zero? pol)
        (=zero? (term-list pol))
    )

    (define (get-final-variable var1 var2)
        (if (not (eq? var1 'no_variable)) var1 var2)
    )

    (define (change-variable-internal pol new-var)
        (if (same-variable? (variable pol) new-var)
            pol
            (let (
                (terms (term-list pol))
                (current-var (variable pol))
            )
                (if (empty-termlist? terms)
                    (make-poly new-var (the-empty-termlist-with-type-of terms))
                    (let (
                        (ft (first-term terms))
                        (order-as-polynomial (make_polynomial current-var (adjoin-term (make-term (order (first-term terms)) 1) (the-empty-termlist-with-type-of terms))))
                        (coeff-as-polynomial (make_polynomial current-var (adjoin-term (make-term 0 (coeff (first-term terms))) (the-empty-termlist-with-type-of terms))))
                    )
                        (add-poly
                            (if (is-polynomial? (coeff ft))
                                (mul-poly
                                    ;;; Not fully working in some cases, as pol * num is not working currently. We need the coercion logic here
                                    (make-poly new-var 
                                        (adjoin-term 
                                            (make-term 0 order-as-polynomial)
                                            (the-empty-termlist-with-type-of terms)))
                                    (change-variable-internal (contents (coeff ft)) new-var)
                                )
                                (make-poly new-var 
                                    (adjoin-term 
                                        (make-term 0 (mul order-as-polynomial coeff-as-polynomial))
                                        (the-empty-termlist-with-type-of terms)))
                            )
                            (change-variable-internal (make-poly (variable pol) (rest-terms (term-list pol))) new-var))
                    )
                )
            )
        )
    )

    (define (is-polynomial? tagged-obj)
        (eq? (type-tag tagged-obj) 'polynomial)
    )

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'div '(polynomial polynomial) (lambda (p1 p2) (tag (div-poly p1 p2))))
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
    (put 'change-variable 'polynomial (lambda (pol new-var) (tag (change-variable-internal pol new-var))))
    (put '=zero? '(polynomial) polynomial_zero?)
    'done)

(define (make_polynomial var terms)
    ((get 'make 'polynomial) var terms)
)
(define (change-variable pol new-var)
    ((get 'change-variable 'polynomial) (contents pol) new-var)
)

(define (number->polynomial number)
    (make_polynomial 'no_variable (adjoin-term (make-term 0 number) (the-empty-sparse-termlist)))
)

(put-coercion 'scheme-number 'polynomial number->polynomial)
(put-coercion 'rational 'polynomial number->polynomial)
(put-coercion 'complex 'polynomial number->polynomial)