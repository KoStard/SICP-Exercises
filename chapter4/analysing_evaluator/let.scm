(define (install-let)
    (define tag 'let)
    (define (let? exp) (tagged-list? exp tag))
    (define (let-bindings exp) (if (named-let? exp) (caddr exp) (cadr exp)))
    (define (let-body exp) (if (named-let? exp) (cdddr exp) (cddr exp)))
    (define (let-binding-var binding) (car binding))
    (define (let-binding-exp binding) (cadr binding))
    (define (named-let? exp) (symbol? (cadr exp)))
    (define (name exp) (cadr exp))
    (define (let->combination exp)
        (cond ((named-let? exp) 
                (let ((the-lambda (make-lambda (map let-binding-var (let-bindings exp)) (let-body exp))))
                    (make-application 
                        (make-lambda
                            (list (name exp))
                            (make-application
                                the-lambda
                                (map let-binding-exp (let-bindings exp))
                            )
                        )
                        (list the-lambda)
                    )
                )
            )
            (else 
                (make-application
                    (make-lambda (map let-binding-var (let-bindings exp)) (let-body exp))
                    (map let-binding-exp (let-bindings exp))
                )
            )
        )
    )

    (define (analyze-let exp)
        (analyze (let->combination exp))
    )

    (define (make-let bindings body)
        (cons tag (cons bindings body))
    )

    (define (make-binding var exp) (list var exp))
    
    (put-checker tag let?)
    (put-analyzer tag analyze-let)
    (put tag 'make make-let)
    (put tag 'make-binding make-binding)
    (put tag 'let-bindings let-bindings)
    (put tag 'let-body let-body)
    (put tag 'let-binding-var let-binding-var)
    (put tag 'let-binding-exp let-binding-exp)
)

(install-let)

(define make-let (get 'let 'make))
(define make-binding (get 'let 'make-binding))
(define let-bindings (get 'let 'let-bindings))
(define let-body (get 'let 'let-body))
(define let-binding-var (get 'let 'let-binding-var))
(define let-binding-exp (get 'let 'let-binding-exp))
(define (make-application proc args) (cons proc args))