(load "./chapter4/5.scm")

(define (install-let)
    (define tag 'let)
    (define (let? exp) (tagged-list? exp 'let))
    (define (let-bindings exp) (cadr exp))
    (define (let-body exp) (cddr exp))
    (define (let-binding-var binding) (car binding))
    (define (let-binding-exp binding) (cadr binding))
    (define (let->combination exp)
        (make-application
            (make-lambda (map let-binding-var (let-bindings exp)) (let-body exp))
            (map let-binding-exp (let-bindings exp))
        )
    )

    (define (eval-let exp env)
        (evl (let->combination exp) env)
    )

    (define (make-let bindings body)
        (list tag bindings body)
    )

    (define (make-binding var exp) (list var exp))
    
    (put-checker tag let?)
    (put-evaluator tag eval-let)
    (put tag 'make make-let)
    (put tag 'make-binding make-binding)
)

(install-let)

(define make-let (get 'let 'make))
(define make-binding (get 'let 'make-binding))