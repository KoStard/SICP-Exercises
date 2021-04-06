(define (install-definition) 
    (define tag 'define)
    (define (definition? exp)
        (tagged-list? exp tag))
    (define (definition-variable exp)
        (if (symbol? (cadr exp))
            (cadr exp)
            (caadr exp)))
    (define (definition-value exp)
        (if (symbol? (cadr exp))
            (caddr exp)
            (make-lambda (cdadr exp)   ; formal parameters
                        (cddr exp)))) ; body

    (define (analyze-definition exp)
        (let ((var (definition-variable exp))
                (vproc (analyze (definition-value exp))))
            (lambda (env)
            (define-variable! var (vproc env) env)
            'ok)))


    (define (make-define variable value)
        (list tag variable value)
    )

    (put-checker tag definition?)
    (put-analyzer tag analyze-definition)
    (put tag 'make make-define)
    (put tag 'definition-variable definition-variable)
    (put tag 'definition-value definition-value)
)
(install-definition) 
(define (make-define . args)
    (apply (get 'define 'make) args)
)

(define (definition-variable . args) (apply (get 'define 'definition-variable) args))
(define (definition-value . args) (apply (get 'define 'definition-value) args))
