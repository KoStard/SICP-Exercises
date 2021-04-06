(define (install-lambda) 
    (define tag 'lambda)
    (define (lambda? exp) (tagged-list? exp 'lambda))
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))

    (define (make-lambda parameters body)
        (cons 'lambda (cons parameters body)))

    
    (define (analyze-lambda exp)
        (let ((vars (lambda-parameters exp))
                (bproc (analyze-sequence (lambda-body exp))))
            (lambda (env) (make-procedure vars bproc env))))

    (put-checker tag lambda?)
    (put-analyzer tag analyze-lambda)
    (put tag 'make-lambda make-lambda)
)
(install-lambda) 
(define (make-lambda . args)
    (apply (get 'lambda 'make-lambda) args)
)