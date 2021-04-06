(define (install-quoted) 
    (define tag 'quote)
    (define (quoted? exp)
        (tagged-list? exp tag))

    (define (text-of-quotation exp) (cadr exp))

    (define (analyze-quoted exp)
        (let ((qval (text-of-quotation exp)))
            (lambda (env) qval)))

    (put-checker tag quoted?)
    (put-analyzer tag analyze-quoted)
)

(install-quoted)