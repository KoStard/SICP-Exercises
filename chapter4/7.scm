(load "./chapter4/6.scm")

(define (install-letx)
    (define tag 'let*)
    (define (letx? exp) (tagged-list? exp 'letx))
    (define (letx-bindings exp) (cadr exp))
    (define (letx-body exp) (cddr exp))
    (define (letx-binding-var binding) (car binding))
    (define (letx-binding-exp binding) (cadr binding))

    (define (letx->lets exp)
        ;;; Check if there are any bindings
        (if (null? (letx-bindings exp))
            (sequence->exp (letx-body exp))
            (expand-bindings (letx-bindings exp) (letx-body exp))
        )
    )

    (define (expand-bindings bindings body)
        (if (null? bindings)
            body  ; We don't create begin here, as we expect that there were bindings, if not, then they should be handled before
            (make-let (list (car bindings)) (expand-bindings (cdr bindings)))
        )
    )

    (define (eval-letx exp env)
        (evl (letx->lets exp) env)
    )

    (put-checker tag letx?)
    (put-evaluator tag eval-letx)
)

(install-letx)