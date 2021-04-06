(load "./chapter4/17.scm")

(define (install-letrec)
    (define tag 'letrec)
    (define (matches? exp) (tagged-list? exp tag))

    (define (add-assignments bindings body)
        (if (null? bindings) body
            (cons (make-assignment (let-binding-var (car bindings)) (let-binding-exp (car bindings)))
                (add-assignments (cdr bindings) body)
            )
        )
    )

    (define (letrec->let exp)
        (let (
            (bindings (let-bindings exp))
            (body (let-body exp))
        )
            (make-let
                (map (lambda (binding) (make-binding (let-binding-var binding) ''*unassigned*)) bindings)
                (add-assignments bindings body)
            )
        )
    )

    (define (eval-element exp env)
        (display (letrec->let exp)) (newline)
        (evl (letrec->let exp) env)
    )

    (put-checker tag matches?)
    (put-evaluator tag eval-element)
)

(install-letrec)

;;; (evl '(letrec (
;;;         (a 4)
;;;         (b a)
;;;     ) 
;;;     (display b)
;;;     ) the-global-environment)