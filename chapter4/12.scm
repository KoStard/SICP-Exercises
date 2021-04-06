(load "./chapter4/11.scm")

;;; Will return the pair of var and val, if is false, then not found
(define (scan-bindings bindings var)
    (cond 
        ((null? bindings) false)
        ((eq? var (caar bindings)) (car bindings))
        (else (scan-bindings (cdr bindings) var))
    )
)

(define (make-binding var val)
    (list var val)
)

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons (make-binding var val) (frame-bindings frame)))
)

(define (scan-environment env var)
    (if (eq? env the-empty-environment)
        false
        (let ((bindings (frame-bindings (first-frame env))))
            (let ((binding (scan-bindings bindings var))) 
                (if binding
                    (list binding env)
                    (scan-environment (enclosing-environment env) var)
                )
            )
        )
    )
)

(define (lookup-variable-value var env)
    (let ((binding-env (scan-environment env var)))
        (if binding-env
            (cadr (car binding-env))
            (error "Unbound variable" var)
        )
    )
)

(define (set-variable-value! var val env)
    (let ((binding-env (scan-environment env var)))
        (if binding-env
            (set-car! (cdar binding-env) val)
            (error "Unbound variable -- SET" var)
        )
    )
)

(define (define-variable! var val env)
    (let ((binding-env (scan-environment env var)))
        (if binding-env
            (set-car! (cdar binding-env) val)
            (add-binding-to-frame! var val (first-frame env))
        )
    )
)

;;; (let ((env (extend-environment (list ) the-empty-environment)))
;;;     (define-variable! 'a 4 env)
;;;     (display (lookup-variable-value 'a env))
;;;     ;;; (display (frame-bindings (first-frame env)))
;;; )