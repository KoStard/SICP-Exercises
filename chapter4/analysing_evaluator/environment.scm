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

(define (filter-out-cdr ls val)
    (if (eq? (cadr ls) val)
        (cons (car ls) (cddr ls))
        (cons (car ls) (filter-out-cdr (cdr ls) val))
    )
)

(define (make-unbound! var env)
    (let ((binding-env (scan-environment env var)))
        (if binding-env
            (let ((owner-env (cadr binding-env)))
                (let ((frame (first-frame owner-env)))
                    (if (eq? var (caar (frame-bindings frame)))
                        (set-car! frame (cdr (frame-bindings frame)))
                        (set-car! frame (filter-out-cdr (frame-bindings frame) var))
                    )
                )
            )
            (error "Unbound variable passed to be unbound -- MAKE-UNBOUND!" var)
        )
    )
)

(define (extend-environment bindings base-env)
    (cons (make-frame bindings) base-env)
)

(define (frame-bindings frame) (car frame))
(define (make-frame bindings)
  (list bindings))

(define the-empty-environment '())

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

