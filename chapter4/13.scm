;;; If we remove only from the first frame, then this will create a lot of confusion with let, internal lambdas, etc, as we would not notice
;;; when did the new frame generate. So I think removing from the first found one is the best option

(load "./chapter4/12.scm")

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


;;; (let ((env (extend-environment (list ) the-empty-environment)))
;;;     ;;; (define-variable! 'a 4 env)
;;;     ;;; (display (lookup-variable-value 'a env)) (newline)
;;;     ;;; (make-unbound! 'a env)
;;;     ;;; (display (lookup-variable-value 'a env)) (newline)

;;;     (evl 
;;;         '(define a 4)
;;;         env
;;;     )
;;;     (display env)

;;;     ;;; (display (frame-bindings (first-frame env)))
;;; )