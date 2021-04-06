(load "./chapter4/14.base.scm")

(define (lookup-variable-value var env)
    (let ((binding-env (scan-environment env var)))
        (if binding-env
            (let ((value (cadar binding-env)))
                (if (and (symbol? value) (eq? value '*unassigned*))
                    (error "Trying to access variable that has not been assigned yet")
                    value
                )
            ) 
            (error "Unbound variable" var)
        )
    )
)

(define (scan-out-defines body)
    (define (internal-loop current-body)
        (if (null? current-body)
            (cons (list ) (list ))  ; new body, the defines
            (let ((rest-processed (internal-loop (cdr current-body)))
                (first-exp (car current-body))
            )
                (if ((get-checker 'define) first-exp)
                    (cons 
                        (cons 
                            (make-assignment 
                                (definition-variable first-exp)
                                (definition-value first-exp)
                            )
                            (car rest-processed)
                        )
                        (cons
                            (definition-variable first-exp)
                            (cdr rest-processed)
                        )
                    )
                    (cons 
                        (cons first-exp (car rest-processed))
                        (cdr rest-processed)
                    )
                )
            )
        )
    )
    (let ((processed (internal-loop body)))
        (if (null? (cdr processed))
            (car processed)
            (list 
                (make-let (map (lambda (var) (make-binding var ''*unassigned*)) (cdr processed))
                    (car processed)
                )
            )
        )
    )
)

;;; (let () ; ((exp (scan-out-defines '((define a 4) (display a)))))
;;;     ;;; (for-each (lambda (ex) (display ex) (newline) (evl ex the-global-environment)) exp)
;;;     ;;; (evl '(let ((a 4)) (display a)) the-global-environment)
;;;     ;;; (display (make-let (list (make-binding 'a 4)) (list ))) (newline)
;;;     ;;; (display (evl (make-let (list (make-binding 'a 4)) (list '(display a) '(display "\n"))) the-global-environment))
;;;     ;;; (display (lookup-variable-value 'display the-global-environment))
;;;     (evl '(define (conss x y)
;;;         (lambda (m) (m x y))
;;;     ) the-global-environment)

;;;     (evl '(define (cars z)
;;;         (z (lambda (x y) x))
;;;     ) the-global-environment)

;;;     (evl '(define (cdrs z)
;;;         (z (lambda (x y) y))
;;;     ) the-global-environment)

;;;     ; Trying out


;;;     (evl '(let (
;;;         (z (conss 4 5))
;;;         ) 
;;;         (display (cars z))
;;;         (newline)
;;;         (display (cdrs z))
;;;         (newline)
;;;         ) the-global-environment)

;;; )