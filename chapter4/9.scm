(load "./chapter4/8.scm")


;;; Syntax
;;; (do 
;;;     ...
;;; (while ...))
;;; 
;;; Will become 
;;; (let ((checker ...)
;;;        (evaluator ...))
;;;     (define (loop)
;;;         (evaluator)
;;;         (if (checker) (loop))
;;;     )
;;; )
(define (install-do-while)
    (define tag 'do)
    (define (matches? exp) (tagged-list? exp 'let))
    (define (statements exp) (cdr exp))
    (define (while-clause? statement) (and (pair? statement) (eq? (car statement) 'while)))

    (define (get-body exp)
        (let ((first (car exp)) (rest (cdr exp)))
            (cond 
                ((while-clause? first)
                    (if (not (null? rest))
                        '()
                        (error "The while clause is not at the end of the body -- DO-WHILE")
                    )
                )
                ((null? rest) (error "No while clause found -- DO-WHILE"))
                (else (cons first (get-body rest)))
            )
        )
    )

    ;;; The structure is checked in the get-body
    (define (get-while-clause exp)
        (let ((first (car exp)) (rest (cdr exp)))
            (if (while-clause? first) first (get-while-clause rest))
        )
    )

    (define (do-while->procedure exp)
        (let* ((body (get-body exp)) 
            (while-clause (get-while-clause exp))
            (body-evaluator (make-lambda '() body)))
            (make-let 
                (list 
                    (make-binding '__interpreter_internal_do_while_checker while-clause)
                    (make-binding '__interpreter_internal_do_while_evaluator body-evaluator)
                )
                (make-define '__interpreter_internal_do_while_loop 
                    (make-lambda '()
                        '( 
                            (__interpreter_internal_do_while_evaluator)
                            (if (__interpreter_internal_do_while_checker) 
                                (__interpreter_internal_do_while_loop))
                        )
                    )
                )
            )
        )
    )

    (define (eval-element exp env)
        (evl (do-while->procedure exp) env)
    )

    (put-checker tag matches?)
    (put-evaluator tag eval-element)
)
