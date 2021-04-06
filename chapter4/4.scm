;;; Implement 'and and 'or and install them
;;; Also implement as macros

(load "./chapter4/3.scm")

(define (install-and)
    (define tag 'and)
    (define (and? exp) (tagged-list? exp 'and))

    (define (predicates exp) (cdr exp))

    (define (eval-and exp env)
        (define (internal-loop exps)
            (if (null? exps)
                'true
                (if (evl (car exp)) (internal-loop (cdr exps)) 'false)
            )
        )
        (internal-loop (predicates exp))
    )
    (put-checker tag and?)
    (put-evaluator tag eval-and)
)

(define (install-or)
    (define tag 'or)
    (define (or? exp) (tagged-list? exp 'or))

    (define (predicates exp) (cdr exp))

    (define (eval-or exp env)
        (define (internal-loop exps)
            (if (null? exps)
                'false
                (if (evl (car exp)) 'true (internal-loop (cdr exps)))
            )
        )
        (internal-loop (predicates exp))
    )
    (put-checker tag or?)
    (put-evaluator tag eval-or)
)

(install-and)
(install-or)
