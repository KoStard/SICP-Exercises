;;; Implement 'and and 'or and install them
;;; Also implement as macros

(load "./chapter4/3.scm")

(define (install-and)
    (define tag 'and)
    (define (and? exp) (tagged-list? exp 'and))

    (define (predicates exp) (cdr exp))

    (define (expand-and preds)
        (if (null? preds)
            'true
            (let ((first (car preds)) (rest (cdr preds)))
                (make-if first (expand-and rest) 'false)
            )
        )
    )

    (define (eval-and exp env)
        (evl (expand-and exp) env)
    )
    (put-checker tag and?)
    (put-evaluator tag eval-and)
)

(define (install-or)
    (define tag 'or)
    (define (or? exp) (tagged-list? exp 'or))

    (define (predicates exp) (cdr exp))

    (define (expand-or preds)
        (if (null? preds)
            'false
            (let ((first (car preds)) (rest (cdr preds)))
                (make-if first 'true (expand-or rest))
            )
        )
    )

    (define (eval-or exp env)
        (evl (expand-or exp) env)
    )
    (put-checker tag or?)
    (put-evaluator tag eval-or)
)

(install-and)
(install-or)
