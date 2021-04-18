(load "./chapter4/lazy_evaluator/ch4-leval.scm")

(define (lazy-car z)
    (z (lambda (p q) p)))
(define (lazy-cdr z)
    (z (lambda (p q) q)))
(define (lazy-cons x y)
    (lambda (m) (m x y)))

(define primitive-procedures
  (list (list 'car lazy-car)
        (list 'cdr lazy-cdr)
        (list 'cons lazy-cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

(define (native->lazy-list l)
    (if (null? l)
        '()
        (lazy-cons (car l) (native->lazy-list (cdr l)))
        ; (list 'cons (car l) (native->lazy-list (cdr l)))
    )
)

(define (text-of-quotation exp) 
    (let ((content (cadr exp)))
        (if (and (pair? content) (not (null? content)))
            (native->lazy-list content)
            content
        )
    )
)

(define the-global-environment (setup-environment))

(define (run-exp exp)
    (eval exp the-global-environment)
)

(define (run-all seq)
    (run-exp (car seq))
    (if (not (null? (cdr seq))) 
        (run-all (cdr seq))
    ))


(run-all
    '(
        (display (car '(a b c)))
    )
)